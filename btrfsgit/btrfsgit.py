#!/usr/bin/env python3

"""
BtrFsGit

ID5 mount points:
- due to kernel and btrfs limitations, you should use id5 mount points for the btrfs filesystems you want to work with.
 - there was previously a parameter for this, but now we expect a subvolume .bfg containing a file named id5
  - subvolume and not just a directory, because we need to avoid transferring it when we're transferring the root subvolume
  - the id5 file should contain the id5 mount point of the filesystem

the missing abstraction that would link a received snapshot to the (optional) target subvolume:
- a subvolume can be "checked out" on the other side
- for example, running: `bfg prune_remote --LOCAL_SUBVOL=/var --REMOTE_SUBVOL=/backup/var` needs to know that snapshots, of all in the filesystem, belong to /backup/var
- i don't want to make this tool rely on the database for this kind of info,
- but the only other way is to either introduce some kind of local metadata file mechanism somewhat equivalent to a local database
- or to rely on naming conventions, which is what we do here

snapshot naming conventions:
- a fix is needed to produce subdirectories even for the local case.
- the name itself should ideally be the full path (relative to id5 mount point), with slashes replaced by something you're not likely to find in a directory name (some unicode?), or the actual character should be escaped.
- the path should always be something like .bfg_snapshots/<subvol>/<snapshot>, to solve the previous point

pruning:
- pruning should work even when the remote machine is offline,
- to know which snapshots to keep, we need to know which snapshots are the most recent common snapshots (MRCS) between the local and each remote filesystem
- we also need to link each snapshot to the source or target subvol:
-- for local backups, which are local snapshots of local subvols, this is easy, with parent_uuid
-- for the snapshots transferred to remote machines, this is hard:
--- they have no inherent link to the intended target subvol
--- they have received_uuid, but that only tells us the uuid of the snapshot that was sent, and that snapshot can already be gone, so, no way to trace it back to the source subvol


- a database is used and should be automatically updated from both (all) machines after backups and before pruning
- if it's not updated, a snapshot that is really the Most Recent Common Snapshot (MRCS) will be deleted, and the next backup will have to transfer extra data.

implementation points:
- btrfs subvolume list omits the actual id5 subvol, so, the code has to work around this by making an extra btrfs sub show call (if my_uuid not in all_subvols2:...)


"""

import logging
from btrfsgit.bfg_logging import configure_logging
configure_logging()
logbtrfs = logging.getLogger('btrfs')
logbtrfs.setLevel(logging.WARNING)
logbfg = logging.getLogger('bfg')


from sqlalchemy.orm import undefer
from pathlib import Path
from pathvalidate import sanitize_filename
import sys, os
import time
import shutil
import subprocess
import fire
import shlex  # python 3.8 required (for shlex.join)
from typing import List, Optional
from .volwalker import *
from . import volwalker2
from collections import defaultdict
import re
from datetime import datetime
import btrfsgit.db as db


def datetime_to_json(o):
	"""
	json serialization for datetime objects
	"""
	if isinstance(o, datetime):
		return o.isoformat()
	elif isinstance(o, Path):
		return str(o)
	raise TypeError(f"Type {type(o)} not serializable")


def dash_is_none(string):
	if string == '-':
		return None
	else:
		return string


def try_unlink(f):
	try:
		os.unlink(f)
	except FileNotFoundError:
		pass


# exit code of the receive pipeline when the target series' in-flight-transfer
# marker (_series_lock_path) is already held: someone else is transferring this
# series right now, so the send is redundant and gets skipped, not failed.
# 75 = EX_TEMPFAIL, same "transient, try again later" convention backup.py uses.
SERIES_BUSY_EXIT = 75


def parse_size(size):
	"""'500G', '1.5T', '100MiB', '2TB' or plain bytes -> int bytes (binary units)"""
	if isinstance(size, (int, float)):
		return int(size)
	m = re.match(r'^([\d.]+)\s*([KMGTP]?)(I?B)?$', str(size).strip(), re.IGNORECASE)
	if m is None:
		raise ValueError(f'cannot parse size: {size!r}')
	unit = m.group(2).upper() if m.group(2) else ''
	return int(float(m.group(1)) * 1024 ** (' KMGTP'.index(unit) if unit else 0))


def _prerr(*args, sep=' ', **kwargs):
	message = sep.join(str(arg) for arg in args)
	logging.info(message, **kwargs)


class Res:
	"""helper class for passing results of Fire-invoked functions around and making sure they're printed understandably and machine-readably"""
	def __init__(s, value):
		s.val = value
	def __repr__(s):
		return json.dumps({'result':s.val})
	def __str__(s):
		return json.dumps({'result':s.val})


def prompt(question, dry_run=False):
		"""Ask a yes/no question via raw_input() and return their answer.

		"question" is a string that is presented to the user.
		"default" is the presumed answer if the user just hits <Enter>.
				It must be "yes" (the default), "no" or None (meaning
				an answer is required of the user).

		The "answer" return value is True for "yes" or False for "no".
		"""
		default = "yes"

		valid = {"yes": True, "y": True, "ye": True, "no": False, "n": False}
		if default is None:
			prompt = " [y/n] "
		elif default == "yes":
			prompt = " [Y/n] "
		elif default == "no":
			prompt = " [y/N] "
		else:
			raise ValueError("invalid default answer: '%s'" % default)

		while True:
			sys.stdout.write(question + prompt)
			if dry_run:
				sys.stdout.write('\n')
				return False
			choice = input().lower()
			sys.stdout.write('\n')
			if default is not None and choice == "":
				return valid[default]
			elif choice in valid:
				return valid[choice]
			else:
				sys.stdout.write("Please respond with 'yes' or 'no' " "(or 'y' or 'n').\n")


class Bfg:

	def __init__(s, sshstr='', YES=False):

		logbfg.debug(f'__init__...')

		# in current implementation, this should only ever hold one value, for the local fs we're working with
		s._local_fs_id5_mount_point = {}
		s._remote_fs_id5_mount_point = {}
		s._local_fs_uuid = {}

		s._yes_was_given_on_command_line = YES
		s._sshstr = sshstr
		# s._shush_ssh_stderr = shush_ssh_stderr # todo  # , SHUSH_SSH_STDERR=True
		if sshstr == '':
			s._remote_str = '(here)'
		else:
			s._remote_str = '(on the other machine)'
		s._local_str = '(here)'
		s._sudo = ['sudo']
		s.host = subprocess.check_output(['hostname'], text=True).strip()


	def _yes(s, msg, dry_run=False):
		"""
		interactive confirmation prompt for dangerous operations
		"""
		if s._yes_was_given_on_command_line:
			return True
		return prompt(msg, dry_run)

	"""

	helper functions for running subprocessess locally and over ssh

	"""

	def _remote_cmd(s, cmd, die_on_error=True, logger=None, capture_stderr=False):
		"""potentially remote command"""
		if logger is None:
			logger = logging.getLogger('btrfs')
		if not isinstance(cmd, list):
			cmd = shlex.split(cmd)
		else:
			cmd = [str(x) for x in cmd]
		if s._sshstr != '':
			ssh = shlex.split(s._sshstr)
			cmd2 = ssh + s._sudo + cmd
			logger.debug(shlex.join(cmd2))
			return s._cmd(cmd2, die_on_error, capture_stderr=capture_stderr)
		else:
			return s._local_cmd(cmd, die_on_error, capture_stderr=capture_stderr)


	def _local_cmd(s, c, die_on_error=True, logger=None, capture_stderr=False):
		if logger is None:
			logger = logging.getLogger('btrfs')
		if not isinstance(c, list):
			c = shlex.split(c)
		c = s._sudo + [str(x) for x in c]
		logger.debug(shlex.join(c))
		return s._cmd(c, die_on_error, capture_stderr)


	def _cmd(s, c, die_on_error, capture_stderr=False):
		"""
		capture_stderr: don't let stderr go to the terminal. This is used for commands expected to fail, like using cp --reflink to detect filesystem boundaries.
		"""
		try:
			if capture_stderr:
				stderr = subprocess.STDOUT
			else:
				stderr = None
			result = subprocess.check_output(c, text=True, stderr=stderr)
			# fixme: use popen so that we can capture stderr and log it even on failure
			logbfg.debug(result)
			return result
		except subprocess.CalledProcessError as e:
			if die_on_error:
				_prerr(e)
				exit(1)
			else:
				return -1



	"""
	determine id5 mount point
	"""

	def local_fs_id5_mount_point(self, subvolume):
		subvolume = str(subvolume)
		# if subvolume not in self._local_fs_id5_mount_points:
		# 	self._local_fs_id5_mount_points[subvolume] = self.find_local_fs_id5_mount_point(subvolume)
		# return self._local_fs_id5_mount_points[subvolume]
		if self._local_fs_id5_mount_point == {}:
			self._local_fs_id5_mount_point = self.find_local_fs_id5_mount_point(subvolume)
		return self._local_fs_id5_mount_point


	def remote_fs_id5_mount_point(self, subvolume):
		subvolume = str(subvolume)
		# if subvolume not in self._remote_fs_id5_mount_points:
		# 	self._remote_fs_id5_mount_points[subvolume] = self.find_remote_fs_id5_mount_point(subvolume)
		# return self._remote_fs_id5_mount_points[subvolume]
		if self._remote_fs_id5_mount_point == {}:
			self._remote_fs_id5_mount_point = self.find_remote_fs_id5_mount_point(subvolume)
		return self._remote_fs_id5_mount_point


	def find_local_fs_id5_mount_point(s, subvolume):
		dir = Path(subvolume)
		while True:
			try:
				fn = dir / '.bfg' / 'id5'
				logbfg.debug(f'find_local_fs_id5_mount_point: {fn=}')
				with open(fn, 'r') as f:
					return Path(f.read().strip())
			except FileNotFoundError:
				new_dir = dir.parent
				if new_dir == dir:
					raise Exception(f'could not find id5 for local {subvolume}')
				dir = new_dir


	def find_remote_fs_id5_mount_point(s, subvolume):
		dir = Path(subvolume)
		while True:
			r = s._remote_cmd(['cat', dir / '.bfg' / 'id5'], die_on_error=False, capture_stderr=True)
			if r != -1:
				return Path(r.strip())
			new_dir = dir.parent
			if new_dir == dir:
				raise Exception(f'could not find id5 for remote {subvolume}, id5 file missing?')
			dir = new_dir



	"""
	low-level btrfs stuff
	"""


	def _get_subvolumes(s, command_runner, subvolume, src):
		"""
		:param subvolume: filesystem path to a subvolume on the filesystem that we want to get a list of subvolumes of
		:return: list of records, one for each subvolume on the filesystem
		"""
		subvols = []
		logger = logging.getLogger('_get_subvolumes')

		if src == 'local':
			fs = s.local_fs_id5_mount_point(subvolume)
			fs_uuid = s.local_fs_uuid(subvolume)
			host = s.host
		else:
			fs = s.remote_fs_id5_mount_point(subvolume)
			fs_uuid = s.remote_fs_uuid(subvolume)[0]
			host = s._remote_cmd('hostname').strip()

		cmd = ['btrfs', 'subvolume', 'list', '-q', '-t', '-R', '-u']
		for line in command_runner(cmd + [subvolume], logger=logbtrfs).splitlines()[2:]:
			subvol = s._make_snapshot_struct_from_sub_list_output_line(fs, line)
			subvol['src'] = src + '_btrfs'
			logger.debug(subvol)
			subvols.append(subvol)

		ro_subvols = set()
		for line in command_runner(cmd + ['-r', subvolume], logger=logbtrfs).splitlines()[2:]:
			subvol = s._make_snapshot_struct_from_sub_list_output_line(fs, line)
			ro_subvols.add(subvol['local_uuid'])
		# _prerr(str(ro_subvols))

		for i in subvols:
			i['ro'] = i['local_uuid'] in ro_subvols
			# a live listing by definition contains no deleted subvols; the key exists so
			# these records compose with db rows in deleted-aware code (walkers, filters)
			i['deleted'] = False
			i['host'] = host
			i['fs_uuid'] = fs_uuid
			if '.bfg_snapshots' in i['path'].parts:
				# an unparseable name is not a bfg snapshot (manual rename, foreign tool);
				# it must not kill every operation on the filesystem - warn and move on,
				# leaving the record without 'dt' (snapshot-handling code skips those)
				try:
					i['dt'] = s.snapshot_dt(i)
				except ValueError:
					logbfg.warning(f"ignoring subvol with unparseable snapshot name: {i['path']}")

		subvols.sort(key=lambda sv: -sv['subvol_id'])
		logbfg.debug(f'_get_subvolumes: {len(subvols)=}')
		return subvols


	def _make_snapshot_struct_from_sub_list_output_line(s, fs, line):
		#logging.debug('line:'+line)
		items = line.split()
		subvol_id = items[0]
		parent_uuid = dash_is_none(items[3])
		received_uuid = dash_is_none(items[4])
		local_uuid = items[5]

		snapshot = {}
		snapshot['received_uuid'] = received_uuid
		snapshot['parent_uuid'] = parent_uuid
		snapshot['local_uuid'] = local_uuid
		snapshot['subvol_id'] = int(subvol_id)
		snapshot['path'] = fs / items[6]
		logging.debug(snapshot)

		return snapshot


	def local_fs_uuid(self, subvol):
		subvol = str(subvol)
		# if subvol not in self._local_fs_uuid:
		# 	self._local_fs_uuid[subvol] = self.get_fs_uuid(subvol)
		# # check that all values are the same, because we should only be working with one local filesystem
		# if not all([x == self._local_fs_uuid[subvol] for x in self._local_fs_uuid.values()]):
		# 	raise Exception(f'weird, local_fs_uuids are not the same: {self._local_fs_uuid}')
		# return self._local_fs_uuid[subvol]
		if self._local_fs_uuid == {}:
			self._local_fs_uuid = self.get_fs_uuid(subvol)
		return self._local_fs_uuid


	def fs_uuid_from_fs_show_output(self, output):
		line = output.splitlines()[0]
		r = r"Label:\s+.*\s+uuid:\s+([a-f0-9-]+)$"
		fs_uuid = re.match(r, line).group(1)
		logbfg.debug(f'get_fs: {fs_uuid=}')
		return fs_uuid


	def remote_fs_uuid(s, subvol):
		logbfg.debug(f'remote_fs_uuid {subvol=}')
		mp = s.remote_fs_id5_mount_point(subvol)
		uuid = s.fs_uuid_from_fs_show_output(s._remote_cmd(f'btrfs filesystem show ' + str(mp)))
		return uuid, mp


	def get_fs_uuid(s, subvol):
		logbfg.debug(f'get_fs_uuid {subvol=}')
		return s.fs_uuid_from_fs_show_output(s._local_cmd(f'btrfs filesystem show ' + str(s.local_fs_id5_mount_point(subvol))))


	def get_subvol(s, runner, path):
		out = runner(f'btrfs sub show {path}')
		lines = out.splitlines()

		sv = {}
		sv['received_uuid'] = dash_is_none(lines[4].split()[2])
		sv['parent_uuid'] = dash_is_none(lines[3].split()[2])
		sv['local_uuid'] = lines[2].split()[1]
		sv['subvol_id'] = int(lines[6].split()[2])
		sv['ro'] = lines[11].split()[1] == 'readonly'
		sv['src'] = 'btrfs_sub_show'

		r = Res(sv)
		logbtrfs.debug('get_subvol: %s', str(sv))
		return r


	"""
	db stuff
	"""

	def update_db(s, FS):
		"""
		blast the db with all the subvols we can find on the filesystem.
		"""
		with db.advisory_lock():
			s._update_db(FS)

	def _update_db(s, FS):
		snapshots = s.get_all_subvols_on_filesystem(FS).val
		logbfg.debug(f'db.session()...')
		session = db.session()
		with session.begin():
			logbfg.debug(f'got db session...')

			logbfg.info(f'updating db with current list of snapshots on {FS}...')

			logbfg.debug(f'purge db of all snapshots with fs_uuid={s.local_fs_uuid(FS)}...')
			session.query(db.Snapshot).filter(db.Snapshot.fs_uuid == s.local_fs_uuid(FS)).delete()

			logbfg.debug(f'insert snapshots into db...')
			for i,snapshot in enumerate(snapshots):
				if i % 100 == 0:
					logbfg.debug(f'{i=}')
				logbfg.debug(f'{snapshot=}')
				db_snapshot = db.Snapshot(
					id=snapshot['fs_uuid']+'_'+snapshot['local_uuid'],
					local_uuid=snapshot['local_uuid'],
					parent_uuid=snapshot['parent_uuid'],
					received_uuid=snapshot['received_uuid'],
					host=snapshot['host'],
					fs=str(FS),
					path=str(snapshot['path']),
					fs_uuid=snapshot['fs_uuid'],
					subvol_id=snapshot['subvol_id'],
					ro=snapshot['ro'],
				)
				session.add(db_snapshot)
			logbfg.debug(f'commit...')


	def all_subvols_from_db(s):
		logbfg.debug(f'all_snapshots_from_db...')
		session = db.session()
		with session.begin():
			logbfg.debug(f'got db session.')
			logbfg.debug(f'query all snapshots from db...')
			all = list(session.query(db.Snapshot).options(undefer("*")).all())

			r = [{
				column.name: getattr(x, column.name)
				for column in x.__table__.columns} for x in all]

			for x in r:
				x['path'] = Path(x['path'])
				if '.bfg_snapshots' in x['path'].parts:
					# see _get_subvolumes: unparseable names are warned about, not fatal
					try:
						x['dt'] = s.snapshot_dt(x)
					except ValueError:
						logbfg.warning(f"ignoring db row with unparseable snapshot name: {x['path']}")
				x['src'] = 'db'

			logbfg.debug(f'got {len(r)} snapshots from db.')
			return r


	def remote_fs_uuids(s, all, subvol):
		""" remote fs uuids by db """
		logbfg.debug(f'remote_fs_uuids...')
		fss = {}
		for snap in all:
			snap_fs_uuid = snap['fs_uuid']
			if snap_fs_uuid not in fss:
				fss[snap_fs_uuid] = {'hosts': set()}
			fss[snap_fs_uuid]['hosts'].add(snap['host'])
		del fss[s.local_fs_uuid(subvol)]
		return fss



	"""
	helper bfg stuff
	"""

	def bucket(s, dt: datetime, now: datetime) -> str:
		age_seconds = (now - dt).total_seconds()

		if age_seconds < 60:
			return "under-1-min"

		elif age_seconds < 3600:
			# Bucket by minute
			return dt.strftime("minute-%Y_%m_%d_%H_%M")

		elif age_seconds < 86400:
			# Bucket by hour
			return dt.strftime("hour-%Y_%m_%d_%H")

		elif age_seconds < 2592000:
			# ~30 days
			return dt.strftime("day-%Y_%m_%d")

		else:
			return dt.strftime("month-%Y_%m")  # year-month



	def put_snapshots_into_buckets(s, snapshots):
		"""
		Group snapshots by bucket.
		"""
		grouped = defaultdict(list)
		now = datetime.now()

		for snap in snapshots:
			dt = snap['dt']
			b = s.bucket(dt, now)
			grouped[b].append(snap)

		for bucket, snaplist in grouped.items():
			snaplist.sort(key=lambda s: s['dt'])

		return grouped


	def _figure_out_snapshot_name(s, SUBVOL, TAG, SNAPSHOT, SNAPSHOT_NAME):
		if TAG and SNAPSHOT:
			_prerr(f'please specify SNAPSHOT or TAG, not both')
			sys.exit(-1)
		if TAG and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or TAG, not both')
			sys.exit(-1)
		if SNAPSHOT and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or SNAPSHOT, not both')
			sys.exit(-1)

		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path('local', SUBVOL, TAG, SNAPSHOT_NAME).val
		return SNAPSHOT


	def parse_snapshot_name(s, dname):
		"""
		parse a snapshot name into its parts: {'name': <series/subvol name>, 'dt': <datetime>,
		'tags': <tag>}. Typical patterns:
		  <subvol>_bfg_snapshots_<timestamp>_<tag>   (legacy)
		  <subvol>_<timestamp>_<tag>
		"""
		m = re.match(r'(.+)_bfg_snapshots_(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2})_(.*)', dname)
		if m is None:
			m = re.match(r'(.+)_(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2})_(.*)', dname)
		if m is None:
			raise ValueError(f'could not parse snapshot folder: {dname}')
		return {'name': m.group(1),
				'dt': datetime.strptime(m.group(2), "%Y-%m-%d_%H-%M-%S"),
				'tags': m.group(3)}


	def snapshot_dt(s, snapshot):
		logbfg.debug(f'snapshot_dt {snapshot=}')
		return s.parse_snapshot_name(snapshot['path'].name)['dt']


	def calculate_default_snapshot_parent_dir(s, machine: str, SUBVOL):
		"""
		fixme: in fact calculates also the base of the actual directory name now.

		SUBVOL: your subvolume (for example /data).
		Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots/data` (snapshots then become `/.bfg_snapshots/data_<ts>_<tag>`), if that is still the same filesystem. For the root subvolume `/` the name `__root` is used: `/.bfg_snapshots/__root_<ts>_<tag>`.
		"""
		SUBVOL = Path(SUBVOL)
		if SUBVOL.anchor == '//':
			# POSIX allows pathlib to keep an exactly-double leading slash; on Linux it means '/'.
			SUBVOL = Path('/', *SUBVOL.parts[1:])
		parent = SUBVOL.parent

		logger = logging.getLogger('calculate_default_snapshot_parent_dir')

		logger.debug(f'calculate_default_snapshot_parent_dir for {SUBVOL=}')

		# is parent the same filesystem as SUBVOL? if not, then SUBVOL is the top level subvolume, and we need to make the snapshot inside it, rather than outside.

		if machine == 'local':
			runner = s._local_cmd
		else:
			runner = s._remote_cmd

		if runner(['test', '-e', str(SUBVOL)], die_on_error=False, logger=logger) == -1:
			# we assume that if the target filesystem is mounted. This implies that if we're transferring the root subvol, the directory exists. This is the only case where the snapshot parent dir will be inside the subvol, rather than outside. Therefore, if the destination does not exist (as a directory or subvolume), it is safe to assume that it is not the root subvolume.
			snapshot_parent_dir = parent
		else:

			runner(['mkdir', '-p', str(SUBVOL)], logger=logger)

			f1 = SUBVOL / '.bfg_touch'
			f2 = parent / '.bfg_touch_reflink_test'
			if runner(['test', '-e', str(f1)], die_on_error=False, logger=logger) == -1:
				runner(['touch', str(f1)], logger=logger)

			if runner(['cp', '--reflink', f1, f2], die_on_error=False, logger=logger, capture_stderr=True) != -1:
				snapshot_parent_dir = parent
				runner(['rm', f2])
			else:
				logbfg.debug(
					f'cp --reflink failed, this means that {parent} is not the same filesystem, going to make snapshot inside {SUBVOL} instead of {parent}')
				snapshot_parent_dir = SUBVOL

		# For the root subvolume, Path('/').parts[-1] is '/' itself. Concatenated as a string it
		# collapsed the '.bfg_snapshots/' directory component into a bare name prefix, so root
		# snapshots ended up as top-level entries of / (//.bfg_snapshots_<ts>_<tag>): invisible to
		# the '.bfg_snapshots' in path.parts filters (never pruned, never usable as a send parent)
		# and bind-mounted one by one by Flatpak sandboxes, which then pinned them after deletion.
		# Name the root series '__root', matching how backup targets name the received root subvol.
		name = SUBVOL.parts[-1]
		if not name.strip('/'):
			name = '__root'
		r = str((Path(snapshot_parent_dir) / '.bfg_snapshots' / name).absolute())
		logging.getLogger('utils').debug(f'calculate_default_snapshot_parent_dir: {SUBVOL=} -> {r=}')
		return Res(r)


	def calculate_default_snapshot_path(s, machine, SUBVOL, TAG, NAME_OVERRIDE=None):  # , TAG2):
		"""
		calculate the filesystem path where a snapshot should go, given a subvolume and a tag
		"""
		parent = s.calculate_default_snapshot_parent_dir(machine, SUBVOL).val

		if NAME_OVERRIDE is not None:
			name = NAME_OVERRIDE
		else:

			tss = time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime())
			# tss = subprocess.check_output(['date', '-u', "+%Y-%m-%d_%H-%M-%S"], text=True).strip()
			ts = sanitize_filename(tss.replace(' ', '_'))

			if TAG is None:
				TAG = 'from_' + s.host
			name = ts + '_' + TAG

		res = Res(str(Path(str(parent) + '_' + name)))
		return res



	"""

	high-level, compound commands

	"""

	def commit_and_push_and_checkout(s, SUBVOL, REMOTE_SUBVOL, PARENT: str = None):
		"""
		Snapshot your data, "btrfs send"/"btrfs receive" the snapshot to the other machine, and checkout it there
		:param SUBVOL: your data
		:param REMOTE_SUBVOL: desired filesystem path of your data on the other machine
		:return: filesystem path of the snapshot created on the other machine
		"""
		remote_snapshot_path = s.commit_and_push(SUBVOL, REMOTE_SUBVOL, PARENT=PARENT).val
		if remote_snapshot_path is None:
			return Res(None)  # push skipped (series transfer already in flight)
		s.checkout_remote(remote_snapshot_path, REMOTE_SUBVOL)
		return Res(REMOTE_SUBVOL)


	def remote_commit_and_pull(s, REMOTE_SUBVOL, SUBVOL):
		"""
		same as commit_and_push_and_checkout but going the other direction

		:param FS_ROOT_MOUNT_POINT:
		:param REMOTE_SUBVOL:
		:param SUBVOL:
		:return:
		"""
		remote_snapshot_path = s.remote_commit(REMOTE_SUBVOL).val
		local_snapshot_path = s.pull(remote_snapshot_path, SUBVOL).val
		if local_snapshot_path is None:
			return Res(None)  # pull skipped (series transfer already in flight)
		s.checkout_local(local_snapshot_path, SUBVOL)
		_prerr(f'DONE, \n\tpulled {remote_snapshot_path} \n\tinto {SUBVOL}\n.')
		return Res(SUBVOL)


	def commit_and_generate_patch(s, SUBVOL='/', PATCH_FILE_DIR='/', PARENT: Optional[str]=None):
		"""
		store a `btrfs send` stream locally

		:param SUBVOL:
		:param PATCH_FILE_DIR:
		:param PARENTS:
		:return:
		"""
		snapshot = s.local_commit(SUBVOL).val
		# print(Path(snapshot).parts[-2:])
		fn = PATCH_FILE_DIR + '/' + '__'.join(Path(snapshot).parts[-2:])
		# print(fn)
		s.local_send(snapshot, ' > ' + fn, PARENT)
		_prerr(f'DONE, generated patch \n\tfrom {snapshot} \n\tinto {fn}\n.')
		return Res(fn)


	def commit_and_push(s, SUBVOL, REMOTE_SUBVOL, SNAPSHOT_TAG=None, SNAPSHOT_PATH=None, SNAPSHOT_NAME=None,
						PARENT=None, CLONESRCS: List[str] = []):
		"""commit, and transfer the snapshot into .bfg_snapshots on the other machine"""
		snapshot = s.local_commit(SUBVOL, SNAPSHOT_TAG, SNAPSHOT_PATH, SNAPSHOT_NAME).val
		return Res(s.push(SUBVOL, snapshot, REMOTE_SUBVOL, PARENT, CLONESRCS).val)



	"""
	basic commands
	"""



	def get_local_bfg_snapshots(s, SUBVOL):
		"""list snapshots in .bfg_snapshots"""
		logger = logging.getLogger('get_local_bfg_snapshots')
		logger.debug('get_local_bfg_snapshots...')
		local_snapshots = s.get_local_snapshots(SUBVOL).val
		result = []
		for snapshot in local_snapshots:
			logger.debug(f'{snapshot=}')
			if '.bfg_snapshots' in snapshot['path'].parts:
				if 'dt' not in snapshot:
					# name did not parse in _get_subvolumes (already warned): not a bfg snapshot
					continue
				logger.debug(f'YES')
				result.append(snapshot)
		logbfg.debug(f'get_local_bfg_snapshots_for_subvol: {len(result)=}')
		return Res(result)


	def get_local_snapshots(s, SUBVOL):
		"""list snapshots of SUBVOL on the local machine, that is, all read-only subvolumes on the filesystem, that are children of SUBVOL"""
		logbtrfs.debug(f'get_local_snapshots...')
		logger = logging.getLogger('get_local_snapshots')
		uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']
		subvols = s._get_subvolumes(s._local_cmd, SUBVOL, 'local')
		logger.debug(f'{subvols=}')

		snapshots = []
		for subvol in subvols:
			logger.debug(f'{subvol=}')
			if subvol['parent_uuid'] == uuid and subvol['ro']:
				logger.debug(f'YES')
				snapshots.append(subvol)
			else:
				logger.debug(f'NO')

		logbtrfs.debug(f'get_local_snapshots: {len(snapshots)=}')
		return Res(snapshots)


	def get_all_subvols_on_filesystem(s, subvol):
		"""list all subvolumes on the filesystem"""
		logbfg.debug(f'get_all_subvols_on_filesystem...')
		logger = logging.getLogger('get_all_subvols_on_filesystem')
		subvols = s._get_subvolumes(s._local_cmd, s.local_fs_id5_mount_point(subvol), 'local')
		logger.debug(f'{subvols=}')

		snapshots = []
		for subvol in subvols:
			logger.debug(f'{subvol=}')
			subvol['fs_uuid'] = s._local_fs_uuid
			subvol['id'] = subvol['fs_uuid'] + '_' + subvol['local_uuid']
			subvol['host'] = s.host

		logbtrfs.debug(f'get_all_subvols_on_filesystem: {len(subvols)=}')
		return Res(subvols)



	def get_local_subvolumes(s, SUBVOL):
		"""list subvolumes on the local machine"""
		return Res(s._get_subvolumes(s._local_cmd, SUBVOL), 'local')



	def checkout_local(s, SNAPSHOT, SUBVOL):
		"""stash your SUBVOL, and replace it with SNAPSHOT"""
		s.stash_local(SUBVOL)
		s._local_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOL}')
		_prerr(f'DONE {s._local_str}, \n\tchecked out {SNAPSHOT} \n\tinto {SUBVOL}\n.')
		return Res(SUBVOL)



	def checkout_remote(s, SNAPSHOT, SUBVOL):
		"""ssh into the other machine,
		stash your SUBVOL, and replace it with SNAPSHOT"""
		s.stash_remote(SUBVOL)
		s._remote_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOL}')
		_prerr(f'DONE {s._remote_str}, \n\tchecked out {SNAPSHOT} \n\tinto {SUBVOL}\n.')
		return Res(SUBVOL)



	def stash_local(s, SUBVOL, SNAPSHOT_TAG='stash', SNAPSHOT_NAME=None):
		"""
		snapshot and delete your SUBVOL

		todo: maybe an alternative way should be to just move it?
		"""
		if s._local_cmd(['ls', SUBVOL], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._local_str}, {SUBVOL} doesn\'t exist.')
			return None
		else:
			snapshot = s._local_make_ro_snapshot(SUBVOL,
												 s.calculate_default_snapshot_path('local', SUBVOL, SNAPSHOT_TAG,
																				   SNAPSHOT_NAME).val)

			cmd = f'btrfs subvolume delete {SUBVOL}'
			if not s._yes(cmd):
				exit(1)
			s._local_cmd(cmd)
			_prerr(f'DONE {s._local_str}, \n\tsnapshotted {SUBVOL} into \n\t{snapshot}\n, and deleted it.')
			return Res(snapshot)



	def stash_remote(s, SUBVOL):
		"""snapshot and delete your SUBVOL"""
		if s._remote_cmd(['test', '-e', SUBVOL], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._remote_str}, {SUBVOL} doesn\'t exist.')
			return None
		else:
			_prerr(f'gonna stash {s._remote_str}, {SUBVOL}.')
			snapshot = s._remote_make_ro_snapshot(SUBVOL,
												  s.calculate_default_snapshot_path('remote', Path(SUBVOL),
																					'stash_before_remote_checkout').val)

			cmd = f'btrfs subvolume delete {SUBVOL}'
			if not s._yes(cmd):
				exit(1)
			s._remote_cmd(cmd)

			_prerr(f'DONE {s._remote_str}, \n\tsnapshotted {SUBVOL} \n\tinto {snapshot}\n, and deleted it.')
			return Res(snapshot)


	def local_commit(s, SUBVOL='/', TAG=None, SNAPSHOT=None, SNAPSHOT_NAME=None):
		"""
		come up with a filesystem path for a snapshot, and snapshot SUBVOL.
		:param SNAPSHOT: override default filesystem path where snapshot will be created
		:param TAG: override the tag for the default SNAPSHOT (hostname by default)
		"""
		SUBVOL = Path(SUBVOL).absolute()
		SNAPSHOT = s._figure_out_snapshot_name(SUBVOL, TAG, SNAPSHOT, SNAPSHOT_NAME)
		s._local_make_ro_snapshot(SUBVOL, SNAPSHOT)
		return Res(SNAPSHOT)



	def prune_local(s, SUBVOL, DB=True, DRY_RUN=False):
		"""
		Prune old snapshots under SUBVOL according to a time-based retention policy.

		1) Keep the oldest snapshot.
		2) Keep the newest snapshot.
		3) For snapshots < 1 minute old, keep only the most recent in that window.
		4) For snapshots < 1 hour old, keep one per minute.
		5) For snapshots < 1 day old, keep one per hour.
		6) For snapshots < 1 month old (~30 days), keep one per day.
		7) For snapshots >= 1 month old, keep one per month.
		8) Delete everything else.
		"""
		with db.advisory_lock():
			s._prune_local(SUBVOL, DB, DRY_RUN)

	def _prune_local(s, SUBVOL, DB, DRY_RUN):

		logbfg.info(f"Pruning snapshots for {SUBVOL=}")
		logbfg.debug(f'{DB=} {DRY_RUN=}')

		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']

		if DB:
			all = s.all_subvols_from_db()
			mrcs = set([x['path'] for x in s.most_recent_common_snapshots(all, SUBVOL)])
			logbfg.debug(f"{mrcs=}")
		else:
			mrcs = set()

		# use the live list of snapshots (not the db) so we only ever try to delete
		# snapshots that actually still exist - the db lags behind concurrent backup runs
		local_snapshots = s.get_local_bfg_snapshots(SUBVOL).val
		local_snapshots = sorted(local_snapshots, key=lambda x: x['dt'])

		if len(local_snapshots) == 0:
			logbfg.info(f"No snapshots to check for {SUBVOL}")
			return

		newest = local_snapshots[-1]['path']

		s._delete_prunable(s._prune_decisions(local_snapshots, mrcs, newest), DRY_RUN, s._local_cmd)


	def _delete_prunable(s, decisions, DRY_RUN, runner):
		"""log each retention decision and delete the prunable snapshots (shared by
		_prune_local, _prune_remote and the series commands - they differ only in the runner)"""
		for d in decisions:
			path = d['snap']['path']
			logbfg.info(f"  {path} - {d['reason']}")

			if d['prunable'] and not DRY_RUN:
				cmd = ['btrfs', 'subvolume', 'delete', str(path)]
				if not s._yes(shlex.join(cmd)):
					continue
				if runner(cmd, die_on_error=False) == -1:
					# e.g. already deleted by a concurrently running backup's prune
					logbfg.warning(f"could not delete {path} (deleted concurrently?), continuing")
					continue
				# flag each deletion immediately: a crash mid-loop must not leave the
				# already-deleted snapshots as unflagged phantom rows in the db
				s._mark_deleted_in_db([d['snap'].get('local_uuid')])
				_prerr(f"Deleted snapshot: {path}")

		_prerr("Done pruning.")


	def _mark_deleted_in_db(s, local_uuids):
		"""
		Best-effort: flag just-deleted snapshots in the db, so that other machines
		computing shared parents between two update_db runs don't base their
		decisions on phantom rows.
		"""
		local_uuids = [u for u in local_uuids if u]
		if not local_uuids:
			return
		try:
			db.mark_deleted(local_uuids)
			logbfg.debug(f'marked {len(local_uuids)} snapshot(s) deleted in db')
		except Exception as e:
			logbfg.warning(f'could not mark {len(local_uuids)} deleted snapshot(s) in db: {e}')


	def _prune_decisions(s, local_snapshots_sorted, mrcs, newest):
		"""
		Decide, per snapshot, whether prune_local's time-based retention policy would delete it,
		and why. This is the single source of truth shared by _prune_local (which acts on it) and
		report_local (which only displays it).

		Policy: bucket snapshots by age, keep the newest snapshot in each bucket, keep the overall
		newest, and keep every most recent common snapshot (shared parent). Everything else is
		prunable - including snapshots newer than a shared parent, which get thinned by the bucket
		policy just like the rest. That is safe: a snapshot newer than a remote's shared parent is
		by definition not on that remote, so it can never be that remote's `btrfs send -p` parent;
		the shared parent itself is kept (it is in mrcs) and remains available for incrementals.

		Returns a list of {'snap', 'prunable', 'reason'} in processing order (oldest first).
		"""
		decisions = []
		buckets = s.put_snapshots_into_buckets(local_snapshots_sorted)

		for bucket, snaplist in buckets.items():
			for i, snap in enumerate(snaplist):
				path = snap['path']

				is_newest = path == newest
				is_mrc = path in mrcs
				is_last = i == len(snaplist) - 1

				if is_mrc:
					decisions.append({'snap': snap, 'prunable': False, 'reason': 'keep (shared parent)'})
				elif is_newest:
					decisions.append({'snap': snap, 'prunable': False, 'reason': 'keep (newest)'})
				elif is_last:
					decisions.append({'snap': snap, 'prunable': False, 'reason': f'keep (newest in {bucket})'})
				else:
					decisions.append({'snap': snap, 'prunable': True, 'reason': f'prune (extra in {bucket})'})

		return decisions


	def clean_local(s, SUBVOL, PERCENT=30, DB=True, DRY_RUN=False):
		"""
		Aggressively clean old snapshots under SUBVOL.

		Unlike prune_local, which keeps a time-bucketed spread of snapshots according to a
		retention policy, clean_local simply deletes the oldest PERCENT% of snapshots. It
		refuses to delete only the snapshots that are critical for future incremental sends:

		1) the most recent common snapshot (MRCS) shared with each remote filesystem - these
		   are needed as a `btrfs send -p` parent so the next backup doesn't have to resend
		   everything (this is the "shared parent" that a future shared-parent search relies on),
		2) the single newest local snapshot - the latest restore point and the likely basis for
		   the next commit.

		Everything else within the oldest PERCENT% is fair game.

		:param SUBVOL: the subvolume whose snapshots to clean
		:param PERCENT: how many of the oldest snapshots to consider for cleaning, as a
			percentage of the total number of snapshots (default 30)
		:param DB: use the shared database to figure out which snapshots are shared with remotes.
			Without it (DB=False) no snapshot can be recognised as a shared parent, so only the
			newest is protected - dangerous, as it may delete a snapshot still needed as a parent.
		:param DRY_RUN: only report what would be deleted, delete nothing
		"""
		with db.advisory_lock():
			s._clean_local(SUBVOL, PERCENT, DB, DRY_RUN)

	def _clean_local(s, SUBVOL, PERCENT, DB, DRY_RUN):

		logbfg.info(f"Cleaning snapshots for {SUBVOL=} (oldest {PERCENT}%)")
		logbfg.debug(f'{DB=} {DRY_RUN=}')

		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']

		# the snapshots we must never delete: the most recent snapshot shared with each remote
		# filesystem, i.e. the ones a future shared-parent search would pick. protected maps
		# snapshot path -> list of "host:fs" labels it is a shared parent for.
		protected = s._shared_parents(SUBVOL) if DB else {}
		if not DB:
			logbfg.warning("clean_local with DB=False: cannot identify shared parents; "
						   "nothing will be protected except the newest snapshot!")

		# use the live list of snapshots (not the db) so we only ever try to delete snapshots
		# that actually still exist - e.g. after a prune has just run in the same pipeline.
		local_snapshots = s.get_local_bfg_snapshots(SUBVOL).val
		local_snapshots = sorted(local_snapshots, key=lambda x: x['dt'])

		if len(local_snapshots) == 0:
			logbfg.info(f"No snapshots to clean for {SUBVOL}")
			return

		s._clean_snapshots(local_snapshots, protected, PERCENT, DRY_RUN)


	def _clean_snapshots(s, snapshots, protected, PERCENT, DRY_RUN):
		"""
		shared core of clean_local/clean_snapshots/clean_fs: delete the oldest PERCENT% of
		`snapshots` (a non-empty list sorted oldest first), sparing the protected ones
		(a map of path -> list of "host:fs" labels they are shared with) and the newest.
		"""
		PERCENT = float(PERCENT)
		if PERCENT < 0 or PERCENT > 100:
			_prerr(f'PERCENT must be between 0 and 100, got {PERCENT}')
			sys.exit(1)

		logbfg.info(f"protecting {len(protected)} shared parent snapshot(s):")
		for p, labels in protected.items():
			logbfg.info(f"  keep (shared parent -> {'; '.join(labels)}): {p}")

		n = len(snapshots)
		# always keep the newest snapshot
		newest = snapshots[-1]['path']

		count_to_clean = int(n * PERCENT / 100.0)
		logbfg.info(f"{n} snapshot(s) total, considering the oldest {count_to_clean} for cleaning")

		deleted = 0
		for i, snap in enumerate(snapshots):
			if i >= count_to_clean:
				break
			path = snap['path']

			is_newest = path == newest
			is_protected = path in protected
			is_cleanable = not is_newest and not is_protected

			flags = ''
			if is_protected:
				flags += f" (shared parent -> {'; '.join(protected[path])} - keep)"
			if is_newest:
				flags += ' (newest - keep)'
			if is_cleanable:
				flags += ' (cleanable)'
			logbfg.info(f"  {path}{flags}")

			if is_cleanable and not DRY_RUN:
				cmd = ['btrfs', 'subvolume', 'delete', str(path)]
				if not s._yes(shlex.join(cmd)):
					continue
				if s._local_cmd(cmd, die_on_error=False) == -1:
					# e.g. already deleted by a concurrently running backup's prune
					logbfg.warning(f"could not delete {path} (deleted concurrently?), continuing")
					continue
				deleted += 1
				# flag each deletion immediately: a crash mid-loop must not leave the
				# already-deleted snapshots as unflagged phantom rows in the db
				s._mark_deleted_in_db([snap.get('local_uuid')])
				_prerr(f"Deleted snapshot: {path}")

		_prerr(f"Done cleaning. Deleted {deleted} snapshot(s).")


	def _shared_parents(s, SUBVOL):
		"""
		Map snapshot path -> list of "host:fs" labels for which that snapshot is the most recent
		common snapshot (shared parent). These are the snapshots that must be kept so that future
		backups to those remotes can still find a common parent for an incremental `btrfs send -p`.
		"""
		all = s.all_subvols_from_db()
		result = {}
		for entry in s.most_recent_common_snapshots_by_fs(all, SUBVOL):
			label = s._remote_fs_label(all, entry['fs_uuid'], entry['hosts'])
			result.setdefault(entry['snapshot']['path'], []).append(label)
		return result


	def _human_age(s, seconds):
		"""compact human-readable age, e.g. 45s, 12m, 3h, 6d, 8mo, 2y"""
		seconds = int(seconds)
		if seconds < 60:
			return f'{seconds}s'
		minutes = seconds // 60
		if minutes < 60:
			return f'{minutes}m'
		hours = minutes // 60
		if hours < 24:
			return f'{hours}h'
		days = hours // 24
		if days < 30:
			return f'{days}d'
		if days < 365:
			return f'{days // 30}mo'
		return f'{days // 365}y'


	def report_local(s, SUBVOL, PERCENT=30, DB=True, ALL=False):
		"""
		Read-only: print a table of the local snapshots of SUBVOL, the action prune+clean would
		take on each, and the reason. Nothing is deleted. Assumes the db is reasonably current
		(run update_db first, or use `backup report`, for accurate shared-parent detection).

		By default only the interesting rows are shown - snapshots that would be removed, the
		shared parents that are held back, and the newest one. Long runs of snapshots that are
		simply kept because they're recent are collapsed into a single "... N kept ..." line.
		Pass ALL=true to list every snapshot.

		:param SUBVOL: the subvolume whose snapshots to report on
		:param PERCENT: the clean percentage to simulate (default 30), so the report matches what
			`clean_local --PERCENT=...` would do
		:param DB: use the db to detect shared parents (as clean/prune do)
		:param ALL: list every snapshot instead of collapsing the uninteresting kept ones
		"""
		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']

		shared = s._shared_parents(SUBVOL) if DB else {}

		snapshots = sorted(s.get_local_bfg_snapshots(SUBVOL).val, key=lambda x: x['dt'])
		heading = f'subvol: {SUBVOL}' + ('' if DB else ' (DB disabled - shared parents unknown)')
		s._report_snapshots(heading, snapshots, shared, PERCENT, ALL)


	def _report_snapshots(s, heading, snapshots, shared, PERCENT, ALL):
		"""
		shared core of report_local/report_snapshots/report_fs (read-only): print the
		prune+clean plan for `snapshots` (sorted oldest first), with `shared` mapping
		protected paths to the "host:fs" labels they are shared with.
		"""
		PERCENT = float(PERCENT)
		n = len(snapshots)

		print(f'{heading}   ({n} snapshot(s))')
		if n == 0:
			return

		mrcs = set(shared.keys())
		newest = snapshots[-1]['path']

		# simulate prune, then simulate clean on whatever prune would leave behind - this mirrors
		# the prune-then-clean pipeline in backup.py.
		prune_reason = {}
		prune_drop = set()
		for d in s._prune_decisions(snapshots, mrcs, newest):
			path = d['snap']['path']
			prune_reason[path] = d['reason']
			if d['prunable']:
				prune_drop.add(path)

		survivors = [snap for snap in snapshots if snap['path'] not in prune_drop]
		count_to_clean = int(len(survivors) * PERCENT / 100.0)
		clean_drop = set()
		for i, snap in enumerate(survivors):
			if i >= count_to_clean:
				break
			path = snap['path']
			if path not in mrcs and path != newest:
				clean_drop.add(path)

		now = datetime.now()
		rows = []
		for snap in snapshots:
			path = snap['path']
			age = s._human_age((now - snap['dt']).total_seconds())
			if path in shared:
				action, reason, interesting = 'KEEP', 'shared parent -> ' + '; '.join(shared[path]), True
			elif path == newest:
				action, reason, interesting = 'KEEP', 'newest', True
			elif path in prune_drop:
				action, reason, interesting = 'prune', prune_reason[path], True
			elif path in clean_drop:
				action, reason, interesting = 'clean', f'oldest {PERCENT:g}%', True
			else:
				# just kept because it's recent / a bucket survivor - noise, collapse by default
				action, reason, interesting = 'keep', prune_reason.get(path, 'keep'), False
			rows.append({
				'date': snap['dt'].strftime('%Y-%m-%d %H:%M'),
				'age': age,
				'action': action,
				'reason': reason,
				'interesting': interesting or ALL,
			})

		shown = [r for r in rows if r['interesting']]
		header = ('DATE', 'AGE', 'ACTION', 'REASON')
		date_w = max([len(header[0])] + [len(r['date']) for r in shown])
		age_w = max([len(header[1])] + [len(r['age']) for r in shown])
		action_w = max([len(header[2])] + [len(r['action']) for r in shown])
		print(f'{header[0]:<{date_w}}  {header[1]:<{age_w}}  {header[2]:<{action_w}}  {header[3]}')

		# print interesting rows, collapsing contiguous runs of hidden (uninteresting) keeps
		run = []
		def flush_run():
			if not run:
				return
			if len(run) == 1:
				r = run[0]
				print(f"{r['date']:<{date_w}}  {r['age']:<{age_w}}  {r['action']:<{action_w}}  {r['reason']}")
			else:
				print(f"   ... {len(run)} more kept ({run[0]['date']} -> {run[-1]['date']}) ...")
			run.clear()

		for r in rows:
			if r['interesting']:
				flush_run()
				print(f"{r['date']:<{date_w}}  {r['age']:<{age_w}}  {r['action']:<{action_w}}  {r['reason']}")
			else:
				run.append(r)
		flush_run()

		n_shared = sum(1 for r in rows if r['reason'].startswith('shared parent'))
		n_keep = n - len(prune_drop) - len(clean_drop)
		print(f'summary: {n} snapshots: {len(prune_drop)} prune, {len(clean_drop)} clean, '
			  f'{n_shared} shared-parent kept, {n_keep} kept total')


	"""
	snapshot-pile commands, addressed by location rather than by origin subvolume.

	Backup targets hold piles of received snapshots (e.g.
	/bac20/backups/jj/.bfg_snapshots/dev3/dev3_<ts>_<tag>) for which no origin
	subvolume exists on this filesystem, so the *_local commands don't apply.
	These commands find snapshot series by naming convention - grouped by
	(parent directory, series name), which covers both the received layout
	(.bfg_snapshots/<subvol>/<subvol>_<ts>_<tag>) and the flat local layout
	(.bfg_snapshots/<subvol>_<ts>_<tag>) - and protect, per series:
	 - for each other filesystem that holds a copy of a member's content, the
	   newest such shared member (the receive-side counterpart of the shared
	   parent, needed so future incremental sends/receives keep working), and
	 - the newest member.
	Content identity comes from the db, by propagated origin uuid (received_uuid
	if set, the snapshot's own uuid otherwise).
	"""

	def prune_snapshots(s, PARENT_DIR, DB=True, DRY_RUN=False):
		"""
		Apply prune_local's time-bucketed retention policy to each snapshot series found
		directly inside PARENT_DIR, e.g. /bac20/backups/jj/.bfg_snapshots/dev3.
		Also sweeps provably-dead aborted receives in PARENT_DIR.
		"""
		with db.advisory_lock():
			for key, members, protected in s._snapshot_series(PARENT_DIR, PARENT_DIR, DB):
				s._delete_prunable(s._prune_decisions(members, set(protected), members[-1]['path']), DRY_RUN, s._local_cmd)
			s._sweep_aborted_receives(PARENT_DIR, PARENT_DIR, DRY_RUN)


	def clean_snapshots(s, PARENT_DIR, PERCENT=30, DB=True, DRY_RUN=False):
		"""
		Apply clean_local's oldest-PERCENT% policy to each snapshot series found directly
		inside PARENT_DIR, sparing shared and newest members (see class of commands above).
		"""
		with db.advisory_lock():
			for key, members, protected in s._snapshot_series(PARENT_DIR, PARENT_DIR, DB):
				s._clean_snapshots(members, protected, PERCENT, DRY_RUN)


	def report_snapshots(s, PARENT_DIR, PERCENT=30, DB=True, ALL=False):
		"""
		Read-only: the report_local table for each snapshot series found directly inside
		PARENT_DIR, plus any aborted receives. Nothing is deleted.
		"""
		for key, members, protected in s._snapshot_series(PARENT_DIR, PARENT_DIR, DB):
			s._report_snapshots(s._series_heading(key, DB), members, protected, PERCENT, ALL)
		s._report_aborted_receives(PARENT_DIR, PARENT_DIR)


	def prune_fs(s, FS, DB=True, DRY_RUN=False):
		"""prune_snapshots for every snapshot series on the whole filesystem FS.
		Also sweeps provably-dead aborted receives fs-wide."""
		with db.advisory_lock():
			for key, members, protected in s._snapshot_series(FS, None, DB):
				logbfg.info(f"Pruning series {key[0]}/{key[1]}*")
				s._delete_prunable(s._prune_decisions(members, set(protected), members[-1]['path']), DRY_RUN, s._local_cmd)
			s._sweep_aborted_receives(FS, None, DRY_RUN)


	def clean_fs(s, FS, PERCENT=30, DB=True, DRY_RUN=False, MIN_FREE=None):
		"""
		clean_snapshots for every snapshot series on the whole filesystem FS.

		With MIN_FREE (bytes, or '500G'/'1.5T'), the oldest-PERCENT%-per-series policy
		is replaced by a goal: delete unprotected snapshots fs-wide, oldest first,
		only until the filesystem reports at least MIN_FREE free. Idempotent by goal -
		safe to re-run (and to cron) without compounding history loss.
		"""
		with db.advisory_lock():
			if MIN_FREE is not None:
				s._clean_fs_min_free(FS, MIN_FREE, DB, DRY_RUN)
				return
			for key, members, protected in s._snapshot_series(FS, None, DB):
				logbfg.info(f"Cleaning series {key[0]}/{key[1]}*")
				s._clean_snapshots(members, protected, PERCENT, DRY_RUN)


	def _free_bytes(s, path):
		st = os.statvfs(path)
		return st.f_bavail * st.f_frsize


	def _clean_fs_min_free(s, FS, MIN_FREE, DB, DRY_RUN):
		target = parse_size(MIN_FREE)
		free = s._free_bytes(FS)
		logbfg.info(f'{FS}: {free / 2**30:.1f} GiB free, target {target / 2**30:.1f} GiB')
		if free >= target:
			_prerr(f'Nothing to clean: {FS} already has {free / 2**30:.1f} GiB free.')
			return

		if not DRY_RUN:
			# blanket settle before deciding to delete anything: deletions queued
			# earlier (the prune that just ran, a previous clean, ...) may still be
			# reclaiming space in the background, so the reading above under-reads
			logbfg.info('below target; waiting for pending deletions to settle...')
			s._local_cmd(['btrfs', 'subvolume', 'sync', str(FS)], die_on_error=False)
			time.sleep(60)
			free = s._free_bytes(FS)
			logbfg.info(f'{FS}: {free / 2**30:.1f} GiB free after settling')
			if free >= target:
				_prerr(f'Nothing to clean: {FS} reached {free / 2**30:.1f} GiB free '
					   f'once pending deletions settled.')
				return

		# pool every deletable member fs-wide (not protected, not the newest of its
		# series), oldest first - so all series contribute their oldest history first
		candidates = []
		for key, members, protected in s._snapshot_series(FS, None, DB):
			newest = members[-1]['path']
			for m in members:
				if m['path'] == newest or m['path'] in protected:
					continue
				candidates.append(m)
		candidates.sort(key=lambda x: x['dt'])

		if DRY_RUN:
			logbfg.info(f'DRY_RUN: would delete up to {len(candidates)} snapshot(s), oldest first, '
						f'until {target / 2**30:.1f} GiB is free (freed sizes unknowable in advance):')
			for m in candidates:
				logbfg.info(f'  {m["path"]}')
			return

		deleted = 0
		for m in candidates:
			free = s._free_bytes(FS)
			if free >= target:
				break
			path = m['path']
			cmd = ['btrfs', 'subvolume', 'delete', str(path)]
			if not s._yes(shlex.join(cmd)):
				continue
			if s._local_cmd(cmd, die_on_error=False) == -1:
				logbfg.warning(f"could not delete {path} (deleted concurrently?), continuing")
				continue
			s._mark_deleted_in_db([m.get('local_uuid')])
			deleted += 1
			_prerr(f"Deleted snapshot: {path}")
			# subvol deletion frees space asynchronously (btrfs cleaner thread); wait
			# for it so the next free-space check reflects this deletion
			s._local_cmd(['btrfs', 'subvolume', 'sync', str(FS)], die_on_error=False)
			if s._free_bytes(FS) >= target:
				break
			# safety net: even after subvolume sync, free-space accounting may lag a
			# little (e.g. extents pinned until the next transaction commit). An
			# under-read here would delete more history than the goal needs, so give
			# the accounting a minute to settle before deciding to delete more.
			logbfg.info('below target after sync; waiting 60s for freed space to settle '
						'before deleting more...')
			time.sleep(60)

		free = s._free_bytes(FS)
		if free >= target:
			_prerr(f'Done: deleted {deleted} snapshot(s), {free / 2**30:.1f} GiB free.')
		else:
			logbfg.warning(f'Deleted all {deleted} deletable snapshot(s) but only reached '
						   f'{free / 2**30:.1f} GiB free of the {target / 2**30:.1f} GiB target - '
						   f'the rest is protected shared pairs, series-newest snapshots, or non-snapshot data.')


	def report_fs(s, FS, PERCENT=30, DB=True, ALL=False):
		"""Read-only: the report_local table for every snapshot series on the filesystem FS,
		plus any aborted receives."""
		for key, members, protected in s._snapshot_series(FS, None, DB):
			s._report_snapshots(s._series_heading(key, DB), members, protected, PERCENT, ALL)
		s._report_aborted_receives(FS, None)


	def _series_heading(s, key, DB):
		parent_dir, name = key
		return f'series: {parent_dir}/{name}*' + ('' if DB else ' (DB disabled - shared parents unknown)')


	def _snapshot_series(s, path, restrict_dir, DB):
		"""
		Discover snapshot series: read-only subvols under a .bfg_snapshots directory, from
		the live btrfs listing, grouped by (parent directory, series name) and sorted oldest
		first. Yields ((parent_dir, name), members, protected) per series; protected is the
		shared-content map from _shared_snapshots ({} when DB is off). restrict_dir limits
		the result to series directly inside that directory; None means the whole filesystem.
		"""
		path = Path(path).absolute()
		if restrict_dir is not None:
			restrict_dir = Path(restrict_dir).absolute()

		groups = defaultdict(list)
		for x in s._get_subvolumes(s._local_cmd, path, 'local'):
			if not x['ro'] or '.bfg_snapshots' not in x['path'].parts:
				continue
			if restrict_dir is not None and x['path'].parent != restrict_dir:
				continue
			try:
				name = s.parse_snapshot_name(x['path'].name)['name']
			except ValueError:
				logbfg.warning(f'ignoring subvol with unparseable snapshot name: {x["path"]}')
				continue
			groups[(x['path'].parent, name)].append(x)

		if not groups:
			logbfg.info(f'no snapshot series found in {restrict_dir if restrict_dir is not None else path}')
			return

		all_rows = s.all_subvols_from_db() if DB else []
		my_fs_uuid = s.local_fs_uuid(path) if DB else None
		if not DB:
			logbfg.warning("DB=False: cannot identify shared snapshots; "
						   "nothing will be protected except the newest of each series!")

		for key in sorted(groups, key=lambda k: (str(k[0]), k[1])):
			members = sorted(groups[key], key=lambda x: x['dt'])
			protected = s._shared_snapshots(all_rows, my_fs_uuid, members) if DB else {}
			logbfg.info(f'series {key[0]}/{key[1]}*: {len(members)} snapshot(s), {len(protected)} shared')
			yield key, members, protected


	def _shared_snapshots(s, all_rows, my_fs_uuid, members):
		"""
		Map member path -> list of "host:fs" labels of other filesystems that hold a copy
		of that member's content according to the db, keeping only the newest shared member
		per other filesystem. Content identity is the propagated origin uuid: received_uuid
		if set (as on received snapshots), the snapshot's own uuid otherwise.
		"""
		def content_key(x):
			return x['received_uuid'] or x['local_uuid']

		fss_by_key = defaultdict(set)
		hosts_by_fs = defaultdict(set)
		for row in all_rows:
			if row.get('deleted'):
				# a deleted copy is no evidence the content still exists on that fs; counting
				# it would shift "newest shared" past the newest real pair, unprotecting it
				continue
			if row['fs_uuid'] == my_fs_uuid:
				continue
			fss_by_key[content_key(row)].add(row['fs_uuid'])
			hosts_by_fs[row['fs_uuid']].add(row['host'])

		# members come oldest first, so the last write per filesystem wins
		newest_shared = {}
		for m in members:
			for fs_uuid in fss_by_key.get(content_key(m), ()):
				newest_shared[fs_uuid] = m

		result = {}
		for fs_uuid, m in newest_shared.items():
			label = s._remote_fs_label(all_rows, fs_uuid, hosts_by_fs[fs_uuid])
			result.setdefault(m['path'], []).append(label)
		return result


	"""
	aborted receives.

	btrfs receive creates the target subvol WRITABLE and it stays so for the whole
	transfer; ro and received_uuid are stamped together only at successful
	end-of-stream. So "rw + no received_uuid + snapshot-shaped name under a
	.bfg_snapshots location" is the signature of a receive that either aborted or is
	still running. The two are told apart by the per-snapshot receive lock (see
	_receive_cmd_str): the receiver holds it for the whole transfer and the kernel
	drops it the instant the receiver dies, so lock-free == provably dead.
	Partials with no lock file (from before receive locking, or made by a raw
	btrfs receive) are only ever reported, never deleted.
	"""

	def _filter_aborted_receives(s, subvols, restrict_dir):
		"""aborted/in-progress receives among `subvols` (see class comment above);
		restrict_dir as in _snapshot_series."""
		if restrict_dir is not None:
			restrict_dir = Path(restrict_dir).absolute()
		out = []
		for x in subvols:
			if x['ro'] or x['received_uuid'] is not None:
				continue
			if '.bfg_snapshots' not in x['path'].parts:
				continue
			if restrict_dir is not None and x['path'].parent != restrict_dir:
				continue
			try:
				s.parse_snapshot_name(x['path'].name)
			except ValueError:
				continue
			out.append(x)
		return out


	def _aborted_receive_status(s, x):
		"""('dead'|'in-flight'|'unproven', lock path) for one partial"""
		lock = s._receive_lock_path(x['path'].parent, x['path'].name)
		if s._local_cmd(['test', '-e', str(lock)], die_on_error=False) == -1:
			return 'unproven', lock
		if s._local_cmd(['flock', '-n', str(lock), 'true'], die_on_error=False) == -1:
			return 'in-flight', lock
		return 'dead', lock


	def _sweep_aborted_receives(s, path, restrict_dir, DRY_RUN):
		"""
		Delete provably-dead aborted receives (the deletion itself runs under flock -n
		on the receive lock, so a receive restarting concurrently is never pulled out
		from under - it just makes the delete a no-op). Then GC receive lock files
		that can no longer have a holder.
		"""
		subvols = s._get_subvolumes(s._local_cmd, path, 'local')
		for x in s._filter_aborted_receives(subvols, restrict_dir):
			p = x['path']
			status, lock = s._aborted_receive_status(x)
			if status == 'unproven':
				logbfg.warning(
					f'ABORTED RECEIVE (unproven): {p} - rw with no received_uuid, but no receive lock '
					f'file exists to prove the receive is dead (transfer predates receive locking?). '
					f'If no btrfs receive is running for it, delete manually: btrfs subvolume delete {p}')
			elif status == 'in-flight':
				logbfg.info(f'receive in progress, skipping: {p}')
			elif DRY_RUN:
				logbfg.info(f'ABORTED RECEIVE: would delete {p} (DRY_RUN)')
			else:
				cmd = ['flock', '-n', str(lock), 'btrfs', 'subvolume', 'delete', str(p)]
				if not s._yes(shlex.join(cmd)):
					continue
				if s._local_cmd(cmd, die_on_error=False) == -1:
					logbfg.warning(f'could not delete aborted receive {p} '
								   f'(receive restarted concurrently?), continuing')
					continue
				s._mark_deleted_in_db([x.get('local_uuid')])
				_prerr(f'Deleted aborted receive: {p}')
		if not DRY_RUN:
			s._gc_receive_locks(subvols, restrict_dir)


	def _gc_receive_locks(s, subvols, restrict_dir):
		"""
		Remove receive lock files that can no longer have a holder: the same-name
		subvol is read-only (receive succeeded; a same-name retry would fail at
		creation, so no receiver for that name can ever run again) or gone (received
		and later pruned, or a swept partial). A lock file whose name matches a live
		rw partial is load-bearing and kept. Removal runs under flock -n on the file
		itself: a receive that started after our subvols listing was taken (lock held,
		subvol not created yet) makes the flock fail and its lock file survives -
		unlinking a held lock file would split the lock and demote that receive to
		'unproven' forever.
		"""
		if restrict_dir is not None:
			restrict_dir = Path(restrict_dir).absolute()
		by_dir = defaultdict(dict)
		for x in subvols:
			if '.bfg_snapshots' in x['path'].parts:
				by_dir[x['path'].parent][x['path'].name] = x

		lock_dirs = set(by_dir.keys()) if restrict_dir is None else {restrict_dir}

		for d in sorted(lock_dirs):
			lockdir = s._receive_locks_dir(d)
			listing = s._local_cmd(['ls', '-1', str(lockdir)], die_on_error=False)
			if listing == -1:
				continue
			for name in listing.splitlines():
				name = name.strip()
				if not name:
					continue
				subvol = by_dir[d].get(name)
				if subvol is not None and not subvol['ro']:
					continue  # live partial (or in-flight receive) - its lock is the proof mechanism
				lock = lockdir / name
				logbfg.debug(f'GC receive lock {lock}')
				s._local_cmd(['flock', '-n', str(lock), 'rm', str(lock)], die_on_error=False)


	def _report_aborted_receives(s, path, restrict_dir):
		"""read-only: print every partial in scope and what prune would do about it"""
		subvols = s._get_subvolumes(s._local_cmd, path, 'local')
		msgs = {
			'dead': 'provably dead - prune will delete it',
			'in-flight': 'receive lock held - receive in progress',
			'unproven': 'no receive lock file - cannot prove dead; if no receive is running, delete manually',
		}
		for x in s._filter_aborted_receives(subvols, restrict_dir):
			status, lock = s._aborted_receive_status(x)
			print(f'ABORTED RECEIVE: {x["path"]}   ({msgs[status]})')


	def prune_remote(s, LOCAL_SUBVOL, REMOTE_SUBVOL, DRY_RUN=False):
		with db.advisory_lock():
			s._prune_remote(LOCAL_SUBVOL, REMOTE_SUBVOL, DRY_RUN)

	def _prune_remote(s, LOCAL_SUBVOL, REMOTE_SUBVOL, DRY_RUN):

		# prepare_prune

		logbfg.info(f"Pruning remote snapshots of {LOCAL_SUBVOL=}")
		s._subvol_uuid = s.get_subvol(s._local_cmd, LOCAL_SUBVOL).val['local_uuid']


		all = s.all_subvols_from_db()

		local_mrcs = s.most_recent_common_snapshots(all, LOCAL_SUBVOL)
		# local_mrcs is a list with one snapshot entry for each "remote" filesystem
		remote_mrcs = []

		remote_fs_uuid, remote_fs_mp = s.remote_fs_uuid(REMOTE_SUBVOL)

		for snap in local_mrcs:
			for snap2 in all:
				if snap2['fs_uuid'] == remote_fs_uuid:
					if snap['local_uuid'] == snap2['received_uuid']:
						remote_mrcs.append(snap2)

		mrcs = set([x['path'] for x in remote_mrcs])

		logbfg.debug(f"{mrcs=}")


		remote_snapshots = s.remote_bfg_snapshots(remote_fs_mp, remote_fs_uuid)
		remote_snapshots = sorted(remote_snapshots, key=lambda x: x['dt'])

		if len(remote_snapshots) == 0:
			logbfg.info(f"No snapshots found for {REMOTE_SUBVOL}")
			return

		newest = remote_snapshots[-1]['path']

		# same retention decisions as prune_local (keep shared parents + newest + one per bucket,
		# thin the rest, no freeze past the shared parent), but delete on the remote side -
		# with the same delete-failure tolerance and immediate mark_deleted flagging.
		s._delete_prunable(s._prune_decisions(remote_snapshots, mrcs, newest), DRY_RUN, s._remote_cmd)



	def local_bfg_snapshots(s, all, SUBVOL):
		result = []
		for snap in all:
			if snap['parent_uuid'] == s._subvol_uuid and not snap['deleted'] and '.bfg_snapshots' in snap['path'].parts:
				result.append(snap)
		return result


	def remote_bfg_snapshots(s, REMOTE_SUBVOL, remote_fs_uuid):
		all = s._get_subvolumes(s._remote_cmd, REMOTE_SUBVOL, 'remote')
		result = []

		p = Path(s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOL)).val)
		logbfg.debug(f"remote_bfg_snapshots: calculate_default_snapshot_parent_dir: {p=}")

		for snap in all:
			path = snap['path']
			logbfg.debug(f"remote_bfg_snapshots {path=}")
			if not '.bfg_snapshots' in path.parts:
				continue
			if path.parent != p:
				continue
			result.append(snap)
			logbfg.debug(f"remote_bfg_snapshots: found {snap['path']}")

		return result



	def most_recent_common_snapshots(s, all, SUBVOL):
		"""
		Find the most recent common snapshots between the local and each remote filesystem.
		"""
		return [x['snapshot'] for x in s.most_recent_common_snapshots_by_fs(all, SUBVOL)]


	def most_recent_common_snapshots_by_fs(s, all, SUBVOL):
		"""
		Like most_recent_common_snapshots, but keep the association between each most recent
		common snapshot and the remote filesystem it is shared with. Returns a list of dicts:
		{'fs_uuid': <remote fs uuid>, 'hosts': <set of hostnames>, 'snapshot': <local snapshot record>}.
		"""
		result = []

		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']
		fss = s.remote_fs_uuids(all, SUBVOL)
		logbfg.debug(f"most_recent_common_snapshots: {fss=}")

		for fs_uuid, fs in fss.items():
			hosts = fs['hosts']

			logbfg.debug(f"most_recent_common_snapshots for {fs_uuid=} (hosts:{hosts})....")

			all2 = []
			for snap in all:
				x = dict(snap)
				if x['fs_uuid'] == s.local_fs_uuid(SUBVOL):
					x['machine'] = 'local'
				elif x['fs_uuid'] == fs_uuid:
					x['machine'] = 'remote'
				else:
					x['machine'] = 'other'
				all2.append(x)
				#logbfg.debug(f"all2: {x=}")

			logbfg.debug(f"all2: {len(all2)}")
			logbfg.debug(f"_parent_candidates2...")
			candidates = list(s._parent_candidates2(all2, SUBVOL, s._subvol_uuid , ('local', 'remote')))

			logbfg.debug(f"shared parents: {len(candidates)}")

			for candidate in candidates:
				logbfg.debug(f"  {candidate['local_uuid']}")

			if len(candidates) > 0:
				result.append({'fs_uuid': fs_uuid, 'hosts': hosts, 'snapshot': candidates[0]})

		return result


	def _remote_fs_label(s, all, fs_uuid, hosts):
		"""human-readable "host:fs" label for a remote filesystem, derived from the db records"""
		fs_paths = sorted(set(str(snap['fs']) for snap in all if snap['fs_uuid'] == fs_uuid))
		host_str = ','.join(sorted(hosts)) if hosts else '?'
		fs_str = ','.join(fs_paths) if fs_paths else fs_uuid
		return f'{host_str}:{fs_str}'



	def remote_commit(s, REMOTE_SUBVOL, TAG=None, SNAPSHOT=None, SNAPSHOT_NAME=None):
		if TAG and SNAPSHOT:
			_prerr(f'please specify SNAPSHOT or TAG, not both')
			return -1
		if TAG and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or TAG, not both')
			return -1
		if SNAPSHOT and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or SNAPSHOT, not both')
			return -1
		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path('remote', Path(REMOTE_SUBVOL), 'remote_commit',
														 SNAPSHOT_NAME).val
		s._remote_make_ro_snapshot(REMOTE_SUBVOL, SNAPSHOT)
		_prerr(f'DONE {s._remote_str},\n\tsnapshotted {REMOTE_SUBVOL} \n\tinto {SNAPSHOT}\n.')
		return Res(SNAPSHOT)



	def _receive_locks_dir(s, target_dir):
		"""per-target-dir directory of receive lock files, one per incoming snapshot"""
		return Path(target_dir) / '.bfg_receive_locks'


	def _receive_lock_path(s, target_dir, snapshot_name):
		return s._receive_locks_dir(target_dir) / snapshot_name


	def _series_lock_path(s, target_dir):
		"""
		The in-flight-transfer marker for a series dir: every receive into target_dir
		takes this exclusive flock non-blocking and holds it for the whole transfer,
		so a second transfer into the same series fails instantly instead of queueing
		behind the first only to duplicate its work (two concurrent full sends of the
		same series is the expensive failure this prevents). Dot-named so the `ls -1`
		in _gc_receive_locks never lists it: it is never unlinked - unlinking a lock
		file a receiver holds would split the lock.
		"""
		return s._receive_locks_dir(target_dir) / '.series'


	def _receive_cmd_str(s, target_dir, snapshot_name):
		"""
		The receive side of a send pipeline: btrfs receive wrapped in flock(1) on a
		per-snapshot lock file, held for the whole transfer. The kernel drops the lock
		the instant the receiving process dies (ssh cut, ENOSPC, OOM, ctrl-C), so
		"is this partial's receive still alive?" becomes a kernel fact that
		_sweep_aborted_receives can test exactly, with no process/age heuristics.
		The outer flock -n on the series lock makes a concurrent transfer into the
		same series dir exit SERIES_BUSY_EXIT immediately (distinguishable from a
		genuine receive failure), see _series_lock_path; push/transfer_snapshot
		treat that as skip-and-continue, not as an error.
		"""
		lock = s._receive_lock_path(target_dir, snapshot_name)
		series = s._series_lock_path(target_dir)
		return ('flock -n -E ' + str(SERIES_BUSY_EXIT) + ' ' + str(series)
				+ ' flock ' + str(lock)
				+ ' btrfs receive ' + str(target_dir))


	def push(s, SUBVOL, SNAPSHOT, REMOTE_SUBVOL, PARENT=None, CLONESRCS=[]):
		"""
		Try to figure out shared parents, if not provided, and send SNAPSHOT to the other side.
		"""
		snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOL)).val
		logbfg.debug(f'mkdir -p {snapshot_parent_dir}')
		s._remote_cmd(['mkdir', '-p', str(s._receive_locks_dir(snapshot_parent_dir))])

		if PARENT is None:
			logbfg.debug(f'get_subvol...')
			my_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']
			logbfg.debug(f'find_common_parent...')
			PARENT = s.find_common_parent(SUBVOL, str(snapshot_parent_dir), my_uuid, ('local', 'remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		try:
			s.local_send(SNAPSHOT, ' | ' + s._sshstr + ' ' + s._sudo[0] + ' '
						 + s._receive_cmd_str(snapshot_parent_dir, Path(SNAPSHOT).name), PARENT,
						 CLONESRCS)
		except subprocess.CalledProcessError as e:
			if e.returncode == SERIES_BUSY_EXIT:
				logbfg.warning(f'skipping push of {SNAPSHOT}: a transfer into '
							   f'{snapshot_parent_dir} is already in flight (series lock held)')
				return Res(None)
			raise
		_prerr(f'DONE, \n\tpushed {SNAPSHOT} \n\tinto {snapshot_parent_dir}\n.')
		return Res(str(snapshot_parent_dir) + '/' + Path(SNAPSHOT).parts[-1])



	def pull(s, REMOTE_SNAPSHOT, LOCAL_SUBVOL, PARENT=None, CLONESRCS=[]):
		local_snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('local', Path(LOCAL_SUBVOL)).val
		s._local_cmd(['mkdir', '-p', str(local_snapshot_parent_dir)])

		if PARENT is None:
			my_uuid = s.get_subvol(s._remote_cmd, REMOTE_SNAPSHOT).val['local_uuid']
			PARENT = s.find_common_parent(local_snapshot_parent_dir, REMOTE_SNAPSHOT, my_uuid, ('remote', 'local')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		if not s.remote_send(REMOTE_SNAPSHOT, local_snapshot_parent_dir, PARENT, CLONESRCS):
			return Res(None)

		local_snapshot = str(local_snapshot_parent_dir) + '/' + Path(REMOTE_SNAPSHOT).parts[-1]

		_prerr(f'DONE, \n\tpulled {REMOTE_SNAPSHOT} \n\tinto {local_snapshot}\n.')
		return Res(local_snapshot)



	def transfer_snapshot(s, SNAPSHOT, REMOTE_PARENT_DIR, PARENT=None, CLONESRCS=[]):
		"""
		Send one existing local snapshot into a directory on the other side, figuring out
		the -p parent if not given. Unlike push, no origin subvolume is involved - useful
		for re-establishing shared pairs on a pile and for distributing backups over hops.

		:param SNAPSHOT: path of the existing local read-only snapshot to send
		:param REMOTE_PARENT_DIR: directory on the other side to receive into
			(e.g. /bac20/backups/jj/.bfg_snapshots/dev3)
		:return: path of the snapshot created on the other side
		"""
		SNAPSHOT = Path(SNAPSHOT).absolute()
		REMOTE_PARENT_DIR = Path(REMOTE_PARENT_DIR)

		logbfg.debug(f'mkdir -p {REMOTE_PARENT_DIR}')
		s._remote_cmd(['mkdir', '-p', str(s._receive_locks_dir(REMOTE_PARENT_DIR))])

		if PARENT is None:
			my_uuid = s.get_subvol(s._local_cmd, SNAPSHOT).val['local_uuid']
			PARENT = s.find_common_parent(str(SNAPSHOT), str(REMOTE_PARENT_DIR), my_uuid, ('local', 'remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		try:
			s.local_send(str(SNAPSHOT), ' | ' + s._sshstr + ' ' + s._sudo[0] + ' '
						 + s._receive_cmd_str(REMOTE_PARENT_DIR, SNAPSHOT.name), PARENT,
						 CLONESRCS)
		except subprocess.CalledProcessError as e:
			if e.returncode == SERIES_BUSY_EXIT:
				logbfg.warning(f'skipping transfer of {SNAPSHOT}: a transfer into '
							   f'{REMOTE_PARENT_DIR} is already in flight (series lock held)')
				return Res(None)
			raise

		remote_snapshot = str(REMOTE_PARENT_DIR / SNAPSHOT.name)
		_prerr(f'DONE, \n\ttransferred {SNAPSHOT} \n\tinto {remote_snapshot}\n.')
		return Res(remote_snapshot)



	"""

	low-level operations
	"""

	def _local_make_ro_snapshot(s, SUBVOL, SNAPSHOT):
		"""make a read-only snapshot of SUBVOL into SNAPSHOT, locally"""
		SNAPSHOT_PARENT = os.path.split(Path(SNAPSHOT))[0]
		s._local_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._local_cmd(f'btrfs subvolume snapshot -r {SUBVOL} {SNAPSHOT}')
		_prerr(f'DONE {s._local_str}, \n\tsnapshotted {SUBVOL} \n\tinto {SNAPSHOT}\n.')
		return SNAPSHOT



	def _remote_make_ro_snapshot(s, SUBVOL, SNAPSHOT):
		"""make a read-only snapshot of SUBVOL into SNAPSHOT, remotely"""
		SNAPSHOT_PARENT = os.path.split(Path(SNAPSHOT))[0]
		s._remote_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._remote_cmd(f'btrfs subvolume snapshot -r {SUBVOL} {SNAPSHOT}')
		return SNAPSHOT



	def _parent_args(s, PARENT, CLONESRCS):
		parents_args = []

		if PARENT:
			parents_args.append('-p')
			parents_args.append(PARENT)

		for c in CLONESRCS:
			parents_args.append('-c')
			parents_args.append(c)

		return parents_args



	def local_send(s, SNAPSHOT, target, PARENT, CLONESRCS=[]):
		parents_args = s._parent_args(PARENT, CLONESRCS)

		# _prerr((str(parents_args)) + ' #...')
		cmd = shlex.join(s._sudo + ['btrfs', 'send'] + parents_args + [SNAPSHOT]) + target
		logbtrfs.info(f'local_send: {cmd=} #...')
		subprocess.check_call(cmd, shell=True)



	def remote_send(s, REMOTE_SNAPSHOT, LOCAL_DIR, PARENT, CLONESRCS):
		parents_args = s._parent_args(PARENT, CLONESRCS)

		# receive under the series in-flight marker and a per-snapshot flock,
		# see _series_lock_path and _receive_cmd_str
		s._local_cmd(['mkdir', '-p', str(s._receive_locks_dir(LOCAL_DIR))])
		lock = s._receive_lock_path(LOCAL_DIR, Path(REMOTE_SNAPSHOT).name)

		cmd1 = shlex.split(s._sshstr) + s._sudo + ['btrfs', 'send'] + parents_args + [REMOTE_SNAPSHOT]
		cmd2 = s._sudo + ['flock', '-n', '-E', str(SERIES_BUSY_EXIT),
						  str(s._series_lock_path(LOCAL_DIR)),
						  'flock', str(lock), 'btrfs', 'receive', str(LOCAL_DIR)]
		logbtrfs.info(shlex.join(cmd1) + ' >>|>> ' + shlex.join(cmd2))
		p1 = subprocess.Popen(
			cmd1,
			stdout=subprocess.PIPE)
		p2 = subprocess.Popen(
			cmd2,
			stdin=p1.stdout)
		p1.stdout.close()  # https://www.titanwolf.org/Network/q/91c3c5dd-aa49-4bf4-911d-1bfe5ac304da/y
		p2.communicate()
		if p2.returncode == SERIES_BUSY_EXIT:
			logbfg.warning(f'skipping pull of {REMOTE_SNAPSHOT}: a transfer into '
						   f'{LOCAL_DIR} is already in flight (series lock held)')
			return False
		if p2.returncode != 0:
			logbfg.error('exit code ' + str(p2.returncode))
			exit(1)
		return True



	def find_common_parent(s, subvolume, remote_subvolume, my_uuid, direction):
		logbfg.debug(f'parent_candidates...')
		candidates = s.parent_candidates(subvolume, remote_subvolume, my_uuid, direction).val
		# candidates.sort(key = lambda sv: sv['subvol_id']) # nope, subvol id is a crude approximation. What happens when you snapshot and old ro snapshot? It gets the highest id.
		if len(candidates) != 0:
			winner = candidates[0]
			s._add_abspath(winner)
			logbtrfs.debug(f'PICKED COMMON PARENT {winner["abspath"]}.')
			logbtrfs.debug(f'details: {winner}.')
			return Res(winner)
		else:
			return Res(None)



	def best_shared_parent(s, SUBVOL, REMOTE_SUBVOL, my_uuid=None, direction=('local', 'remote')):
		"""
		Read-only: the parent candidate that push/pull would pick for transferring SUBVOL
		to REMOTE_SUBVOL (the first of parent_candidates), or None.
		"""
		if my_uuid is None:
			my_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']
		candidates = list(s._parent_candidates(SUBVOL, REMOTE_SUBVOL, my_uuid, direction))
		if len(candidates) > 0:
			return Res(candidates[0])
		else:
			return Res(None)



	def _add_abspath(s, subvol_record):
		if subvol_record['machine'] == 'remote':
			s._remote_add_abspath(subvol_record)
		else:
			s._local_add_abspath(subvol_record)



	def _local_add_abspath(s, subvol_record):
		id5_mp = s.local_fs_id5_mount_point(subvol_record['path'])
		subvol_record['abspath'] = str(id5_mp) + '/' + s._local_cmd(
			['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), id5_mp]).strip()



	def _remote_add_abspath(s, subvol_record):
		id5_mp = s.remote_fs_id5_mount_point(subvol_record['path'])
		subvol_record['abspath'] = str(id5_mp) + '/' + s._remote_cmd(
			['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), id5_mp]).strip()



	def parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction):
		candidates = []
		for c in s._parent_candidates(subvolume, remote_subvolume, my_uuid, direction):
			candidates.append(c)
			logbtrfs.debug('shared parent: ' + c['local_uuid'])
		return Res(candidates)


	def get_local_subvol(s, subvol_path):
		toplevel_subvol = s.get_subvol(s._local_cmd, subvol_path).val
		toplevel_subvol['src'] = 'get_subvol'
		toplevel_subvol['machine'] = 'local'
		return toplevel_subvol


	def _parent_candidates(s, subvol_path, remote_subvolume, my_uuid, direction):
		logbfg.debug(f'_get_subvolumes remote...')
		remote_subvols = s._get_subvolumes(s._remote_cmd, remote_subvolume, 'remote')
		logbfg.debug(f'_get_subvolumes local...')
		local_subvols = s._get_subvolumes(s._local_cmd, subvol_path, 'local')

		all_subvols = []
		for machine, lst in [
			('remote', remote_subvols),
			('local', local_subvols),
		]:
			for v in lst:
				v['machine'] = machine
				all_subvols.append(v)

		yield from s._parent_candidates2(all_subvols, subvol_path, my_uuid, direction)



	def _parent_candidates2(s, all_subvols, subvol_path, my_uuid, direction):
		"""
		my uuid is the local_uuid of the local rw subvolume that we're trying to transfer to the remote machine.
		direction is either ('local', 'remote') or ('remote', 'local')

		The walk is done by volwalker2 (Prolog) by default; the BFG_VOLWALKER env var
		selects 'v1' (the legacy walker, kept one release as an escape hatch) or
		'shadow' (run both, use v1's answer, and log disagreements - volwalker2 finding
		extra candidates is its expected improvement, volwalker2 missing a v1 candidate
		is a red flag and the input is dumped for analysis). Default flipped to 'v2'
		2026-07-17 after a clean shadow period (zero mismatch dumps) and the randomized
		differential/oracle suite in tests/test_common_parents.py.
		"""

		all_subvols2 = {}
		for i in all_subvols:
			all_subvols2[i['local_uuid']] = i
		if my_uuid not in all_subvols2:
			all_subvols = all_subvols + [s.get_local_subvol(subvol_path)]

		all_subvols2 = {}
		for i in all_subvols:

			if i['local_uuid'] in all_subvols2:
				if i != all_subvols2[i['local_uuid']]:
					logging.warning('duplicate subvols:')
					logging.warning(json.dumps(i, indent=2, default=datetime_to_json, sort_keys=True))
					logging.warning(json.dumps(all_subvols2[i['local_uuid']], indent=2, default=datetime_to_json, sort_keys=True))
					raise 'wut'

			# if not Path('.bfg_snapshots') in Path(i['path']).parts:
			# 	continue

			all_subvols2[i['local_uuid']] = i
			logging.debug('_parent_candidates2:' + json.dumps(i, indent=2, default=datetime_to_json, sort_keys=True))

		logging.debug(f'_parent_candidates2 all_subvols: {len(all_subvols)}')

		mode = os.environ.get('BFG_VOLWALKER', 'v2')
		if mode not in ('v1', 'v2', 'shadow'):
			raise Exception(f'BFG_VOLWALKER must be v1, v2 or shadow, not {mode!r}')
		if mode != 'v1' and shutil.which('swipl') is None:
			if mode == 'v2':
				raise Exception('volwalker2 (the default) needs swipl (SWI-Prolog); '
								'install it, or set BFG_VOLWALKER=v1 for the legacy walker')
			logbfg.debug('swipl not installed, skipping volwalker2 shadow run')
			mode = 'v1'

		v1_res = v2_res = None
		if mode in ('v1', 'shadow'):
			v1_res = list(VolWalker(all_subvols2, direction).walk(my_uuid))
		if mode in ('v2', 'shadow'):
			try:
				v2_res = list(s._volwalker2_candidates(all_subvols2, my_uuid, direction))
			except Exception as e:
				if mode == 'v2':
					raise
				logbfg.warning(f'volwalker2 shadow run failed: {e}')

		if mode == 'shadow' and v2_res is not None:
			s._volwalker_shadow_compare(all_subvols2, my_uuid, direction, v1_res, v2_res)

		yield from (v2_res if mode == 'v2' else v1_res)


	def _volwalker2_candidates(s, all_subvols2, my_uuid, direction):
		"""
		Run volwalker2 on the same input VolWalker gets: the machine labels
		('local'/'remote'/'other') serve as its filesystem identifiers.
		"""
		by_uuid = {uuid: dict(x, fs_uuid=x['machine']) for uuid, x in all_subvols2.items()}
		return volwalker2.common_parents(by_uuid, my_uuid, direction[1])


	def _volwalker_shadow_compare(s, all_subvols2, my_uuid, direction, v1_res, v2_res):
		"""
		volwalker2 is a generalization of volwalker: it must find everything v1 finds
		(plus multi-hop candidates v1 misses). Extras are logged as info, missing
		candidates as a warning with the input dumped for turning into a test case.
		"""
		v1_uuids = set(x['local_uuid'] for x in v1_res)
		v2_uuids = set(x['local_uuid'] for x in v2_res)
		if v1_uuids == v2_uuids:
			logbfg.debug(f'volwalker shadow: v1 and v2 agree on {len(v1_uuids)} candidate(s)')
			return
		extra = v2_uuids - v1_uuids
		missing = v1_uuids - v2_uuids
		if extra:
			logbfg.info(f'volwalker shadow: volwalker2 found {len(extra)} additional candidate(s) (expected improvement): {sorted(extra)}')
		if missing:
			fn = f'/tmp/bfg_volwalker_mismatch_{time.strftime("%Y-%m-%d_%H-%M-%S")}.json'
			logbfg.warning(f'volwalker shadow: volwalker2 MISSED {len(missing)} candidate(s) found by v1: {sorted(missing)}, dumping input to {fn}')
			try:
				with open(fn, 'w') as f:
					json.dump({
						'my_uuid': my_uuid,
						'direction': list(direction),
						'v1': sorted(v1_uuids),
						'v2': sorted(v2_uuids),
						'subvols': list(all_subvols2.values()),
					}, f, default=datetime_to_json, indent=1)
			except Exception as e:
				logbfg.warning(f'could not dump volwalker mismatch: {e}')



def main():
	fire.Fire(Bfg)


if __name__ == "__main__":
	main()  # pragma: no cover
