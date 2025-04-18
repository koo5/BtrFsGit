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
- Snapshots are stored directly under a `.bfg_snapshots` directory.
- This directory is located either alongside the subvolume being snapshotted (e.g., `/parent/.bfg_snapshots/`) or inside it if the subvolume is the filesystem root (e.g., `/mnt/myfs/.bfg_snapshots/`).
- The snapshot name itself incorporates the original subvolume's base name, a timestamp, and a tag, joined by underscores (e.g., `subvolbasename_YYYY-MM-DD_HH-MM-SS_tag`).
- Example path: `/parent/.bfg_snapshots/data_2025-04-16_10-30-00_from_myhost`

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
import subprocess
import fire
import shlex  # python 3.8 required (for shlex.join)
from typing import List, Optional
# from .volwalker import * # Replaced by Prolog script
import subprocess # Added for calling Prolog
import json # Added for passing data to Prolog
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


	def _validate_snapshot_args(s, TAG, SNAPSHOT, SNAPSHOT_NAME):
		"""Helper to validate mutually exclusive snapshot naming arguments."""
		if TAG and SNAPSHOT:
			_prerr(f'Error: Please specify SNAPSHOT or TAG, not both.')
			sys.exit(1)
		if TAG and SNAPSHOT_NAME:
			_prerr(f'Error: Please specify SNAPSHOT_NAME or TAG, not both.')
			sys.exit(1)
		if SNAPSHOT and SNAPSHOT_NAME:
			_prerr(f'Error: Please specify SNAPSHOT_NAME or SNAPSHOT, not both.')
			sys.exit(1)


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
		else:
			fs = s.remote_fs_id5_mount_point(subvolume)

		# Always list subvolumes relative to the filesystem root (fs)
		# to ensure we capture all potential parents, regardless of the input 'subvolume' path.
		cmd = ['btrfs', 'subvolume', 'list', '-q', '-t', '-R', '-u']
		for line in command_runner(cmd + [str(fs)], logger=logbtrfs).splitlines()[2:]: # Use fs here
			subvol = s._make_snapshot_struct_from_sub_list_output_line(fs, line)
			subvol['src'] = src + '_btrfs'
			logger.debug(subvol)
			subvols.append(subvol)

		ro_subvols = set()
		# Also list read-only subvolumes relative to the filesystem root (fs).
		for line in command_runner(cmd + ['-r', str(fs)], logger=logbtrfs).splitlines()[2:]: # Use fs here
			subvol = s._make_snapshot_struct_from_sub_list_output_line(fs, line)
			ro_subvols.add(subvol['local_uuid'])
		# _prerr(str(ro_subvols))

		for i in subvols:
			i['ro'] = i['local_uuid'] in ro_subvols
			# we should not need this for remote subvolumes:
			if src == 'local':
				i['host'] = s.host
				i['fs_uuid'] = s.local_fs_uuid(subvolume)
			if '.bfg_snapshots' in i['path'].parts:
				i['dt'] = s.snapshot_dt(i)

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
					x['dt'] = s.snapshot_dt(x)
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
		s._validate_snapshot_args(TAG, SNAPSHOT, SNAPSHOT_NAME)

		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path('local', SUBVOL, TAG, SNAPSHOT_NAME).val
		return SNAPSHOT


	def snapshot_dt(s, snapshot):
		logbfg.debug(f'snapshot_dt {snapshot=}')
		dname = snapshot['path'].name
		return s.parse_snapshot_name(dname)['dt']


	def parse_snapshot_name(s, dname):
		# Expected pattern: <subvolbasename>_<timestamp>_<tag>
		# Example: mydata_2025-04-16_10-30-00_from_myhost

		# Regex captures: 1: subvolbasename, 2: timestamp, 3: tag
		m = re.match(r'(.+)_(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2})_(.*)', dname)
		if m is None:
			logbfg.error(f"Could not parse snapshot name format: {dname}")
			raise ValueError(f'Could not parse snapshot name format: {dname}')
		return (
			{'name': m.group(1),
			 'dt': datetime.strptime(m.group(2), "%Y-%m-%d_%H-%M-%S"),
			 'tags': m.group(3)})



	def calculate_default_snapshot_parent_dir(s, machine: str, SUBVOL):
		"""
		fixme: in fact calculates also the base of the actual directory name now.

		SUBVOL: your subvolume (for example /data).
		Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots/`, if that is still the same filesystem.
		"""
		SUBVOL = Path(SUBVOL)
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

			# hope to come up with a unique file names:
			f1 = 'bfg_' + str(time.time())
			f2 = f1 + "_dest"
			runner(['touch', str(SUBVOL / f1)], logger=logger)

			if runner(['cp', '--reflink', SUBVOL / f1, parent / f2], die_on_error=False, logger=logger, capture_stderr=True) != -1:
				snapshot_parent_dir = parent
				runner(['rm', parent / f2])
			else:
				logbfg.debug(
					f'cp --reflink failed, this means that {parent} is not the same filesystem, going to make snapshot inside {SUBVOL} instead of {parent}')
				snapshot_parent_dir = SUBVOL
			runner(['rm', SUBVOL / f1])

		# Return path only up to .bfg_snapshots, the subvol name is part of the snapshot name itself
		r = str(Path(str(snapshot_parent_dir) + '/.bfg_snapshots').absolute())
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

		# Construct the final snapshot path:
		# parent dir / (subvol_basename + _ + timestamp_tag)
		# e.g., /path/to/.bfg_snapshots / mydata_2025-04-16_10-30-00_from_myhost
		subvol_basename = Path(SUBVOL).name
		snapshot_filename = f"{subvol_basename}_{name}"
		res = Res(str(Path(parent) / snapshot_filename))
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
				logger.debug(f'YES')
				result.append(snapshot)
				snapshot['dt'] = s.snapshot_dt(snapshot)
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

		logbfg.info(f"Pruning snapshots for {SUBVOL=}")
		logbfg.debug(f'{DB=} {DRY_RUN=}')

		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']

		if DB:
			all = s.all_subvols_from_db()
			mrcs = set([x['path'] for x in s.most_recent_common_snapshots(all, SUBVOL)])
			logbfg.debug(f"{mrcs=}")
		else:
			all = []
			mrcs = set()

		local_snapshots = s.local_bfg_snapshots(all, SUBVOL)
		local_snapshots = sorted(local_snapshots, key=lambda x: x['dt'])

		if len(local_snapshots) == 0:
			logbfg.info(f"No snapshots to check for {SUBVOL}")
			return

		newest = local_snapshots[-1]['path']

		buckets = s.put_snapshots_into_buckets(local_snapshots)

		for bucket, snaplist in buckets.items():
			logbfg.info(f"Bucket: {bucket}")

			for i,snap in enumerate(snaplist):
				path = snap['path']

				is_newest = path == newest
				is_mrc = path in mrcs
				is_last = i == len(snaplist) - 1
				is_prunable = not is_newest and not is_mrc and not is_last

				flags = ''
				if is_mrc:
					flags += ' (mrc)'
				if is_newest:
					flags += ' (newest)'
				if is_last:
					flags += ' (last)'
				if is_prunable:
					flags += ' (prunable)'
				else:
					flags += ' - keep'
				logbfg.info(f"  {path}{flags}")

				if is_mrc:
					logbfg.debug(f"this is the most recent common snapshot as calculated from db, stopping here.")
					return

				if is_prunable and not DRY_RUN:
					cmd = ['btrfs', 'subvolume', 'delete', str(path)]
					if not s._yes(shlex.join(cmd)):
						continue
					s._local_cmd(cmd)
					_prerr(f"Deleted snapshot: {path}")

		_prerr("Done pruning.")


	def prune_remote(s, LOCAL_SUBVOL, REMOTE_SUBVOL, DRY_RUN=False):

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
		buckets = s.put_snapshots_into_buckets(remote_snapshots)


		for bucket, snaplist in buckets.items():
			logbfg.info(f"Bucket: {bucket}")

			for i,snap in enumerate(snaplist):
				path = snap['path']

				is_newest = path == newest
				is_mrc = path in mrcs
				is_last = i == len(snaplist) - 1
				is_prunable = not is_newest and not is_mrc and not is_last

				flags = ''
				if is_mrc:
					flags += ' (mrc)'
				if is_newest:
					flags += ' (newest)'
				if is_last:
					flags += ' (last)'
				if is_prunable:
					flags += ' (prunable)'
				logbfg.info(f"  {path}{flags}")

				if is_mrc:
					logbfg.info(f"this is the most recent common snapshot as calculated from db, stopping here.")
					return

				if is_prunable and not DRY_RUN:
					cmd = ['btrfs', 'subvolume', 'delete', str(path)]
					if not s._yes(shlex.join(cmd)):
						continue
					s._remote_cmd(cmd)
					_prerr(f"Deleted snapshot: {path}")

		_prerr("No more buckets.")



	def local_bfg_snapshots(s, all, SUBVOL):
		""" Filter snapshots from 'all' (typically from DB) that belong to SUBVOL's .bfg_snapshots dir """
		result = []
		# Calculate the expected parent directory for this subvolume's snapshots
		expected_parent_dir = Path(s.calculate_default_snapshot_parent_dir('local', Path(SUBVOL)).val)
		logbfg.debug(f"local_bfg_snapshots: filtering based on parent dir: {expected_parent_dir=}")

		for snap in all:
			# Ensure the snapshot's parent directory matches the expected one
			# Also check parent_uuid to be reasonably sure it's a snapshot *of* SUBVOL,
			# although parent_uuid can be None for received snapshots.
			# The primary filter is the directory location.
			if snap['path'].parent == expected_parent_dir and snap['parent_uuid'] == s._subvol_uuid:
				logbfg.debug(f"local_bfg_snapshots: added {snap['path']} (parent match)")
				result.append(snap)
			else:
				logbfg.debug(f"local_bfg_snapshots: skipped {snap['path']} (parent mismatch: {snap['path'].parent} != {expected_parent_dir})")
		return result


	def remote_bfg_snapshots(s, REMOTE_SUBVOL, remote_fs_uuid):
		all = s._get_subvolumes(s._remote_cmd, REMOTE_SUBVOL, 'remote')
		result = []

		p = Path(s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOL)).val)
		logbfg.debug(f"remote_bfg_snapshots: filtering based on parent dir: {p=}")

		for snap in all:
			path = snap['path']
			logbfg.debug(f"remote_bfg_snapshots checking {path=}")
			# Ensure the snapshot is directly inside the calculated .bfg_snapshots directory
			if path.parent == p:
				result.append(snap)
				logbfg.debug(f"remote_bfg_snapshots: added {snap['path']}")
			else:
				logbfg.debug(f"remote_bfg_snapshots: skipped {snap['path']} (parent mismatch: {path.parent} != {p})")

		return result



	def most_recent_common_snapshots(s, all, SUBVOL):
		"""
		Find the most recent common snapshots between the local and each remote filesystem.
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
				result.append(candidates[0])

		return result



	def remote_commit(s, REMOTE_SUBVOL, TAG=None, SNAPSHOT=None, SNAPSHOT_NAME=None):
		s._validate_snapshot_args(TAG, SNAPSHOT, SNAPSHOT_NAME)

		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path('remote', Path(REMOTE_SUBVOL), 'remote_commit',
														 SNAPSHOT_NAME).val
		s._remote_make_ro_snapshot(REMOTE_SUBVOL, SNAPSHOT)
		_prerr(f'DONE {s._remote_str},\n\tsnapshotted {REMOTE_SUBVOL} \n\tinto {SNAPSHOT}\n.')
		return Res(SNAPSHOT)



	def push(s, SUBVOL, SNAPSHOT, REMOTE_SUBVOL, PARENT=None, CLONESRCS=[]):
		"""
		Try to figure out shared parents, if not provided, and send SNAPSHOT to the other side.
		"""
		snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOL)).val
		logbfg.debug(f'mkdir -p {snapshot_parent_dir}')
		s._remote_cmd(['mkdir', '-p', str(snapshot_parent_dir)])

		if PARENT is None:
			logbfg.debug(f'get_subvol...')
			my_uuid = s.get_subvol(s._local_cmd, SUBVOL).val['local_uuid']
			logbfg.debug(f'find_common_parent...')
			PARENT = s.find_common_parent(SUBVOL, str(snapshot_parent_dir), my_uuid, ('local', 'remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		s.local_send(SNAPSHOT, ' | ' + s._sshstr + ' ' + s._sudo[0] + " btrfs receive " + str(snapshot_parent_dir), PARENT,
					 CLONESRCS)
		# Construct the final remote path using the same logic as calculate_default_snapshot_path
		# (parent dir + '/' + snapshot basename)
		# The snapshot basename already contains the subvol name and timestamp/tag.
		remote_snapshot_path = str(Path(snapshot_parent_dir) / Path(SNAPSHOT).name)
		_prerr(f'DONE, \n\tpushed {SNAPSHOT} \n\tinto {remote_snapshot_path}\n.')
		return Res(remote_snapshot_path)


	def transfer_snapshot(s, SNAPSHOT, REMOTE_PARENT_DIR, PARENT=None, CLONESRCS=[]):
		"""
		Transfer a specific local snapshot to a remote directory, figuring out the common parent.

		:param SNAPSHOT: Filesystem path to the existing local snapshot to send.
		:param REMOTE_PARENT_DIR: Filesystem path to the parent directory on the remote machine
								  where the snapshot should be received (e.g., /backup/.bfg_snapshots).
		:param PARENT: Optional: Explicitly specify the parent snapshot path for the send operation.
		:param CLONESRCS: Optional: List of clone source paths for the send operation.
		:return: Filesystem path of the snapshot created on the other machine.
		"""
		SNAPSHOT = Path(SNAPSHOT).absolute()
		REMOTE_PARENT_DIR = Path(REMOTE_PARENT_DIR)

		logbfg.debug(f'transfer_snapshot: {SNAPSHOT=} {REMOTE_PARENT_DIR=}')

		# Ensure remote parent directory exists
		logbfg.debug(f'mkdir -p {REMOTE_PARENT_DIR}')
		s._remote_cmd(['mkdir', '-p', str(REMOTE_PARENT_DIR)])

		if PARENT is None:
			logbfg.debug(f'get_subvol for local snapshot {SNAPSHOT}...')
			# Use get_local_subvol which handles the 'local' machine context
			my_uuid = s.get_local_subvol(SNAPSHOT)['local_uuid']
			logbfg.debug(f'find_common_parent using local snapshot path and remote parent dir...')
			# Pass LOCAL_SNAPSHOT_PATH as the 'local subvolume context' and REMOTE_PARENT_DIR as the 'remote subvolume context'
			# This assumes find_common_parent/parent_candidates can work with these potentially disparate paths
			# by fetching subvolume lists rooted appropriately.
			PARENT = s.find_common_parent(str(SNAPSHOT), str(REMOTE_PARENT_DIR), my_uuid, ('local', 'remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath'] # Use the absolute path determined by find_common_parent

		logbfg.info(f"Sending {SNAPSHOT} to {REMOTE_PARENT_DIR} with parent {PARENT}")
		s.local_send(str(SNAPSHOT), ' | ' + s._sshstr + ' ' + s._sudo[0] + " btrfs receive " + str(REMOTE_PARENT_DIR), PARENT, CLONESRCS)

		# Construct the final remote path
		remote_snapshot_path = str(REMOTE_PARENT_DIR / SNAPSHOT.name)
		_prerr(f'DONE, \n\ttransferred {SNAPSHOT} \n\tinto {remote_snapshot_path}\n.')
		return Res(remote_snapshot_path)


	def pull(s, REMOTE_SNAPSHOT, LOCAL_SUBVOL, PARENT=None, CLONESRCS=[]):
		local_snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('local', Path(LOCAL_SUBVOL)).val
		s._local_cmd(['mkdir', '-p', str(local_snapshot_parent_dir)])

		if PARENT is None:
			my_uuid = s.get_subvol(s._remote_cmd, REMOTE_SNAPSHOT).val['local_uuid']
			PARENT = s.find_common_parent(local_snapshot_parent_dir, REMOTE_SNAPSHOT, my_uuid, ('remote', 'local')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		s.remote_send(REMOTE_SNAPSHOT, local_snapshot_parent_dir, PARENT, CLONESRCS)

		# Construct the final local path using the same logic as calculate_default_snapshot_path
		# (parent dir + '/' + snapshot basename)
		# The snapshot basename already contains the subvol name and timestamp/tag.
		local_snapshot_path = str(Path(local_snapshot_parent_dir) / Path(REMOTE_SNAPSHOT).name)

		_prerr(f'DONE, \n\tpulled {REMOTE_SNAPSHOT} \n\tinto {local_snapshot_path}\n.')
		return Res(local_snapshot_path)



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

		cmd1 = shlex.split(s._sshstr) + s._sudo + ['btrfs', 'send'] + parents_args + [REMOTE_SNAPSHOT]
		cmd2 = s._sudo + ['btrfs', 'receive', LOCAL_DIR]
		logbtrfs.info(shlex.join(cmd1) + ' >>|>> ' + shlex.join(cmd2))
		p1 = subprocess.Popen(
			cmd1,
			stdout=subprocess.PIPE)
		p2 = subprocess.Popen(
			cmd2,
			stdin=p1.stdout)
		p1.stdout.close()  # https://www.titanwolf.org/Network/q/91c3c5dd-aa49-4bf4-911d-1bfe5ac304da/y
		p2.communicate()
		if p2.returncode != 0:
			loggfg.error('exit code ' + str(p2.returncode))
			exit(1)



	def find_common_parent(s, subvolume, remote_subvolume, my_uuid, direction):
		logbfg.debug(f'parent_candidates...')
		candidates = s.parent_candidates(subvolume, remote_subvolume, my_uuid, direction).val

		if len(candidates) != 0:
			# Sort candidates by subvolume ID descending to pick the most recent common ancestor.
			# While subvol_id isn't perfect (e.g., snapshotting an old RO snapshot gives it a high ID),
			# it's often a better heuristic than generation or taking the first arbitrary result from the walk.
			candidates.sort(key=lambda sv: sv['subvol_id'], reverse=True)
			winner = candidates[0]
			s._add_abspath(winner)
			logbtrfs.info(f'Found {len(candidates)} potential common parents. Picked candidate with highest subvol_id.')
			logbtrfs.debug(f'PICKED COMMON PARENT {winner["abspath"]}.')
			logbtrfs.debug(f'details: {winner}.')
			return Res(winner)
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
					continue
					raise 'wut'

			# if not Path('.bfg_snapshots') in Path(i['path']).parts:
			# 	continue

			all_subvols2[i['local_uuid']] = i
			logging.debug('_parent_candidates2:' + json.dumps(i, indent=2, default=datetime_to_json, sort_keys=True))

		logging.debug(f'_parent_candidates2 all_subvols: {len(all_subvols2)}')

		# Prepare data for Prolog script
		source_uuid = my_uuid
		source_machine = direction[0]
		target_machine = direction[1]
		# Convert Path objects to strings for JSON serialization
		serializable_subvols = []
		for uuid, subvol_data in all_subvols2.items():
			data_copy = subvol_data.copy()
			if 'path' in data_copy and isinstance(data_copy['path'], Path):
				data_copy['path'] = str(data_copy['path'])
			serializable_subvols.append(data_copy)

		json_data = json.dumps(serializable_subvols, default=datetime_to_json)

		# Construct the command to run the Prolog script
		script_path = Path(__file__).parent / 'volwalker2.pl'
		cmd = ['swipl', str(script_path), source_uuid, source_machine, target_machine, json_data]
		logbfg.debug(f"Running Prolog script: {' '.join(cmd[:5])} <JSON_DATA>") # Avoid logging large JSON

		try:
			# Run the Prolog script
			result = subprocess.run(cmd, capture_output=True, text=True, check=True)
			logbfg.debug(f"Prolog script stdout:\n{result.stdout}")
			logbfg.debug(f"Prolog script stderr:\n{result.stderr}")

			# Parse the output (one candidate UUID per line)
			candidate_uuids = result.stdout.strip().splitlines()
			logbfg.info(f"Prolog script found {len(candidate_uuids)} candidate parent UUIDs.")

			# Yield the corresponding subvolume dictionaries from our map
			for uuid in candidate_uuids:
				if uuid in all_subvols2:
					logbfg.debug(f"Yielding candidate: {uuid}")
					yield all_subvols2[uuid]
				else:
					logbfg.warning(f"Prolog returned UUID {uuid} which is not in the initial subvolume map.")

		except FileNotFoundError:
			logbfg.error(f"Error: 'swipl' command not found. Is SWI-Prolog installed and in PATH?")
			raise
		except subprocess.CalledProcessError as e:
			logbfg.error(f"Error running Prolog script: {e}")
			logbfg.error(f"Command: {' '.join(cmd[:5])} <JSON_DATA>")
			logbfg.error(f"Return code: {e.returncode}")
			logbfg.error(f"Stdout: {e.stdout}")
			logbfg.error(f"Stderr: {e.stderr}")
			raise
		except Exception as e:
			logbfg.error(f"An unexpected error occurred while running/parsing Prolog script: {e}")
			raise



def main():
	fire.Fire(Bfg)


if __name__ == "__main__":
	main()  # pragma: no cover
