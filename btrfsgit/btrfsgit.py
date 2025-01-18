#!/usr/bin/env python3

"""
BtrFsGit
"""
import logging

from sqlalchemy.orm import undefer

from btrfsgit.bfg_logging import configure_logging
configure_logging()

logbtrfs = logging.getLogger('btrfs')
logutils = logging.getLogger('utils')
logbfg = logging.getLogger('bfg')


from pathlib import Path
from pathvalidate import sanitize_filename
import sys, os
import time
import subprocess
import fire
import shlex  # python 3.8 required (at least for shlex.join)
from typing import List, Optional
from .utils import *
from collections import defaultdict
import re
from datetime import datetime
import btrfsgit.db as db




def datetime_to_json(o):
    if isinstance(o, datetime):
        return o.isoformat()
    raise TypeError(f"Type {type(o)} not serializable")



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
			if default is not None and choice == "":
				return valid[default]
			elif choice in valid:
				return valid[choice]
			else:
				sys.stdout.write("Please respond with 'yes' or 'no' " "(or 'y' or 'n').\n")

class Bfg:

	def __init__(s, LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, sshstr='',
				 YES=False):

		logbfg.debug(f'__init__...')

		s._local_fs_id5_mount_point = LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT

		s._remote_fs_id5_mount_point = REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT
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
		s._local_fs_uuid = s.get_fs()


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

	def _remote_cmd(s, cmd, die_on_error=True, logger=None):
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
			return s._cmd(cmd2, die_on_error)
		else:
			return s._local_cmd(cmd, die_on_error)

	def _local_cmd(s, c, die_on_error=True, logger=None):
		if logger is None:
			logger = logging.getLogger('btrfs')
		if not isinstance(c, list):
			c = shlex.split(c)
		c = s._sudo + [str(x) for x in c]
		logger.debug(shlex.join(c))
		return s._cmd(c, die_on_error)

	def _cmd(s, c, die_on_error):
		try:
			return subprocess.check_output(c, text=True)
		except Exception as e:
			if die_on_error:
				_prerr(e)
				exit(1)
			else:
				return -1

	"""

	helper stuff
	"""

	def calculate_default_snapshot_parent_dir(s, machine: str, SUBVOLUME):
		"""
		fixme: in fact calculates also the base of the actual directory name now.

		SUBVOLUME: your subvolume (for example /data).
		Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots.data`, if that is still the same filesystem.
		"""
		SUBVOLUME = Path(SUBVOLUME)
		parent = SUBVOLUME.parent

		logger = logging.getLogger('calculate_default_snapshot_parent_dir')

		logger.debug(f'calculate_default_snapshot_parent_dir for {SUBVOLUME=}')

		# is parent the same filesystem as SUBVOLUME? if not, then SUBVOLUME is the top level subvolume, and we need to make the snapshot inside it, rather than outside.

		if machine == 'local':
			runner = s._local_cmd
		else:
			runner = s._remote_cmd

		if runner(['test', '-e', str(SUBVOLUME)], die_on_error=False, logger=logger) == -1:
			# we assume that if the target filesystem is mounted. This implies that if we're transferring the root subvol, the directory exists. This is the only case where the snapshot parent dir will be inside the subvol, rather than outside. Therefore, if the destination does not exist (as a directory or subvolume), it is safe to assume that it is not the root subvolume.
			snapshot_parent_dir = parent
		else:

			runner(['mkdir', '-p', str(SUBVOLUME)], logger=logger)

			# hope to come up with a unique file names:
			f1 = str(time.time())
			f2 = f1 + "_dest"
			runner(['touch', str(SUBVOLUME / f1)], logger=logger)

			if runner(['cp', '--reflink', SUBVOLUME / f1, parent / f2], die_on_error=False, logger=logger) != -1:
				snapshot_parent_dir = parent
			else:
				_prerr(
					f'cp --reflink failed, this means that {parent} is not the same filesystem, going to make snapshot inside {SUBVOLUME} instead of {parent}')
				snapshot_parent_dir = SUBVOLUME

		r = str(Path(
			str(snapshot_parent_dir) + '/.bfg_snapshots/' + Path(SUBVOLUME).parts[-1]).absolute())
		logging.getLogger('utils').info(f'calculate_default_snapshot_parent_dir: {SUBVOLUME=} -> {r=}')
		return Res(r)

	def calculate_default_snapshot_path(s, machine, SUBVOLUME, TAG, NAME_OVERRIDE=None):  # , TAG2):
		"""
		calculate the filesystem path where a snapshot should go, given a subvolume and a tag
		"""
		parent = s.calculate_default_snapshot_parent_dir(machine, SUBVOLUME).val

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


	def get_fs(s):
		l = s._local_cmd(f'btrfs filesystem show ' + s._local_fs_id5_mount_point).splitlines()[0]
		r = r"Label:\s+'.*'\s+uuid:\s+([a-f0-9-]+)$"
		fs_uuid = re.match(r, l).group(1)
		logbfg.info(f'get_fs: {fs_uuid=}')
		return fs_uuid


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

	high-level, compound commands

	"""

	def commit_and_push_and_checkout(s, SUBVOLUME, REMOTE_SUBVOLUME, PARENT: str = None):
		"""
		Snapshot your data, "btrfs send"/"btrfs receive" the snapshot to the other machine, and checkout it there

		:param FS_ROOT_MOUNT_POINT: mount point of SUBVOLUME filesystem
		:param SUBVOLUME: your data
		:param REMOTE_SUBVOLUME: desired filesystem path of your data on the other machine
		:return: filesystem path of the snapshot created on the other machine
		"""
		# todo: subvolume could default to s._local_fs_id5_mount_point

		remote_snapshot_path = s.commit_and_push(SUBVOLUME, REMOTE_SUBVOLUME, PARENT=PARENT).val
		s.checkout_remote(remote_snapshot_path, REMOTE_SUBVOLUME)
		return Res(REMOTE_SUBVOLUME)


	def remote_commit_and_pull(s, REMOTE_SUBVOLUME, SUBVOLUME):
		"""
		same as commit_and_push_and_checkout but going the other direction

		:param FS_ROOT_MOUNT_POINT:
		:param REMOTE_SUBVOLUME:
		:param SUBVOLUME:
		:return:
		"""
		remote_snapshot_path = s.remote_commit(REMOTE_SUBVOLUME).val
		local_snapshot_path = s.pull(remote_snapshot_path, SUBVOLUME).val
		s.checkout_local(local_snapshot_path, SUBVOLUME)
		_prerr(f'DONE, \n\tpulled {remote_snapshot_path} \n\tinto {SUBVOLUME}\n.')
		return Res(SUBVOLUME)


	def commit_and_generate_patch(s, SUBVOLUME='/', PATCH_FILE_DIR='/', PARENT: Optional[str]=None):
		"""
		store a `btrfs send` stream locally

		:param SUBVOLUME:
		:param PATCH_FILE_DIR:
		:param PARENTS:
		:return:
		"""
		snapshot = s.local_commit(SUBVOLUME).val
		# print(Path(snapshot).parts[-2:])
		fn = PATCH_FILE_DIR + '/' + '__'.join(Path(snapshot).parts[-2:])
		# print(fn)
		s.local_send(snapshot, ' > ' + fn, PARENT)
		_prerr(f'DONE, generated patch \n\tfrom {snapshot} \n\tinto {fn}\n.')
		return Res(fn)


	def commit_and_push(s, SUBVOLUME, REMOTE_SUBVOLUME, SNAPSHOT_TAG=None, SNAPSHOT_PATH=None, SNAPSHOT_NAME=None,
						PARENT=None, CLONESRCS: List[str] = []):
		"""commit, and transfer the snapshot into .bfg_snapshots on the other machine"""
		snapshot = s.local_commit(SUBVOLUME, SNAPSHOT_TAG, SNAPSHOT_PATH, SNAPSHOT_NAME).val
		return Res(s.push(SUBVOLUME, snapshot, REMOTE_SUBVOLUME, PARENT, CLONESRCS).val)




	"""
	basic commands
	"""



	def get_local_subvol(s, SUBVOLUME):
		"""get info about a subvolume"""
		return s.get_subvol(s._local_cmd, SUBVOLUME)



	def get_local_bfg_snapshots(s, SUBVOLUME):
		"""list snapshots in .bfg_snapshots"""
		logger = logging.getLogger('get_local_bfg_snapshots')
		logger.debug('get_local_bfg_snapshots...')
		local_snapshots = s.get_local_snapshots(SUBVOLUME).val
		result = []
		for snapshot in local_snapshots:
			logger.debug(f'{snapshot=}')
			if '.bfg_snapshots' in Path(snapshot['path']).parts:
				logger.debug(f'YES')
				result.append(snapshot)
				snapshot['dt'] = s.snapshot_dt(snapshot)
		logbfg.info(f'get_local_bfg_snapshots_for_subvol: {len(result)=}')
		return Res(result)



	def snapshot_dt(s, snapshot):

		dname = snapshot['path'].split('/')[-1]
		# Typical pattern might be:
		#   <subvol>_bfg_snapshots_<timestamp>_<tag>
		#   <subvol>_<timestamp>_<tag>
		# We'll attempt to capture all via two regex tries:

		m = re.match(r'(.+)_bfg_snapshots_(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2})_(.*)', dname)
		if m is None:
			m = re.match(r'(.+)_(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2})_(.*)', dname)
		if m is None:
			raise Exception(f'could not parse snapshot folder: {dname}')
		return datetime.strptime(m.group(2), "%Y-%m-%d_%H-%M-%S")




	def get_local_snapshots(s, SUBVOLUME):
		"""list snapshots of SUBVOLUME on the local machine, that is, all read-only subvolumes on the filesystem, that are children of SUBVOLUME"""
		logbtrfs.debug(f'get_local_snapshots...')
		logger = logging.getLogger('get_local_snapshots')
		uuid = s.get_subvol(s._local_cmd, SUBVOLUME).val['local_uuid']
		subvols = s._get_subvolumes(s._local_cmd, SUBVOLUME)
		logger.debug(f'{subvols=}')

		snapshots = []
		for subvol in subvols:
			logger.debug(f'{subvol=}')
			if subvol['parent_uuid'] == uuid and subvol['ro']:
				logger.debug(f'YES')
				snapshots.append(subvol)
			else:
				logger.debug(f'NO')

		logbtrfs.info(f'get_local_snapshots: {len(snapshots)=}')
		return Res(snapshots)


	def get_all_local_bfg_snapshots_on_filesystem(s):
		"""list all read-only subvolumes on the filesystem that exist somewhere under any .bfg_snapshots dir"""
		logbfg.info	(f'get_all_local_bfg_snapshots_on_filesystem...')
		logger = logging.getLogger('get_all_local_bfg_snapshots_on_filesystem')
		subvols = s._get_subvolumes(s._local_cmd, s._local_fs_id5_mount_point)
		logger.debug(f'{subvols=}')

		snapshots = []
		for subvol in subvols:
			logger.debug(f'{subvol=}')
			subvol['fs_uuid'] = s._local_fs_uuid

			if subvol['ro'] and ('.bfg_snapshots' in Path(subvol['path']).parts):
				logger.debug(f'YES')
				snapshots.append(subvol)
			else:
				logger.debug(f'NO')

		for snapshot in snapshots:
			snapshot['host'] = s.host

		logbtrfs.info(f'get_all_local_bfg_snapshots_on_filesystem: {len(snapshots)=}')
		return Res(snapshots)



	def update_db_with_local_bfg_snapshots(s):
		"""
		blast the db with all the bfg snapshots we can find on the filesystem.
		update the global database:
			walk the snapshots and insert missing snapshots into db
			walk the table and mark missing snapshots as deleted in db
		"""
		snapshots = s.get_all_local_bfg_snapshots_on_filesystem().val
		logbfg.info(f'db.session()...')
		session = db.session()
		with session.begin():
			logbfg.info(f'insert missing snapshots into db...')
			all_ = session.query(db.Snapshot).all()
			all = {s.local_uuid: s for s in all_}

			for i,snapshot in enumerate(snapshots):
				if i % 100 == 0:
					logbfg.info(f'{i=}')
				logbfg.debug(f'{snapshot=}')
				db_snapshot = all.get(snapshot['local_uuid'])
				logbfg.debug(f'{db_snapshot=}')
				if db_snapshot is None:
					db_snapshot = db.Snapshot(
						local_uuid=snapshot['local_uuid'],
						parent_uuid=snapshot['parent_uuid'],
						received_uuid=snapshot['received_uuid'],
						host=snapshot['host'],
						fs=s._local_fs_id5_mount_point,
						path=snapshot['path'],
						fs_uuid=snapshot['fs_uuid']
					)
					session.add(db_snapshot)
			logbfg.info(f'mark missing snapshots as deleted in db...')
			local_uuids = [s['local_uuid'] for s in snapshots]
			for i,db_snapshot in enumerate(all_):
				if i % 1000 == 0:
					logbfg.info(f'{i=}')
				if db_snapshot.host != s.host or Path(db_snapshot.fs) != Path(s._local_fs_id5_mount_point):
					continue
				if db_snapshot.local_uuid not in local_uuids:
					logbfg.info(f'mark {db_snapshot.local_uuid} as deleted')
					db_snapshot.deleted = True
			logbfg.info(f'commit...')


	def all_snapshots_from_db(self):
		"""
		"""
		logbfg.info(f'all_snapshots_from_db...')
		session = db.session()
		with session.begin():
			all = session.query(db.Snapshot).options(undefer("*")).all()

			r = [{
				column.name: getattr(x, column.name)
				for column in x.__table__.columns} for x in all]

			for x in r:
				x['dt'] = self.snapshot_dt(x)

			return r



	def get_local_subvolumes(s, SUBVOLUME):
		"""list subvolumes on the local machine"""
		return Res(s._get_subvolumes(s._local_cmd, SUBVOLUME))



	def checkout_local(s, SNAPSHOT, SUBVOLUME):
		"""stash your SUBVOLUME, and replace it with SNAPSHOT"""
		s.stash_local(SUBVOLUME)
		s._local_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOLUME}')
		_prerr(f'DONE {s._local_str}, \n\tchecked out {SNAPSHOT} \n\tinto {SUBVOLUME}\n.')
		return Res(SUBVOLUME)



	def checkout_remote(s, SNAPSHOT, SUBVOLUME):
		"""ssh into the other machine,
		stash your SUBVOLUME, and replace it with SNAPSHOT"""
		s.stash_remote(SUBVOLUME)
		s._remote_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOLUME}')
		_prerr(f'DONE {s._remote_str}, \n\tchecked out {SNAPSHOT} \n\tinto {SUBVOLUME}\n.')
		return Res(SUBVOLUME)



	def stash_local(s, SUBVOLUME, SNAPSHOT_TAG='stash', SNAPSHOT_NAME=None):
		"""
		snapshot and delete your SUBVOLUME

		todo: maybe an alternative way should be to just move it?
		"""
		if s._local_cmd(['ls', SUBVOLUME], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._local_str}, {SUBVOLUME} doesn\'t exist.')
			return None
		else:
			snapshot = s._local_make_ro_snapshot(SUBVOLUME,
												 s.calculate_default_snapshot_path('local', SUBVOLUME, SNAPSHOT_TAG,
																				   SNAPSHOT_NAME).val)

			cmd = f'btrfs subvolume delete {SUBVOLUME}'
			if not s._yes(cmd):
				exit(1)
			s._local_cmd(cmd)
			_prerr(f'DONE {s._local_str}, \n\tsnapshotted {SUBVOLUME} into \n\t{snapshot}\n, and deleted it.')
			return Res(snapshot)



	def stash_remote(s, SUBVOLUME):
		"""snapshot and delete your SUBVOLUME"""
		if s._remote_cmd(['test', '-e', SUBVOLUME], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._remote_str}, {SUBVOLUME} doesn\'t exist.')
			return None
		else:
			_prerr(f'gonna stash {s._remote_str}, {SUBVOLUME}.')
			snapshot = s._remote_make_ro_snapshot(SUBVOLUME,
												  s.calculate_default_snapshot_path('remote', Path(SUBVOLUME),
																					'stash_before_remote_checkout').val)

			cmd = f'btrfs subvolume delete {SUBVOLUME}'
			if not s._yes(cmd):
				exit(1)
			s._remote_cmd(cmd)

			_prerr(f'DONE {s._remote_str}, \n\tsnapshotted {SUBVOLUME} \n\tinto {snapshot}\n, and deleted it.')
			return Res(snapshot)



	def local_commit(s, SUBVOLUME='/', TAG=None, SNAPSHOT=None, SNAPSHOT_NAME=None):
		"""
		come up with a filesystem path for a snapshot, and snapshot SUBVOLUME.
		:param SNAPSHOT: override default filesystem path where snapshot will be created
		:param TAG: override the tag for the default SNAPSHOT (hostname by default)
		"""
		SUBVOLUME = Path(SUBVOLUME).absolute()
		SNAPSHOT = s._figure_out_snapshot_name(SUBVOLUME, TAG, SNAPSHOT, SNAPSHOT_NAME)
		s._local_make_ro_snapshot(SUBVOLUME, SNAPSHOT)
		return Res(SNAPSHOT)



	def _figure_out_snapshot_name(s, SUBVOLUME, TAG, SNAPSHOT, SNAPSHOT_NAME):
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
			SNAPSHOT = s.calculate_default_snapshot_path('local', SUBVOLUME, TAG, SNAPSHOT_NAME).val
		return SNAPSHOT




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


	def prune(s, SUBVOLUME, CHECK_WITH_DB=True, DRY_RUN=False):
		"""
		Prune old snapshots under SUBVOLUME according to a time-based retention policy.

		1) Keep the oldest snapshot.
		2) Keep the newest snapshot.
		3) For snapshots < 1 minute old, keep only the most recent in that window.
		4) For snapshots < 1 hour old, keep one per minute.
		5) For snapshots < 1 day old, keep one per hour.
		6) For snapshots < 1 month old (~30 days), keep one per day.
		7) For snapshots >= 1 month old, keep one per month.
		8) Delete everything else.
		"""

		logbfg.info(f"Pruning snapshots for {SUBVOLUME=}")
		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOLUME).val['local_uuid']

		all = s.all_snapshots_from_db()
		local_snapshots = s.local_bfg_snapshots(all, SUBVOLUME)
		local_snapshots = sorted(local_snapshots, key=lambda x: x['dt'])
		if len(local_snapshots) == 0:
			logbfg.info(f"No snapshots found for {SUBVOLUME}")
			return
		newest = Path(local_snapshots[0]['path'])
		buckets = s.put_snapshots_into_buckets(local_snapshots)
		mrcs = set([Path(x['path']) for x in s.most_recent_common_snapshots(all, SUBVOLUME)])
		logbfg.info(f"{mrcs=}")

		for bucket, snaplist in buckets.items():

			logbfg.info(f"Bucket: {bucket}")

			for i,snap in enumerate(snaplist):

				path = Path(snap['path'])

				is_newest = path == newest
				is_mrc = path in mrcs
				is_last = i == len(snaplist) - 1
				#logbfg.info(f"{path} {is_newest=} {is_mrc=} {is_last=}, {i=}/{len(snaplist)}")
				is_prunable = not is_newest and not is_mrc and not is_last

				logbfg.info(f"  {snap['path']}" + (" (mrcs)" if is_mrc else "") + (" (newest)" if is_newest else (" (last)" if is_last else "") + (" (prune?)" if is_prunable else "")))

				if is_prunable and not DRY_RUN:

					cmd = ['btrfs', 'subvolume', 'delete', str(path)]
					if not s._yes(shlex.join(cmd)):
						continue
					s._local_cmd(cmd)
					_prerr(f"Deleted snapshot: {path}")

		_prerr("No more buckets.")



	def local_bfg_snapshots(s, all, SUBVOLUME):
		result = []

		for snap in all:
			if snap['parent_uuid'] == s._subvol_uuid and not snap['deleted'] and '.bfg_snapshots' in Path(snap['path']).parts:
				result.append(snap)

		return result




	def most_recent_common_snapshots(s, all, SUBVOLUME):
		"""
		Find the most recent common snapshots between the local and each remote filesystem.

		"""

		result = []

		s._subvol_uuid = s.get_subvol(s._local_cmd, SUBVOLUME).val['local_uuid']
		fss = s.remote_fs_uuids(all)
		logbfg.info(f"{fss=}")

		for fs_uuid, fs in fss.items():
			hosts = fs['hosts']

			logbfg.info(f"most_recent_common_snapshots for {fs_uuid=} (hosts:{hosts})....")

			all2 = []
			for snap in all:
				x = dict(snap)
				if x['fs_uuid'] == s._local_fs_uuid:
					x['machine'] = 'local'
				elif x['fs_uuid'] == fs_uuid:
					x['machine'] = 'remote'
				else:
					x['machine'] = 'other'
				all2.append(x)
				logbfg.info(f"  {x=}")

			logbfg.info(f"all2: {len(all2)}")
			logbfg.info(f"_parent_candidates2...")
			candidates = list(s._parent_candidates2(all2, s._subvol_uuid , ('local', 'remote')))

			logbfg.info(f"candidates: {len(candidates)}")

			for candidate in candidates:
				logbfg.info(f"  {candidate['local_uuid']}")

			if len(candidates) > 0:
				result.append(Path(candidates[0]))

		return result



	def remote_fs_uuids(s, all):
		fss = {}
		for snap in all:
			snap_fs_uuid = snap['fs_uuid']
			if snap_fs_uuid not in fss:
				fss[snap_fs_uuid] = {'hosts': set()}
			fss[snap_fs_uuid]['hosts'].add(snap['host'])
		del fss[s._local_fs_uuid]
		return fss


	def remote_commit(s, REMOTE_SUBVOLUME, TAG=None, SNAPSHOT=None, SNAPSHOT_NAME=None):
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
			SNAPSHOT = s.calculate_default_snapshot_path('remote', Path(REMOTE_SUBVOLUME), 'remote_commit',
														 SNAPSHOT_NAME).val
		s._remote_make_ro_snapshot(REMOTE_SUBVOLUME, SNAPSHOT)
		_prerr(f'DONE {s._remote_str},\n\tsnapshotted {REMOTE_SUBVOLUME} \n\tinto {SNAPSHOT}\n.')
		return Res(SNAPSHOT)



	def push(s, SUBVOLUME, SNAPSHOT, REMOTE_SUBVOLUME, PARENT=None, CLONESRCS=[]):
		"""
		Try to figure out shared parents, if not provided, and send SNAPSHOT to the other side.
		"""
		snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOLUME)).val
		s._remote_cmd(['mkdir', '-p', str(snapshot_parent_dir)])

		if PARENT is None:
			my_uuid = s.get_subvol(s._local_cmd, SUBVOLUME).val['local_uuid']
			PARENT = s.find_common_parent(SUBVOLUME, str(snapshot_parent_dir), my_uuid, ('local', 'remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		s.local_send(SNAPSHOT, ' | ' + s._sshstr + ' ' + s._sudo[0] + " btrfs receive " + str(snapshot_parent_dir), PARENT,
					 CLONESRCS)
		_prerr(f'DONE, \n\tpushed {SNAPSHOT} \n\tinto {snapshot_parent_dir}\n.')
		return Res(str(snapshot_parent_dir) + '/' + Path(SNAPSHOT).parts[-1])



	def pull(s, REMOTE_SNAPSHOT, LOCAL_SUBVOLUME, PARENT=None, CLONESRCS=[]):
		local_snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('local', Path(LOCAL_SUBVOLUME)).val
		s._local_cmd(['mkdir', '-p', str(local_snapshot_parent_dir)])

		if PARENT is None:
			my_uuid = s.get_subvol(s._remote_cmd, REMOTE_SNAPSHOT).val['local_uuid']
			PARENT = s.find_common_parent(local_snapshot_parent_dir, REMOTE_SNAPSHOT, my_uuid, ('remote', 'local')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		s.remote_send(REMOTE_SNAPSHOT, local_snapshot_parent_dir, PARENT, CLONESRCS)

		local_snapshot = str(local_snapshot_parent_dir) + '/' + Path(REMOTE_SNAPSHOT).parts[-1]

		_prerr(f'DONE, \n\tpulled {REMOTE_SNAPSHOT} \n\tinto {local_snapshot}\n.')
		return Res(local_snapshot)



	"""

	low-level operations
	"""

	def _local_make_ro_snapshot(s, SUBVOLUME, SNAPSHOT):
		"""make a read-only snapshot of SUBVOLUME into SNAPSHOT, locally"""
		SNAPSHOT_PARENT = os.path.split((SNAPSHOT))[0]
		s._local_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._local_cmd(f'btrfs subvolume snapshot -r {SUBVOLUME} {SNAPSHOT}')
		_prerr(f'DONE {s._local_str}, \n\tsnapshotted {SUBVOLUME} \n\tinto {SNAPSHOT}\n.')
		return SNAPSHOT



	def _remote_make_ro_snapshot(s, SUBVOLUME, SNAPSHOT):
		"""make a read-only snapshot of SUBVOLUME into SNAPSHOT, remotely"""
		SNAPSHOT_PARENT = os.path.split((SNAPSHOT))[0]
		s._remote_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._remote_cmd(f'btrfs subvolume snapshot -r {SUBVOLUME} {SNAPSHOT}')
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
		_prerr((cmd) + ' #...')
		subprocess.check_call(cmd, shell=True)



	def remote_send(s, REMOTE_SNAPSHOT, LOCAL_DIR, PARENT, CLONESRCS):
		parents_args = s._parent_args(PARENT, CLONESRCS)

		cmd1 = shlex.split(s._sshstr) + s._sudo + ['btrfs', 'send'] + parents_args + [REMOTE_SNAPSHOT]
		cmd2 = s._sudo + ['btrfs', 'receive', LOCAL_DIR]
		_prerr(shlex.join(cmd1) + ' >>|>> ' + shlex.join(cmd2))
		p1 = subprocess.Popen(
			cmd1,
			stdout=subprocess.PIPE)
		p2 = subprocess.Popen(
			cmd2,
			stdin=p1.stdout)
		p1.stdout.close()  # https://www.titanwolf.org/Network/q/91c3c5dd-aa49-4bf4-911d-1bfe5ac304da/y
		p2.communicate()
		if p2.returncode != 0:
			_prerr('exit code ' + str(p2.returncode))
			exit(1)



	def find_common_parent(s, subvolume, remote_subvolume, my_uuid, direction):
		candidates = s.parent_candidates(subvolume, remote_subvolume, my_uuid, direction).val
		# candidates.sort(key = lambda sv: sv['subvol_id']) # nope, subvol id is a crude approximation. What happens when you snapshot and old ro snapshot? It gets the highest id.
		if len(candidates) != 0:
			winner = candidates[0]
			s._add_abspath(winner)
			_prerr(f'PICKED COMMON PARENT {winner}.')
			return Res(winner)
		else:
			return Res(None)



	def _add_abspath(s, subvol_record):
		if subvol_record['machine'] == 'remote':
			s._remote_add_abspath(subvol_record)
		else:
			s._local_add_abspath(subvol_record)



	def _local_add_abspath(s, subvol_record):
		if s._local_fs_id5_mount_point is None:
			s._local_fs_id5_mount_point = prompt(
				{
					'type': 'input',
					'name': 'path',
					'message': "where did you mount the top level subvolume (ID 5, not your /@ root)? Yes this is silly but i really need to know."
				}
			)['path']
		subvol_record['abspath'] = s._local_fs_id5_mount_point + '/' + s._local_cmd(
			['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), s._local_fs_id5_mount_point]).strip()



	def _remote_add_abspath(s, subvol_record):
		if s._remote_fs_id5_mount_point is None:
			s._remote_fs_id5_mount_point = prompt(
				{
					'type': 'input',
					'name': 'path',
					'message': "where did you mount the top level subvolume (ID 5, not your /@ root) on the remote machine? Yes this is silly but i really need to know."
				}
			)['path']
		subvol_record['abspath'] = s._remote_fs_id5_mount_point + '/' + s._remote_cmd(
			['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), s._remote_fs_id5_mount_point]).strip()



	def parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction):
		candidates = []
		for c in s._parent_candidates(subvolume, remote_subvolume, my_uuid, direction):
			candidates.append(c)
			_prerr('shared parent: ' + c['local_uuid'])
		return Res(candidates)



	def _parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction):

		remote_subvols = s._get_subvolumes(s._remote_cmd, remote_subvolume, 'remote')
		local_subvols = s._get_subvolumes(s._local_cmd, subvolume, 'local')

		#other_subvols = load_subvol_dumps()
		#toplevel_subvol = s.get_subvol(s._local_cmd, subvolume).val
		#toplevel_subvol['src'] = 'get_subvol'
		#toplevel_subvols = [toplevel_subvol]

		all_subvols = []
		for machine, lst in [
			('remote', remote_subvols),
			('local', local_subvols),
#			('other', other_subvols),
#			('local', toplevel_subvols)
		]:
			for v in lst:
				v['machine'] = machine
				all_subvols.append(v)

		yield from s._parent_candidates2(all_subvols, my_uuid, direction)



	def _parent_candidates2(s, all_subvols, my_uuid, direction):
		"""
		my uuid is the local_uuid of the local rw subvolume that we're trying to transfer to the remote machine.
		direction is either ('local', 'remote') or ('remote', 'local')

		"""

		all_subvols2 = {}
		for i in all_subvols:
			if i['local_uuid'] in all_subvols2:
				if i != all_subvols2[i['local_uuid']]:
					logging.warning('duplicate subvols:')
					logging.warning(json.dumps(i, indent=2, default=datetime_to_json))
					logging.warning(json.dumps(all_subvols2[i['local_uuid']], indent=2, default=datetime_to_json))
					raise 'wut'
			all_subvols2[i['local_uuid']] = i

		logging.debug('all_subvols2:')
		logging.debug(json.dumps(all_subvols2, indent=2, default=datetime_to_json))

		yield from VolWalker(all_subvols2, direction).walk(my_uuid)



	def _get_subvolumes(s, command_runner, subvolume, src):
		"""
		:param subvolume: filesystem path to a subvolume on the filesystem that we want to get a list of subvolumes of
		:return: list of records, one for each subvolume on the filesystem
		"""
		subvols = []
		logger = logging.getLogger('_get_subvolumes')

		cmd = ['btrfs', 'subvolume', 'list', '-q', '-t', '-R', '-u']
		for line in command_runner(cmd + [subvolume], logger=logbtrfs).splitlines()[2:]:
			subvol = s._make_snapshot_struct_from_sub_list_output_line(line)
			subvol['src'] = src + '_btrfs'
			logger.debug(subvol)
			subvols.append(subvol)

		ro_subvols = set()
		for line in command_runner(cmd + ['-r', subvolume], logger=logbtrfs).splitlines()[2:]:
			subvol = s._make_snapshot_struct_from_sub_list_output_line(line)
			ro_subvols.add(subvol['local_uuid'])
		# _prerr(str(ro_subvols))

		for i in subvols:
			ro = i['local_uuid'] in ro_subvols
			i['ro'] = ro
			i['host'] = s.host
			i['fs_uuid'] = s._local_fs_uuid
		# _prerr(str(i))

		subvols.sort(key=lambda sv: -sv['subvol_id'])
		logbtrfs.debug(f'_get_subvolumes: {len(subvols)=}')
		return subvols



	def _make_snapshot_struct_from_sub_list_output_line(s, line):
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
		snapshot['path'] = str(Path(s._local_fs_id5_mount_point +'/'+items[6]))
		logging.debug(snapshot)

		return snapshot



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



def main():
	fire.Fire(Bfg)


if __name__ == "__main__":
	main()  # pragma: no cover
