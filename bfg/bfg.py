#!/usr/bin/env python3


"""
The Big Friendly F.....g Gun!
"""



import logging
from pathlib import Path
from pathvalidate import sanitize_filename
import sys,os
import time
import subprocess
import fire
import shlex # python 3.8 required (at least for shlex.join)
from typing import List
import json
from .utils import *




logging.basicConfig(level=logging.DEBUG)



def prompt(*args, **kwargs):
	try:
		import PyInquirer
	except:
		print('`import PyInquirer` failed. PyInquirer is needed for interactive prompts:' + str(args) + ' ' + str(kwargs))
		sys.exit(1)
	return PyInquirer.prompt(*args, **kwargs)

class Bfg:


	def __init__(s, LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=None, sshstr='', YES=False):

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


	def _yes(s, msg):
		"""
		interactive confirmation prompt for dangerous operations
		"""
		if s._yes_was_given_on_command_line:
			return True
		return prompt(
			{
			'type': 'confirm',
			'name': 'ok',
			'message': msg + ' ( --YES=True )',
			'default': False
			}
		)['ok']



	"""

	helper functions for running subprocessess locally and over ssh

	"""


	def _remote_cmd(s, cmd, die_on_error=True):
		"""potentially remote command"""
		if not isinstance(cmd, list):
			cmd = shlex.split(cmd)
		else:
			cmd = [str(x) for x in cmd]
		if s._sshstr != '':
			ssh = shlex.split(s._sshstr)
			cmd2 = ssh + s._sudo + cmd
			_prerr(shlex.join(cmd2))
			return s._cmd(cmd2, die_on_error)
		else:
			return s._local_cmd(cmd, die_on_error)


	def _local_cmd(s, c, die_on_error=True):
		if not isinstance(c, list):
			c = shlex.split(c)
		c = s._sudo + [str(x) for x in c]
		_prerr(shlex.join(c))
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
		SUBVOLUME: your subvolume (for example /data).
		Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots.data`, if that is still the same filesystem.
		"""
		SUBVOLUME = Path(SUBVOLUME)
		parent = SUBVOLUME.parent

		_prerr(f'calculate_default_snapshot_parent_dir for {SUBVOLUME=}')

		# is parent the same filesystem as SUBVOLUME? if not, then SUBVOLUME is the top level subvolume, and we need to make the snapshot inside it, rather than outside.


		if machine == 'local':
			runner = s._local_cmd
		else:
			runner = s._remote_cmd

		if runner(['test', '-e', str(SUBVOLUME)], die_on_error=False) == -1:
			# we assume that if the target filesystem is mounted. This implies that if we're transferring the root subvol, the directory exists. This is the only case where the snapshot parent dir will be inside the subvol, rather than outside. Therefore, if the destination does not exist (as a directory or subvolume), it is safe to assume that it is not the root subvolume.
			snapshot_parent_dir = parent
		else:

			runner(['mkdir', '-p', str(SUBVOLUME)])

			# hope to come up with a unique file names:
			f1 = str(time.time())
			runner(['touch', str(SUBVOLUME/f1)])

			if runner(['cp', '--reflink', SUBVOLUME/f1, parent], die_on_error=False) != -1:
				snapshot_parent_dir = parent
			else:
				_prerr(f'cp --reflink failed, this means that {parent} is not the same filesystem, going to make snapshot inside {SUBVOLUME} instead of {parent}')
				snapshot_parent_dir = SUBVOLUME

		return Res(str(Path(str(snapshot_parent_dir) + '/.bfg_snapshots/' + Path(SUBVOLUME).parts[-1] + '_bfg_snapshots').absolute()))



	def calculate_default_snapshot_path(s, machine, SUBVOLUME, TAG, NAME_OVERRIDE=None): #, TAG2):
		"""
		calculate the filesystem path where a snapshot should go, given a subvolume and a tag
		"""
		parent = s.calculate_default_snapshot_parent_dir(machine, SUBVOLUME).val

		if NAME_OVERRIDE is not None:
			name = NAME_OVERRIDE
		else:

			tss = time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime())
			#tss = subprocess.check_output(['date', '-u', "+%Y-%m-%d_%H-%M-%S"], text=True).strip()
			ts = sanitize_filename(tss.replace(' ', '_'))

			if TAG is None:
				TAG = 'from_' + subprocess.check_output(['hostname'], text=True).strip()
			name = ts + '_' + TAG

		return Res(str(Path(str(parent) + '_' + name)))




	def get_subvol_uuid_by_path(s, runner, path):
		out = runner(f'btrfs sub show {path}')
		r = Res(out.splitlines()[2].split()[1])
        logging.debug('get_subvol_uuid_by_path', r)




	"""

	high-level, compound commands

	"""

	def commit_and_push_and_checkout(s, SUBVOLUME, REMOTE_SUBVOLUME, PARENT:str=None):
		"""
		Snapshot your data, "btrfs send"/"btrfs receive" the snapshot to the other machine, and checkout it there

		:param FS_ROOT_MOUNT_POINT: mount point of SUBVOLUME filesystem
		:param SUBVOLUME: your data
		:param REMOTE_SUBVOLUME: desired filesystem path of your data on the other machine
		:return: filesystem path of the snapshot created on the other machine
		"""
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


	def commit_and_generate_patch(s, SUBVOLUME='/', PATCH_FILE_DIR='/', PARENTS:List[str]=None):
		"""
		store a `btrfs send` stream locally

		:param SUBVOLUME:
		:param PATCH_FILE_DIR:
		:param PARENTS:
		:return:
		"""
		snapshot = s.local_commit(SUBVOLUME).val
		#print(Path(snapshot).parts[-2:])
		fn = PATCH_FILE_DIR + '/' + '__'.join(Path(snapshot).parts[-2:])
		#print(fn)
		s.local_send(snapshot, ' > ' + fn, PARENTS)
		_prerr(f'DONE, generated patch \n\tfrom {snapshot} \n\tinto {fn}\n.')
		return Res(fn)



	def commit_and_push(s, SUBVOLUME, REMOTE_SUBVOLUME, SNAPSHOT_TAG=None, SNAPSHOT_PATH=None, SNAPSHOT_NAME=None, PARENT=None, CLONESRCS:List[str]=[]):
		"""commit, and transfer the snapshot into .bfg_snapshots on the other machine"""
		snapshot = s.local_commit(SUBVOLUME, SNAPSHOT_TAG, SNAPSHOT_PATH, SNAPSHOT_NAME).val
		return Res(s.push(SUBVOLUME, snapshot, REMOTE_SUBVOLUME, PARENT, CLONESRCS).val)




	"""


	basic commands
	"""

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


	def stash_local(s, SUBVOLUME, SNAPSHOT_TAG = 'stash', SNAPSHOT_NAME=None):
		"""
		snapshot and delete your SUBVOLUME

		todo: maybe an alternative way should be to just move it?
		"""
		if s._local_cmd(['ls', SUBVOLUME], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._local_str}, {SUBVOLUME} doesn\'t exist.')
			return None
		else:
			snapshot = s._local_make_ro_snapshot(SUBVOLUME, s.calculate_default_snapshot_path('local', SUBVOLUME, SNAPSHOT_TAG, SNAPSHOT_NAME).val)

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
			snapshot = s._remote_make_ro_snapshot(SUBVOLUME, s.calculate_default_snapshot_path('remote', Path(SUBVOLUME), 'stash_before_remote_checkout').val)

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
		if TAG and SNAPSHOT:
			_prerr(f'please specify SNAPSHOT or TAG, not both')
			return -1
		if TAG and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or TAG, not both')
			return -1
		if SNAPSHOT and SNAPSHOT_NAME:
			_prerr(f'please specify SNAPSHOT_NAME or SNAPSHOT, not both')
			return -1
		SUBVOLUME = Path(SUBVOLUME).absolute()
		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path('local', SUBVOLUME, TAG, SNAPSHOT_NAME).val
		s._local_make_ro_snapshot(SUBVOLUME, SNAPSHOT)
		return Res(SNAPSHOT)



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
			SNAPSHOT = s.calculate_default_snapshot_path('remote', Path(REMOTE_SUBVOLUME), 'remote_commit', SNAPSHOT_NAME).val
		s._remote_make_ro_snapshot(REMOTE_SUBVOLUME, SNAPSHOT)
		_prerr(f'DONE {s._remote_str},\n\tsnapshotted {REMOTE_SUBVOLUME} \n\tinto {SNAPSHOT}\n.')
		return Res(SNAPSHOT)



	def push(s, SUBVOLUME, SNAPSHOT, REMOTE_SUBVOLUME, PARENT=None, CLONESRCS=[]):
		"""
		Try to figure out shared parents, if not provided, and send SNAPSHOT to the other side.

		todo: subvolume is probably not needed and fs_root_mount_point can be used?

		"""
		snapshot_parent = s.calculate_default_snapshot_parent_dir('remote', Path(REMOTE_SUBVOLUME)).val
		s._remote_cmd(['mkdir', '-p', str(snapshot_parent)])

		if PARENT is None:
			my_uuid = s.get_subvol_uuid_by_path(s._local_cmd, SUBVOLUME).val
			PARENT = s.find_common_parent(SUBVOLUME, str(snapshot_parent), my_uuid, ('local','remote')).val
			if PARENT is not None:
				PARENT = PARENT['abspath']

		s.local_send(SNAPSHOT, ' | ' + s._sshstr + ' ' + s._sudo[0] + " btrfs receive " + str(snapshot_parent), PARENT, CLONESRCS)
		_prerr(f'DONE, \n\tpushed {SNAPSHOT} \n\tinto {snapshot_parent}\n.')
		return Res(str(snapshot_parent) + '/' + Path(SNAPSHOT).parts[-1])


	def pull(s, REMOTE_SNAPSHOT, LOCAL_SUBVOLUME, PARENT=None, CLONESRCS=[]):
		local_snapshot_parent_dir = s.calculate_default_snapshot_parent_dir('local', Path(LOCAL_SUBVOLUME)).val
		s._local_cmd(['mkdir', '-p', str(local_snapshot_parent_dir)])

		if PARENT is None:
			my_uuid = s.get_subvol_uuid_by_path(s._remote_cmd, REMOTE_SNAPSHOT).val
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


	def local_send(s, SNAPSHOT, target, PARENT, CLONESRCS):
		parents_args = s._parent_args(PARENT, CLONESRCS)

		#_prerr((str(parents_args)) + ' #...')
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
		p1.stdout.close()  #  https://www.titanwolf.org/Network/q/91c3c5dd-aa49-4bf4-911d-1bfe5ac304da/y
		p2.communicate()
		if p2.returncode != 0:
			_prerr('exit code ' + str(p2.returncode))
			exit(1)



	def find_common_parent(s, subvolume, remote_subvolume, my_uuid, direction):
		candidates = s.parent_candidates(subvolume, remote_subvolume, my_uuid, direction).val
		#candidates.sort(key = lambda sv: sv['subvol_id']) # nope, subvol id is a crude approximation. What happens when you snapshot and old ro snapshot? It gets the highest id.
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
		subvol_record['abspath'] = s._local_fs_id5_mount_point + '/' + s._local_cmd(['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), s._local_fs_id5_mount_point]).strip()

	def _remote_add_abspath(s, subvol_record):
		if s._remote_fs_id5_mount_point is None:
			s._remote_fs_id5_mount_point = prompt(
			{
			'type': 'input',
			'name': 'path',
			'message': "where did you mount the top level subvolume (ID 5, not your /@ root) on the remote machine? Yes this is silly but i really need to know."
			}
		)['path']
		subvol_record['abspath'] = s._remote_fs_id5_mount_point + '/' + s._remote_cmd(['btrfs', 'ins', 'sub', str(subvol_record['subvol_id']), s._remote_fs_id5_mount_point]).strip()


	def parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction):
		candidates = []
		for c in s._parent_candidates(subvolume, remote_subvolume, my_uuid, direction):
			candidates.append(c)
			_prerr('shared parent: ' + c['local_uuid'])
		return Res(candidates)


	def _parent_candidates(s, subvolume, remote_subvolume, my_uuid, direction):

		remote_subvols = _get_subvolumes(s._remote_cmd, remote_subvolume)
		local_subvols = _get_subvolumes(s._local_cmd, subvolume)
		other_subvols = load_subvol_dumps()

		all_subvols = []
		for machine,lst in {
			'remote':remote_subvols,
			'local':local_subvols,
			'other':other_subvols
		}.items():
			for v in lst:
				v['machine'] = machine
				all_subvols.append(v)


		all_subvols2 = {}
		for i in all_subvols:
			if i['local_uuid'] in all_subvols2:
				raise 'wut'
			all_subvols2[i['local_uuid']] = i

        logging.debug('all_subvols2:')
        logging.debug(json.dumps(all_subvols2, indent=2))

		yield from VolWalker(all_subvols2, direction).walk(my_uuid)




def _get_subvolumes(command_runner, subvolume):
	"""
	:param subvolume: filesystem path to a subvolume on the filesystem that we want to get a list of subvolumes of
	:return: list of subvolume records
	"""
	subvols = []
	cmd = ['btrfs', 'subvolume', 'list', '-q', '-t', '-R', '-u']
	for line in command_runner(cmd + [subvolume]).splitlines()[2:]:
		subvol = _make_snapshot_struct_from_sub_list_output_line(line)
		subvols.append(subvol)

	ro_subvols = set()
	for line in command_runner(cmd + ['-r', subvolume]).splitlines()[2:]:
		subvol = _make_snapshot_struct_from_sub_list_output_line(line)
		ro_subvols.add(subvol['local_uuid'])
	#_prerr(str(ro_subvols))

	for i in subvols:
		ro = i['local_uuid'] in ro_subvols
		i['ro'] = ro
		#_prerr(str(i))

	subvols.sort(key = lambda sv: -sv['subvol_id'])
	return subvols



def _make_snapshot_struct_from_sub_list_output_line(line):
	logging.debug(line)
	items = line.split()
	subvol_id = items[0]
	parent_uuid = items[3]
	received_uuid = items[4]
	local_uuid = items[5]

	snapshot = {}

	if received_uuid == '-':
		received_uuid = None
	if parent_uuid == '-':
		parent_uuid = None

	snapshot['received_uuid'] = received_uuid
	snapshot['parent_uuid'] = parent_uuid
	snapshot['local_uuid'] = local_uuid
	snapshot['subvol_id'] = int(subvol_id)

	return snapshot


def try_unlink(f):
	try:
		os.unlink(f)
	except FileNotFoundError:
		pass


def _prerr(*a):
	print(*a, file = sys.stderr)
	pass


def main():
	fire.Fire(Bfg)


if __name__ == "__main__":
	main() # pragma: no cover
