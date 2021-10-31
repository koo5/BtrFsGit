#!/usr/bin/env python3.8



import logging
from pathlib import Path
from pathvalidate import sanitize_filename
import sys,os
import time
import subprocess
import fire
import shlex
from typing import List
from PyInquirer import prompt
import json



#logging.basicConfig(level=logging.DEBUG)



class Bfg:


	def __init__(s, LOCAL_FS_ROOT_MOUNT_POINT, sshstr='', YES=False):

		s._local_fs_root_mount_point = LOCAL_FS_ROOT_MOUNT_POINT
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
			'message': msg,
			'default': False
			}
		)['ok']



	"""

	helper functions for running subprocessess locally and over ssh

	"""


	def _remote_cmd(s, cmd, die_on_error=True):
		"""potentionally remote command"""
		if not isinstance(cmd, list):
			cmd = shlex.split(cmd)
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
		c = s._sudo + c
		_prerr(shlex.join(c) + ' ...')
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


	def calculate_default_snapshot_parent_dir(s, SUBVOLUME):
		"""
		SUBVOLUME: your subvolume (for example /data).
		Calculate the default snapshot parent dir. In the filesystem tree, it is on the same level as your subvolume, for example `/.bfg_snapshots.data`
		"""
		return Res(str(Path(str(SUBVOLUME.parent) + '/.bfg_snapshots.' + SUBVOLUME.parts[-1]).absolute()))


	def calculate_default_snapshot_path(s, SUBVOLUME, TAG):
		"""
		calculate the filesystem path where a snapshot should go, given a subvolume and a tag
		"""
		parent = s.calculate_default_snapshot_parent_dir(SUBVOLUME).val

		tss = time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime())
		#tss = subprocess.check_output(['date', '-u', "+%Y-%m-%d_%H-%M-%S"], text=True).strip()
		ts = sanitize_filename(tss.replace(' ', '_'))

		if TAG is None:
			TAG = 'from_' + subprocess.check_output(['hostname'], text=True).strip()

		return Res(str(Path(str(parent) + '/' + ts + '_' + TAG)))


	def get_subvol_uuid_by_path(s, runner, path):
		out = runner(f'btrfs sub show {path}')
		return Res(out.splitlines()[2].split()[1])




	"""
		
	high-level, compound commands
	
	"""

	def commit_and_push_and_checkout(s, SUBVOLUME, REMOTE_SUBVOLUME, PARENTS:List[str]=None):
		"""
		Snapshot your data, "btrfs send"/"btrfs receive" the snapshot to the other machine, and checkout it there

		:param FS_ROOT_MOUNT_POINT: mount point of SUBVOLUME filesystem
		:param SUBVOLUME: your data
		:param REMOTE_SUBVOLUME: desired filesystem path of your data on the other machine
		:return: filesystem path of the snapshot created on the other machine
		"""
		remote_snapshot_path = s.commit_and_push(SUBVOLUME, REMOTE_SUBVOLUME, PARENTS).val
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
		local_snapshot_path = s.pull(remote_snapshot_path).val
		s.checkout_local(local_snapshot_path, SUBVOLUME)
		_prerr(f'DONE, pulled {remote_snapshot_path} into {SUBVOLUME}')
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
		s._send(snapshot, ' > ' + fn, PARENTS)
		_prerr(f'DONE, generated patch from {snapshot} into {fn}')
		return Res(fn)

		

	def commit_and_push(s, SUBVOLUME='/', REMOTE_SUBVOLUME='/bfg', PARENTS:List[str]=None):
		"""commit, and transfer the snapshot into .bfg_snapshots on the other machine"""
		snapshot = s.local_commit(SUBVOLUME).val
		return Res(s.push(SUBVOLUME, snapshot, REMOTE_SUBVOLUME, PARENTS).val)




	"""
	
	
	basic commands
	"""

	def checkout_local(s, SNAPSHOT, SUBVOLUME):
		"""stash your SUBVOLUME, and replace it with SNAPSHOT"""
		stash_local(SUBVOLUME)
		s._local_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOLUME}')
		_prerr(f'DONE {s._local_str}, checked out {SNAPSHOT} into {SUBVOLUME}')
		return Res(SUBVOLUME)


	def checkout_remote(s, SNAPSHOT, SUBVOLUME):
		"""ssh into the other machine,
		stash your SUBVOLUME, and replace it with SNAPSHOT"""
		s.stash_remote(SUBVOLUME)
		s._remote_cmd(f'btrfs subvolume snapshot {SNAPSHOT} {SUBVOLUME}')
		_prerr(f'DONE {s._remote_str}, checked out {SNAPSHOT} into {SUBVOLUME}')
		return Res(SUBVOLUME)
		

	def stash_local(s, SUBVOLUME):
		"""snapshot and delete your SUBVOLUME"""
		snapshot = s._local_make_ro_snapshot(SUBVOLUME, s.calculate_default_snapshot_path(SUBVOLUME, 'stash_before_local_checkout').val)
		s._local_cmd(f'btrfs subvolume delete {SUBVOLUME}')
		_prerr(f'DONE {s._local_str}, snapshotted {SUBVOLUME} into {snapshot}, and deleted it.')
		return Res(snapshot)

	def stash_remote(s, SUBVOLUME):
		"""snapshot and delete your SUBVOLUME"""
		if s._remote_cmd(['ls', SUBVOLUME], die_on_error=False) == -1:
			_prerr(f'nothing to stash {s._remote_str}, {SUBVOLUME} doesn\'t exist.')
			return None
		else:
			snapshot = s._remote_make_ro_snapshot(SUBVOLUME, s.calculate_default_snapshot_path(Path(SUBVOLUME), 'stash_before_remote_checkout'))
			s._remote_cmd(f'btrfs subvolume delete {SUBVOLUME}')
			_prerr(f'DONE {s._remote_str}, snapshotted {SUBVOLUME} into {snapshot}, and deleted it.')
			return Res(snapshot)
		


	def local_commit(s, SUBVOLUME='/', TAG=None, SNAPSHOT=None):
		"""
		come up with a filesystem path for a snapshot, and snapshot SUBVOLUME.
		:param SNAPSHOT: override default filesystem path where snapshot will be created
		:param TAG: override the tag for the default SNAPSHOT (hostname by default)
		"""
		if TAG and SNAPSHOT:
			_prerr(f'please specify SNAPSHOT or TAG, not both')
			return -1
		SUBVOLUME = Path(SUBVOLUME).absolute()
		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = s.calculate_default_snapshot_path(SUBVOLUME, TAG).val
		s._local_make_ro_snapshot(SUBVOLUME, SNAPSHOT)
		return Res(SNAPSHOT)



	def remote_commit(s, REMOTE_SUBVOLUME):
		snapshot = s._remote_make_ro_snapshot(REMOTE_SUBVOLUME, s.calculate_default_snapshot_path(Path(REMOTE_SUBVOLUME), 'remote_commit'))
		_prerr(f'DONE {s._remote_str}, snapshotted {REMOTE_SUBVOLUME} into {snapshot}.')
		return Res(snapshot)


				
	def push(s, SUBVOLUME, SNAPSHOT, REMOTE_SUBVOLUME, PARENT=None, CLONESRCS=[]):
		"""
		Try to figure out shared parents, if not provided.

		todo: subvolume is probably not needed and fs_root_mount_point can be used?
		
		"""
		snapshot_parent = s.calculate_default_snapshot_parent_dir(Path(REMOTE_SUBVOLUME)).val
		s._remote_cmd(['mkdir', '-p', str(snapshot_parent)])

		if PARENT is None:
			# there will be zero or one parent
			PARENT = s.find_common_parent(SUBVOLUME, str(snapshot_parent)).val

		s._send(SNAPSHOT, ' | ' + s._sshstr + ' ' + s._sudo[0] + " btrfs receive " + str(snapshot_parent), PARENT, CLONESRCS)
		_prerr(f'DONE, pushed {SNAPSHOT} into {snapshot_parent}')
		return Res(str(snapshot_parent) + '/' + Path(SNAPSHOT).parts[-1])




	"""
	
	low-level operations
	"""

	def _local_make_ro_snapshot(s, SUBVOLUME, SNAPSHOT):
		"""make a read-only snapshot of SUBVOLUME into SNAPSHOT, locally"""
		SNAPSHOT_PARENT = os.path.split((SNAPSHOT))[0]
		s._local_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._local_cmd(f'btrfs subvolume snapshot -r {SUBVOLUME} {SNAPSHOT}')
		_prerr(f'DONE {s._local_str}, snapshotted {SUBVOLUME} into {SNAPSHOT}')
		return SNAPSHOT


	def _remote_make_ro_snapshot(s, SUBVOLUME, SNAPSHOT):
		"""make a read-only snapshot of SUBVOLUME into SNAPSHOT, remotely"""
		SNAPSHOT_PARENT = os.path.split((SNAPSHOT))[0]
		s._remote_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		s._remote_cmd(f'btrfs subvolume snapshot -r {SUBVOLUME} {SNAPSHOT}')
		return SNAPSHOT


	def _send(s, SNAPSHOT, target, PARENT, CLONESRCS):

		parents_args = []

		if PARENT:
			parents_args.append('-p')
			parents_args.append(PARENT)

		for c in CLONESRCS:
			parents_args.append('-c')
			parents_args.append(c)

		cmd = shlex.join(s._sudo + ['btrfs', 'send'] + parents_args + [SNAPSHOT]) + target
		_prerr((cmd) + ' #...')
		subprocess.check_call(cmd, shell=True)



	def find_common_parent(s, subvolume, remote_subvolume):
		candidates = s.parent_candidates(subvolume, remote_subvolume).val
		candidates.sort(key = lambda sv: -sv['subvol_id'])
		if len(candidates) != 0:
			winner = candidates[-1]
			s._add_abspath(winner)
			_prerr(f'found COMMON PARENT {winner}.')
			return Res(winner)
		else:
			return Res(None)


	def _add_abspath(s, subvol_record):
		if s._local_fs_root_mount_point is None:
			s._local_fs_root_mount_point = prompt(
			{
			'type': 'input',
			'name': 'path',
			'message': "where did you mount the top level subvolume (ID 5, not your /@ root)? Yes this is silly but i really need it right now."
			}
		)['path']
		subvol_record['abspath'] = s._local_fs_root_mount_point + '/' + s._local_cmd(['btrfs', 'ins', 'sub', v, subvolume]).strip()


	def parent_candidates(s, subvolume, remote_subvolume):
		return Res(list(s._parent_candidates(subvolume, remote_subvolume)))

	def _parent_candidates(s, subvolume, remote_subvolume):
		my_uuid = s.get_subvol_uuid_by_path(s._local_cmd, subvolume).val

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


		yield from VolWalker(all_subvols2).walk(my_uuid)



class VolWalker:

	def __init__(s, subvols_by_local_uuid):


		logging.debug('subvols_by_local_uuid:')
		for k,v in subvols_by_local_uuid.items():
			logging.debug((k,v))
		logging.debug('/subvols_by_local_uuid')


		s.by_uuid = subvols_by_local_uuid

	def parent(s, uuid):
		v = s.by_uuid[uuid]
		logging.debug(v)
		if v['received_uuid']:
			return v['received_uuid']
		if v['parent_uuid']:
			return v['parent_uuid']


	def walk(s, my_uuid):
		logging.debug('walk ' + repr(my_uuid))
		yield from s.ro_descendants_chain(my_uuid)
		p = s.parent(my_uuid)
		logging.debug('parent is ' + repr(p))
		if p:
			yield from s.walk(p)


	def ro_descendants_chain(s, my_uuid):
		v = s.by_uuid.get(my_uuid)
		if not v:
			return

		# at any case, if the read-only-ness chain is broken,
		# the subvol or its descendants are of no use
		if v['ro'] == False:
			return	

		# if this item of the chain happens to be on the remote machine,
		# it's a good candidate for -p
		if v['machine'] == 'remote':
			yield v

		# find all descendants created through send/receive or snapshotting
		for k,v in s.by_uuid.items():
			if v['received_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'])
			if v['parent_uuid'] == my_uuid:
				yield from s.ro_descendants_chain(v['local_uuid'])



def create_subvol_dump():
	"""
	I feel that it makes more sense to store the dumps in their raw form, that is, the outputs of `btrfs sub list`.
	This way, we can diff, review, delete lines etc easily.
	This is gonna be a bit silly in that we need to store the outputs of `sub list` and `sub list -r` separately, because `sub list` has no option to just print the read-only status in a column or something.
	dumps should probably be organized first by fs uuid and second by timestamp

	:return: subvol dump path
	"""
	pass

def load_subvol_dumps():
	"""
	todo.
	only the most recent dump for each fs will be used.

	"""
	return []



def _get_subvolumes(command_runner, subvolume):
	"""
	:param subvolume: filesystem path to a subvolume on the filesystem that we want to get a list of subvolumes of
	:return: list of subvolume records
	"""
	subvols = []
	cmd = ['btrfs', 'subvolume', 'list', '-q', '-t', '-u', '-R', '-u']
	for line in command_runner(cmd + [subvolume]).splitlines()[2:]:
		subvol = _make_snapshot_struct_from_sub_list_output_line(line)
		subvols.append(subvol)

	ro_subvols = set()
	for line in command_runner(cmd + ['-r', subvolume]).splitlines()[2:]:
		subvol = _make_snapshot_struct_from_sub_list_output_line(line)
		ro_subvols.add(subvol['local_uuid'])

	for i in subvols:
		i['ro'] = (subvol['local_uuid'] in ro_subvols)

	return subvols



def _make_snapshot_struct_from_sub_list_output_line(line):
	logging.debug(line)
	items = line.split()
	subvol_id = items[0]
	parent_uuid = items[3]
	received_uuid = items[4]
	local_uuid = items[5]
	path = items[6]

	snapshot = {}

	if received_uuid == '-':
		received_uuid = None
	if parent_uuid == '-':
		parent_uuid = None

	snapshot['received_uuid'] = received_uuid
	snapshot['parent_uuid'] = parent_uuid
	snapshot['local_uuid'] = local_uuid
	snapshot['subvol_id'] = subvol_id

	return snapshot




class Res:
	"""helper class for passing results of Fire-invoked functions around and make sure they're printed understandably and machine-readably"""
	def __init__(s, value):
		s.val = value
	def __repr__(s):
		return json.dumps({'result':s.val})
	def __str__(s):
		return json.dumps({'result':s.val})


def _prerr(*a):
	print(*a, file = sys.stderr)


if __name__ == '__main__':
	fire.Fire(Bfg)

