#!/usr/bin/env python3



from pathlib import Path
from pathvalidate import sanitize_filename
import sys,os
import time
import subprocess
import fire
import shlex
from typing import List



class Bfg:


	#sshstr = '/opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20'
	
	def __init__(s, sshstr=''):
		s.sshstr = sshstr


	def commit_and_push(s, fs_root_mount_point=None, subvolume='/', remote_subvolume='/bfg', parents:List[str]=None):
		if fs_root_mount_point is None:
			fs_root_mount_point = subvolume
		snapshot = s.commit(subvolume)
		s.push(fs_root_mount_point, subvolume, snapshot, remote_subvolume, parents)
		
	def checkout(s, what, where):
		local_stash(where)
		local_cmd(f'btrfs subvolume snapshot {what} {where}')
		return where

	def stash(s, what):
		return s.local_make_ro_snapshot(what, snapshot_path(what, 'stash'))
		local_cmd(f'btrfs subvolume delete {what}')
	
	

	def local_make_ro_snapshot(s, VOL, SNAPSHOT):
		SNAPSHOT_PARENT = os.path.split((SNAPSHOT))[0]
		local_cmd(f'mkdir -p {SNAPSHOT_PARENT}')
		local_cmd(f'btrfs subvolume snapshot -r {VOL} {SNAPSHOT}')
		prerr(f'done, snapshotted {VOL} into {SNAPSHOT}')
		return SNAPSHOT



	def commit(s, VOL='/', SNAPSHOTS_CONTAINER=None, TAG=None, SNAPSHOT=None):

		VOL = Path(VOL).absolute()

		if SNAPSHOT is not None:
			SNAPSHOT = Path(SNAPSHOT).absolute()
		else:
			SNAPSHOT = snapshot_path(VOL, TAG)

		s.local_make_ro_snapshot(VOL, SNAPSHOT)

		return (SNAPSHOT)

		
		
	def push(s, fs_root_mount_point, subvolume, snapshot, remote_subvolume, parents=None):
		SNAPSHOT_PARENT = snapshot_parent_dir(Path(remote_subvolume))
		s.remote_cmd_runner(['sudo', 'mkdir', '-p', str(SNAPSHOT_PARENT)])

		if parents is None:
			parents = []
			for p in s.find_common_parents(fs_root_mount_point, subvolume, str(SNAPSHOT_PARENT)):
				parents.append(p)

		parents_args = []
		for p in parents:
			parents_args.append('-c')
			parents_args.append(p)

		cmd = shlex.join(['sudo', 'btrfs', 'send'] + parents_args + [snapshot]) + ' | ' + s.sshstr + " sudo btrfs receive " + str(SNAPSHOT_PARENT)
		prerr((cmd) + ' ...')
		subprocess.check_call(cmd, shell=True)
		prerr(f'done, pushed {snapshot} into {SNAPSHOT_PARENT}')
		return remote_subvolume
		

	def find_common_parents(s, fs_root_mount_point='/', subvolume='/', remote_subvolume='/'):
		
		remote_subvols = get_ro_subvolumes(s.remote_cmd_runner, remote_subvolume)
		local_subvols = get_ro_subvolumes(local_cmd, subvolume)
		
		#print('remote_subvols:')
		#print(remote_subvols)
		#print('local_subvols:')
		#print(local_subvols)
		#print("common_parents:")
		
		common_parents = []
		for k,v in local_subvols.items():
			if k in remote_subvols:
				abspath = fs_root_mount_point + '/' + local_cmd(['btrfs', 'ins', 'sub', v, subvolume]).strip()
				common_parents.append(abspath)
		
		#print(common_parents)
		return common_parents
		
		

	def commit_and_push_and_checkout(s, subvolume='/', remote_subvolume='/'):
		pass

	
	def commit_and_generate_patch(s):
		pass
		
			

	def remote_cmd_runner(s, cmd):
		if s.sshstr != '':
			ssh = shlex.split(s.sshstr)
			cmd2 = ssh + cmd
			prerr(cmd2)
			return subprocess.check_output(cmd2, text=True)
		else: 
			return local_cmd(cmd)


def local_cmd(c):
	if not isinstance(c, list):
		c = shlex.split(c)
	prerr(shlex.join(c))
	try:
		return subprocess.check_output(c, text=True)
	except Exception as e:
		prerr(e)
		exit(1)


def snapshot_parent_dir(VOL):
		return Path(str(VOL.parent) + '/.bfg_snapshots.' + VOL.parts[-1]).absolute()


def snapshot_path(VOL, TAG):
	parent = snapshot_parent_dir(VOL)
	
	tss = time.strftime("%Y-%m-%d_%H-%M-%S", time.localtime())
	#tss = subprocess.check_output(['date', '-u', "+%Y-%m-%d_%H-%M-%S"], text=True).strip()
	ts = sanitize_filename(tss.replace(' ', '_'))

	if TAG is None:
		TAG = 'from_' + subprocess.check_output(['hostname'], text=True).strip()

	return str(Path(str(parent) + '/' + ts + '_' + TAG))

 
def prerr(*a):
	print(*a, file = sys.stderr)
 


def get_ro_subvolumes(command_runner, subvolume):
	snapshots = {}
	
	# todo: limit this to snapshots of the subvolume we want to send, at least on the sending side, otherwise, we are giving 'btrfs send' -c's that are unrelated to the given subvolume
	
	# also, what if a snapshot is snapshotted again (two hops), does it retain received-uuid?	
	
	
	for line in command_runner(['sudo', 'btrfs', 'subvolume', 'list', '-t', '-r', '-R', '-u', subvolume]).splitlines()[2:]:
		#prerr(line)
		items = line.split()
		received_uuid = items[3]
		local_uuid = items[4]
		subvol_id = items[0]
		if received_uuid != '-':
			snapshots[received_uuid] = subvol_id
		if local_uuid != '-':
			snapshots[local_uuid] = subvol_id
	return snapshots




if __name__ == '__main__':
        fire.Fire(Bfg)

