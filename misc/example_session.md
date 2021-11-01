```
koom@r6 ~/bfg (master)> ./main.py   --YES=true    --LOCAL_FS_ROOT_MOUNT_POINT=/nvme0n1p6_crypt_root    --sshstr='/opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20'    commit_and_push_and_checkout   --SUBVOLUME=/d  --REMOTE_SUBVOLUME=/mx500data/lean
sudo mkdir -p //.bfg_snapshots.d ...
sudo btrfs subvolume snapshot -r /d //.bfg_snapshots.d/2021-11-01_20-47-36_from_r6 ...
DONE (here), 
	snapshotted /d 
	into //.bfg_snapshots.d/2021-11-01_20-47-36_from_r6
.
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo mkdir -p /mx500data/.bfg_snapshots.lean
WARNING: ENABLED NONE CIPHER!!!
sudo btrfs sub show /d ...
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume list -q -t -R -u /mx500data/.bfg_snapshots.lean
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume list -q -t -R -u -r /mx500data/.bfg_snapshots.lean
WARNING: ENABLED NONE CIPHER!!!
sudo btrfs subvolume list -q -t -R -u /d ...
sudo btrfs subvolume list -q -t -R -u -r /d ...
DEBUG:root:walk '72c7596f-e5f6-9440-92fc-cec39d865d9b'
DEBUG:root:{'received_uuid': None, 'parent_uuid': 'addd12c8-28d3-614a-8883-64ae3f93f11a', 'local_uuid': '72c7596f-e5f6-9440-92fc-cec39d865d9b', 'subvol_id': 7326, 'ro': False, 'machine': 'local'}
DEBUG:root:parent is 'addd12c8-28d3-614a-8883-64ae3f93f11a'
DEBUG:root:walk 'addd12c8-28d3-614a-8883-64ae3f93f11a'
DEBUG:root:{'received_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'parent_uuid': '641d3f18-5cfe-9d46-8980-25482e4ee7b9', 'local_uuid': 'addd12c8-28d3-614a-8883-64ae3f93f11a', 'subvol_id': 7324, 'ro': True, 'machine': 'local'}
DEBUG:root:parent is 'cdd28b62-f967-fd40-bdcb-218a7582508a'
DEBUG:root:walk 'cdd28b62-f967-fd40-bdcb-218a7582508a'
DEBUG:root:cdd28b62-f967-fd40-bdcb-218a7582508a is on remote.
DEBUG:root:local counterpart: addd12c8-28d3-614a-8883-64ae3f93f11a.
shared parent: addd12c8-28d3-614a-8883-64ae3f93f11a
DEBUG:root:{'received_uuid': None, 'parent_uuid': '53bb3b3f-a8ea-c247-8b97-76429563111d', 'local_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'subvol_id': 1099, 'ro': True, 'machine': 'remote'}
DEBUG:root:parent is '53bb3b3f-a8ea-c247-8b97-76429563111d'
DEBUG:root:walk '53bb3b3f-a8ea-c247-8b97-76429563111d'
DEBUG:root:cdd28b62-f967-fd40-bdcb-218a7582508a is on remote.
DEBUG:root:local counterpart: addd12c8-28d3-614a-8883-64ae3f93f11a.
shared parent: addd12c8-28d3-614a-8883-64ae3f93f11a
DEBUG:root:{'received_uuid': None, 'parent_uuid': '9dd3869c-033a-e047-8ccd-b0465e7c787b', 'local_uuid': '53bb3b3f-a8ea-c247-8b97-76429563111d', 'subvol_id': 1098, 'ro': False, 'machine': 'remote'}
DEBUG:root:parent is '9dd3869c-033a-e047-8ccd-b0465e7c787b'
DEBUG:root:walk '9dd3869c-033a-e047-8ccd-b0465e7c787b'
DEBUG:root:9dd3869c-033a-e047-8ccd-b0465e7c787b is on remote.
DEBUG:root:{'received_uuid': '641d3f18-5cfe-9d46-8980-25482e4ee7b9', 'parent_uuid': '6aa9ce1b-8ba4-e14b-82c5-20bac9c1183b', 'local_uuid': '9dd3869c-033a-e047-8ccd-b0465e7c787b', 'subvol_id': 1096, 'ro': True, 'machine': 'remote'}
DEBUG:root:parent is '641d3f18-5cfe-9d46-8980-25482e4ee7b9'
DEBUG:root:walk '641d3f18-5cfe-9d46-8980-25482e4ee7b9'
DEBUG:root:9dd3869c-033a-e047-8ccd-b0465e7c787b is on remote.
DEBUG:root:local counterpart: 641d3f18-5cfe-9d46-8980-25482e4ee7b9.
shared parent: 641d3f18-5cfe-9d46-8980-25482e4ee7b9
DEBUG:root:local counterpart: addd12c8-28d3-614a-8883-64ae3f93f11a.
shared parent: addd12c8-28d3-614a-8883-64ae3f93f11a
DEBUG:root:9dd3869c-033a-e047-8ccd-b0465e7c787b is on remote.
DEBUG:root:{'received_uuid': None, 'parent_uuid': '2889e68e-6305-1748-9499-f7919a753734', 'local_uuid': '641d3f18-5cfe-9d46-8980-25482e4ee7b9', 'subvol_id': 7323, 'ro': True, 'machine': 'local'}
DEBUG:root:parent is '2889e68e-6305-1748-9499-f7919a753734'
DEBUG:root:walk '2889e68e-6305-1748-9499-f7919a753734'
sudo btrfs ins sub 7324 /nvme0n1p6_crypt_root ...
PICKED COMMON PARENT {'received_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'parent_uuid': '641d3f18-5cfe-9d46-8980-25482e4ee7b9', 'local_uuid': 'addd12c8-28d3-614a-8883-64ae3f93f11a', 'subvol_id': 7324, 'ro': True, 'machine': 'local', 'abspath': '/nvme0n1p6_crypt_root/@/.bfg_snapshots.d/2021-11-01_20-46-18_remote_commit'}.
sudo btrfs send -p /nvme0n1p6_crypt_root/@/.bfg_snapshots.d/2021-11-01_20-46-18_remote_commit //.bfg_snapshots.d/2021-11-01_20-47-36_from_r6 | /opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20 sudo btrfs receive /mx500data/.bfg_snapshots.lean #...
At subvol //.bfg_snapshots.d/2021-11-01_20-47-36_from_r6
WARNING: ENABLED NONE CIPHER!!!
At snapshot 2021-11-01_20-47-36_from_r6
DONE, 
	pushed //.bfg_snapshots.d/2021-11-01_20-47-36_from_r6 
	into /mx500data/.bfg_snapshots.lean
.
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo ls /mx500data/lean
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo mkdir -p /mx500data/.bfg_snapshots.lean
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume snapshot -r /mx500data/lean /mx500data/.bfg_snapshots.lean/2021-11-01_20-47-38_stash_before_remote_checkout
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume delete /mx500data/lean
WARNING: ENABLED NONE CIPHER!!!
DONE (on the other machine), 
	snapshotted /mx500data/lean 
	into /mx500data/.bfg_snapshots.lean/2021-11-01_20-47-38_stash_before_remote_checkout
, and deleted it.
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume snapshot /mx500data/.bfg_snapshots.lean/2021-11-01_20-47-36_from_r6 /mx500data/lean
WARNING: ENABLED NONE CIPHER!!!
DONE (on the other machine), 
	checked out /mx500data/.bfg_snapshots.lean/2021-11-01_20-47-36_from_r6 
	into /mx500data/lean
.
{"result": "/mx500data/lean"}
```

and back:
```
koom@r6 ~/bfg (master)> ./main.py   --YES=true    --REMOTE_FS_ROOT_MOUNT_POINT=/mx500data    --sshstr='/opt/hpnssh/usr/bin/ssh   -p 2222   -o TCPRcvBufPoll=yes -o NoneSwitch=yes  -o NoneEnabled=yes     koom@10.0.0.20'   remote_commit_and_pull   --SUBVOLUME=/d  --REMOTE_SUBVOLUME=/mx500data/lean
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo mkdir -p /mx500data/.bfg_snapshots.lean
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume snapshot -r /mx500data/lean /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
WARNING: ENABLED NONE CIPHER!!!
DONE (on the other machine),
	snapshotted /mx500data/lean 
	into /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
.
sudo mkdir -p //.bfg_snapshots.d ...
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs sub show /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume list -q -t -R -u /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
WARNING: ENABLED NONE CIPHER!!!
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs subvolume list -q -t -R -u -r /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
WARNING: ENABLED NONE CIPHER!!!
sudo btrfs subvolume list -q -t -R -u //.bfg_snapshots.d ...
sudo btrfs subvolume list -q -t -R -u -r //.bfg_snapshots.d ...
DEBUG:root:walk 'e6a172c6-3eec-9e43-966f-2148b9d1d0c7'
DEBUG:root:{'received_uuid': None, 'parent_uuid': 'ee8f3f00-1786-e246-9090-0e8634e5b536', 'local_uuid': 'e6a172c6-3eec-9e43-966f-2148b9d1d0c7', 'subvol_id': 1103, 'ro': True, 'machine': 'remote'}
DEBUG:root:parent is 'ee8f3f00-1786-e246-9090-0e8634e5b536'
DEBUG:root:walk 'ee8f3f00-1786-e246-9090-0e8634e5b536'
DEBUG:root:{'received_uuid': None, 'parent_uuid': '743772b5-01fd-fa49-a4bb-0e33eeeb995c', 'local_uuid': 'ee8f3f00-1786-e246-9090-0e8634e5b536', 'subvol_id': 1102, 'ro': False, 'machine': 'remote'}
DEBUG:root:parent is '743772b5-01fd-fa49-a4bb-0e33eeeb995c'
DEBUG:root:walk '743772b5-01fd-fa49-a4bb-0e33eeeb995c'
DEBUG:root:{'received_uuid': '9e0b1570-3f62-964a-846f-9d1fa1ea9a3a', 'parent_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'local_uuid': '743772b5-01fd-fa49-a4bb-0e33eeeb995c', 'subvol_id': 1100, 'ro': True, 'machine': 'remote'}
DEBUG:root:parent is '9e0b1570-3f62-964a-846f-9d1fa1ea9a3a'
DEBUG:root:walk '9e0b1570-3f62-964a-846f-9d1fa1ea9a3a'
DEBUG:root:9e0b1570-3f62-964a-846f-9d1fa1ea9a3a is on remote.
DEBUG:root:local counterpart: 743772b5-01fd-fa49-a4bb-0e33eeeb995c.
shared parent: 743772b5-01fd-fa49-a4bb-0e33eeeb995c
DEBUG:root:{'received_uuid': None, 'parent_uuid': '72c7596f-e5f6-9440-92fc-cec39d865d9b', 'local_uuid': '9e0b1570-3f62-964a-846f-9d1fa1ea9a3a', 'subvol_id': 7327, 'ro': True, 'machine': 'local'}
DEBUG:root:parent is '72c7596f-e5f6-9440-92fc-cec39d865d9b'
DEBUG:root:walk '72c7596f-e5f6-9440-92fc-cec39d865d9b'
DEBUG:root:9e0b1570-3f62-964a-846f-9d1fa1ea9a3a is on remote.
DEBUG:root:local counterpart: 743772b5-01fd-fa49-a4bb-0e33eeeb995c.
shared parent: 743772b5-01fd-fa49-a4bb-0e33eeeb995c
DEBUG:root:{'received_uuid': None, 'parent_uuid': 'addd12c8-28d3-614a-8883-64ae3f93f11a', 'local_uuid': '72c7596f-e5f6-9440-92fc-cec39d865d9b', 'subvol_id': 7326, 'ro': False, 'machine': 'local'}
DEBUG:root:parent is 'addd12c8-28d3-614a-8883-64ae3f93f11a'
DEBUG:root:walk 'addd12c8-28d3-614a-8883-64ae3f93f11a'
DEBUG:root:addd12c8-28d3-614a-8883-64ae3f93f11a is on remote.
DEBUG:root:{'received_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'parent_uuid': '641d3f18-5cfe-9d46-8980-25482e4ee7b9', 'local_uuid': 'addd12c8-28d3-614a-8883-64ae3f93f11a', 'subvol_id': 7324, 'ro': True, 'machine': 'local'}
DEBUG:root:parent is 'cdd28b62-f967-fd40-bdcb-218a7582508a'
DEBUG:root:walk 'cdd28b62-f967-fd40-bdcb-218a7582508a'
DEBUG:root:addd12c8-28d3-614a-8883-64ae3f93f11a is on remote.
DEBUG:root:local counterpart: cdd28b62-f967-fd40-bdcb-218a7582508a.
shared parent: cdd28b62-f967-fd40-bdcb-218a7582508a
DEBUG:root:local counterpart: 743772b5-01fd-fa49-a4bb-0e33eeeb995c.
shared parent: 743772b5-01fd-fa49-a4bb-0e33eeeb995c
DEBUG:root:addd12c8-28d3-614a-8883-64ae3f93f11a is on remote.
DEBUG:root:{'received_uuid': None, 'parent_uuid': '53bb3b3f-a8ea-c247-8b97-76429563111d', 'local_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'subvol_id': 1099, 'ro': True, 'machine': 'remote'}
DEBUG:root:parent is '53bb3b3f-a8ea-c247-8b97-76429563111d'
DEBUG:root:walk '53bb3b3f-a8ea-c247-8b97-76429563111d'
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs ins sub 1100 /mx500data
WARNING: ENABLED NONE CIPHER!!!
PICKED COMMON PARENT {'received_uuid': '9e0b1570-3f62-964a-846f-9d1fa1ea9a3a', 'parent_uuid': 'cdd28b62-f967-fd40-bdcb-218a7582508a', 'local_uuid': '743772b5-01fd-fa49-a4bb-0e33eeeb995c', 'subvol_id': 1100, 'ro': True, 'machine': 'remote', 'abspath': '/mx500data/.bfg_snapshots.lean/2021-11-01_20-47-36_from_r6'}.
/opt/hpnssh/usr/bin/ssh -p 2222 -o TCPRcvBufPoll=yes -o NoneSwitch=yes -o NoneEnabled=yes koom@10.0.0.20 sudo btrfs send -p /mx500data/.bfg_snapshots.lean/2021-11-01_20-47-36_from_r6 /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit >>|>> sudo btrfs receive //.bfg_snapshots.d
WARNING: ENABLED NONE CIPHER!!!
At subvol /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit
At snapshot 2021-11-01_20-49-53_remote_commit
DONE, 
	pulled /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit 
	into //.bfg_snapshots.d/2021-11-01_20-49-53_remote_commit
.
sudo ls /d ...
sudo mkdir -p //.bfg_snapshots.d ...
sudo btrfs subvolume snapshot -r /d //.bfg_snapshots.d/2021-11-01_20-49-55_stash_before_local_checkout ...
DONE (here), 
	snapshotted /d 
	into //.bfg_snapshots.d/2021-11-01_20-49-55_stash_before_local_checkout
.
sudo btrfs subvolume delete /d ...
DONE (here), 
	snapshotted /d into 
	//.bfg_snapshots.d/2021-11-01_20-49-55_stash_before_local_checkout
, and deleted it.
sudo btrfs subvolume snapshot //.bfg_snapshots.d/2021-11-01_20-49-53_remote_commit /d ...
DONE (here), 
	checked out //.bfg_snapshots.d/2021-11-01_20-49-53_remote_commit 
	into /d
.
DONE, 
	pulled /mx500data/.bfg_snapshots.lean/2021-11-01_20-49-53_remote_commit 
	into /d
.
{"result": "/d"}
```
