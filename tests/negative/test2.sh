#!/usr/bin/env bash

#
# this is a cornercase experiment, not necessarily meant to succeed. Indeed BTRFS refuses the snapshot as a parent, because there is an rw snapshot in the chain, even if no write was done to it.
#


# die on error
set -e
# print commands
set -x


./testing_fss_mount_empty.sh
sudo btrfs subvolume create testing/mounts/btrfs1/subvol1
sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs1/subvol1/stuff1"


set +x
source venv/bin/activate
set -x


# create ro "s1" from "subvol1" and send it to the other side
./main.py  --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1  commit_and_push   --SUBVOLUME=testing/mounts/btrfs1/subvol1 --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1  --SNAPSHOT_NAME=s1

# create rw "s1_rw_snap" from "s1"
sudo btrfs subvolume snapshot testing/mounts/btrfs1/.bfg_snapshots.subvol1/s1 testing/mounts/btrfs1/s1_rw_snap

# delete "s1"
sudo btrfs subvolume delete testing/mounts/btrfs1/.bfg_snapshots.subvol1/*

# create ro "s1_ro_snap" from "s1_rw_snap"
sudo btrfs subvolume snapshot -r testing/mounts/btrfs1/s1_rw_snap testing/mounts/btrfs1/s1_ro_snap

# is "s1_ro_snap" a good parent for sending a snapshot of "subvol1" again?
# BFG wouldn't try this
./main.py  --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1  commit_and_push   --SUBVOLUME=testing/mounts/btrfs1/subvol1 --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1  --PARENT='testing/mounts/btrfs1/s1_ro_snap'

