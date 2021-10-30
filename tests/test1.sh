#!/usr/bin/env bash

# die on error
set -e
# print commands
set -x

./testing_fss_mount_empty.sh

sudo btrfs subvolume create testing/mounts/btrfs/subvol1
sudo touch testing/mounts/btrfs/subvol1/stuff1

./main.py  commit_and_push_and_checkout   --FS_ROOT_MOUNT_POINT=testing/mounts/btrfs  --SUBVOLUME=testing/mounts/btrfs/subvol1 --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1


