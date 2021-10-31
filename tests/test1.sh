#!/usr/bin/env bash

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

./main.py  --LOCAL_FS_ROOT_MOUNT_POINT=testing/mounts/btrfs1  commit_and_push_and_checkout   --SUBVOLUME=testing/mounts/btrfs1/subvol1 --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1

sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs2/subvol1/stuff2"

./main.py  --LOCAL_FS_ROOT_MOUNT_POINT=testing/mounts/btrfs1  remote_commit_and_pull  --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1   --SUBVOLUME=testing/mounts/btrfs1/subvol1
