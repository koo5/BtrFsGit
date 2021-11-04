#!/usr/bin/env bash

# die on error
set -e
# print commands
set -x


./misc/testing_fss_mount_empty.sh
sudo btrfs subvolume create testing/mounts/btrfs1/subvol1
sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs1/subvol1/stuff1"


bfg  --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1  commit_and_push_and_checkout   --SUBVOLUME=testing/mounts/btrfs1/subvol1 --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1

sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs2/subvol1/stuff2"

bfg  --YES=True --REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs2  remote_commit_and_pull  --REMOTE_SUBVOLUME=testing/mounts/btrfs2/subvol1   --SUBVOLUME=testing/mounts/btrfs1/subvol1
