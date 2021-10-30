#!/usr/bin/env bash


sudo umount testing/mounts/btrfs
sudo umount testing/mounts/btrfs2


# die on error
set -e

sudo cp --reflink=auto testing/images_empty/btrfs.raw testing/images/btrfs.raw
sudo cp --reflink=auto testing/images_empty/btrfs2.raw testing/images/btrfs2.raw

set +e

sudo losetup -D /dev/loop60
sudo losetup -D /dev/loop61

sudo losetup -P  /dev/loop60  testing/images/btrfs.raw
sudo losetup -P  /dev/loop61  testing/images/btrfs2.raw

set -e

sudo mount /dev/loop60 testing/mounts/btrfs
sudo mount /dev/loop61 testing/mounts/btrfs2
