#!/usr/bin/env bash


sudo umount testing/mounts/btrfs
sudo umount testing/mounts/btrfs2


# die on error
set -e

mkdir -p testing/images
mkdir -p testing/images_empty
mkdir -p testing/mounts

dd  count=200 bs=1M  if=/dev/zero of="testing/images_empty/btrfs.raw"
dd  count=200 bs=1M  if=/dev/zero of="testing/images_empty/btrfs2.raw"

set +e

sudo losetup -D /dev/loop60
sudo losetup -D /dev/loop61

sudo losetup -P  /dev/loop60  testing/images_empty/btrfs.raw
sudo losetup -P  /dev/loop61  testing/images_empty/btrfs2.raw

set -e

sudo mkfs -t btrfs /dev/loop60  
sudo mkfs -t btrfs /dev/loop61

sudo mkdir -p testing/mounts/btrfs
sudo mkdir -p testing/mounts/btrfs2

