#!/usr/bin/env bash

# Clean up test environment - unmount filesystems and detach loop devices

echo "Cleaning up test environment..."

# Unmount any mounted test filesystems
sudo umount -f testing/mounts/btrfs1 2>/dev/null || true
sudo umount -f testing/mounts/btrfs2 2>/dev/null || true

# Detach loop devices
sudo losetup -d /dev/loop60 2>/dev/null || true
sudo losetup -d /dev/loop61 2>/dev/null || true

# Kill any hanging swipl processes from tests
pkill -f "swipl.*volwalker2.pl" 2>/dev/null || true

echo "Cleanup complete."