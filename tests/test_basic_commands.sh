#!/usr/bin/env bash

# Test script for BtrfsGit basic commands
# This script tests the core functionality of BtrfsGit

# die on error
set -e
# print commands
set -x

# Setup test environment
./tests/testing_fss_init.sh
./tests/testing_fss_mount_empty.sh

# Create test subvolumes and files
echo "Creating test data..."
sudo mkdir -p testing/mounts/btrfs1/.bfg
echo "testing/mounts/btrfs1" | sudo tee testing/mounts/btrfs1/.bfg/id5
sudo mkdir -p testing/mounts/btrfs2/.bfg
echo "testing/mounts/btrfs2" | sudo tee testing/mounts/btrfs2/.bfg/id5

# Setup source subvolume
sudo btrfs subvolume create testing/mounts/btrfs1/source_subvol
sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs1/source_subvol/initial_file" status=none

# Test 1: Local commit
echo "Test 1: Testing local commit..."
bfg --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1 local_commit --SUBVOL=testing/mounts/btrfs1/source_subvol --TAG=test_snapshot
# Verify snapshot exists
sudo ls -la testing/mounts/btrfs1/.bfg_snapshots/

# Test 2: Commit and push
echo "Test 2: Testing commit and push..."
bfg --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1 commit_and_push --SUBVOL=testing/mounts/btrfs1/source_subvol --REMOTE_SUBVOL=testing/mounts/btrfs2/target_subvol
# Verify snapshot exists on remote
sudo ls -la testing/mounts/btrfs2/.bfg_snapshots/

# Test 3: Checkout remote
echo "Test 3: Testing checkout remote..."
# Get the most recent remote snapshot
REMOTE_SNAPSHOT=$(sudo find testing/mounts/btrfs2/.bfg_snapshots -type d -name "source_subvol_*" | sort | tail -1)
bfg checkout_remote --SNAPSHOT=$REMOTE_SNAPSHOT --SUBVOL=testing/mounts/btrfs2/target_subvol
# Verify checkout worked
sudo ls -la testing/mounts/btrfs2/target_subvol/

# Test 4: Create some changes on remote
echo "Test 4: Creating changes on remote..."
sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs2/target_subvol/remote_file" status=none

# Test 5: Remote commit
echo "Test 5: Testing remote commit..."
bfg --REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs2 remote_commit --REMOTE_SUBVOL=testing/mounts/btrfs2/target_subvol
# Verify snapshot exists
sudo ls -la testing/mounts/btrfs2/.bfg_snapshots/

# Test 6: Pull
echo "Test 6: Testing pull..."
# Get the most recent remote snapshot
REMOTE_SNAPSHOT=$(sudo find testing/mounts/btrfs2/.bfg_snapshots -type d -name "target_subvol_*" | sort | tail -1)
bfg pull --REMOTE_SNAPSHOT=$REMOTE_SNAPSHOT --LOCAL_SUBVOL=testing/mounts/btrfs1/pulled_subvol
# Verify pull worked
sudo ls -la testing/mounts/btrfs1/pulled_subvol/

# Test 7: Update DB
echo "Test 7: Testing DB update..."
bfg update_db --FS=testing/mounts/btrfs1
# Add more changes for DB testing
sudo dd count=10 bs=1M if=/dev/zero of="testing/mounts/btrfs1/source_subvol/additional_file" status=none
bfg --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1 local_commit --SUBVOL=testing/mounts/btrfs1/source_subvol --TAG=for_pruning

# Test 8: Prune local
echo "Test 8: Testing prune local with dry run..."
bfg prune_local --SUBVOL=testing/mounts/btrfs1/source_subvol --DRY_RUN=True

# Test 9: Combined operations
echo "Test 9: Testing commit_and_push_and_checkout..."
# Modify source data
sudo dd count=5 bs=1M if=/dev/zero of="testing/mounts/btrfs1/source_subvol/another_file" status=none
bfg --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1 commit_and_push_and_checkout --SUBVOL=testing/mounts/btrfs1/source_subvol --REMOTE_SUBVOL=testing/mounts/btrfs2/another_target
# Verify checkout worked
sudo ls -la testing/mounts/btrfs2/another_target/

# Test 10: Remote commit and pull
echo "Test 10: Testing remote_commit_and_pull..."
# Modify remote data
sudo dd count=5 bs=1M if=/dev/zero of="testing/mounts/btrfs2/another_target/yet_another_file" status=none
bfg --YES=True --REMOTE_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs2 remote_commit_and_pull --REMOTE_SUBVOL=testing/mounts/btrfs2/another_target --SUBVOL=testing/mounts/btrfs1/final_pull
# Verify pull worked
sudo ls -la testing/mounts/btrfs1/final_pull/

# Test 11: Commit and generate patch
echo "Test 11: Testing commit_and_generate_patch..."
sudo mkdir -p testing/mounts/btrfs1/patches
bfg --LOCAL_FS_TOP_LEVEL_SUBVOL_MOUNT_POINT=testing/mounts/btrfs1 commit_and_generate_patch --SUBVOL=testing/mounts/btrfs1/source_subvol --PATCH_FILE_DIR=testing/mounts/btrfs1/patches
# Verify patch file exists
sudo ls -la testing/mounts/btrfs1/patches/

# Cleanup
echo "Cleaning up..."
sudo umount testing/mounts/btrfs1
sudo umount testing/mounts/btrfs2
sudo losetup -D /dev/loop60
sudo losetup -D /dev/loop61

echo "All tests completed successfully!"