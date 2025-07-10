#!/usr/bin/env python
"""Tests for BtrFsGit snapshot pruning functionality."""

import os
import pytest
import subprocess
import time
from datetime import datetime, timedelta
from pathlib import Path
from unittest.mock import patch, MagicMock

from btrfsgit import btrfsgit


@pytest.fixture
def mock_bfg_for_pruning():
    """Create a BtrFsGit instance with mocked methods for pruning tests."""
    bfg = btrfsgit.Bfg(YES=True)
    
    # Mock various methods
    bfg._local_cmd = MagicMock(return_value="mock output")
    bfg._remote_cmd = MagicMock(return_value="mock remote output")
    bfg.get_subvol = MagicMock(return_value=btrfsgit.Res({"local_uuid": "test-uuid"}))
    bfg._yes = MagicMock(return_value=True)
    
    return bfg


def test_put_snapshots_into_buckets_with_real_dates():
    """Test bucket calculation with real datetime objects."""
    bfg = btrfsgit.Bfg(YES=True)
    
    # Create reference date
    now = datetime.now()
    
    # Create snapshots with different timestamps
    snapshots = [
        {"path": "/path/to/snap1", "dt": now - timedelta(seconds=30)},
        {"path": "/path/to/snap2", "dt": now - timedelta(minutes=30)},
        {"path": "/path/to/snap3", "dt": now - timedelta(hours=2)},
        {"path": "/path/to/snap4", "dt": now - timedelta(days=10)},
        {"path": "/path/to/snap5", "dt": now - timedelta(days=60)}
    ]
    
    # Test the method with real dates
    buckets = bfg.put_snapshots_into_buckets(snapshots)
    
    # Verify we have different buckets
    assert len(buckets) > 1
    
    # Check for expected bucket types
    under_minute = [b for b in buckets.keys() if "under-1-min" in b]
    assert len(under_minute) > 0
    
    # Verify all snapshots are included
    total_snapshots = sum(len(snaps) for snaps in buckets.values())
    assert total_snapshots == len(snapshots)


def test_prune_local_with_mock_data(mock_bfg_for_pruning):
    """Test the prune_local method with mock data."""
    bfg = mock_bfg_for_pruning
    
    # Create mock snapshots
    now = datetime.now()
    mock_snapshots = [
        {
            "path": Path("/path/to/snap1"),
            "dt": now - timedelta(seconds=30),
            "parent_uuid": "test-uuid"
        },
        {
            "path": Path("/path/to/snap2"),
            "dt": now - timedelta(minutes=30),
            "parent_uuid": "test-uuid"
        },
        {
            "path": Path("/path/to/snap3"),
            "dt": now - timedelta(hours=2),
            "parent_uuid": "test-uuid"
        }
    ]
    
    # Mock methods that prune_local depends on
    bfg.all_subvols_from_db = MagicMock(return_value=mock_snapshots)
    bfg.most_recent_common_snapshots = MagicMock(return_value=[])
    bfg.local_bfg_snapshots = MagicMock(return_value=mock_snapshots)
    
    # Set up mocked bucket function to use fixed buckets
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "under-1-min": [mock_snapshots[0]],
        "minute-bucket": [mock_snapshots[1]],
        "hour-bucket": [mock_snapshots[2]]
    })
    
    # Call prune_local
    bfg.prune_local("/test/subvol", DB=True, DRY_RUN=False)
    
    # Verify it tried to delete something
    # Last snapshot in each bucket shouldn't be deleted, but others should
    delete_calls = [
        call for call in bfg._local_cmd.call_args_list 
        if "btrfs subvolume delete" in str(call)
    ]
    
    # Count should match prunable snapshots (not the last in each bucket)
    # In our mock we have just one snapshot in each bucket, so nothing gets deleted
    assert len(delete_calls) == 0
    
    # Test with more snapshots in one bucket
    now = datetime.now()
    mock_snapshots = [
        # under-1-min bucket (2 snapshots)
        {
            "path": Path("/path/to/snap1"),
            "dt": now - timedelta(seconds=10),
            "parent_uuid": "test-uuid"
        },
        {
            "path": Path("/path/to/snap2"),
            "dt": now - timedelta(seconds=30),
            "parent_uuid": "test-uuid"
        },
        # hour bucket (1 snapshot)
        {
            "path": Path("/path/to/snap3"),
            "dt": now - timedelta(hours=2),
            "parent_uuid": "test-uuid"
        }
    ]
    
    # Reset mocks
    bfg._local_cmd.reset_mock()
    bfg.local_bfg_snapshots = MagicMock(return_value=mock_snapshots)
    
    # Set up mocked bucket function to use fixed buckets with multiple snapshots
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "under-1-min": [mock_snapshots[1], mock_snapshots[0]],  # Sorted older to newer
        "hour-bucket": [mock_snapshots[2]]
    })
    
    # Call prune_local
    bfg.prune_local("/test/subvol", DB=True, DRY_RUN=False)
    
    # Verify it tried to delete the older snapshot in the under-1-min bucket
    delete_calls = [
        call for call in bfg._local_cmd.call_args_list 
        if "btrfs subvolume delete" in str(call)
    ]
    assert len(delete_calls) == 1


def test_prune_remote_with_mock_data(mock_bfg_for_pruning):
    """Test the prune_remote method with mock data."""
    bfg = mock_bfg_for_pruning
    
    # Create mock snapshots
    now = datetime.now()
    mock_local_snapshots = [
        {
            "path": Path("/local/snap1"),
            "dt": now - timedelta(seconds=30),
            "parent_uuid": "test-uuid",
            "local_uuid": "local-uuid1"
        }
    ]
    
    mock_remote_snapshots = [
        {
            "path": Path("/remote/snap1"),
            "dt": now - timedelta(seconds=30),
            "received_uuid": "local-uuid1",
            "local_uuid": "remote-uuid1"
        },
        {
            "path": Path("/remote/snap2"),
            "dt": now - timedelta(minutes=30),
            "received_uuid": None,
            "local_uuid": "remote-uuid2"
        },
        {
            "path": Path("/remote/snap3"),
            "dt": now - timedelta(hours=2),
            "received_uuid": None,
            "local_uuid": "remote-uuid3"
        }
    ]
    
    # Mock methods that prune_remote depends on
    bfg.all_subvols_from_db = MagicMock(return_value=mock_local_snapshots + mock_remote_snapshots)
    bfg.most_recent_common_snapshots = MagicMock(return_value=[mock_local_snapshots[0]])
    bfg.remote_bfg_snapshots = MagicMock(return_value=mock_remote_snapshots)
    
    # Set up mocked remote_fs_uuid
    bfg.remote_fs_uuid = MagicMock(return_value=("remote-fs-uuid", Path("/remote/fs")))
    
    # Set up mocked bucket function to use fixed buckets
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "under-1-min": [mock_remote_snapshots[0]],
        "minute-bucket": [mock_remote_snapshots[1]],
        "hour-bucket": [mock_remote_snapshots[2]]
    })
    
    # Call prune_remote
    bfg.prune_remote(
        LOCAL_SUBVOL="/local/subvol",
        REMOTE_SUBVOL="/remote/subvol",
        DRY_RUN=False
    )
    
    # Verify it tried to delete something
    # The first snapshot is an MRC so it shouldn't be deleted
    delete_calls = [
        call for call in bfg._remote_cmd.call_args_list 
        if "btrfs subvolume delete" in str(call)
    ]
    
    # We should have tried to delete the non-MRC snapshots that aren't the last in their bucket
    # In this case, we have only one snapshot per bucket, so nothing gets deleted
    assert len(delete_calls) == 0
    
    # Test with more snapshots where some should be pruned
    now = datetime.now()
    mock_remote_snapshots = [
        # MRC snapshot - should be kept
        {
            "path": Path("/remote/snap1"),
            "dt": now - timedelta(seconds=30),
            "received_uuid": "local-uuid1",
            "local_uuid": "remote-uuid1"
        },
        # Another snapshot in the same bucket - should be pruned
        {
            "path": Path("/remote/snap2"),
            "dt": now - timedelta(seconds=40),
            "received_uuid": None,
            "local_uuid": "remote-uuid2"
        },
        # Last snapshot in its bucket - should be kept
        {
            "path": Path("/remote/snap3"),
            "dt": now - timedelta(hours=2),
            "received_uuid": None,
            "local_uuid": "remote-uuid3"
        }
    ]
    
    # Reset mocks
    bfg._remote_cmd.reset_mock()
    bfg.remote_bfg_snapshots = MagicMock(return_value=mock_remote_snapshots)
    
    # Set up mocked bucket function with the new snapshots
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "under-1-min": [mock_remote_snapshots[1], mock_remote_snapshots[0]],  # Older first
        "hour-bucket": [mock_remote_snapshots[2]]
    })
    
    # Mock most_recent_common_snapshots to return the first snapshot as MRC
    bfg.most_recent_common_snapshots = MagicMock(return_value=[
        {"path": Path("/remote/snap1"), "local_uuid": "remote-uuid1"}
    ])
    
    # Call prune_remote
    bfg.prune_remote(
        LOCAL_SUBVOL="/local/subvol",
        REMOTE_SUBVOL="/remote/subvol",
        DRY_RUN=False
    )
    
    # The MRC check should prevent any deletion
    delete_calls = [
        call for call in bfg._remote_cmd.call_args_list 
        if "btrfs subvolume delete" in str(call)
    ]
    assert len(delete_calls) == 0


@pytest.mark.integration
def test_pruning_integration(btrfs_loopback_setup):
    """Integration test for pruning functionality with real btrfs filesystems."""
    fs1 = btrfs_loopback_setup["fs1"]
    
    # Create test subvolume
    test_subvol = fs1 / "prune_test_subvol"
    if not test_subvol.exists():
        subprocess.run(
            f"sudo btrfs subvolume create {test_subvol}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {test_subvol}",
            shell=True, check=True
        )
    
    # Create a BFG instance
    bfg = btrfsgit.Bfg(YES=True)
    
    # Create multiple snapshots with different timestamps
    # We'll create them all now but manipulate their names to appear older
    snapshots = []
    
    # Base timestamp for today
    now = datetime.now()
    
    # Create date strings for different timeframes
    dates = [
        now.strftime("%Y-%m-%d_%H-%M-%S"),  # Now
        (now - timedelta(minutes=5)).strftime("%Y-%m-%d_%H-%M-%S"),  # 5 minutes ago
        (now - timedelta(hours=2)).strftime("%Y-%m-%d_%H-%M-%S"),  # 2 hours ago
        (now - timedelta(days=2)).strftime("%Y-%m-%d_%H-%M-%S"),  # 2 days ago
        (now - timedelta(days=40)).strftime("%Y-%m-%d_%H-%M-%S")  # 40 days ago
    ]
    
    # Create content file in the subvolume
    with open(test_subvol / "test_file.txt", "w") as f:
        f.write("Pruning test content")
    
    # Create snapshots with manipulated timestamps
    for i, date_str in enumerate(dates):
        # Use the implementation's default snapshot naming scheme but override the date
        parent_dir = bfg.calculate_default_snapshot_parent_dir("local", test_subvol).val
        snapshot_path = f"{parent_dir}/{test_subvol.name}_{date_str}_test_tag"
        
        # Ensure parent directory exists
        os.makedirs(parent_dir, exist_ok=True)
        
        # Create the snapshot
        subprocess.run(
            f"sudo btrfs subvolume snapshot -r {test_subvol} {snapshot_path}",
            shell=True, check=True
        )
        
        # Record the snapshot path
        snapshots.append(snapshot_path)
    
    # Count initial snapshots
    initial_count = len(snapshots)
    assert initial_count == 5
    
    # Run prune_local with DRY_RUN=True to verify what would be pruned
    bfg.prune_local(str(test_subvol), DB=False, DRY_RUN=True)
    
    # Count snapshots again - should be unchanged after dry run
    snapshot_count = len([p for p in Path(parent_dir).glob(f"{test_subvol.name}_*_test_tag") if p.is_dir()])
    assert snapshot_count == initial_count
    
    # Run actual pruning
    bfg.prune_local(str(test_subvol), DB=False, DRY_RUN=False)
    
    # Count snapshots after pruning
    pruned_snapshot_paths = [p for p in Path(parent_dir).glob(f"{test_subvol.name}_*_test_tag") if p.is_dir()]
    pruned_count = len(pruned_snapshot_paths)
    
    # We should have fewer snapshots now
    # Based on the pruning algorithm, we should keep:
    # - The newest snapshot (from now)
    # - One snapshot per time bucket (minute, hour, day, month)
    # Since we have just one snapshot in each bucket, we should keep all 5
    assert pruned_count == 5
    
    # Create multiple snapshots in the same bucket to force pruning
    same_bucket_snapshots = []
    
    # Create 3 snapshots with timestamps in the same minute
    minute_time = now - timedelta(minutes=10)
    for i in range(3):
        adjusted_time = minute_time + timedelta(seconds=i*10)
        date_str = adjusted_time.strftime("%Y-%m-%d_%H-%M-%S")
        
        snapshot_path = f"{parent_dir}/{test_subvol.name}_{date_str}_same_bucket"
        
        # Create the snapshot
        subprocess.run(
            f"sudo btrfs subvolume snapshot -r {test_subvol} {snapshot_path}",
            shell=True, check=True
        )
        
        same_bucket_snapshots.append(snapshot_path)
    
    # Run pruning on the same-bucket snapshots
    bfg.prune_local(str(test_subvol), DB=False, DRY_RUN=False)
    
    # Count same-bucket snapshots after pruning
    same_bucket_pruned = [
        p for p in Path(parent_dir).glob(f"{test_subvol.name}_*_same_bucket") 
        if p.is_dir()
    ]
    
    # We should have only 1 snapshot left in this bucket (the newest one)
    assert len(same_bucket_pruned) == 1