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
from btrfsgit import db as btrfsgit_db
import contextlib


def _delete_calls(mock_cmd):
    """Return the snapshot paths that `mock_cmd` was asked to `btrfs subvolume delete`."""
    paths = []
    for c in mock_cmd.call_args_list:
        args = c.args[0] if c.args else None
        if isinstance(args, list) and [str(x) for x in args[:3]] == ["btrfs", "subvolume", "delete"]:
            paths.append(str(args[3]))
    return paths


@pytest.fixture
def mock_bfg_for_pruning():
    """Create a BtrFsGit instance with mocked methods for pruning tests."""
    bfg = btrfsgit.Bfg(YES=True)
    
    # Mock various methods
    bfg._local_cmd = MagicMock(return_value="mock output")
    bfg._remote_cmd = MagicMock(return_value="mock remote output")
    bfg.get_subvol = MagicMock(return_value=btrfsgit.Res({"local_uuid": "test-uuid"}))
    bfg._yes = MagicMock(return_value=True)
    # exercise the pruning logic without needing a real database for the advisory lock
    bfg._orig_advisory_lock = btrfsgit_db.advisory_lock
    btrfsgit_db.advisory_lock = lambda: contextlib.nullcontext()
    
    try:
        yield bfg
    finally:
        btrfsgit_db.advisory_lock = bfg._orig_advisory_lock


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
    
    # With just one snapshot per bucket, nothing is prunable.
    assert _delete_calls(bfg._local_cmd) == []
    
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
    
    # The older snapshot in the under-1-min bucket is the only prunable one.
    assert _delete_calls(bfg._local_cmd) == [str(mock_snapshots[1]["path"])]


def test_prune_local_thins_past_shared_parent(mock_bfg_for_pruning):
    """
    prune_local must keep the shared parent (MRC) and the newest snapshot, but should still
    thin snapshots NEWER than the shared parent by the bucket policy (keep one per bucket).
    Regression guard for dropping the old "stop at the first shared parent" early-return:
    with the freeze this deletes nothing; with per-bucket thinning it deletes the extra
    recent snapshot.
    """
    bfg = mock_bfg_for_pruning
    now = datetime.now()

    sp = {"path": Path("/snap/sp"), "dt": now - timedelta(days=30), "parent_uuid": "test-uuid"}       # shared parent, oldest
    x1 = {"path": Path("/snap/x1"), "dt": now - timedelta(hours=2), "parent_uuid": "test-uuid"}        # newer than sp, extra in bucket
    x2 = {"path": Path("/snap/x2"), "dt": now - timedelta(hours=1), "parent_uuid": "test-uuid"}        # newer than sp, newest in bucket
    newest = {"path": Path("/snap/newest"), "dt": now - timedelta(minutes=5), "parent_uuid": "test-uuid"}
    snaps = [sp, x1, x2, newest]

    bfg.all_subvols_from_db = MagicMock(return_value=snaps)
    bfg.local_bfg_snapshots = MagicMock(return_value=snaps)
    bfg.most_recent_common_snapshots = MagicMock(return_value=[sp])  # sp is the shared parent
    # deterministic buckets, oldest first; x1 and x2 share a bucket so one of them is prunable
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "month": [sp],
        "hour": [x1, x2],
        "minute": [newest],
    })

    bfg.prune_local("/test/subvol", DB=True, DRY_RUN=False)

    deleted = _delete_calls(bfg._local_cmd)
    # x1 is newer than the shared parent, not the newest, and not the last in its bucket -> prunable.
    assert deleted == [str(x1["path"])], deleted
    # the shared parent, its bucket's newest sibling, and the overall newest are all kept
    assert str(sp["path"]) not in deleted
    assert str(x2["path"]) not in deleted
    assert str(newest["path"]) not in deleted


def test_prune_remote_with_mock_data(mock_bfg_for_pruning):
    """
    prune_remote keeps the most recent common snapshot (the send parent on the backup fs) and
    the newest, and thins everything else per bucket - including snapshots newer than the shared
    parent, just like prune_local (no early-return / freeze).
    """
    bfg = mock_bfg_for_pruning
    now = datetime.now()

    # snapshots on the remote backup filesystem (fs_uuid = "remote-fs")
    r_old1 = {"path": Path("/remote/old1"), "dt": now - timedelta(days=40), "received_uuid": "s-old1", "local_uuid": "r-old1", "fs_uuid": "remote-fs"}
    r_old2 = {"path": Path("/remote/old2"), "dt": now - timedelta(days=35), "received_uuid": "s-old2", "local_uuid": "r-old2", "fs_uuid": "remote-fs"}
    r_mrc = {"path": Path("/remote/mrc"), "dt": now - timedelta(days=10), "received_uuid": "s-mrc", "local_uuid": "r-mrc", "fs_uuid": "remote-fs"}
    r_new1 = {"path": Path("/remote/new1"), "dt": now - timedelta(hours=2), "received_uuid": "s-new1", "local_uuid": "r-new1", "fs_uuid": "remote-fs"}
    r_new2 = {"path": Path("/remote/new2"), "dt": now - timedelta(hours=1), "received_uuid": "s-new2", "local_uuid": "r-new2", "fs_uuid": "remote-fs"}
    r_newest = {"path": Path("/remote/newest"), "dt": now - timedelta(minutes=5), "received_uuid": "s-newest", "local_uuid": "r-newest", "fs_uuid": "remote-fs"}
    remote_snaps = [r_old1, r_old2, r_mrc, r_new1, r_new2, r_newest]

    # the local most-recent-common snapshot; its received copy on the remote is r_mrc
    local_mrc = {"path": Path("/local/mrc"), "local_uuid": "s-mrc", "fs_uuid": "local-fs"}

    bfg.all_subvols_from_db = MagicMock(return_value=remote_snaps + [local_mrc])
    bfg.most_recent_common_snapshots = MagicMock(return_value=[local_mrc])
    bfg.remote_fs_uuid = MagicMock(return_value=("remote-fs", Path("/remote/fs")))
    bfg.remote_bfg_snapshots = MagicMock(return_value=remote_snaps)
    # deterministic buckets, oldest first: two old snapshots share a bucket, the shared parent is
    # alone, and two recent snapshots (newer than the shared parent) share a bucket.
    bfg.put_snapshots_into_buckets = MagicMock(return_value={
        "month": [r_old1, r_old2],
        "mrc": [r_mrc],
        "hour": [r_new1, r_new2],
        "minute": [r_newest],
    })

    bfg.prune_remote(LOCAL_SUBVOL="/local/subvol", REMOTE_SUBVOL="/remote/subvol", DRY_RUN=False)

    deleted = _delete_calls(bfg._remote_cmd)
    # r_old1 (extra in the month bucket) AND r_new1 (extra in the recent bucket, newer than the
    # shared parent) are both pruned - prune_remote thins past the shared parent, like prune_local.
    assert deleted == [str(r_old1["path"]), str(r_new1["path"])], deleted
    assert str(r_mrc["path"]) not in deleted         # shared parent kept
    assert str(r_new2["path"]) not in deleted        # newest in its bucket kept
    assert str(r_newest["path"]) not in deleted      # overall newest kept


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