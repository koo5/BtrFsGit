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
    # exercise the pruning logic without needing a real database: null the advisory
    # lock and record (instead of executing) the marking of deleted db rows
    orig_advisory_lock = btrfsgit_db.advisory_lock
    orig_mark_deleted = btrfsgit_db.mark_deleted
    btrfsgit_db.advisory_lock = lambda: contextlib.nullcontext()
    bfg.marked_deleted = []
    btrfsgit_db.mark_deleted = lambda uuids: bfg.marked_deleted.extend(uuids)

    try:
        yield bfg
    finally:
        btrfsgit_db.advisory_lock = orig_advisory_lock
        btrfsgit_db.mark_deleted = orig_mark_deleted


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
    bfg.get_local_bfg_snapshots = MagicMock(return_value=btrfsgit.Res(mock_snapshots))
    
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
    bfg.get_local_bfg_snapshots = MagicMock(return_value=btrfsgit.Res(mock_snapshots))
    
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
    bfg.get_local_bfg_snapshots = MagicMock(return_value=btrfsgit.Res(snaps))
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

"""
snapshot-pile (series) commands: prune/clean/report_snapshots and _fs
"""


def _member(path, uuid, received, dt):
    return {'path': Path(path), 'local_uuid': uuid, 'received_uuid': received,
            'parent_uuid': None, 'ro': True, 'dt': dt, 'subvol_id': 0}


def test_parse_snapshot_name():
    bfg = btrfsgit.Bfg(YES=True)
    p = bfg.parse_snapshot_name('dev3_2026-06-22_08-15-49_from_jj')
    assert p['name'] == 'dev3'
    assert p['dt'] == datetime(2026, 6, 22, 8, 15, 49)
    assert p['tags'] == 'from_jj'
    legacy = bfg.parse_snapshot_name('data_bfg_snapshots_2023-05-10_10-00-00_tag')
    assert legacy['name'] == 'data'
    with pytest.raises(Exception):
        bfg.parse_snapshot_name('nonsense')


def test_shared_snapshots_protects_newest_shared_per_fs(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    # three received members; their content identities are the origin uuids o1..o3
    m1 = _member('/bac/.bfg_snapshots/dev3/dev3_a', 'r1', 'o1', now - timedelta(days=3))
    m2 = _member('/bac/.bfg_snapshots/dev3/dev3_b', 'r2', 'o2', now - timedelta(days=2))
    m3 = _member('/bac/.bfg_snapshots/dev3/dev3_c', 'r3', 'o3', now - timedelta(days=1))
    rows = [
        # the origin fs still holds o1 and o2; o3's origin snapshot is gone
        {'fs_uuid': 'd2fs', 'host': 'jj', 'fs': '/d2', 'local_uuid': 'o1', 'received_uuid': None},
        {'fs_uuid': 'd2fs', 'host': 'jj', 'fs': '/d2', 'local_uuid': 'o2', 'received_uuid': None},
        # an offsite fs holds a copy of o1 only
        {'fs_uuid': 'bac9fs', 'host': 'bac9', 'fs': '/bac9', 'local_uuid': 'x1', 'received_uuid': 'o1'},
        # rows on our own fs must be ignored
        {'fs_uuid': 'bacfs', 'host': 'jj', 'fs': '/bac', 'local_uuid': 'r1', 'received_uuid': 'o1'},
    ]
    shared = bfg._shared_snapshots(rows, 'bacfs', [m1, m2, m3])
    # newest member shared with the origin fs is m2; newest shared with bac9 is m1;
    # m3 is shared with nothing (its origin was pruned away) so it is not protected
    assert shared == {
        m2['path']: ['jj:/d2'],
        m1['path']: ['bac9:/bac9'],
    }


def test_shared_snapshots_ignores_deleted_rows(mock_bfg_for_pruning):
    """A row flagged deleted (mark_deleted between update_db runs) must not count as a
    copy: otherwise it shifts "newest shared" past the newest real pair, unprotecting it."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    m1 = _member('/bac/.bfg_snapshots/dev3/dev3_a', 'r1', 'o1', now - timedelta(days=2))
    m2 = _member('/bac/.bfg_snapshots/dev3/dev3_b', 'r2', 'o2', now - timedelta(days=1))
    rows = [
        {'fs_uuid': 'd2fs', 'host': 'jj', 'fs': '/d2', 'local_uuid': 'o1', 'received_uuid': None,
         'deleted': False},
        # the origin of the newer member was just deleted on jj - a phantom row
        {'fs_uuid': 'd2fs', 'host': 'jj', 'fs': '/d2', 'local_uuid': 'o2', 'received_uuid': None,
         'deleted': True},
    ]
    shared = bfg._shared_snapshots(rows, 'bacfs', [m1, m2])
    # protection must stay on m1 (the newest REAL pair), not shift to m2
    assert shared == {m1['path']: ['jj:/d2']}


def _flat_listing(now):
    """two series (dev3, home) flat in one .bfg_snapshots dir, plus ignorable entries"""
    dev3 = [_member(f'/bac/.bfg_snapshots/dev3_2026-06-{d:02d}_00-00-00_t', f'd{d}', f'od{d}',
                    now - timedelta(days=30 - d)) for d in range(1, 11)]
    home = [_member(f'/bac/.bfg_snapshots/home_2026-06-{d:02d}_00-00-00_t', f'h{d}', f'oh{d}',
                    now - timedelta(days=20 - d)) for d in range(1, 5)]
    other = [
        # rw subvol: ignored
        {'path': Path('/bac/backups/jj/dev3'), 'local_uuid': 'rw1', 'received_uuid': None,
         'parent_uuid': None, 'ro': False, 'subvol_id': 0},
    ]
    return dev3, home, dev3 + home + other


def test_clean_snapshots_splits_series_and_spares_shared(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    dev3, home, listing = _flat_listing(now)
    bfg._get_subvolumes = MagicMock(return_value=listing)
    bfg.local_fs_uuid = MagicMock(return_value='bacfs')
    # the origin fs still holds the content of dev3[2] -> newest shared, protected
    bfg.all_subvols_from_db = MagicMock(return_value=[
        {'fs_uuid': 'd2fs', 'host': 'jj', 'fs': '/d2', 'local_uuid': 'od3', 'received_uuid': None},
    ])

    bfg.clean_snapshots('/bac/.bfg_snapshots', PERCENT=50, DB=True, DRY_RUN=False)

    deleted = _delete_calls(bfg._local_cmd)
    # dev3: oldest 5 of 10 considered, dev3[2] spared as shared -> d1, d2, d4, d5 deleted.
    # home: oldest 2 of 4 considered, nothing shared -> h1, h2 deleted.
    expected = [str(dev3[i]['path']) for i in (0, 1, 3, 4)] + [str(home[i]['path']) for i in (0, 1)]
    assert deleted == expected, deleted
    # ...and each deletion was flagged in the db, so other machines don't see phantom rows
    assert bfg.marked_deleted == ['d1', 'd2', 'd4', 'd5', 'h1', 'h2']


def test_prune_fs_thins_each_series(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    dev3, home, listing = _flat_listing(now)
    bfg._get_subvolumes = MagicMock(return_value=listing)
    bfg.local_fs_uuid = MagicMock(return_value='bacfs')
    bfg.all_subvols_from_db = MagicMock(return_value=[])
    # deterministic buckets per series: everything in one bucket -> keep only the last
    bfg.put_snapshots_into_buckets = MagicMock(side_effect=lambda snaps: {'bucket': list(snaps)})

    bfg.prune_fs('/bac', DB=True, DRY_RUN=False)

    deleted = _delete_calls(bfg._local_cmd)
    # per series, everything but the last-in-bucket (= newest) goes
    expected = [str(x['path']) for x in dev3[:-1]] + [str(x['path']) for x in home[:-1]]
    assert deleted == expected, deleted


def test_snapshots_dry_run_deletes_nothing(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    _, _, listing = _flat_listing(now)
    bfg._get_subvolumes = MagicMock(return_value=listing)
    bfg.local_fs_uuid = MagicMock(return_value='bacfs')
    bfg.all_subvols_from_db = MagicMock(return_value=[])

    bfg.clean_snapshots('/bac/.bfg_snapshots', PERCENT=100, DB=True, DRY_RUN=True)
    bfg.prune_fs('/bac', DB=True, DRY_RUN=True)

    assert _delete_calls(bfg._local_cmd) == []


def test_delete_failure_does_not_abort(mock_bfg_for_pruning):
    """A snapshot vanishing between listing and deletion (concurrent backup run) must be
    logged and skipped, not kill the whole prune/clean pipeline."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    snaps = [
        {"path": Path(f"/snap/s{i}"), "dt": now - timedelta(days=10 - i), "parent_uuid": "test-uuid"}
        for i in range(4)
    ]
    bfg.all_subvols_from_db = MagicMock(return_value=[])
    bfg.most_recent_common_snapshots = MagicMock(return_value=[])
    bfg.get_local_bfg_snapshots = MagicMock(return_value=btrfsgit.Res(snaps))
    bfg.put_snapshots_into_buckets = MagicMock(return_value={"bucket": list(snaps)})
    # every delete fails (returns -1, as _local_cmd does with die_on_error=False)
    bfg._local_cmd = MagicMock(return_value=-1)

    bfg.prune_local("/test/subvol", DB=True, DRY_RUN=False)  # must not raise or exit

    # all three prunable snapshots were attempted despite each attempt failing
    assert len(_delete_calls(bfg._local_cmd)) == 3

    # clean path: same tolerance
    bfg._local_cmd.reset_mock()
    bfg._shared_parents = MagicMock(return_value={})
    bfg.clean_local("/test/subvol", PERCENT=100, DB=True, DRY_RUN=False)
    assert len(_delete_calls(bfg._local_cmd)) == 3
