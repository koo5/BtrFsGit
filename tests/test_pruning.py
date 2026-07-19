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


"""
per-deletion mark_deleted, aborted receives, min-free cleaning
"""


def test_mark_deleted_is_per_deletion(mock_bfg_for_pruning):
    """a crash mid-loop must not leave already-deleted snapshots unflagged: each
    successful delete is flagged immediately, failed deletes are not flagged."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    snaps = [
        {"path": Path(f"/snap/s{i}"), "dt": now - timedelta(days=10 - i),
         "parent_uuid": "test-uuid", "local_uuid": f"u{i}"}
        for i in range(4)
    ]
    bfg.all_subvols_from_db = MagicMock(return_value=[])
    bfg.most_recent_common_snapshots = MagicMock(return_value=[])
    bfg.get_local_bfg_snapshots = MagicMock(return_value=btrfsgit.Res(snaps))
    bfg.put_snapshots_into_buckets = MagicMock(return_value={"bucket": list(snaps)})
    # s0 deletes fine, s1 fails, s2 deletes fine (s3 is newest - kept)
    bfg._local_cmd = MagicMock(side_effect=["", -1, ""])

    bfg.prune_local("/test/subvol", DB=True, DRY_RUN=False)

    assert bfg.marked_deleted == ["u0", "u2"]


def test_prune_remote_marks_deleted(mock_bfg_for_pruning):
    """prune_remote flags its remote deletions in the db like every other delete path."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    r1 = {"path": Path("/remote/r1"), "dt": now - timedelta(days=40),
          "received_uuid": "s1", "local_uuid": "r1", "fs_uuid": "remote-fs"}
    r2 = {"path": Path("/remote/r2"), "dt": now - timedelta(days=35),
          "received_uuid": "s2", "local_uuid": "r2", "fs_uuid": "remote-fs"}
    bfg.all_subvols_from_db = MagicMock(return_value=[r1, r2])
    bfg.most_recent_common_snapshots = MagicMock(return_value=[])
    bfg.remote_fs_uuid = MagicMock(return_value=("remote-fs", Path("/remote/fs")))
    bfg.remote_bfg_snapshots = MagicMock(return_value=[r1, r2])
    bfg.put_snapshots_into_buckets = MagicMock(return_value={"b": [r1, r2]})

    bfg.prune_remote(LOCAL_SUBVOL="/l", REMOTE_SUBVOL="/r", DRY_RUN=False)

    assert _delete_calls(bfg._remote_cmd) == [str(r1["path"])]
    assert bfg.marked_deleted == ["r1"]

    # ...and a failing remote delete neither aborts nor flags
    bfg._remote_cmd = MagicMock(return_value=-1)
    bfg.marked_deleted.clear()
    bfg.prune_remote(LOCAL_SUBVOL="/l", REMOTE_SUBVOL="/r", DRY_RUN=False)
    assert bfg.marked_deleted == []


def test_unparseable_name_is_skipped_not_fatal(mock_bfg_for_pruning):
    """a manually renamed / foreign subvol under .bfg_snapshots must not kill the
    series commands - it is skipped with a warning."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    good = [_member(f'/bac/.bfg_snapshots/dev3_2026-06-{d:02d}_00-00-00_t', f'd{d}', f'od{d}',
                    now - timedelta(days=10 - d)) for d in range(1, 4)]
    # ro subvol with a name that does not parse: no 'dt' key, exactly as the real
    # _get_subvolumes leaves it after its warn-and-skip guard
    junk = {'path': Path('/bac/.bfg_snapshots/manual-backup-KEEP'), 'local_uuid': 'j1',
            'received_uuid': None, 'parent_uuid': None, 'ro': True, 'subvol_id': 0}
    bfg._get_subvolumes = MagicMock(return_value=good + [junk])
    bfg.local_fs_uuid = MagicMock(return_value='bacfs')
    bfg.all_subvols_from_db = MagicMock(return_value=[])

    bfg.clean_snapshots('/bac/.bfg_snapshots', PERCENT=100, DB=True, DRY_RUN=False)

    deleted = _delete_calls(bfg._local_cmd)
    # the two oldest of the good series go; the junk subvol is untouched
    assert deleted == [str(good[0]['path']), str(good[1]['path'])], deleted
    assert str(junk['path']) not in deleted


def _partial(dirname, name, uuid='pu1'):
    return {'path': Path(dirname) / name, 'local_uuid': uuid, 'received_uuid': None,
            'parent_uuid': None, 'ro': False, 'subvol_id': 0}


def _abort_cmd_router(lock_exists, lock_held, deletes, rms, lockdir_listing=None):
    """route the sweep's _local_cmd calls by command shape"""
    def route(cmd, die_on_error=True, logger=None, capture_stderr=False):
        cmd = [str(x) for x in cmd]
        if cmd[0] == 'test':
            return "" if lock_exists else -1
        if cmd[0] == 'flock' and cmd[-1] == 'true':
            return -1 if lock_held else ""
        if cmd[0] == 'flock' and cmd[3:6] == ['btrfs', 'subvolume', 'delete']:
            deletes.append(cmd[6])
            return ""
        if cmd[0] == 'flock' and cmd[3] == 'rm':
            # GC removes lock files only under flock -n on the file itself; a plain
            # rm is a regression (would unlink a lock a just-started receive holds)
            assert cmd[2] == cmd[4], cmd
            rms.append(cmd[4])
            return ""
        if cmd[0] == 'ls':
            return lockdir_listing if lockdir_listing is not None else -1
        return ""
    return route


def test_aborted_receive_swept_when_provably_dead(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    d = '/bac/.bfg_snapshots/dev3'
    partial = _partial(d, 'dev3_2026-06-22_08-15-49_from_jj')
    bfg._get_subvolumes = MagicMock(return_value=[partial])
    deletes, rms = [], []
    bfg._local_cmd = MagicMock(side_effect=_abort_cmd_router(True, False, deletes, rms))

    bfg._sweep_aborted_receives(d, d, DRY_RUN=False)

    assert deletes == [str(partial['path'])]
    assert bfg.marked_deleted == ['pu1']


def test_aborted_receive_spared_when_in_flight(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    d = '/bac/.bfg_snapshots/dev3'
    partial = _partial(d, 'dev3_2026-06-22_08-15-49_from_jj')
    bfg._get_subvolumes = MagicMock(return_value=[partial])
    deletes, rms = [], []
    bfg._local_cmd = MagicMock(side_effect=_abort_cmd_router(True, True, deletes, rms))

    bfg._sweep_aborted_receives(d, d, DRY_RUN=False)

    assert deletes == []
    assert bfg.marked_deleted == []


def test_aborted_receive_unproven_is_reported_not_deleted(mock_bfg_for_pruning):
    """no lock file (receive predates locking, or raw btrfs receive): never delete."""
    bfg = mock_bfg_for_pruning
    d = '/bac/.bfg_snapshots/dev3'
    partial = _partial(d, 'dev3_2026-06-22_08-15-49_from_jj')
    bfg._get_subvolumes = MagicMock(return_value=[partial])
    deletes, rms = [], []
    bfg._local_cmd = MagicMock(side_effect=_abort_cmd_router(False, False, deletes, rms))

    bfg._sweep_aborted_receives(d, d, DRY_RUN=False)

    assert deletes == []
    assert bfg.marked_deleted == []


def test_aborted_receive_dry_run_deletes_nothing(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    d = '/bac/.bfg_snapshots/dev3'
    partial = _partial(d, 'dev3_2026-06-22_08-15-49_from_jj')
    bfg._get_subvolumes = MagicMock(return_value=[partial])
    deletes, rms = [], []
    bfg._local_cmd = MagicMock(side_effect=_abort_cmd_router(True, False, deletes, rms))

    bfg._sweep_aborted_receives(d, d, DRY_RUN=True)

    assert deletes == []
    assert rms == []  # GC also skipped in dry-run


def test_receive_lock_gc(mock_bfg_for_pruning):
    """lock files for succeeded (ro) or gone snapshots are GCd; a live partial's is kept."""
    bfg = mock_bfg_for_pruning
    d = Path('/bac/.bfg_snapshots/dev3')
    now = datetime.now()
    done = _member(str(d / 'dev3_2026-06-20_00-00-00_t'), 'r1', 'o1', now)
    partial = _partial(str(d), 'dev3_2026-06-22_08-15-49_from_jj')
    listing = '\n'.join([
        done['path'].name,      # ro sibling exists -> GC
        partial['path'].name,   # live rw partial -> KEEP
        'dev3_2026-01-01_00-00-00_gone',  # no subvol at all -> GC
    ])
    bfg._get_subvolumes = MagicMock(return_value=[done, partial])
    deletes, rms = [], []
    # lock exists but held: the partial is in-flight, so only the GC part acts
    bfg._local_cmd = MagicMock(side_effect=_abort_cmd_router(True, True, deletes, rms,
                                                             lockdir_listing=listing))

    bfg._sweep_aborted_receives(str(d), str(d), DRY_RUN=False)

    assert deletes == []
    lockdir = str(d / '.bfg_receive_locks')
    assert rms == [f"{lockdir}/{done['path'].name}",
                   f"{lockdir}/dev3_2026-01-01_00-00-00_gone"], rms


def test_receive_cmd_str(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    assert bfg._receive_cmd_str('/bac/.bfg_snapshots/dev3', 'dev3_2026-06-22_08-15-49_from_jj') == \
        'flock -n -E 75 /bac/.bfg_snapshots/dev3/.bfg_receive_locks/.series ' \
        'flock /bac/.bfg_snapshots/dev3/.bfg_receive_locks/dev3_2026-06-22_08-15-49_from_jj ' \
        'btrfs receive /bac/.bfg_snapshots/dev3'


def test_series_lock_is_dot_named(mock_bfg_for_pruning):
    """the GC's `ls -1` must never list the series lock (dotfile), or it would try
    to remove a file that in-flight transfers hold; snapshot names can't collide
    with it either, since parse_snapshot_name requires a timestamped name."""
    bfg = mock_bfg_for_pruning
    lock = bfg._series_lock_path('/bac/.bfg_snapshots/dev3')
    assert lock.parent == bfg._receive_locks_dir('/bac/.bfg_snapshots/dev3')
    assert lock.name.startswith('.')


def test_push_skips_on_series_busy(mock_bfg_for_pruning):
    """a series-lock conflict (exit 75) means someone else is transferring this
    series: push must skip gracefully, not raise."""
    bfg = mock_bfg_for_pruning
    bfg.calculate_default_snapshot_parent_dir = MagicMock(
        return_value=btrfsgit.Res(Path('/r/.bfg_snapshots/data')))
    bfg.local_send = MagicMock(
        side_effect=subprocess.CalledProcessError(btrfsgit.SERIES_BUSY_EXIT, 'send|receive'))

    res = bfg.push('/fs/data', '/fs/.bfg_snapshots/data_2026-06-22_08-15-49_t',
                   '/r/data', PARENT='/fs/.bfg_snapshots/data_2026-06-21_08-15-49_t')
    assert res.val is None


def test_push_reraises_on_real_receive_failure(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    bfg.calculate_default_snapshot_parent_dir = MagicMock(
        return_value=btrfsgit.Res(Path('/r/.bfg_snapshots/data')))
    bfg.local_send = MagicMock(side_effect=subprocess.CalledProcessError(1, 'send|receive'))

    with pytest.raises(subprocess.CalledProcessError):
        bfg.push('/fs/data', '/fs/.bfg_snapshots/data_2026-06-22_08-15-49_t',
                 '/r/data', PARENT='/fs/.bfg_snapshots/data_2026-06-21_08-15-49_t')


def test_transfer_snapshot_skips_on_series_busy(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    bfg.local_send = MagicMock(
        side_effect=subprocess.CalledProcessError(btrfsgit.SERIES_BUSY_EXIT, 'send|receive'))

    res = bfg.transfer_snapshot('/fs/.bfg_snapshots/data_2026-06-22_08-15-49_t',
                                '/r/.bfg_snapshots/data',
                                PARENT='/fs/.bfg_snapshots/data_2026-06-21_08-15-49_t')
    assert res.val is None


def test_remote_send_returns_false_on_series_busy(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    bfg._sshstr = 'ssh fake@host'
    bfg._sudo = ['sudo']
    p1 = MagicMock()
    p2 = MagicMock()
    p2.returncode = btrfsgit.SERIES_BUSY_EXIT
    with patch('btrfsgit.btrfsgit.subprocess.Popen', side_effect=[p1, p2]):
        assert bfg.remote_send('/r/.bfg_snapshots/data_2026-06-22_08-15-49_t',
                               '/bac/.bfg_snapshots/data', None, []) is False
    p2.returncode = 0
    with patch('btrfsgit.btrfsgit.subprocess.Popen', side_effect=[p1, p2]):
        assert bfg.remote_send('/r/.bfg_snapshots/data_2026-06-22_08-15-49_t',
                               '/bac/.bfg_snapshots/data', None, []) is True


def test_parse_size():
    assert btrfsgit.parse_size(12345) == 12345
    assert btrfsgit.parse_size('500G') == 500 * 1024**3
    assert btrfsgit.parse_size('1.5T') == int(1.5 * 1024**4)
    assert btrfsgit.parse_size('100MiB') == 100 * 1024**2
    assert btrfsgit.parse_size('2TB') == 2 * 1024**4
    assert btrfsgit.parse_size('777') == 777
    with pytest.raises(ValueError):
        btrfsgit.parse_size('lots')


def test_clean_fs_min_free_stops_at_target(mock_bfg_for_pruning):
    """min-free mode deletes oldest-first across all series, skips protected and
    series-newest, syncs after each delete, and stops once the goal is met."""
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    a1 = _member('/bac/.bfg_snapshots/a/a_1', 'a1', 'oa1', now - timedelta(days=30))
    a2 = _member('/bac/.bfg_snapshots/a/a_2', 'a2', 'oa2', now - timedelta(days=20))
    a3 = _member('/bac/.bfg_snapshots/a/a_3', 'a3', 'oa3', now - timedelta(days=1))
    b1 = _member('/bac/.bfg_snapshots/b/b_1', 'b1', 'ob1', now - timedelta(days=25))
    b2 = _member('/bac/.bfg_snapshots/b/b_2', 'b2', 'ob2', now - timedelta(days=15))
    b3 = _member('/bac/.bfg_snapshots/b/b_3', 'b3', 'ob3', now - timedelta(days=2))
    series = [
        (('/bac/.bfg_snapshots/a', 'a'), [a1, a2, a3], {a2['path']: ['jj:/d2']}),
        (('/bac/.bfg_snapshots/b', 'b'), [b1, b2, b3], {}),
    ]
    bfg._snapshot_series = MagicMock(return_value=iter(series))
    # initial 10; blanket settle re-measure 10 (still below);
    # a1 top-check 10 -> delete, post-sync 40 -> settle-sleep;
    # b1 top-check 40 -> delete, post-sync 120 -> goal met, stop (no sleep); final 120
    bfg._free_bytes = MagicMock(side_effect=[10, 10, 10, 40, 40, 120, 120])

    with patch('btrfsgit.btrfsgit.time.sleep') as mock_sleep:
        bfg.clean_fs('/bac', DB=True, DRY_RUN=False, MIN_FREE=100)

    deleted = _delete_calls(bfg._local_cmd)
    # oldest-first across series: a1 (30d), b1 (25d); a2 protected, b2 not reached,
    # a3/b3 newest-of-series never candidates
    assert deleted == [str(a1['path']), str(b1['path'])], deleted
    assert bfg.marked_deleted == ['a1', 'b1']
    syncs = [c.args[0] for c in bfg._local_cmd.call_args_list
             if [str(x) for x in c.args[0][:3]] == ['btrfs', 'subvolume', 'sync']]
    assert len(syncs) == 3  # blanket settle + one per delete
    # the settle-sleep safety net runs only when we intend to delete (more): the
    # blanket one, one after a1 (still below target), none after b1 (goal met)
    assert mock_sleep.call_args_list == [((60,),), ((60,),)]


def test_clean_fs_min_free_blanket_settle_can_satisfy_goal(mock_bfg_for_pruning):
    """pending deletions from the preceding prune may reach the goal on their own -
    then nothing at all is deleted."""
    bfg = mock_bfg_for_pruning
    bfg._snapshot_series = MagicMock(return_value=iter([]))
    # initial read under-reads (cleaner still working); after settling the goal is met
    bfg._free_bytes = MagicMock(side_effect=[10, 150])

    with patch('btrfsgit.btrfsgit.time.sleep') as mock_sleep:
        bfg.clean_fs('/bac', DB=True, DRY_RUN=False, MIN_FREE=100)

    assert _delete_calls(bfg._local_cmd) == []
    bfg._snapshot_series.assert_not_called()
    assert mock_sleep.call_args_list == [((60,),)]


def test_clean_fs_min_free_noop_when_enough_free(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    bfg._snapshot_series = MagicMock(return_value=iter([]))
    bfg._free_bytes = MagicMock(return_value=200)

    bfg.clean_fs('/bac', DB=True, DRY_RUN=False, MIN_FREE=100)

    assert _delete_calls(bfg._local_cmd) == []
    bfg._snapshot_series.assert_not_called()


def test_clean_fs_min_free_dry_run_deletes_nothing(mock_bfg_for_pruning):
    bfg = mock_bfg_for_pruning
    now = datetime.now()
    a1 = _member('/bac/.bfg_snapshots/a/a_1', 'a1', 'oa1', now - timedelta(days=30))
    a2 = _member('/bac/.bfg_snapshots/a/a_2', 'a2', 'oa2', now - timedelta(days=1))
    bfg._snapshot_series = MagicMock(return_value=iter([(('/bac/.bfg_snapshots/a', 'a'), [a1, a2], {})]))
    bfg._free_bytes = MagicMock(return_value=10)

    bfg.clean_fs('/bac', DB=True, DRY_RUN=True, MIN_FREE=100)

    assert _delete_calls(bfg._local_cmd) == []
