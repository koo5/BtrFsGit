#!/usr/bin/env python
"""Tests for BtrFsGit database interactions."""

import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock
from datetime import datetime

from btrfsgit import btrfsgit
from btrfsgit import db


@pytest.fixture
def mock_bfg_with_db(mock_db):
    """Create a BtrFsGit instance with a mocked database."""
    bfg = btrfsgit.Bfg(YES=True)
    
    # Mock filesystem/subvolume methods
    bfg._local_cmd = MagicMock(return_value="mock output")
    bfg._remote_cmd = MagicMock(return_value="mock remote output")
    bfg.local_fs_id5_mount_point = MagicMock(return_value=Path("/mnt/test_fs"))
    bfg.remote_fs_id5_mount_point = MagicMock(return_value=Path("/mnt/remote_fs"))
    bfg.local_fs_uuid = MagicMock(return_value="test-fs-uuid")
    bfg.remote_fs_uuid = MagicMock(return_value=("remote-fs-uuid", Path("/mnt/remote_fs")))
    
    # Pre-populate the database with test data
    with db.session() as session:
        # Add some test snapshots to the database
        snapshot1 = db.Snapshot(
            id="test-fs-uuid_uuid1",
            local_uuid="uuid1",
            parent_uuid="parent-uuid1",
            received_uuid=None,
            host="test-host",
            fs="/mnt/test_fs",
            path="/mnt/test_fs/.bfg_snapshots/data_2023-05-10_10-00-00_tag",
            fs_uuid="test-fs-uuid",
            subvol_id=101,
            ro=True,
            deleted=False
        )
        
        snapshot2 = db.Snapshot(
            id="test-fs-uuid_uuid2",
            local_uuid="uuid2",
            parent_uuid="uuid1",  # Child of snapshot1
            received_uuid=None,
            host="test-host",
            fs="/mnt/test_fs",
            path="/mnt/test_fs/.bfg_snapshots/data_2023-05-11_10-00-00_tag",
            fs_uuid="test-fs-uuid",
            subvol_id=102,
            ro=True,
            deleted=False
        )
        
        snapshot3 = db.Snapshot(
            id="remote-fs-uuid_uuid3",
            local_uuid="uuid3",
            parent_uuid=None,
            received_uuid="uuid1",  # Received from snapshot1
            host="remote-host",
            fs="/mnt/remote_fs",
            path="/mnt/remote_fs/.bfg_snapshots/data_2023-05-10_10-00-00_tag",
            fs_uuid="remote-fs-uuid",
            subvol_id=201,
            ro=True,
            deleted=False
        )
        
        session.add_all([snapshot1, snapshot2, snapshot3])
        session.commit()
    
    return bfg


def test_db_structure():
    """Test the database schema and model structure."""
    # Test Snapshot model attributes
    assert hasattr(db.Snapshot, 'id')
    assert hasattr(db.Snapshot, 'local_uuid')
    assert hasattr(db.Snapshot, 'parent_uuid')
    assert hasattr(db.Snapshot, 'received_uuid')
    assert hasattr(db.Snapshot, 'host')
    assert hasattr(db.Snapshot, 'fs')
    assert hasattr(db.Snapshot, 'path')
    assert hasattr(db.Snapshot, 'deleted')
    assert hasattr(db.Snapshot, 'subvol_id')
    assert hasattr(db.Snapshot, 'ro')


def test_update_db(mock_bfg_with_db, mock_db):
    """Test the update_db method."""
    # Mock get_all_subvols_on_filesystem to return test data
    mock_snapshots = [
        {
            'local_uuid': 'uuid4',
            'parent_uuid': 'uuid2',
            'received_uuid': None,
            'host': 'test-host',
            'path': Path('/mnt/test_fs/.bfg_snapshots/data_2023-05-12_10-00-00_tag'),
            'fs_uuid': 'test-fs-uuid',
            'subvol_id': 103,
            'ro': True,
            'deleted': False
        },
        {
            'local_uuid': 'uuid5',
            'parent_uuid': 'uuid4',
            'received_uuid': None,
            'host': 'test-host',
            'path': Path('/mnt/test_fs/.bfg_snapshots/data_2023-05-13_10-00-00_tag'),
            'fs_uuid': 'test-fs-uuid',
            'subvol_id': 104,
            'ro': True,
            'deleted': False
        }
    ]
    
    mock_bfg_with_db.get_all_subvols_on_filesystem = MagicMock(
        return_value=btrfsgit.Res(mock_snapshots)
    )
    
    # Call update_db
    mock_bfg_with_db.update_db("/mnt/test_fs")
    
    # Verify database state
    with db.session() as session:
        # Should have removed all test-fs-uuid snapshots and added the new ones
        snapshots = session.query(db.Snapshot).all()
        
        # We should only have the remote snapshot left (uuid3) plus the two new ones (uuid4, uuid5)
        assert len(snapshots) == 3
        
        # Verify the remote snapshot is still there
        remote_snap = session.query(db.Snapshot).filter(db.Snapshot.local_uuid == 'uuid3').first()
        assert remote_snap is not None
        assert remote_snap.fs_uuid == 'remote-fs-uuid'
        
        # Verify new snapshots were added
        new_snap1 = session.query(db.Snapshot).filter(db.Snapshot.local_uuid == 'uuid4').first()
        assert new_snap1 is not None
        assert new_snap1.parent_uuid == 'uuid2'
        
        new_snap2 = session.query(db.Snapshot).filter(db.Snapshot.local_uuid == 'uuid5').first()
        assert new_snap2 is not None
        assert new_snap2.parent_uuid == 'uuid4'


def test_all_subvols_from_db(mock_bfg_with_db, mock_db):
    """Test the all_subvols_from_db method."""
    # Call the method
    all_snapshots = mock_bfg_with_db.all_subvols_from_db()
    
    # Verify the result
    assert len(all_snapshots) == 3
    
    # Find snapshots by UUID
    snapshot1 = next(s for s in all_snapshots if s['local_uuid'] == 'uuid1')
    snapshot2 = next(s for s in all_snapshots if s['local_uuid'] == 'uuid2')
    snapshot3 = next(s for s in all_snapshots if s['local_uuid'] == 'uuid3')
    
    # Verify snapshot1 attributes
    assert snapshot1['parent_uuid'] == 'parent-uuid1'
    assert snapshot1['fs_uuid'] == 'test-fs-uuid'
    assert snapshot1['path'] == Path('/mnt/test_fs/.bfg_snapshots/data_2023-05-10_10-00-00_tag')
    assert 'dt' in snapshot1  # Should have calculated datetime
    
    # Verify snapshot2 attributes
    assert snapshot2['parent_uuid'] == 'uuid1'
    assert snapshot2['fs_uuid'] == 'test-fs-uuid'
    
    # Verify snapshot3 attributes
    assert snapshot3['received_uuid'] == 'uuid1'
    assert snapshot3['fs_uuid'] == 'remote-fs-uuid'


def test_remote_fs_uuids(mock_bfg_with_db, mock_db):
    """Test the remote_fs_uuids method."""
    # Get all snapshots from DB
    all_snapshots = mock_bfg_with_db.all_subvols_from_db()
    
    # Call the method
    result = mock_bfg_with_db.remote_fs_uuids(all_snapshots, '/mnt/test_fs')
    
    # Verify result
    assert len(result) == 1
    assert 'remote-fs-uuid' in result
    assert result['remote-fs-uuid']['hosts'] == {'remote-host'}


@patch.object(db, 'session')
def test_best_shared_parent(mock_session, mock_bfg_with_db):
    """Test the best_shared_parent method."""
    # Mock parent_candidates to return a list of candidate snapshots
    candidates = [
        {'local_uuid': 'uuid1', 'path': '/path/to/snap1', 'subvol_id': 101},
        {'local_uuid': 'uuid2', 'path': '/path/to/snap2', 'subvol_id': 102}
    ]
    
    mock_bfg_with_db._parent_candidates = MagicMock(return_value=candidates)
    
    # Call the method
    result = mock_bfg_with_db.best_shared_parent(
        '/mnt/test_fs/data',
        '/mnt/remote_fs/data',
        'test-uuid',
        'remote-fs-uuid'
    ).val
    
    # Verify result
    assert result == candidates[0]  # Should pick the first candidate
    
    # Test with no candidates
    mock_bfg_with_db._parent_candidates = MagicMock(return_value=[])
    
    result = mock_bfg_with_db.best_shared_parent(
        '/mnt/test_fs/data',
        '/mnt/remote_fs/data',
        'test-uuid',
        'remote-fs-uuid'
    ).val
    
    assert result is None  # Should return None when no candidates


def test_most_recent_common_snapshots(mock_bfg_with_db):
    """Test the most_recent_common_snapshots method."""
    # Mock required methods
    mock_bfg_with_db.get_subvol = MagicMock(
        return_value=btrfsgit.Res({'local_uuid': 'test-uuid'})
    )
    
    mock_bfg_with_db.remote_fs_uuids = MagicMock(
        return_value={'remote-fs-uuid': {'hosts': {'remote-host'}}}
    )
    
    mock_bfg_with_db.best_shared_parent = MagicMock(
        return_value=btrfsgit.Res({
            'local_uuid': 'uuid1',
            'path': '/mnt/test_fs/.bfg_snapshots/data_2023-05-10_10-00-00_tag'
        })
    )
    
    # Call the method
    result = mock_bfg_with_db.most_recent_common_snapshots(
        mock_bfg_with_db.all_subvols_from_db(),
        '/mnt/test_fs/data'
    )
    
    # Verify result
    assert len(result) == 1
    assert result[0]['local_uuid'] == 'uuid1'
    assert result[0]['path'] == Path('/mnt/test_fs/.bfg_snapshots/data_2023-05-10_10-00-00_tag')
    
    # Test with no result from best_shared_parent
    mock_bfg_with_db.best_shared_parent = MagicMock(
        return_value=btrfsgit.Res(None)
    )
    
    result = mock_bfg_with_db.most_recent_common_snapshots(
        mock_bfg_with_db.all_subvols_from_db(),
        '/mnt/test_fs/data'
    )
    
    assert len(result) == 0  # Should return empty list