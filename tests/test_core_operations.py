#!/usr/bin/env python
"""Tests for BtrFsGit core operations."""

import os
import pytest
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock, call
from datetime import datetime, timedelta

from btrfsgit import btrfsgit


@pytest.fixture
def mock_bfg():
    """Create a heavily mocked BtrFsGit instance for testing core operations."""
    bfg = btrfsgit.Bfg(YES=True)
    
    # Mock various methods we don't want to actually call
    bfg._local_cmd = MagicMock(return_value="mock output")
    bfg._remote_cmd = MagicMock(return_value="mock remote output")
    bfg.local_fs_id5_mount_point = MagicMock(return_value=Path("/mnt/test_fs"))
    bfg.remote_fs_id5_mount_point = MagicMock(return_value=Path("/mnt/remote_fs"))
    bfg.local_fs_uuid = MagicMock(return_value="local-fs-uuid")
    bfg.remote_fs_uuid = MagicMock(return_value=("remote-fs-uuid", Path("/mnt/remote_fs")))
    
    return bfg


def test_calculate_default_snapshot_path(mock_bfg):
    """Test the calculate_default_snapshot_path method."""
    # Mock calculate_default_snapshot_parent_dir to return a known path
    mock_bfg.calculate_default_snapshot_parent_dir = MagicMock(
        return_value=btrfsgit.Res("/mnt/test_fs/.bfg_snapshots")
    )
    
    # Test with default parameters
    with patch('time.strftime', return_value="2023-05-13_14-30-45"):
        result = mock_bfg.calculate_default_snapshot_path(
            machine="local", 
            SUBVOL="/mnt/test_fs/data", 
            TAG=None
        ).val
        
        # Should return /mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_from_<hostname>
        assert "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_from_" in result
    
    # Test with custom TAG
    with patch('time.strftime', return_value="2023-05-13_14-30-45"):
        result = mock_bfg.calculate_default_snapshot_path(
            machine="local", 
            SUBVOL="/mnt/test_fs/data", 
            TAG="test_tag"
        ).val
        
        assert result == "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    
    # Test with NAME_OVERRIDE
    result = mock_bfg.calculate_default_snapshot_path(
        machine="local", 
        SUBVOL="/mnt/test_fs/data", 
        TAG=None,
        NAME_OVERRIDE="custom_name"
    ).val
    
    assert result == "/mnt/test_fs/.bfg_snapshots/data_custom_name"


def test_calculate_default_snapshot_parent_dir(mock_bfg):
    """Test the calculate_default_snapshot_parent_dir method."""
    # Mock response for test -e command to simulate existing directory
    mock_bfg._local_cmd.side_effect = lambda cmd, **kwargs: "exists" if cmd[0] == "test" else "cmd output"
    
    # Test with non-root subvolume (parent directory is part of the same filesystem)
    with patch('btrfsgit.btrfsgit.Bfg._local_cmd') as mock_cmd:
        # Configure mock to simulate cp --reflink success (same filesystem)
        mock_cmd.return_value = "success"
        mock_cmd.side_effect = None
        
        result = mock_bfg.calculate_default_snapshot_parent_dir("local", "/mnt/test_fs/data").val
        
        # Should return the parent directory + .bfg_snapshots
        assert result == "/mnt/test_fs/.bfg_snapshots"
    
    # Test with root subvolume (parent directory is NOT part of the same filesystem)
    with patch('btrfsgit.btrfsgit.Bfg._local_cmd') as mock_cmd:
        # Configure mock to simulate cp --reflink failure (different filesystem)
        mock_cmd.side_effect = lambda cmd, die_on_error=True, logger=None, capture_stderr=False: \
            -1 if cmd[0] == "cp" else "success"
        
        result = mock_bfg.calculate_default_snapshot_parent_dir("local", "/mnt/test_fs").val
        
        # Should return the subvolume directory + .bfg_snapshots
        assert result == "/mnt/test_fs/.bfg_snapshots"


@patch('btrfsgit.btrfsgit.prompt', return_value=True)
def test_local_commit(mock_prompt, mock_bfg):
    """Test the local_commit method."""
    # Mock _figure_out_snapshot_name to return a fixed path
    mock_bfg._figure_out_snapshot_name = MagicMock(
        return_value="/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    )
    
    # Mock _local_make_ro_snapshot 
    mock_bfg._local_make_ro_snapshot = MagicMock(
        return_value="/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    )
    
    # Test local_commit
    result = mock_bfg.local_commit(
        SUBVOL="/mnt/test_fs/data",
        TAG="test_tag"
    ).val
    
    # Verify result and method calls
    assert result == "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    mock_bfg._figure_out_snapshot_name.assert_called_once()
    mock_bfg._local_make_ro_snapshot.assert_called_once_with(
        Path("/mnt/test_fs/data").absolute(),
        "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    )


@patch('btrfsgit.btrfsgit.prompt', return_value=True)
def test_push(mock_prompt, mock_bfg):
    """Test the push method."""
    # Mock calculate_default_snapshot_parent_dir
    mock_bfg.calculate_default_snapshot_parent_dir = MagicMock(
        return_value=btrfsgit.Res("/mnt/remote_fs/.bfg_snapshots")
    )
    
    # Mock get_subvol
    mock_bfg.get_subvol = MagicMock(
        return_value=btrfsgit.Res({"local_uuid": "test-uuid"})
    )
    
    # Mock find_common_parent
    mock_bfg.find_common_parent = MagicMock(
        return_value=btrfsgit.Res({"abspath": "/mnt/test_fs/.bfg_snapshots/parent_snapshot"})
    )
    
    # Mock local_send
    mock_bfg.local_send = MagicMock()
    
    # Test push with automatic parent detection
    result = mock_bfg.push(
        SUBVOL="/mnt/test_fs/data",
        SNAPSHOT="/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag",
        REMOTE_SUBVOL="/mnt/remote_fs/data"
    ).val
    
    # Verify result and method calls
    assert result == "/mnt/remote_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    mock_bfg.calculate_default_snapshot_parent_dir.assert_called_once()
    mock_bfg.get_subvol.assert_called_once()
    mock_bfg.find_common_parent.assert_called_once()
    mock_bfg.local_send.assert_called_once()
    
    # Test push with explicit parent
    # Reset the individual mocks
    mock_bfg.calculate_default_snapshot_parent_dir.reset_mock()
    mock_bfg.get_subvol.reset_mock()
    mock_bfg.find_common_parent.reset_mock()
    mock_bfg.local_send.reset_mock()
    
    result = mock_bfg.push(
        SUBVOL="/mnt/test_fs/data",
        SNAPSHOT="/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag",
        REMOTE_SUBVOL="/mnt/remote_fs/data",
        PARENT="/mnt/test_fs/.bfg_snapshots/explicit_parent"
    ).val
    
    # Verify result and method calls
    assert result == "/mnt/remote_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    mock_bfg.calculate_default_snapshot_parent_dir.assert_called_once()
    mock_bfg.get_subvol.assert_not_called()  # Should not be called when PARENT is provided
    mock_bfg.find_common_parent.assert_not_called()  # Should not be called when PARENT is provided
    mock_bfg.local_send.assert_called_once()


def test_commit_and_push(mock_bfg):
    """Test the commit_and_push method."""
    # Mock local_commit
    mock_bfg.local_commit = MagicMock(
        return_value=btrfsgit.Res("/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag")
    )
    
    # Mock push
    mock_bfg.push = MagicMock(
        return_value=btrfsgit.Res("/mnt/remote_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag")
    )
    
    # Test commit_and_push
    result = mock_bfg.commit_and_push(
        SUBVOL="/mnt/test_fs/data",
        REMOTE_SUBVOL="/mnt/remote_fs/data",
        SNAPSHOT_TAG="test_tag"
    ).val
    
    # Verify result and method calls
    assert result == "/mnt/remote_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag"
    mock_bfg.local_commit.assert_called_once_with(
        "/mnt/test_fs/data", "test_tag", None, None
    )
    mock_bfg.push.assert_called_once_with(
        "/mnt/test_fs/data", 
        "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag",
        "/mnt/remote_fs/data", None, []
    )


@patch('subprocess.check_call')
def test_local_send(mock_check_call, mock_bfg):
    """Test the local_send method."""
    # Mock _parent_args to return a fixed list
    mock_bfg._parent_args = MagicMock(return_value=["-p", "/parent/snapshot"])
    
    # Test local_send with target as a pipe
    mock_bfg.local_send(
        SNAPSHOT="/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag",
        target=" | ssh user@remote sudo btrfs receive /mnt/remote_fs/.bfg_snapshots",
        PARENT="/mnt/test_fs/.bfg_snapshots/parent_snapshot"
    )
    
    # Verify method calls
    mock_bfg._parent_args.assert_called_once_with(
        "/mnt/test_fs/.bfg_snapshots/parent_snapshot", []
    )
    mock_check_call.assert_called_once()
    
    # Get the command that was executed
    cmd = mock_check_call.call_args[0][0]
    assert "sudo btrfs send -p /parent/snapshot " in cmd
    assert "/mnt/test_fs/.bfg_snapshots/data_2023-05-13_14-30-45_test_tag" in cmd
    assert " | ssh user@remote sudo btrfs receive /mnt/remote_fs/.bfg_snapshots" in cmd


def test_put_snapshots_into_buckets(mock_bfg):
    """Test the put_snapshots_into_buckets method."""
    # Create mock snapshots with different timestamps
    now = datetime.now()
    
    # Create mock snapshots
    snapshots = [
        {"path": "/s1", "dt": now},  # Under 1 minute
        {"path": "/s2", "dt": now - timedelta(seconds=30)},  # Under 1 minute
        {"path": "/s3", "dt": now - timedelta(minutes=5)},  # Under 1 hour
        {"path": "/s4", "dt": now - timedelta(minutes=10)},  # Under 1 hour
        {"path": "/s5", "dt": now - timedelta(hours=2)},  # Under 1 day
        {"path": "/s6", "dt": now - timedelta(hours=4)},  # Under 1 day
        {"path": "/s7", "dt": now - timedelta(days=5)},  # Under 30 days
        {"path": "/s8", "dt": now - timedelta(days=10)},  # Under 30 days
        {"path": "/s9", "dt": now - timedelta(days=60)},  # Over 30 days
        {"path": "/s10", "dt": now - timedelta(days=90)}  # Over 30 days
    ]
    
    # Mock the bucket method to use specific bucket names for testing
    with patch('datetime.datetime.now', return_value=now):
        with patch.object(mock_bfg, 'bucket') as mock_bucket:
            # Set up bucket return values
            mock_bucket.side_effect = lambda dt, now: (
                "under-1-min" if (now - dt).total_seconds() < 60 else
                "minute" if (now - dt).total_seconds() < 3600 else
                "hour" if (now - dt).total_seconds() < 86400 else
                "day" if (now - dt).total_seconds() < 2592000 else
                "month"
            )
            
            # Call the method
            buckets = mock_bfg.put_snapshots_into_buckets(snapshots)
            
            # Verify results
            assert len(buckets) == 5  # Should have 5 buckets
            assert len(buckets["under-1-min"]) == 2
            assert len(buckets["minute"]) == 2
            assert len(buckets["hour"]) == 2
            assert len(buckets["day"]) == 2
            assert len(buckets["month"]) == 2
            
            # Verify snapshots are sorted within buckets
            assert buckets["minute"][0]["path"] == "/s4"  # Older one first
            assert buckets["minute"][1]["path"] == "/s3"  # Newer one second