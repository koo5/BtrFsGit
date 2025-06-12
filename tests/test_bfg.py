#!/usr/bin/env python
"""Tests for `btrfsgit` package."""
# pylint: disable=redefined-outer-name

import os
import re
import pytest
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock
from datetime import datetime, timedelta

from btrfsgit import btrfsgit


@pytest.fixture
def bfg_instance():
    """Create a BtrFsGit instance for testing."""
    return btrfsgit.Bfg(YES=True)


def test_bfg_initialization(bfg_instance):
    """Test that Bfg class initializes correctly."""
    assert bfg_instance is not None
    assert bfg_instance._yes_was_given_on_command_line is True
    assert bfg_instance._sshstr == ''
    assert bfg_instance._remote_str == '(here)'
    assert bfg_instance._local_str == '(here)'
    assert isinstance(bfg_instance.host, str)
    assert len(bfg_instance.host) > 0


def test_datetime_to_json():
    """Test the datetime_to_json function."""
    now = datetime.now()
    assert btrfsgit.datetime_to_json(now) == now.isoformat()
    
    # Test with Path object
    path = Path("/some/path")
    assert btrfsgit.datetime_to_json(path) == "/some/path"
    
    # Test with unsupported type
    with pytest.raises(TypeError):
        btrfsgit.datetime_to_json(set())


def test_dash_is_none():
    """Test the dash_is_none function."""
    assert btrfsgit.dash_is_none('-') is None
    assert btrfsgit.dash_is_none('something') == 'something'


def test_try_unlink(tmp_path):
    """Test the try_unlink function."""
    # Create a test file
    test_file = tmp_path / "test_file.txt"
    test_file.write_text("test content")
    
    # Unlink existing file
    btrfsgit.try_unlink(test_file)
    assert not test_file.exists()
    
    # Unlink non-existent file (should not raise exception)
    btrfsgit.try_unlink(test_file)


def test_res_class():
    """Test the Res helper class."""
    # Test with simple value
    res = btrfsgit.Res("test")
    assert res.val == "test"
    assert str(res) == '{"result": "test"}'
    assert repr(res) == '{"result": "test"}'
    
    # Test with complex value
    complex_val = {"key": "value", "number": 123}
    res = btrfsgit.Res(complex_val)
    assert res.val == complex_val
    assert str(res) == '{"result": {"key": "value", "number": 123}}'


def test_prompt_with_default_yes(monkeypatch):
    """Test the prompt function with default 'yes'."""
    # Mock input to return empty string (default)
    monkeypatch.setattr('builtins.input', lambda: '')
    assert btrfsgit.prompt("Test question") is True
    
    # Mock input to return 'y'
    monkeypatch.setattr('builtins.input', lambda: 'y')
    assert btrfsgit.prompt("Test question") is True
    
    # Mock input to return 'n'
    monkeypatch.setattr('builtins.input', lambda: 'n')
    assert btrfsgit.prompt("Test question") is False
    
    # Test with dry_run
    assert btrfsgit.prompt("Test question", dry_run=True) is False


@patch('subprocess.check_output')
def test_remote_cmd(mock_check_output, bfg_instance):
    """Test the _remote_cmd method with a remote connection."""
    # Set up a mock SSH string
    bfg_instance._sshstr = 'ssh user@remote'
    
    # Mock the subprocess call
    mock_check_output.return_value = "command output"
    
    # Test with string command
    result = bfg_instance._remote_cmd("test command")
    assert mock_check_output.called
    assert result == "command output"
    
    # Test with list command
    bfg_instance._remote_cmd(["test", "command"])
    mock_check_output.assert_called()


@patch('subprocess.check_output')
def test_local_cmd(mock_check_output, bfg_instance):
    """Test the _local_cmd method."""
    # Mock the subprocess call
    mock_check_output.return_value = "command output"
    
    # Test with string command
    result = bfg_instance._local_cmd("test command")
    assert mock_check_output.called
    assert result == "command output"
    
    # Test with list command
    bfg_instance._local_cmd(["test", "command"])
    mock_check_output.assert_called()


def test_parse_snapshot_name(bfg_instance):
    """Test the parse_snapshot_name method."""
    # Test valid snapshot name
    snapshot_name = "data_2023-05-13_14-30-45_from_host"
    result = bfg_instance.parse_snapshot_name(snapshot_name)
    
    assert result["name"] == "data"
    assert result["tags"] == "from_host"
    assert isinstance(result["dt"], datetime)
    assert result["dt"].year == 2023
    assert result["dt"].month == 5
    assert result["dt"].day == 13
    assert result["dt"].hour == 14
    assert result["dt"].minute == 30
    assert result["dt"].second == 45
    
    # Test invalid snapshot name
    with pytest.raises(ValueError):
        bfg_instance.parse_snapshot_name("invalid_snapshot_name")


def test_bucket(bfg_instance):
    """Test the bucket method."""
    now = datetime.now()
    
    # Test under 1 minute
    dt = now - timedelta(seconds=30)
    assert bfg_instance.bucket(dt, now) == "under-1-min"
    
    # Test under 1 hour
    dt = now - timedelta(minutes=30)
    assert bfg_instance.bucket(dt, now).startswith("minute-")
    
    # Test under 1 day
    dt = now - timedelta(hours=12)
    assert bfg_instance.bucket(dt, now).startswith("hour-")
    
    # Test under 30 days
    dt = now - timedelta(days=15)
    assert bfg_instance.bucket(dt, now).startswith("day-")
    
    # Test over 30 days
    dt = now - timedelta(days=60)
    assert bfg_instance.bucket(dt, now).startswith("month-")


# Test with mocked btrfs commands
@patch('subprocess.check_output')
def test_get_subvol(mock_check_output, bfg_instance):
    """Test the get_subvol method."""
    # Mock the btrfs subvolume show output - format must match expected parsing indices
    mock_output = (
        "line0\n"                                                       # line 0
        "line1\n"                                                       # line 1  
        "UUID: \t12345678-1234-1234-1234-123456789abc\n"               # line 2: split()[1] = uuid
        "Parent UUID: \t87654321-4321-4321-4321-cba987654321\n"        # line 3: split()[2] = parent_uuid
        "Received UUID: \t-\n"                                         # line 4: split()[2] = received_uuid
        "line5\n"                                                      # line 5
        "Subvolume ID: \t123\n"                                       # line 6: split()[2] = subvol_id
        "line7\n"                                                      # line 7
        "line8\n"                                                      # line 8
        "line9\n"                                                      # line 9
        "line10\n"                                                     # line 10
        "Flags: \treadonly\n"                                         # line 11: split()[1] = ro flag
    )
    mock_check_output.return_value = mock_output
    
    # Define a mock runner function
    mock_runner = lambda cmd: mock_output
    
    # Test the get_subvol method
    result = bfg_instance.get_subvol(mock_runner, "/test/path").val
    
    assert result["local_uuid"] == "12345678-1234-1234-1234-123456789abc"
    assert result["parent_uuid"] == "87654321-4321-4321-4321-cba987654321"
    assert result["received_uuid"] is None  # Dash is converted to None
    assert result["subvol_id"] == 123
    assert result["ro"] is True


@patch('btrfsgit.btrfsgit.Bfg._local_cmd')
def test_local_fs_id5_mount_point(mock_local_cmd, bfg_instance, tmp_path):
    """Test the local_fs_id5_mount_point and find_local_fs_id5_mount_point methods."""
    # Create temporary directory structure with .bfg/id5
    subvol_path = tmp_path / "subvol"
    bfg_dir = subvol_path / ".bfg"
    bfg_dir.mkdir(parents=True)
    id5_file = bfg_dir / "id5"
    id5_file.write_text("/mnt/test_fs")
    
    # Mock the behavior of find_local_fs_id5_mount_point
    def side_effect(subvol):
        if ".bfg/id5" in str(subvol):
            with open(id5_file) as f:
                return Path(f.read().strip())
        return None
    
    # Test the local_fs_id5_mount_point method
    with patch('pathlib.Path.open', side_effect=lambda mode: open(id5_file, mode)):
        result = bfg_instance.find_local_fs_id5_mount_point(str(subvol_path))
        assert str(result) == "/mnt/test_fs"
