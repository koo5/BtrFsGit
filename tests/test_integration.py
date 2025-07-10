#!/usr/bin/env python
"""Integration tests for BtrFsGit."""

import os
import pytest
import subprocess
import time
from pathlib import Path
from unittest.mock import patch

from btrfsgit import btrfsgit


@pytest.mark.integration
class TestBtrFsGitIntegration:
    """Integration tests for BtrFsGit that require actual btrfs filesystems."""
    
    @pytest.fixture(autouse=True)
    def setup_btrfs_env(self, btrfs_loopback_setup, btrfs_test_subvol):
        """Set up the test environment with two btrfs filesystems and a test subvolume."""
        self.fs1 = btrfs_loopback_setup["fs1"]
        self.fs2 = btrfs_loopback_setup["fs2"]
        self.test_subvol = btrfs_test_subvol
        
        # Create .bfg directory in fs2 to simulate a remote filesystem
        self.fs2_bfg_dir = self.fs2 / ".bfg"
        if not self.fs2_bfg_dir.exists():
            subprocess.run(
                f"sudo mkdir -p {self.fs2_bfg_dir}",
                shell=True, check=True
            )
            # Create id5 file
            subprocess.run(
                f"echo {self.fs2} | sudo tee {self.fs2_bfg_dir}/id5",
                shell=True, check=True
            )
        
        # Create a bfg instance for testing
        self.bfg = btrfsgit.Bfg(YES=True)
    
    def test_local_commit(self):
        """Test creating a local snapshot (commit)."""
        # Create a test file in the subvolume
        test_file = self.test_subvol / "test_file.txt"
        with open(test_file, "w") as f:
            f.write("Test content for local commit")
        
        # Commit the subvolume
        tag = "test_commit"
        result = self.bfg.local_commit(
            SUBVOL=str(self.test_subvol),
            TAG=tag
        ).val
        
        # Verify the snapshot was created
        assert Path(result).exists()
        
        # Verify the snapshot has the right tag
        assert tag in str(result)
        
        # Verify the snapshot is read-only
        # Get subvol info
        output = subprocess.check_output(
            f"sudo btrfs subvolume show {result}",
            shell=True, text=True
        )
        assert "readonly" in output
        
        # Verify content was captured properly
        # Create a temporary directory to mount the snapshot
        mount_dir = self.fs1 / "temp_mount"
        if not mount_dir.exists():
            os.makedirs(mount_dir)
        
        try:
            # Snapshot the read-only snapshot to create a writable one for testing
            temp_subvol = self.fs1 / "temp_subvol"
            subprocess.run(
                f"sudo btrfs subvolume snapshot {result} {temp_subvol}",
                shell=True, check=True
            )
            
            # Check that files in the original subvolume exist in the snapshot
            snapshot_file = temp_subvol / "test_file.txt"
            assert snapshot_file.exists()
            
            with open(snapshot_file, "r") as f:
                content = f.read()
            assert content == "Test content for local commit"
        
        finally:
            # Clean up
            if Path(temp_subvol).exists():
                subprocess.run(
                    f"sudo btrfs subvolume delete {temp_subvol}",
                    shell=True, check=False
                )
    
    def test_commit_and_push(self):
        """Test committing a subvolume and pushing it to another filesystem."""
        # Create a unique test file
        test_file = self.test_subvol / f"test_push_{time.time()}.txt"
        with open(test_file, "w") as f:
            f.write("Test content for push operation")
        
        # Create a target directory on fs2
        target_subvol = self.fs2 / "target_subvol"
        if not target_subvol.exists():
            subprocess.run(
                f"sudo btrfs subvolume create {target_subvol}",
                shell=True, check=True
            )
            # Set permissions
            subprocess.run(
                f"sudo chown -R $(id -u):$(id -g) {target_subvol}",
                shell=True, check=True
            )
        
        # Commit and push
        result = self.bfg.commit_and_push(
            SUBVOL=str(self.test_subvol),
            REMOTE_SUBVOL=str(target_subvol),
            SNAPSHOT_TAG="test_push"
        ).val
        
        # Verify the snapshot was created on the remote filesystem
        assert Path(result).exists()
        
        # Verify the snapshot has the right tag
        assert "test_push" in str(result)
        
        # Checkout the snapshot to verify content
        checkout_path = self.fs2 / "checkout_test"
        if checkout_path.exists():
            subprocess.run(
                f"sudo btrfs subvolume delete {checkout_path}",
                shell=True, check=False
            )
        
        # Create a snapshot of the received snapshot
        subprocess.run(
            f"sudo btrfs subvolume snapshot {result} {checkout_path}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {checkout_path}",
            shell=True, check=True
        )
        
        # Verify the content matches
        pushed_file = checkout_path / test_file.name
        assert pushed_file.exists()
        
        with open(pushed_file, "r") as f:
            content = f.read()
        assert content == "Test content for push operation"
        
        # Clean up
        subprocess.run(
            f"sudo btrfs subvolume delete {checkout_path}",
            shell=True, check=False
        )
    
    def test_commit_and_push_and_checkout(self):
        """Test the complete workflow of commit, push, and checkout."""
        # Create unique test data
        timestamp = time.time()
        test_file = self.test_subvol / f"checkout_test_{timestamp}.txt"
        with open(test_file, "w") as f:
            f.write(f"Test content for checkout operation {timestamp}")
        
        # Create a target directory on fs2 that we'll checkout to
        checkout_target = self.fs2 / "checkout_target"
        if checkout_target.exists():
            subprocess.run(
                f"sudo btrfs subvolume delete {checkout_target}",
                shell=True, check=False
            )
        
        # Call commit_and_push_and_checkout
        result = self.bfg.commit_and_push_and_checkout(
            SUBVOL=str(self.test_subvol),
            REMOTE_SUBVOL=str(checkout_target)
        ).val
        
        # Verify the checkout was created
        assert Path(result).exists()
        assert Path(result) == checkout_target
        
        # Verify permissions (should be writable)
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {checkout_target}",
            shell=True, check=True
        )
        
        # Verify the content matches
        checkout_file = checkout_target / test_file.name
        assert checkout_file.exists()
        
        with open(checkout_file, "r") as f:
            content = f.read()
        assert f"Test content for checkout operation {timestamp}" in content
    
    def test_incremental_push(self):
        """Test incremental push from a common parent."""
        # First push to establish a common parent
        initial_file = self.test_subvol / "initial.txt"
        with open(initial_file, "w") as f:
            f.write("Initial content")
        
        target_subvol = self.fs2 / "incremental_target"
        if target_subvol.exists():
            subprocess.run(
                f"sudo btrfs subvolume delete {target_subvol}",
                shell=True, check=False
            )
        
        # First commit and push
        first_push = self.bfg.commit_and_push(
            SUBVOL=str(self.test_subvol),
            REMOTE_SUBVOL=str(target_subvol),
            SNAPSHOT_TAG="first_push"
        ).val
        
        # Add new content to the source
        second_file = self.test_subvol / "second.txt"
        with open(second_file, "w") as f:
            f.write("Second push content")
        
        # Second commit and push - should be incremental
        second_push = self.bfg.commit_and_push(
            SUBVOL=str(self.test_subvol),
            REMOTE_SUBVOL=str(target_subvol),
            SNAPSHOT_TAG="second_push"
        ).val
        
        # Verify the second snapshot was created
        assert Path(second_push).exists()
        assert "second_push" in str(second_push)
        
        # Checkout the second snapshot to verify all content
        checkout_path = self.fs2 / "incremental_checkout"
        if checkout_path.exists():
            subprocess.run(
                f"sudo btrfs subvolume delete {checkout_path}",
                shell=True, check=False
            )
        
        # Create a snapshot of the received snapshot
        subprocess.run(
            f"sudo btrfs subvolume snapshot {second_push} {checkout_path}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {checkout_path}",
            shell=True, check=True
        )
        
        # Verify both files exist in the incremental snapshot
        assert (checkout_path / "initial.txt").exists()
        assert (checkout_path / "second.txt").exists()
        
        with open(checkout_path / "initial.txt", "r") as f:
            content = f.read()
        assert content == "Initial content"
        
        with open(checkout_path / "second.txt", "r") as f:
            content = f.read()
        assert content == "Second push content"


@pytest.mark.integration
def test_multiple_machines_with_db(btrfs_loopback_setup, mock_db):
    """
    Test using the database to track snapshots across multiple 'machines'
    (simulated with our two btrfs filesystems).
    """
    fs1 = btrfs_loopback_setup["fs1"]
    fs2 = btrfs_loopback_setup["fs2"]
    
    # Create test subvolumes on both filesystems
    subvol1 = fs1 / "db_test_subvol"
    if not subvol1.exists():
        subprocess.run(
            f"sudo btrfs subvolume create {subvol1}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {subvol1}",
            shell=True, check=True
        )
    
    subvol2 = fs2 / "db_test_subvol"
    if not subvol2.exists():
        subprocess.run(
            f"sudo btrfs subvolume create {subvol2}",
            shell=True, check=True
        )
        subprocess.run(
            f"sudo chown -R $(id -u):$(id -g) {subvol2}",
            shell=True, check=True
        )
    
    # Create initial content
    with open(subvol1 / "db_test.txt", "w") as f:
        f.write("Database test content")
    
    # Create a BFG instance
    bfg = btrfsgit.Bfg(YES=True)
    
    # Commit and push
    push_result = bfg.commit_and_push(
        SUBVOL=str(subvol1),
        REMOTE_SUBVOL=str(subvol2),
        SNAPSHOT_TAG="db_test"
    ).val
    
    # Update DB for both filesystems
    bfg.update_db(str(fs1))
    
    # Simulate second machine updating its DB
    with patch.object(bfg, 'host', 'second-machine'):
        bfg.update_db(str(fs2))
    
    # Get snapshots from DB
    all_snapshots = bfg.all_subvols_from_db()
    
    # Find snapshots related to our test
    db_test_snapshots = [s for s in all_snapshots if 'db_test' in str(s['path'])]
    
    # Should have at least two snapshots (one local, one remote)
    assert len(db_test_snapshots) >= 2
    
    # Find common parents
    common_parents = bfg.most_recent_common_snapshots(all_snapshots, str(subvol1))
    
    # We should have at least one common parent
    assert len(common_parents) >= 1