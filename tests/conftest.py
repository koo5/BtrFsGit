"""Pytest configuration for BtrFsGit tests."""

import os
import shlex
import subprocess
import time
import pytest
from pathlib import Path


@pytest.fixture(scope="session")
def test_images_dir():
    """Create and return the test images directory."""
    dir_path = Path("testing/images")
    dir_path.mkdir(parents=True, exist_ok=True)
    return dir_path


@pytest.fixture(scope="session")
def test_mounts_dir():
    """Create and return the test mounts directory."""
    dir_path = Path("testing/mounts")
    dir_path.mkdir(parents=True, exist_ok=True)
    return dir_path


@pytest.fixture(scope="session")
def btrfs_loopback_setup(test_images_dir, test_mounts_dir):
    """
    Set up btrfs loopback devices for testing.
    
    Creates two btrfs filesystems mounted at testing/mounts/btrfs1 and testing/mounts/btrfs2.
    """
    # Define image and mount paths
    images = [
        test_images_dir / "btrfs1.raw",
        test_images_dir / "btrfs2.raw"
    ]
    mount_points = [
        test_mounts_dir / "btrfs1",
        test_mounts_dir / "btrfs2"
    ]
    loop_devices = ["/dev/loop60", "/dev/loop61"]
    
    # Create mount points
    for mount_point in mount_points:
        mount_point.mkdir(parents=True, exist_ok=True)
    
    try:
        # Create disk images
        for img in images:
            if not img.exists():
                subprocess.run(
                    f"dd count=200 bs=1M if=/dev/zero of={img}",
                    shell=True, check=True
                )
        
        # Clean up any existing mounts and loop devices
        for mount_point in mount_points:
            subprocess.run(
                f"sudo umount -f {mount_point} 2>/dev/null || true",
                shell=True, check=False
            )
        
        # Detach any existing loop devices
        for loop_dev in loop_devices:
            subprocess.run(
                f"sudo losetup -d {loop_dev} 2>/dev/null || true",
                shell=True, check=False
            )
        
        # Set up loop devices
        for i, (loop_dev, img) in enumerate(zip(loop_devices, images)):
            result = subprocess.run(
                f"sudo losetup -P {loop_dev} {img}",
                shell=True, capture_output=True, text=True
            )
            if result.returncode != 0:
                print(f"losetup failed: {result.stderr}")
                raise subprocess.CalledProcessError(result.returncode, result.args, result.stdout, result.stderr)
            
            # Format as btrfs
            result = subprocess.run(
                f"sudo mkfs.btrfs -f {loop_dev}",
                shell=True, capture_output=True, text=True
            )
            if result.returncode != 0:
                print(f"mkfs.btrfs failed: {result.stderr}")
                raise subprocess.CalledProcessError(result.returncode, result.args, result.stdout, result.stderr)
            
            # Mount filesystem
            subprocess.run(
                f"sudo mount {loop_dev} {mount_points[i]}",
                shell=True, check=True
            )
            
            # Create .bfg/id5 to store id5 mount point
            bfg_dir = mount_points[i] / ".bfg"
            subprocess.run(
                f"sudo mkdir -p {bfg_dir}",
                shell=True, check=True
            )
            subprocess.run(
                f"echo {mount_points[i]} | sudo tee {bfg_dir}/id5",
                shell=True, check=True
            )
            
            # Set permissions so tests can access
            subprocess.run(
                f"sudo chown -R $(id -u):$(id -g) {mount_points[i]}",
                shell=True, check=True
            )
        
        # Return the mount points for use in tests
        yield {
            "fs1": mount_points[0],
            "fs2": mount_points[1]
        }
    
    finally:
        # Cleanup after tests
        for mount_point in mount_points:
            subprocess.run(
                f"sudo umount -f {mount_point} 2>/dev/null || true",
                shell=True, check=False
            )
        
        for loop_dev in loop_devices:
            subprocess.run(
                f"sudo losetup -d {loop_dev} 2>/dev/null || true",
                shell=True, check=False
            )


@pytest.fixture
def btrfs_test_subvol(btrfs_loopback_setup):
    """Create a test subvolume on fs1 for testing."""
    fs1 = btrfs_loopback_setup["fs1"]
    subvol_path = fs1 / "test_subvol"
    
    # Create the subvolume
    subprocess.run(
        f"sudo btrfs subvolume create {subvol_path}",
        shell=True, check=True
    )
    
    # Create a test file inside
    test_file = subvol_path / "testfile.txt"
    subprocess.run(
        f"echo 'test content' | sudo tee {test_file}",
        shell=True, check=True
    )
    
    # Set permissions
    subprocess.run(
        f"sudo chown -R $(id -u):$(id -g) {subvol_path}",
        shell=True, check=True
    )
    
    yield subvol_path
    
    # Clean up the subvolume
    subprocess.run(
        f"sudo btrfs subvolume delete {subvol_path}",
        shell=True, check=False
    )


@pytest.fixture
def mock_db(monkeypatch):
    """Set up a mock database session for testing."""
    import btrfsgit.db as db
    from sqlalchemy import create_engine
    from sqlalchemy.orm import Session
    
    # Create in-memory SQLite database for testing
    test_engine = create_engine("sqlite:///:memory:")
    db.Base.metadata.create_all(test_engine)
    
    def mock_get_engine():
        return test_engine
    
    def mock_session():
        return Session(test_engine)
    
    # Replace the real functions with mock ones
    monkeypatch.setattr(db, "get_engine", mock_get_engine)
    monkeypatch.setattr(db, "session", mock_session)
    
    return test_engine