#!/usr/bin/env python
"""Unit tests for the btrfsgit.Bfg class."""

import unittest
from pathlib import Path
import tempfile
import shutil
import subprocess
from btrfsgit.btrfsgit import Bfg, Res

class TestBfgClass(unittest.TestCase):
    """Test the Bfg class functionality."""

    def setUp(self):
        """Set up test environment."""
        self.bfg = Bfg(YES=True)
        
    def test_instance_creation(self):
        """Test that Bfg can be instantiated."""
        self.assertIsInstance(self.bfg, Bfg)
        
    def test_result_class(self):
        """Test the Res helper class."""
        result = Res("test value")
        self.assertEqual(result.val, "test value")
        
    def test_required_methods_exist(self):
        """Test that required methods exist on the Bfg class."""
        # Core operations
        self.assertTrue(hasattr(self.bfg, "local_commit"))
        self.assertTrue(hasattr(self.bfg, "remote_commit"))
        self.assertTrue(hasattr(self.bfg, "push"))
        self.assertTrue(hasattr(self.bfg, "pull"))
        self.assertTrue(hasattr(self.bfg, "checkout_local"))
        self.assertTrue(hasattr(self.bfg, "checkout_remote"))
        self.assertTrue(hasattr(self.bfg, "update_db"))
        self.assertTrue(hasattr(self.bfg, "prune_local"))
        self.assertTrue(hasattr(self.bfg, "prune_remote"))
        
        # Compound operations
        self.assertTrue(hasattr(self.bfg, "commit_and_push"))
        self.assertTrue(hasattr(self.bfg, "commit_and_push_and_checkout"))
        self.assertTrue(hasattr(self.bfg, "remote_commit_and_pull"))
        self.assertTrue(hasattr(self.bfg, "commit_and_generate_patch"))
        
        # Helper methods
        self.assertTrue(hasattr(self.bfg, "get_local_snapshots"))
        self.assertTrue(hasattr(self.bfg, "get_local_bfg_snapshots"))
        self.assertTrue(hasattr(self.bfg, "get_all_subvols_on_filesystem"))
        
    def test_res_json_output(self):
        """Test that Res produces JSON output."""
        test_value = "test"
        result = Res(test_value)
        self.assertEqual(str(result), '{"result": "test"}')
        self.assertEqual(repr(result), '{"result": "test"}')

if __name__ == "__main__":
    unittest.main()