# BtrfsGit Test Suite

This directory contains tests for the BtrfsGit package.

## Test Structure

- `test_basic_commands.sh`: Tests the basic functionality of BtrfsGit commands
- `test1.sh`: Original functional test script
- `test_bfg.py`: Python unit tests for the BtrfsGit package
- `test_bfg_class.py`: Python unit tests specifically for the Bfg class
- `run_all_tests.sh`: Script to run all tests in sequence
- `negative/test2.sh`: Test for a specific corner case (expected to fail)

## Running Tests

### Running All Tests

```bash
./tests/run_all_tests.sh
```

### Running Individual Tests

```bash
# Run basic command tests
./tests/test_basic_commands.sh

# Run original test1
./tests/test1.sh

# Run Python unit tests
python -m pytest tests/test_bfg.py
```

### Running with Tox

```bash
tox
```

## Test Environment

The tests require:

1. Root/sudo access for btrfs operations
2. Loop devices (loop60 and loop61) to be available
3. Python environment with all dependencies installed

The test scripts automatically:
- Create test BTRFS filesystems in loop devices
- Mount them in testing/mounts/
- Run the commands against these filesystems
- Clean up after themselves