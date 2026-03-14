#!/usr/bin/env bash

# Run all BtrfsGit tests
# This script runs all test scripts in sequence

# Set flags
set -e  # Exit on error
set -o pipefail  # Exit if any command in a pipe fails

# Print header
echo "================================================"
echo "         Running BtrfsGit Test Suite            "
echo "================================================"

# Setup Python environment if needed
if [ -d "venv" ]; then
    source venv/bin/activate
    echo "Activated Python virtual environment"
fi

# Run basic command tests
echo -e "\n\n================================================"
echo "Running basic command tests..."
echo "================================================"
./tests/test_basic_commands.sh

# Run test1 original test
echo -e "\n\n================================================"
echo "Running original test1..."
echo "================================================"
./tests/test1.sh

# Print success message
echo -e "\n\n================================================"
echo "All tests completed successfully!"
echo "================================================"