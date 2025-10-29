#!/bin/bash

# Check if an argument was provided
if [ -z "$1" ]; then
  echo "Usage: bash run.sh <TargetName>"
  exit 1
fi

target=$1

# Build and run path
exe_path="build/$target/$target"

# Check if the executable exists before running
if [ ! -f "$exe_path" ]; then
  echo "Error: Executable not found at $exe_path"
  exit 1
fi

# Run it
echo "Running $exe_path..."
"$exe_path"
