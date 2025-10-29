#!/bin/bash

# check if a string was passed
if [ -z "$1" ]; then
  echo "Usage: bash compile.sh <target>"
  exit 1
fi

target=$1

mkdir -p build
cd build
cmake ..
make "$target"
