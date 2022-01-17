#!/bin/bash

## Compile all Solidity smart contracts in a directory.

## Usage:
##    compile-solidity-dir.sh <input-dir>

BENCH_DIR=$1

if [ -z "$BENCH_DIR" ]; then
    echo "Please specify the input Solidity directory!"
    echo ""
    echo "Usage: "
    echo "  compile-solidity-dir.sh <input-dir>"
    exit 1;
fi

for FILE in $BENCH_DIR/*.sol; do
    echo "===================================="
    echo ""
    echo "Compiling $FILE.."
    echo ""
    CMD="solang --no-constant-folding --no-strength-reduce --no-dead-storage --no-vector-to-slice --no-cse --no-implicit-type-cast-check -O none --target ewasm $FILE"
    echo "$CMD"
    eval $CMD
    echo ""
done
