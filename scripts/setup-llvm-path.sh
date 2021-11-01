#!/bin/bash

## Usage:
#    Run this script by:
#            source ./setup-llvm-path.sh /path/to/llvm/dir
#    to update the LLVM path settings ($PATH, $LIBRARY_PATH, $LD_LIBRARY_PATH)

## Some supporting functions

check_array_contains_element () {
    local e match="$1"
    shift
    for e; do
        if [[ "$e" == "$match" ]]; then
            echo "1"
            return 1
        fi
    done
    echo "0"
    return 0
}

filter_path_var_by_pattern() {
    local path_var="${1}"
    local pattern="${2}"
    local path_dirs=$(echo "$path_var" | tr ':' '\n')
    local new_path_dirs=()

    for dir in $path_dirs; do
        if [[ "$dir" == *"$pattern"* ]]; then
            continue
        fi

        local contained=$( check_array_contains_element $new_path_dirs $dir )
        if [[ $contained == "0" ]]; then
            new_path_dirs+=("$dir")
        fi
    done

    local new_path=""
    for dir in ${new_path_dirs[@]}; do
        new_path="$new_path:$dir"
    done

    echo "$new_path"
}

## Get LLVM path from argument

LLVM_PATH=$1

if [ -d "$LLVM_PATH" ]; then
    LLVM_PATH=$(cd $LLVM_PATH; pwd)
    echo "Input LLVM path: $LLVM_PATH"
else
    echo "Input path does not exist: $LLVM_PATH"
    exit 1
fi

echo ""
echo "Updating new LLVM path to \$PATH, \$LIBRARY_PATH, and \$LD_LIBRARY_PATH..."

# Filter existing LLVM settings from some path variables

PATH=$( filter_path_var_by_pattern $PATH "llvm")
LIBRARY_PATH=$( filter_path_var_by_pattern $LIBRARY_PATH "llvm")
LD_LIBRARY_PATH=$( filter_path_var_by_pattern $LD_LIBRARY_PATH "llvm")


# Update new LLVM PATH

PATH="$LLVM_PATH/bin$PATH"
LIBRARY_PATH="$LLVM_PATH/lib$LIBRARY_PATH"
LD_LIBRARY_PATH="$LLVM_PATH/lib$LD_LIBRARY_PATH"

echo ""
echo "Finish updating paths!"
echo ""
echo "\$PATH = $PATH"
echo ""
echo "\$LIBRARY_PATH = $LIBRARY_PATH"
echo ""
echo "\$LD_LIBRARY_PATH = $LD_LIBRARY_PATH"
