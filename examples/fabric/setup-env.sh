#!/bin/sh

# Run `source setup-env.sh` to update environment variables.

export GOPATH=$HOME/workspace/gollvm

export PATH=$GOPATH/bin:$PATH

export LD_LIBRARY_PATH=$GOPATH/lib64:$LD_LIBRARY_PATH
