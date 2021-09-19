#!/bin/sh

SCRIPTDIR=$(dirname "$0")
find $SCRIPTDIR/../src -name '*.ml' | xargs wc -l

find $SCRIPTDIR/../llvm-normalizer/src -regex '.*\.cpp\|.*\.h' | xargs wc -l
