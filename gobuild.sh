#!/bin/sh
#This script creates LLVM bitcode file from Go file
SOURCE_NAME=$1
BITCODE_NAME=$2
GOLLVM=$3
GOBUILD_OUTPUT=$4
eval "$GOLLVM build -a -x -work $SOURCE_NAME 1>$GOBUILD_OUTPUT 2>&1"
WORK_LINE=$(egrep '^WORK=' $GOBUILD_OUTPUT)
eval "$WORK_LINE"

GOC_LINE=$(egrep $SOURCE_NAME $GOBUILD_OUTPUT)
CMD=""
flag="OFF"

for word in $GOC_LINE
do
  if [ "$flag" = "OFF" ];
  then
    CMD="${CMD}$word "
    if [ "$word" = "-o" ];
    then
      flag="ON"
    fi
  else
    CMD="${CMD}$BITCODE_NAME ";
    flag="OFF"
  fi
done

CMD="${CMD} -emit-llvm"

eval "$CMD"
