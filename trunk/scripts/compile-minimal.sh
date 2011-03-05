#/bin/bash

PRGNAME=${1%.tart}

TARTC=tools/tartc/tartc
STDLIB=/home/talin/Projects/tart/trunk/lib/std
SRCPATH=$PWD

$TARTC -nogc -g -i $STDLIB -noreflect -sourcepath $SRCPATH $PRGNAME.tart &&\
  llvm-dis -o $PRGNAME.ll $PRGNAME.bc
