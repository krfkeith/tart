#/bin/bash

PRGNAME=${1%.tart}

TARTC=tools/tartc/tartc
STDLIB=/home/talin/Projects/tart/trunk/lib/std
SRCPATH=$PWD

$TARTC -nogc -g -i $STDLIB -noreflect -sourcepath $SRCPATH $PRGNAME.tart &&\
  llvm-dis -o - $PRGNAME.bc \
  | sed "s/%tart.core.TypeInfoBlock\* bitcast ([^)]\+)/%tart.core.TypeInfoBlock* null/g" \
  > $PRGNAME.ll
