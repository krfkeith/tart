#/bin/bash

SCRIPTDIR=`dirname $0`

PRGNAME=${1%.tart}

TARTC=tools/tartc/tartc
STDLIB=`pwd`/$(dirname $(dirname $0))/lib/std
SRCPATH=$PWD

$TARTC -nogc -g -i $STDLIB -noreflect -sourcepath $SRCPATH $PRGNAME.tart &&\
  llvm-dis -o - $PRGNAME.bc \
  | sed "s/%tart.core.TypeInfoBlock\* bitcast ([^)]\+)/%tart.core.TypeInfoBlock* null/g" \
  | sed "s/external constant %0/constant %0 zeroinitializer/g" \
  > $PRGNAME.ll

