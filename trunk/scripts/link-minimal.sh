#/bin/bash

PRGNAME=${1%.*}

llc -O0 -disable-fp-elim -filetype=obj $PRGNAME.ll && /usr/bin/c++ -g -o $PRGNAME $PRGNAME.o 
