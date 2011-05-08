#!/usr/bin/python

# Script that un-mangles linkage names generated on Linux by LLVM.

import sys
import re

re_demangle = re.compile("_([a-fA-F0-9][a-fA-F0-9])_")

def tochar(m):
  return chr(int(m.group(1), 16))

for line in sys.stdin:
  print re_demangle.sub(tochar, line),
print

  