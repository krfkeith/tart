#!/usr/bin/python3

import os
import sys
import glob

if __name__ == '__main__':
  for path in sys.argv[1:]:
    for file in glob.glob(path):
      fh = open(file, "r")
      linecount = 1
      for line in fh:
        if len(line) > 101:
          print(file, ":", linecount)
        linecount += 1
      fh.close()
        
