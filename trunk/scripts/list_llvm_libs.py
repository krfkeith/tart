#! /usr/bin/python
# 
# Program to list all of the LLVM libs in a form that
# works with FindLLVM.cmake

import os
import re
import glob

BASE_DIR = "/Users/talin/Projects/llvm-head-build/lib"

def decamelize(s):
  return re.sub('(((?<=[a-z])[A-Z])|([A-Z](?![A-Z]|$)))', '_\\1', s).upper().strip('_')
  
libs_found = set()
targets = {}
for target in os.listdir(BASE_DIR + "/Target"):
  target_libs = []
  for lib in glob.glob(BASE_DIR + "/libLLVM%s*.a" % target):
    libs_found.add(lib)
    lib = os.path.splitext(os.path.basename(lib))[0][len(target) + 7:]
    lsym = (target.upper() + '_' + decamelize(lib)).strip('_')
    lname = "LLVM" + target + lib
    target_libs.append((lsym, lname))
  
  if target_libs:
    targets[target] = target_libs

    #arch_libs.append("find_llvm_library(%s, %s)" % (lsym, lname))

common_libs = []
print "# LLVM Libraries"
for lib in glob.glob(BASE_DIR + "/libLLVM*"):
  if lib not in libs_found:
    lib = os.path.splitext(os.path.basename(lib))[0]
    lsym = decamelize(lib[7:])
    lname = lib[3:]
    common_libs.append(lsym)
    print "find_llvm_library(%s %s)" % (lsym, lname)

print
arch_libs = []
for target in sorted(targets.keys()):
  print "# Architecture:", target
  target_libs = targets[target]
  for lsym, lname in target_libs:
    arch_libs.append(lsym)
    print "find_llvm_library(%s %s)" % (lsym, lname)
  print

print
#for target in sorted(targets.keys()):
#  print "    ${LLVM_ARCH_%s}" % target.upper()

for target in sorted(common_libs):
  print "    ${LLVM_%s}" % target.upper()

for target in sorted(arch_libs):
  print "    ${LLVM_%s}" % target.upper()
