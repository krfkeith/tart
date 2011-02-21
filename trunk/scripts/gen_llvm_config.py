#!/usr/bin/python

import sys
import os

# Find LLVM-config
def find_llvm_config():
  for path in os.environ['PATH'].split(':'):
    config_path = os.path.expanduser(os.path.join(path, "llvm-config"))
    if os.access(config_path, os.R_OK):
      return config_path
  print >> sys.stderr, "Can't find llvm-config"
  sys.exit(-1)

def read_config(options):
  return os.popen(llvm_config + " " + options).read().split()

def list_deps(target, components):
  libs = read_config("--libnames " + components)
  libnames = [name[3:-2] for name in libs]  
  print "set(MSVC_DEPS_" + target + " " + " ".join(libnames) + ")"

llvm_config = find_llvm_config()

targets = read_config("--targets-built")

# Removing these because they cause llvm-config to fail.
targets.remove("pic16")
targets.remove("xcore")

tartc_components = "selectiondag bitreader bitwriter"
tartln_components = " ".join(targets) + " linker ipo codegen selectiondag bitwriter bitreader"

list_deps("TARTC", tartc_components)
list_deps("TARTLN", tartln_components)
