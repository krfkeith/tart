#!/usr/bin/python

# Script that attempts to disassemble an executable file produced by Tart,
# and determine how large the various components are.

import re
import sys
import subprocess

# 00000000004019a0 t tart.core.String_5B__5D_.type.idispatch
re_line = re.compile("([\dA-Fa-f]+)\s[tdr]\s(.*)")
re_esc = re.compile("_([\dA-Fa-f]{2})_")

def unesc(s):
  return chr(int(s.group(1), 16))

# Class used to format the result into rows and columns
class Table:
  class Column:
    def __init__(self, title, rightAlign, fmt):
      self.title = title
      self.rightAlign = rightAlign
      self.width = max(len(title), 6)
      self.indentLevel = 0
      self.fmt = "{0:" + fmt + "}"

    def format(self, value):
      value = ("  " * self.indentLevel) + self.fmt.format(value)
      width = len(value)
      if width > self.width:
        self.width = width
      return value
        
    def printField(self, value):
      if self.rightAlign:
        print "{0:>{1}}".format(value, self.width),
      else:
        print "{0:<{1}}".format(value, self.width),

  def __init__(self):
    self.columns = []
    self.rows = []
    
  def indent(self, columnIndex=0):
    self.columns[columnIndex].indentLevel += 1
  
  def unindent(self, columnIndex=0):
    self.columns[columnIndex].indentLevel -= 1
    
  def addColumn(self, name, rightAlign=False, fmt=""):
    self.columns.append(self.Column(name, rightAlign, fmt))
    
  def printReport(self):
    for col in self.columns:
      col.printField(col.title)
    print
    for col in self.columns:
      print "{0:=<{1}}".format('', col.width),
    print
    for row in self.rows:
      if len(row) == 0:
        print
        continue
      colIndex = 0
      for col in self.columns:
        col.printField(row[colIndex])
        colIndex = colIndex + 1
      print
    
  def addRow(self, *data):
    if len(data) == 0:
      self.rows.append([])
      return
    assert len(data) == len(self.columns)
    columnIndex = 0
    row = []
    for value in data:
      col = self.columns[columnIndex]
      row.append(col.format(value))
      columnIndex += 1
    self.rows.append(row)
    
# Represents measurements for a particular type of symbol.
class Stat:
  def __init__(self, name, matcher):
    self.name = name
    self.matcher = matcher
    self.count = 0
    self.size = 0
    
  def accumulate(self, sym, size):
    if self.matcher.match(sym):
      self.count += 1
      self.size += size
      return True
    return False
  
  def report(self, table, total):
    average = self.size / self.count if self.count > 0 else 0
    table.addRow(self.name, self.count, self.size, average, self.size * 100.0 / total)
    
# Represents a related group of stats
class Group:
  def __init__(self, name, stats):
    self.name = name
    self.size = 0
    self.stats = stats
    
  def accumulate(self, sym, size):
    for st in self.stats:
      if st.accumulate(sym, size):
        self.size += size
        return True
    return False

  def add(self, stat):
    self.stats.append(stat)
    
  def report(self, table, total):
    table.addRow(self.name, "--", self.size, "--", self.size * 100.0 / total)
    table.indent()
    for stat in self.stats:
      stat.report(table, total)
    table.unindent()
    table.addRow()
    
# Matches the first part of a symbol name.  
class PrefixMatcher:
  def __init__(self, prefix):
    self.prefix = prefix

  def match(self, sym):
    return sym.startswith(self.prefix)

# Matches the last part of a symbol name.
class SuffixMatcher:
  def __init__(self, suffix):
    self.suffix = suffix
    
  def match(self, sym):
    return sym.endswith(self.suffix)

# Matches the last part of a symbol name.
class RegexMatcher:
  def __init__(self, pattern):
    self.pattern = re.compile(pattern)
    
  def match(self, sym):
    return self.pattern.match(sym)

class Stats:
  def __init__(self, stats):
    self.stats = stats
    self.size = 0
    self.uncategorized = {}
    
  def accumulate(self, sym, size):
    if sym.startswith("GCC") or sym.startswith("_"):
      return
    self.size += size
    found = False
    for st in self.stats:
      if st.accumulate(sym, size):
        found = True

    # Handle symbols for which there is no stat defined.
    if not found and sym.startswith("."):
      key = '.' + sym[1:].partition('.')[0]
      if key in self.uncategorized:
        self.uncategorized[key] += 1
      else:
        self.uncategorized[key] = 1
      
  def report(self):
    table = Table()
    table.addColumn("Category");
    table.addColumn("Count", True);
    table.addColumn("Size", True);
    table.addColumn("Average", True);
    table.addColumn("Percent", True, ".1f");
    
    print
    print "Total size: ", self.size
    print
    
    for stat in self.stats:
      stat.report(table, self.size)

    table.printReport()
    print

    for key, count in self.uncategorized.iteritems():
      print key, count
    
st = Stats([
  Group("Reflection", [
    Stat("Composite types", PrefixMatcher(".compositeType")),
    Stat("Enum types", PrefixMatcher(".enumType")),
    Stat("Other types", PrefixMatcher(".type.")),
    Stat("Type references", PrefixMatcher(".typeRefs")),
    Stat("Type lists", PrefixMatcher(".typelist")),
    Stat("Invokers", PrefixMatcher(".invoke")),
    Stat("Name tables (head)", PrefixMatcher(".names.")),
    Stat("Name tables (simple)", PrefixMatcher(".names_simple")),
    Stat("Name tables (compound)", PrefixMatcher(".names_compound")),
    Stat("Field offsets", PrefixMatcher(".fieldoffsets")),
  ]),
  Group("TIB", [
    Stat("TypeInfoBlocks", SuffixMatcher(".type.tib")),
    Stat("Base class lists", SuffixMatcher(".type.tib.bases")),
    Stat("IDispatch functions", SuffixMatcher(".type.idispatch")),
  ]),
  Group("Standard Library", [
    Stat("tart.core", PrefixMatcher("tart.core")),
    Stat("tart.collections", PrefixMatcher("tart.collections")),
    Stat("tart.gc", PrefixMatcher("tart.gc")),
    Stat("tart.io", PrefixMatcher("tart.io")),
    Stat("tart.reflect", PrefixMatcher("tart.reflect")),
    Stat("tart.testing", PrefixMatcher("tart.testing")),
    Stat("tart.text", PrefixMatcher("tart.text")),
  ]),
  Group("Classes", [
    Stat("tart.core.Array", RegexMatcher("(.*?)\[\]")),
    Stat("tart.core.String", PrefixMatcher("tart.core.String")),
    Stat("tart.collections.ArrayList", PrefixMatcher("tart.collections.ArrayList")),
    Stat("tart.collections.ImmutableList", PrefixMatcher("tart.collections.ImmutableList")),
    Stat("tart.collections.List", PrefixMatcher("tart.collections.List")),
  ]),
  Stat("Static strings", PrefixMatcher(".string")),
  Stat("Trace tables", PrefixMatcher(".tracetable")),
])

#.methodRefs 2
#.members 3

prev_sym = None
prev_addr = 0

print sys.argv[1]
pipe = subprocess.Popen(["nm", "-n", sys.argv[1]], stdout=subprocess.PIPE).stdout
for line in pipe:
  m = re_line.match(line)
  if m:
    addr = int(m.group(1), 16)
    sym = re_esc.sub(unesc, m.group(2))
    if prev_sym:
      size = addr - prev_addr
      st.accumulate(prev_sym, size)
    prev_addr = addr
    prev_sym = sym
    
st.report()
