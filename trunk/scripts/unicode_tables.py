#!/usr/bin/python3

import urllib.request

unicode_db = "http://www.unicode.org/Public/6.0.0/ucd/UnicodeData.txt"
derived_db = "http://www.unicode.org/Public/6.0.0/ucd/DerivedCoreProperties.txt"
unicode_db_file = "../UnicodeData.txt"

categoryIndex = {
  0 : 0,
  'Lu' : 1, # Uppercase_Letter
  'Ll' : 2, # Lowercase_Letter
  'Lt' : 3, # Titlecase_Letter
  'Lm' : 4, # Modifier_Letter
  'Lo' : 5, # Other_Letter
  'Mn' : 6, # Nonspacing_Mark
  'Mc' : 7, # Spacing_Mark
  'Me' : 8, # Enclosing_Mark
  'Nd' : 9, # Decimal_Number
  'Nl' : 10, # Letter_Number
  'No' : 11, # Other_Number
  'Pc' : 12, # Connector_Punctuation
  'Pd' : 13, # Dash_Punctuation
  'Ps' : 14, # Open_Punctuation
  'Pe' : 15, # Close_Punctuation
  'Pi' : 16, # Initial_Punctuation
  'Pf' : 17, # Final_Punctuation
  'Po' : 18, # Other_Punctuation
  'Sm' : 19, # Math_Symbol
  'Sc' : 20, # Currency_Symbol
  'Sk' : 21, # Modifier_Symbol
  'So' : 22, # Other_Symbol
  'Zs' : 23, # Space_Separator
  'Zl' : 24, # Line_Separator
  'Zp' : 25, # Paragraph_Separator
  'Cc' : 26, # Control
  'Cf' : 27, # Format
  'Cs' : 28, # Surrogate
  'Co' : 29, # Private_Use
  'Cn' : 30, # Unassigned
}

class UnicodeData:
  """Class to hold the content of UnicodeData.txt"""

  # Field definitions 
  FIELD_NAME = 1
  FIELD_CATEGORY = 2
  FIELD_UPPER = 12
  FIELD_LOWER = 13
  FIELD_TITLE = 14

  def __init__(self):
    self.data = [None] * 0x100000

  def parse(self, line):
    "Parse UnicodeData.txt"
    fields = line.split(';')
    index = int(fields[0], 16)
    if index < 0x100000:
      self.data[index] = fields
    
  def __len__(self):
    return len(self.data)
  
  def category(self, index):
    return self.data[index][self.FIELD_CATEGORY] if self.data[index] else 0
  
  def uppercaseMapping(self, index):
    return int(self.data[index][self.FIELD_UPPER], 16) if self.data[index] else 0
  
  def lowercaseMapping(self, index):
    return int(self.data[index][self.FIELD_LOWER], 16) if self.data[index] else 0
  
  def titlecaseMapping(self, index):
    return int(self.data[index][self.FIELD_TITLE], 16) if self.data[index] else 0

  def relativeUppercaseMapping(self, index):
    return self.relativeMapping(index, self.FIELD_UPPER)
  
  def relativeLowercaseMapping(self, index):
    return self.relativeMapping(index, self.FIELD_LOWER)
  
  def relativeTitlecaseMapping(self, index):
    return self.relativeMapping(index, self.FIELD_TITLE)

  def relativeMapping(self, index, field):
    "Return the offset from the character at 'index' to the char in field 'field'"
    if self.data[index]:
      ch = self.data[index][field]
      diff = int(ch, 16) - index if ch else 0
      if diff > 0x7fff or diff < -0x7fff:
        # Some wacky characters have very larger offsets, so don't handle them
        # in this simple table, just treat them as exceptions.
        # TODO: We could return a sentinel value here and use that to process
        # those exceptions.
        return 0
      return diff
    else:
      return 0
    
def findOverlap(oldValues, newValues):
  """Search the list 'oldValues' for a range of values which matches 'newValues',
     and return the array index of the match. It's OK if newValues extends past
     the end of oldValues."""

  # Cache lengths
  nlen = len(newValues)
  olen = len(oldValues)
  assert nlen > 0

  # Most new blocks will begin with a run of repeated values, so we can speed up the
  # search by scanning the old list for a run of repeated values large enough to hold
  # the initial run of the new list.
  leadingValue = newValues[0]
  runLength = 1
  while runLength < nlen and newValues[runLength] == leadingValue:
    runLength += 1
    
  # Now, look for a run of values in oldValues of the same value
  pos = 0
  while pos < olen:
    if oldValues[pos] == leadingValue:
      oldRunLength = 1
      while pos + oldRunLength < olen and oldValues[pos + oldRunLength] == leadingValue:
        oldRunLength += 1
      # If the old run length is too small, keep searching, unless we're at the end.
      if oldRunLength < runLength:
        if pos + oldRunLength == olen:
          return pos
        pos += oldRunLength
        continue;
      else:
        # If the old run is longer than the new, then line up their end points.
        pos += oldRunLength - runLength
        # compare the rest
        same = True
        for n in range(runLength, min(nlen, olen - pos)):
          if newValues[n] != oldValues[pos + n]:
            same = False
            break
        if same:
          return pos
        pos += runLength
    else:
      pos += 1

  return pos

def compressBlocks(values, blockSize):
  """Given a list of values, compress the list by breaking it up into blocks of
    size 'blockSize', then finding overlaps between the blocks. The result is
    two arrays: The compressed data, and an index array that gives the starting
    offset of each block."""
  numBlocks = len(values) // blockSize
  cdata = []
  cindex = [0] * numBlocks
  cblocks = [i for i in range(0, numBlocks)]
  for i in cblocks:
    srcindex = i * blockSize
    # Data for one block.
    blockdata = values[srcindex:srcindex+blockSize]
    dstindex = findOverlap(cdata, blockdata)
    cindex[i] = dstindex
    overlap = min(blockSize, len(cdata) - dstindex)
    if overlap < blockSize:
      cdata.extend(blockdata[overlap:])
  return cdata, cindex

class CompressedTable:
  def __init__(self, values, name, dtype, dsize):
    self.name = name
    self.dtype = dtype
    self.dsize = dsize
    self.bitsData = 4
    self.bitsBlock = 7
    self.blockSize = 1 << self.bitsData
    self.planeSize = 1 << self.bitsBlock
    self.blockMask = self.blockSize - 1
    self.planeMask = self.planeSize - 1
    self.data, cblocks = compressBlocks(values, self.blockSize)
    self.blocks, self.planes = compressBlocks(cblocks, self.planeSize)

  def write(self, out):
    out.writeLn("  // {0} lookup function", self.name)
    out.writeLn("  def {0}(ch:char) -> {1} {{", self.name, self.dtype)
    out.writeLn("    if ch >= 0x100000 {{ return 0; }}")
    out.writeLn("    let plane:uint16 = {0}Planes[uint(ch) >> {1}];",
                self.name,
                self.bitsData + self.bitsBlock)
    out.writeLn("    let block:uint16 = {0}Blocks[plane + ((uint(ch) >> {1}) & {2})];",
                self.name,
                self.bitsData,
                self.planeMask)
    out.writeLn("    return {0}Data[block + (uint(ch) & {1})];", self.name, self.blockMask)
    out.writeLn("  }}")
    out.writeLn()
    out.writeLn("  // Plane table for {0}", self.name)
    self.writeTable(out, self.planes, "Planes", "uint16", 16, 5)
    out.writeLn()
    out.writeLn("  // Block table for {0}", self.name)
    self.writeTable(out, self.blocks, "Blocks", "uint16", 16, 5)
    out.writeLn()
    out.writeLn("  // Data table for {0}", self.name)
    self.writeTable(out, self.data,   "Data", self.dtype, 24, self.dsize)
    
  def writeTable(self, out, data, tabname, dtype, rowlength, valsize):
    out.writeLn("  private let {0}{1}:NativeArray[{2}, {3}] = [".format(
        self.name, tabname, dtype, len(data)))
    rowpos = 0
    while rowpos < len(data):
      nextrow = rowpos + rowlength
      rowdata = data[rowpos:nextrow]
      rowstr = self.formatRowData(rowdata, valsize)
      if nextrow < len(data):
        out.writeLn("    {0},".format(rowstr))
      else:
        out.writeLn("    {0}".format(rowstr))
      rowpos = nextrow
    out.writeLn("  ];")
    
  def formatRowData(self, rowData, valsize):
    return ",".join("{0:>{1}}".format(n, valsize) for n in rowData)

class Stream:
  def __init__(self, path):
    self.fh = open(path, "w")

  def writeLn(self, fmt='', *args):
    print(fmt.format(*args), file=self.fh)
    
  def close(self):
    self.fh.close()

def read_unicode_db():
  ucd = UnicodeData()
  print("Reading unicode database")
#  for line in urllib.request.urlopen(unicode_db):
  for line in open(unicode_db_file):
    #line = str(line, encoding='utf8').strip()
    line = line.strip()
    if not line or line[0] == '#': continue
    ucd.parse(line)
  return ucd
    
ucd = read_unicode_db()

print("Generating category table")
categoryArray = [categoryIndex[ucd.category(n)] for n in range(len(ucd))]
categoryTable = CompressedTable(categoryArray, "category", "uint8", 3);

print("Generating uppercase mapping table")
uppercaseMappingArray = [ucd.relativeUppercaseMapping(n) for n in range(len(ucd))]
uppercaseMappingTable = CompressedTable(uppercaseMappingArray, "uppercaseMapping", "int16", 5);

print("Generating lowercase mapping table")
lowercaseMappingArray = [ucd.relativeLowercaseMapping(n) for n in range(len(ucd))]
lowercaseMappingTable = CompressedTable(lowercaseMappingArray, "lowercaseMapping", "int16", 5);

print("Generating titlecase mapping table")
titlecaseMappingArray = [ucd.relativeTitlecaseMapping(n) for n in range(len(ucd))]
titlecaseMappingTable = CompressedTable(titlecaseMappingArray, "titlecaseMapping", "int16", 5);

out = Stream("lib/std/tart/text/CharacterTables.tart")
out.writeLn("namespace CharacterTables {{")
categoryTable.write(out)
out.writeLn()
uppercaseMappingTable.write(out)
out.writeLn()
lowercaseMappingTable.write(out)
out.writeLn()
titlecaseMappingTable.write(out)
out.writeLn("}}")
out.close()

print("* Done *")
