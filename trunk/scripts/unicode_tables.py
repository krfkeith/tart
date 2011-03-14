import urllib2
import re

unicode_db = "http://www.unicode.org/Public/5.2.0/ucd/UnicodeData.txt"
derived_db = "http://www.unicode.org/Public/5.2.0/ucd/DerivedCoreProperties.txt"

""" Some ideas:

    <start of range> <offset to mask, or 0 for all 0, or -1 for all 1>
    
"""

categories = [
  'Lu', # Uppercase_Letter
  'Ll', # Lowercase_Letter
  'Lt', # Titlecase_Letter
  'Lm', # Modifier_Letter
  'Lo', # Other_Letter
  'Mn', # Nonspacing_Mark
  'Mc', # Spacing_Mark
  'Me', # Enclosing_Mark
  'Nd', # Decimal_Number
  'Nl', # Letter_Number
  'No', # Other_Number
  'Pc', # Connector_Punctuation
  'Pd', # Dash_Punctuation
  'Ps', # Open_Punctuation
  'Pe', # Close_Punctuation
  'Pi', # Initial_Punctuation
  'Pf', # Final_Punctuation
  'Po', # Other_Punctuation
  'Sm', # Math_Symbol
  'Sc', # Currency_Symbol
  'Sk', # Modifier_Symbol
  'So', # Other_Symbol
  'Zs', # Space_Separator
  'Zl', # Line_Separator
  'Zp', # Paragraph_Separator
  'Cc', # Control
  'Cf', # Format
  'Cs', # Surrogate
  'Co', # Private_Use
  'Cn', # Unassigned
]

proptable = {}

def parse_derived_property(line):
  range, prop = line.split(';')
  range = range.strip()
  prop = prop.strip().split()[0]
  return range, prop

def generate_table(propname):
  ranges = proptable[propname]

  # Build the table.
  table = []
  rLastEnd = 0
  for rStart, rEnd in ranges:
    if rStart > rLastEnd:
      table.append((rLastEnd, 0))
      table.append((rStart, 1))
    rLastEnd = rEnd + 1
  table.append((rLastEnd, 0))
  
  i = 0
  while i < len(table):
    start, value = table[i]
    j = i + 1
    while j < len(table):
      next = table[j][0]
      if next > start + 32:
        break
      j += 1
      
    if j > i + 3:
      bits = []
      k = i
      while k < j - 1:
        spanStart, spanValue = table[k]
        spanLength = table[k+1][0] - spanStart
        bits += [table[k][1]] * spanLength
        #print spanLength, "".join(str(digit) for digit in bits)
        k += 1
      print (hex(start), "".join(str(digit) for digit in bits))
      i = j - 1
    else:
      print (hex(start), value)
      i += 1
  
for line in urllib2.urlopen(derived_db):
  line = line.strip()
  if not line or line[0] == '#': continue
  range, prop = parse_derived_property(line)

  if ".." in range:
    range = range.split('..')
    rstart = int(range[0], 16)
    rend = int(range[1], 16)
    
  else:
    rstart = rend = int(range, 16)
  
  try:
    propList = proptable[prop]
  except KeyError:
    propList = proptable[prop] = []
    
  propList.append((rstart, rend))

generate_table("Alphabetic")

print "Done"
