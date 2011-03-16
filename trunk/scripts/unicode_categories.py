#!/usr/bin/python

import urllib2
import re

unicode_db = "http://www.unicode.org/Public/5.2.0/ucd/UnicodeData.txt"

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

# Read in unicode database
for line in urllib2.urlopen(unicode_db):
  line = line.strip()
  if not line or line[0] == '#': continue
  print line
