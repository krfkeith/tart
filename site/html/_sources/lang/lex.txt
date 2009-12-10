Lexical Structure
=================

This chapter specifies the lexical structure of the Tart language.

Unicode
-------

Programs are written using the Unicode character set. Information about
this character set and its associated character encodings may be found at:

  http://www.unicode.org
  
The current implementation of the compiler recognizes programs written in
UTF-8, however support for other Unicode encodings is planned. Compiled
programs use sequences of 16-bit code points to represent strings, although
there are APIs to access characters as 32-bit values, as well as a variety
of methods for encoding and decoding strings in other character encodings.

Line Terminators
----------------

A Tart source file consists of a sequence of lines, each of which consists
of zero or more characters terminated by an end-of-line sequence. In source
files, any of the standard platform line termination sequences can be
used - the Unix form using ASCII LF (linefeed), the Windows form using
the ASCII sequence CR LF (return followed by linefeed), or the Macintosh
form using the ASCII CR (return) character. All of these forms can be
used equally, regardless of platform.

Tart is a "free-form" language, meaning that line separators are
considered the same as other whitespace. The one exception to this rule
is the "//" comment form, which is terminated by an end-of-line
sequence.

Input Elements and Tokens
-------------------------

White Space
-----------

Comments
--------

Identifiers
-----------

Keywords
--------

Literals
--------

.. productionlist::
  literal: `int_lit` | `float_lit` | `string_lit` | `char_lit` | `null_lit` | `array_lit` | `type_lit`
  int_lit: 0-9+ 
  float_lit: 0-9+
  string_lit: '"' chars '"'
  char_lit: "'" chars "'"
  bool_lit: "true" | "false"
  null_lit: "null"
  array_lit: "[" [`expression` ("," `expression`)*] "]"
  type_lit: typeof `type_expression`

Integer Literals
^^^^^^^^^^^^^^^^

Floating-Point Literals
^^^^^^^^^^^^^^^^^^^^^^^

Boolean Literals
^^^^^^^^^^^^^^^^

Character Literals
^^^^^^^^^^^^^^^^^^

String Literals
^^^^^^^^^^^^^^^

Escape Sequences for Character and String Literals
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Array Literals
^^^^^^^^^^^^^^

The Null Literal
^^^^^^^^^^^^^^^^

Separators
----------

Operators
---------
