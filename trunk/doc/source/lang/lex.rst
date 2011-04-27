Lexical Structure
=================

This chapter specifies the lexical structure of the Tart language.

Unicode
-------

Programs are written using the Unicode character set. Information about
this character set and its associated character encodings may be found at:

  http://www.unicode.org

The current implementation of the compiler recognizes programs written in
UTF-8, however support for other Unicode encodings is planned. Internally
strings are encoded as UTF-8 byte sequences, although there are APIs to
access characters as 32-bit values, as well as a variety of methods for
encoding and decoding strings in other character encodings.

Line Terminators
----------------

A Tart source file consists of a sequence of lines, each of which consists
of zero or more characters terminated by an end-of-line sequence. In source
files, any of the standard platform line termination sequences can be
used - the Unix form using ASCII LF (linefeed), the Windows form using
the ASCII sequence CR LF (return followed by linefeed), or the old Macintosh
form using the ASCII CR (return) character. All of these forms can be
used equally, regardless of platform.

Tart is a "free-form" language, meaning that line separators are
considered the same as other whitespace. The one exception to this rule
is the "//" comment form, which is terminated by an end-of-line
sequence.

Input Elements and Tokens
-------------------------

A Tart source file consists of a sequence of tokens: identifiers, keywords, operators,
literal values, and punctuation. "White space" characters such as space and tab are not
considered tokens - they have no meaning except to act as separators for tokens. Comments
are treated the same as white space.

Source-code Comments
--------------------

Tart comments are similar to those seen in C++ and Java::

  /* A block comment. */
  // A line comment

In addition, Tart supports two forms of 'tartdoc' comments which can be used to
automatically generate documentation::

  /** A tartdoc block comment. */
  /// A tartdoc line comment.

Tart supports a lightweight form of markup within comments that can be used to
call out parameters and return values. Here's an example::

  /** Collect all type definitions in the AST.
      Parameters:
        defn - where to put the definitions.
        recurse - whether to search recursively.
      Returns:
        The number of definitions found.
  */

The doc comment markup language is designed to minimize the impact on the
readability of comments in the source code. This will be explained in
greater detail in a later section. [TODO link]

Identifiers
-----------

The syntax for identifiers is as follows:

.. productionlist::
  ident_start: `letter` | "_"
  ident_char: `letter` | `digit` | "_"
  ident: `ident_start` `ident_char`*

Keywords
--------

The list of Tart keywords::

    abstract   const      float      internal   or         static    uint16
    and        constable  fn         is         override   struct    uint32
    as         continue   for        isa        private    short     uint64
    bool       def        friend     let        protected  super     uint8
    break      double     get        macro      protocol   switch    undef
    byte       else       if         match      public     throw     var
    case       enum       import     mutable    readonly   true      void
    catch      false      in         namespace  repeat     try       where
    char       final      int        not        return     typecast  while
    class      finally    interface  null       set        uint      yield

Literals
--------

.. productionlist::
  literal: `int_lit` | `float_lit` | `string_lit` | `char_lit` | `null_lit` | `array_lit`
  int_lit: [0-9_]+
   float_lit: [0-9_](.[0-9_]+)?(e[0-9_]+)
  string_lit: '"' chars '"'
  char_lit: "'" chars "'"
  bool_lit: "true" | "false"
  null_lit: "null"
  array_lit: "[" [`expression` ("," `expression`)*] "]"

Integer Literals
^^^^^^^^^^^^^^^^

An integer literal starts with a digit, and may contain any number of digits.
Integer literals may also contain underscores, which can be used as a thousands
separator - for example, the number one million can be written either as
``1000000`` or as ``1_000_000``. The underscores are purely decorative, and
have no affect upon the value of the integer.

Integer literals are typeless by default, meaning that they have no specified
bit size and can be arbitrarily large. The size of the integer is set once the
integer is assigned to a variable, or typecast to a specific integer type. A
check is done to insure that the literal value will fit in the desired space.
This means that calculations with integer literals can have intermediate
results that are larger than the destination size, as long as the final
result of the calculation will fit::

  var i:int8 = 1_000_001 - 1_000_000;

Even though a value of one million cannot fit into an 8-bit integer, there is
no problem here, because the subtraction yields a value of 1, which will fit.

Floating-Point Literals
^^^^^^^^^^^^^^^^^^^^^^^

A floating-point literals starts with a digit or a point, and must contain
either a point or an exponent (signified with the letter 'e') in order to
distinguish it from an integer. The floating-point literal may also contain
underscore characters, similar to integers.

Unlike integers, floating-point literals have distinct sizes, which are
:ctype:`float` (32-bits), :ctype:`double` (64 bits) and :ctype:`long double` which is
somewhere between 80 and 128 bits, depending on the capabilities of the
processor.

Boolean Literals
^^^^^^^^^^^^^^^^

The boolean literals are :const:`true` and :const:`false`.

String Literals
^^^^^^^^^^^^^^^

String literals are delimited by double-quotes. A string literal creates
a instance of class :ctype:`tart.core.String`. String literals may contain
:ref:`escape sequences <escapes>`.

Character Literals
^^^^^^^^^^^^^^^^^^

Character literals are delimited by single quotes. A character literal
creates a single 32-bit Unicode character value. A character literal may also consist of
a character :ref:`escape sequence <escapes>`.

.. _escapes:

Escape Sequences for Character and String Literals
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Escape sequences in character and string literals are introduced by a backslash character (``\``).

  =================== ============================================
  ``\\``              Literal backslash
  ``\0``              NUL character
  ``\'``              Escaped single quote
  ``\"``              Escaped double quote
  ``\r``              Carriage return
  ``\n``              Line feed
  ``\t``              Tab character
  ``\b``              Backspace character
  ``\v``              Vertical tab character
  ``\x``\ *nn*        Character specified as 2 hex digits
  ``\u``\ *nnnn*      Unicode character specified as 4 hex digits
  ``\U``\ *nnnnnnnn*  Unicode character specified as 8 hex digits
  =================== ============================================

Array Literals
^^^^^^^^^^^^^^

The Null Literal
^^^^^^^^^^^^^^^^

The literal :const:`null` represents a null object reference.

Separators
----------

The list of Tart delimiter tokens::

  ( ) [ ] { } . , ;

Operators
---------

The list of Tart operator tokens::

  : :: <: >:
  == != > < >= <= >? <? >=? <=?
  | |= & &= ^
  << <<= >> >>=
  + += - -= * *= / /= % %= ++ --
  .. ...
