.. highlight:: tart
  :linenothreshold: 30

.. index::
  pair: type; builtin
  pair: type; primitive

Type Expressions
================

Because Tart is a strongly-typed language, it supports a rich hierarchy of types. All types in
Tart fall into one of the following groups:

==========  =========================================================================
Type Group  Explanation
==========  =========================================================================
primitive   Primitive types such as :ctype:`int`, :ctype:`float`, :ctype:`bool`, etc.
composite   Includes :keyword:`class`, :keyword:`struct`, :keyword:`interface` and
            :keyword:`protocol`. Arrays and maps are part of this group as well.
enumerated  Enumerated types declared with :keyword:`enum`.
disjoint    Also known as union types, declared with :keyword:`or` such as
            :samp:`int or float`.
function    The type of a function.
tuple       An unnamed sequence of types, such as :samp:`(int, String)`.
native      Native machine pointers and arrays, used for calling C library functions.
==========  =========================================================================

Primitive Types
---------------

Tart supports the following primitive types:

Integer Types
`````````````
========= ================ =================================
Bit width Signed Type Name Unsigned Type Name
========= ================ =================================
8         :ctype:`byte`         :ctype:`ubyte`
16        :ctype:`short`        :ctype:`ushort`
32        :ctype:`int`          :ctype:`uint`, :ctype:`char`
64        :ctype:`long`         :ctype:`ulong`
========= ================ =================================

The naming convention is that unsigned types always begin with the letter 'u'.

The :ctype:`char` type is a 32-bit integer which represents a Unicode character, but it is not the
same as the :ctype:`uint` type. The :ctype:`char` type is generally only used when dealing with
individual characters. For sequences of characters, it is more convenient to use the
:ctype:`String` type, which stores text encoded in UTF-8.

.. note:: There is also a special integer type used for constants, which does not have a fixed
  size (and therefor can hold any sized number). You cannot declare a variable of this type.

Floating-point Types
````````````````````
========= ====================
Bit width Type Name
========= ====================
32        :ctype:`float`
64        :ctype:`double`
96 [*]_   :ctype:`long double`
========= ====================

.. [*] The size of a long double may vary depending on the platform. It will generally be the
  largest type that can be supported by the floating-point coprocessor.

Boolean Type
````````````

The primitive boolean type is called :ctype:`bool` and supports the built-in values
:cdata:`true` and :cdata:`false`.

Null Type
`````````
The special value :const:`null` indicates an invalid object reference. It's type is :ctype:`Nothing`,
however you cannot declare a variable of that type.

.. note:: It is a goal of Tart that the :const:`null` value should be used much less frequently than
  it is in C++. In particular, the addition of disjoint types means that it is unnecessary to
  use :const:`null` as a sentinel value when returning an object reference.
  
  It would be nice to eliminate the :const:`null` value entirely, but it is needed when calling
  external C library functions. It is also the default initialization value for variables
  of reference type.
  
Void Type
`````````
:ctype:`void` is used to indicate "no value". It can be used to declare a function that does not
return a value. It can also be used in disjoint types. Other than those two cases, it is not
permitted to declare a variable having void type.

Derived Types
-------------

Most derived types in Tart are simply generic composite types - that is, classes or structs which
happen to have one or more template parameters. For example, the :ctype:`Array` type is not
a built-in type, it is a template class. (For that matter, :ctype:`Object` is not a built-in type
either, however it is a type that the compiler knows a lot about.)

There are a couple of exceptions, meaning types that are so special (or so weird) that the compiler
has to treat them in a fundamentally different way. These types are *functions*, *disjoint types*,
*tuples*, and *native types*. These all all derived from one or more base types, and their
identity is determined solely by these base types - thus a tuple containing (String, int) is
always the same type as any other tuple containing (String, int).

Although the :ctype:`Array` type is just a template class, there is some special syntax for
declaring arrays - the type expression :samp:`{type}[]` denotes an array of :samp:`{type}`.
