.. highlight:: tart
  :linenothreshold: 30

.. index::
  pair: type; builtin

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

.. index::
  pair: type; primitive

Primitive Types
---------------

Tart supports the following primitive types:

.. index::
  pair: type; integer

Integer Types
`````````````
========= ================ =================================
Bit width Signed Type Name Unsigned Type Name
========= ================ =================================
8         :ctype:`int8`    :ctype:`uint8`
16        :ctype:`int16`   :ctype:`uint16`
32        :ctype:`int32`   :ctype:`uint32`, :ctype:`char`
64        :ctype:`int64`   :ctype:`uint64`
========= ================ =================================

The naming convention is that unsigned types always begin with the letter 'u'.

Types :ctype:`int` and :ctype:`uint` are either 32 or 64 bits, depending on whether
the target platform is a 32-bit or 64-bit processor. On a 32-bit processor, :ctype:`int`
is an alias for :ctype:`int32`, and on a 64-bit processor it is an alias for :ctype:`int64`.

The :ctype:`char` type is a 32-bit integer which represents a Unicode character, but it is not the
same as the :ctype:`uint32` type. The :ctype:`char` type is generally only used when dealing with
individual characters. For sequences of characters, it is more convenient to use the
:ctype:`String` type, which stores text encoded in UTF-8.

.. note:: There is also a special integer type used for constants, which does not have a fixed
  size (and therefore can hold any sized number). You cannot declare a variable of this type.

.. index::
  pair: type; floating-point
  pair: type; float

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

.. index::
  pair: type; boolean type

Boolean Type
````````````
The primitive boolean type is called :ctype:`bool` and supports the built-in values
:cdata:`true` and :cdata:`false`.

.. index::
  pair: type; methods on numeric types

Methods on numeric types
````````````````````````
All of the primitive types have a :meth:`toString` method that returns a string representation of
a value of that type::

  var x = 1;
  var s = x.toString();  // Assign the string "1" to s.

Each type also has a static :meth:`parse` method that can convert a string to that type, or
throw an exception if the string cannot be converted::

  var x = int.parse("1");

In addition, each of the integer types also define two constants, ``minVal`` and ``maxVal``, which
represent the minimum and maximum values that can be represented by that type::

  var x = uint8.minVal; // Assigns the value 256 to x.

.. note:: Even though you can call the :meth:`toString()` method on a variable of
  type :ctype:`int`, this does not mean that integers are actually objects. Unlike some
  languages which are designed around the 'everything is an object' philosophy, Tart tries
  to maximize performance and efficiency, which means that integers are just plain everyday
  machine words like in C++. The compiler transforms the call to :meth:`toString()` into a regular
  function call, with the integer value as the argument to the function.

.. index::
  pair: type; Null

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
  
.. index::
  pair: type; void

Void Type
`````````
:ctype:`void` is used to indicate "no value". It can be used to declare a function that does not
return a value. It can also be used in disjoint types. Other than those two cases, it is not
permitted to declare a variable having void type.

.. index::
  pair: type; enumerations
  pair: type; enum

Enumeration Types
-----------------

Tart supports enumeration types similar to those in C++::

  // A basic enumeration declaration
  enum Direction {
    NORTH = 0,
    EAST,
    SOUTH,
    WEST,
  }
  
  var x = Direction.EAST;
  
As you can see, enumeration constants can be defined with an integer expression. Also,
a trailing comma is allowed in enumeration lists - this makes it easier to add new enumeration
constants.

In addition to the explicitly created enumeration constants, the compiler will automatically
generate the constants ``minVal`` and ``maxVal``, representing the smallest and largest values
in the enumeration, as well as a :meth:`toString` method to convert values of the enum type
to a string representation.

Normally the compiler will automatically determine the size of the enumeration type (that is,
how many bits of storage are required to hold any of the declared values.) However, you can
force the compiler to use a specific size by declaring the enumeration to be based on an
integer type::

  enum Direction : uint8 {
    NORTH,
    EAST,
    SOUTH,
    WEST,
  }
  
Tart also supports bitfield or 'flag' enumerations, using the ``@Flags`` annotation::

  @Flags enum Traits {
    ABSTRACT,
    FINAL,
    STATIC,
    
    MASK = ABSTRACT | FINAL | STATIC,
  }
  
  var x = Traits.ABSTRACT | Traits.FINAL;
  
The presence of the ``@Flags`` annotation causes a number of things to happen:

 * The default sequence of initialization values is 1, 2, 4, 8, and so on, rather
   than 0, 1, 2, 3, etc. In other words, any enumeration constant that does not have
   an explicit initialization expression will be assigned the next available bit number
   after the previous value.
 * The compiler will auto-generate the bitwise operators ``|`` and ``&`` for that type.
 * The compiler will also auto-generate the :meth:`contains` method, which allows
   the use of the ``in`` operator, enabling expressions such as ``if ABSTRACT in traits``.
 * In the current version of Tart, the :meth:`toString` method is not defined on
   flag enums.

.. note:: Tart enumerations do not support Java's ability to add arbitrary properties to enums.

.. index::
  pair: type; derived
  pair: type; composite
  pair: type; function
  pair: type; tuple
  pair: type; union
  pair: type; disjoint
  pair: type; native

Derived Types
-------------

A derived type is a type that is built out of other types. There are several different flavors
of derived types:

  * *Composite types*, such as classes or structs.
  * *Function types*, that is, the type of a callable object.
  * *Tuples*, which represent a heterogeneous sequence of types such as ``(String, int)``.
  * *Disjoint* or *Union* types, which can hold one of several different types.
  * *Native* types, such as machine pointers.
  
Except in the case of Composite types, the identity of each derived type is determined solely
by the base types - thus a tuple containing ``(String, int)`` is always the same type as any other
tuple containing ``(String, int)``.

.. index::
  pair: type; array

You might also notice one type that is missing from this list - array types. The Tart array type
is not a built-in type, but rather it is an instance of the :ctype:`Array` template class.
(For that matter, :ctype:`Object` is not a built-in type either, however it is a type that
the compiler knows a lot about.)
  
Although the :ctype:`Array` type is just a template class, there is some special syntax for
declaring arrays - the type expression :samp:`{type}[]` denotes an array of :samp:`{type}`,
which is equivalent to the type expression :samp:`Array[{type}]`. In the latter case, however,
the square brackets are used to denote a template argument list. (In type expressions, empty
brackets always indicate an array, whereas brackets that are non-empty always indicate
a template argument list. For non-type expressions, the compiler uses contextual information
to distinguish the two cases.)
