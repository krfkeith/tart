Types and Values
================

Primitive Types
---------------

Built-in types:

  ====================  ===========================================================================
  Type Name             Description
  ====================  ===========================================================================
  :ctype:`void`         Represents the absence of a value.
  :ctype:`bool`         Boolean type, can be either :const:`true` or :const:`false`.
  :ctype:`char`         A 32-bit character.
  :ctype:`int8`         An 8-bit signed integer.
  :ctype:`int16`        A 16-bit signed integer.
  :ctype:`int32`        A 32-bit signed integer.
  :ctype:`int64`        A 64-bit signed integer.
  :ctype:`uint8`        An 8-bit unsigned integer.
  :ctype:`uint16`       A 16-bit unsigned integer.
  :ctype:`uint32`       A 32-bit unsigned integer.
  :ctype:`uint64`       A 64-bit unsigned integer.
  :ctype:`byte`         Another name for :ctype:`uint8`.
  :ctype:`int`          Another name for :ctype:`int32` or :ctype:`int64`, depending on platform.
  :ctype:`uint`         Another name for :ctype:`uint32` or :ctype:`uint64`, depending on platform.
  :ctype:`float`        A 32-bit IEEE floating-point value.
  :ctype:`double`       A 64-bit IEEE floating-point value.
  :ctype:`long double`  A floating point value larger than 64 bits (platform specific).
  ====================  ===========================================================================

The :ctype:`int` and :ctype:`uint` types are integers that are the same size as a pointer.

The 'Null' Type
^^^^^^^^^^^^^^^

The :ctype:`Null` type has a single value, :const:`null`, which is used to indicate the absence
of a reference to an object. Any object reference can be compared with :const:`null`.

Reference Types and Values
--------------------------

Types in tart are either *reference types* or *value types*. Variables of value type always contain
their data, whereas variables of reference type store references to objects. With reference types,
it is possible for two variables to reference the same object, and thus possible for operations
on one variable to affect the object referenced by the other variable. With value types, the
variables each have their own copy of the data, and it is not possible for operations on one to
affect the other.

The following example should illustrate the difference::

  class RefExample { // Classes are reference types.
    var data:int;
  }

  struct ValExample { // Structs are value types.
    var data:int;
  }

  let a = RefExample();
  a.data = 1;
  let b = a;
  b.data = 2;   // a.data is now 2, becase a and b are the same object.

  let c = ValExample();
  c.data = 1;
  let d = c;
  d.data = 2;   // c.data is not affected, still contains the value 1.

In Tart, only classes and interfaces are reference types, while other types such as structs,
primitive types, and tuples are value types. Types that have no runtime representation, such
as void or protocol types, are considered to be neither.

Classes and Objects
^^^^^^^^^^^^^^^^^^^

The Object Type
^^^^^^^^^^^^^^^

The String Type
^^^^^^^^^^^^^^^

Array Types
^^^^^^^^^^^

Parameterized Types
^^^^^^^^^^^^^^^^^^^

Subtyping
^^^^^^^^^

Other Types and Values
----------------------

Structs
^^^^^^^

Functions
^^^^^^^^^

Enumerations
^^^^^^^^^^^^

Tuples
^^^^^^

Native Arrays
^^^^^^^^^^^^^

Native Pointers
^^^^^^^^^^^^^^^

Type Modifiers
--------------

Nullable Types
^^^^^^^^^^^^^^

Type Conversions and Promotions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

