.. highlight:: tart
  :linenothreshold: 30

.. index::
  single: casting vs. converting

Typecasts and conversions
=========================

Tart does not have a C-style casting operator. However, it does have syntax for converting or
casting one type into another.

Although the words *cast* and *convert* are often used interchangeably when referring
to types, the design of Tart tries to maintain a distinction between them: "cast" or "type cast"
generally refers to taking an existing value and seeing it in a different way, but without
altering it, whereas "convert" generally implies a transformation of the data. Of course, in
real programs the dividing line is not always so clear-cut.

.. index::
  pair: type; conversion

Type Conversion
---------------

Type conversion is normally done by calling the destination type's constructor::

  let f = float(1);
  let i = int(1.0);

.. index::
  triple: implicit; type; conversion

Implicit Conversion
-------------------

Built-in types such as integers and floats can be converted implicitly as needed. A number type
can be silently converted to any other number type, so long as the destination type is capable
of representing all possible values of the source type.

For user-defined types, the :meth:`coerce` method can be used to define an implicit conversion.
The :meth:`coerce` method is a static method of the destination type. For example, the
:class:`Object` class has a :meth:`coerce` method that handles auto-boxing: It wraps ("boxes")
any value type inside of an object, making it usable as a reference type::

  /** String.format takes a list of Objects as arguments. The integer argument
      will be automatically converted into a ValueRef[int]. */
  let s = String.format("Height: {0}", 20);

Here's the actual definition of the :meth:`coerce` method in class :class:`Object`. There
are two overloaded definitions, one for subclasses of :class:`Object` (which simply
returns the value unchanged), and another for all other types which constructs a
:class:`ValueRef` that wraps the value.::

  /** Implicitly convert non-object values to Boxed types. */
  static def coerce[%T] (value:T) -> Object { return ValueRef[T](value); }
  static def coerce(value:Object) -> Object { return value; }

.. index::
  triple: explicit; type; casting
  keyword: typecast
  
Unconditional casting
---------------------

.. method:: typecast

The :func:`typecast` template function is used to do unconditional casts. If the cast operation
succeeds, then execution proceeds as normal. If the cast fails for any reason, then a
:class:`TypecastError` exception is thrown.

An example of a typecast::

  let str = typecast[String](inVal);

Of course, the template parameters can be omitted if the type can be deduced by the compiler::

  let str:String = typecast(inVal);

Unconditional casts work with disjoint types as well as pointer types::

  // A variable which can either be an int or a float.
  let number:int or float = 10;
  
  // Cast to int, if it wasn't an int then throw InvalidTypeError.
  let intVal:int = typecast(number);

Application programmers are free to create additional overloads of :func:`typecast` for their own
types.

Conditional casts
-----------------

The :stmt:`match` statement is used for doing conditional casts.

.. 
  The :kw:`as` operator represents one type of *conditional cast*,
  meaning that the conversion may silently fail if the destination variable
  is the wrong type to contain the value.
  
  There is another type of conditional cast called an *implied conditional
  cast* which is used in conjunction with the :stmt:`if` statement::
  
     var obj:Object;
     if let s:String = obj {
       // Do something with s
     }
  
  Basically what this says is that we're going to attempt to assign the
  value ``obj`` to the variable ``s``, which is of type :ctype:`String`.
  If ``obj`` is in fact a String, the cast succeeds, and the body of the
  :stmt:if statement is executed. Otherwise, if the cast fails, then
  the if-statement body is skipped. The ``s`` variable is only visible
  within the block of code that is guarded by the condition. (If the
  statement had an :stmt:`else` clause, the ``s`` variable would not
  be defined within that scope.)
  
  Conditional casts also work with :stmt:`while`, or any other type of
  statement that takes a test expression.
  
  It should be noted that the :stmt:`if` statement condition is testing
  whether or not the cast succeeded. It is *not* testing the value of
  the expression, whether it be null, or 0, or whatever.
  
  See also the :stmt:`typeswitch` statement, described in a later section.

.. index::
  pair: type; testing
  keyword: isa

Type testing
------------

Sometimes you need to test the type of a variable, the :kw:`isa` keyword
can be used for this. It works for both reference types and union types::

  if a isa float {
    // ...
  }

.. index::
  triple: union; type; casting

Casts and union types
---------------------

*Union types* are types that can contain two or more types as alternatives.
For example, the type ``int or float`` could contain either an int or
a float. A variable of that type would include an extra bit that says
what type of data it currently contains.

To store a value into a union type, you can just use a regular assignment::

  var a:int or float;
  a = 1; // works
  a = 1.0; // also works.

In order to retrieve the data stored in a union type, you have to know
the type of the data that it currently contains - there's no way to read
the data otherwise. One way to achieve this is to use type testing combined
with an unconditional cast::

  if a isa int {
    let b = typecast[int](a);
  }
  
In general, any type conversion syntax that works on reference types
also works on union types. So just as you can call the :meth:`typecast` function
to convert from a superclass to one of its children, you can also use
the same function to convert from a union type to one of its contained
types.

One common usage pattern for union types is used to indicate that a variable
may optionally contain a value::

  // Either holds an int, or it doesn't.
  var a:int or void;

In this case, the special :ctype:`void` type means that the variable does
not contain a value at all. Because this feature is used fairly often,
a shortcut syntax is available::

  // Either holds an int, or it doesn't.
  var a:optional int;

Note that :ctype:`void` is not the same as ``null``, which is a pointer
that points to address zero. (Null pointers are possible in Tart, but
highly restricted compared to C++.)

.. note::
  ``optional int`` may be changed to ``int?`` in a future version of the compiler.

