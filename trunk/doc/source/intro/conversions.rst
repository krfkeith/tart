.. highlight:: tart
  :linenothreshold: 30

Typecasts and conversions
=========================

.. warning:: This section is out of date.

Tart does not have a C-style casting operator. However, it does have syntax for converting or
casting one type into another.

.. note:: Although the words "cast" and "convert" are often used interchangeably when referring
  to types, the design of Tart tries to maintain a distinction between them: "cast" or "type cast"
  generally refers to taking an existing value and seeing it in a different way, but without
  altering it, whereas "convert" generally implies a transformation of the data. Of course, in
  real programs the dividing line is not always so clear-cut.

Type Conversion
---------------

Type conversion is normally done by calling the destination type's constructor::

  let f = float(1);
  let i = int(1.0);

In cases where the constructor is already being used for something else, a classes will usually
have a static method named :func:`from`.

  let s = String.from(10);

Implicit Conversion
-------------------

At the moment, the only types that are converted implicitly are the built-in primitive types,
especially the number types. A number type can be silently converted to any other number type that
is capable of containing all of the values of the source type.

.. note:: There is not yet a means by which implicit casts can be added to user-defined types,
  although such a feature is under consideration.

Unconditional casting
---------------------

An unconditional cast is one that either succeeds or throws an :exception:`InvalidTypeError`
exception.

The :func:`typecast` template function is used to do unconditional casts::

  let str = typecast<[String]>(inVal);

Of course, the template parameters can be omitted if the type can be deduced by the compiler::

  let str:String = typecast(inVal);

Unconditional casts work with disjoint types as well as pointer types:

  // A variable which can either be an int or a float.
  let number:int or float = 10;
  
  // Cast to int, if it wasn't an int then throw InvalidTypeError.
  let intVal:int = typecast(number);

Application programmers are free to create additional overloads of :func:`typecast` for their own
types.





For downcasting of reference types, the keyword :keyword:`as` is used to do a
dynamic cast::

  var obj:Object;
  var s = obj as String; // s is null if obj is not a String

The template function :func:`typecast` works similarly, except that it throws an exception if the
object is not the correct type::
  
  var obj:Object;
  var s = typecast<[String]>(obj); // throw exception if obj is not a String.
  var s:String = typecast(obj);   // Same as above, except return type is inferred.

The :func:`typecast` function can of course be overridden for specific types.

For converting value types, there are various conversion constructors and
conversion functions::

  var i:int = 1;
  var f = float(i); // Convert int to float
  
Conditional casts
-----------------

The :keyword:`as` operator represents one type of *conditional cast*,
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
the data otherwise. One way to achieve this is to use the implied
conditional cast mentioned above::

  if let i:int = a {
    // a is an int
  }
  
In general, any type conversion syntax that works on reference types
also works on union types. So just as you can call the ``cast`` function
to convert from a superclass to one of its children, you can also use
the same function to convert from a union type to one of its contained
types.

One common usage pattern for union types is used to indicate that a variable
may optionally contain a value::

  // Either holds an int, or it doesn't.
  var a:int or Nothing;

In this case, the special :ctype:`Nothing` type (which means the same as
C++ ``void``) means that the variable might not contain a value at all.
Note that :ctype:`Nothing` is not the same as ``null``, which is a pointer
that points to address zero. (Null pointers are possible in Tart, but
highly restricted compared to C++.)

Type testing
------------

Sometimes you need to test the type of a variable, the :keyword:`isa` keyword
can be used for this. It works for both reference types and union types::

  if a isa float {
    // ...
  }
