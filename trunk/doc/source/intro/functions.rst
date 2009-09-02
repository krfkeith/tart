.. highlight:: tart
  :linenothreshold: 30

.. index::
  pair: function; type
  keyword: def
  keyword: fn

Function Type Expressions
=========================

The keyword :keyword:`fn` declares an expression having function type. It
can be used to declare a named function or an anonymous function (also known
as a 'function literal'.)

Normally, its easier to use :keyword:`def` to define a function and bind it to
a name in one step. Def requires that the function have a *name*, however, and
when working with with function *types* (where we don't know the name of the
specific function) then we have have to use :keyword:`fn`.

The :keyword:`fn` keyword is optionally followed by an argument list. Usually
the argument list will be in parentheses, however if there is only a single
argument, or no arguments at all, then the parentheses can be omitted::

  fn -> int;          // No-argument function
  fn () -> int;       // No-argument function (alternate form)
  fn x:int -> int;    // Single argument function
  fn (x:int, y:int) -> int; // Two argument function
  
If you're just declaring the type of a function, you can omit the argument
name - but you still have to include the colon before the type name::

  fn :int -> int;   // Single argument function type
  
.. index::
  pair: function; return type

Function return type
--------------------

The '->' operator means 'returns value of type', and is right-associative.
This allows you to define functions that return functions::

  fn x:int -> fn y:int -> int;
  fn x:int -> fn (y:int, z:int) -> int;

.. note:: Note for Haskell fans - the use of the ``->`` operator looks similar
  to the way functions are defined in Haskell, but it's actually quite
  different. In Haskell, functions can only have a single argument, and
  support for multiple arguments is done via currying. There are various
  reasons why Tart doesn't do this -- for one thing, it makes it difficult to
  support calls to the native ABI.

.. index::
  pair: function; void return

A function returning no value (i.e. a pure procedure) can be specified in several ways::

  fn :int -> void;    // A function with no return value.
  fn :int;            // Defaults to void
  fn :int -> ();      // A function returning no values.
  fn;                 // A function with no arguments and no return.

If you don't declare a return type, it is assumed to be :ctype:`void` -
unless the function has a body containing a :stmt:`return` statement.
If there is a :stmt:`return` statement, then the type of the value being
returned determines the function's return type. If there's more than one
:stmt:`return` statement, then it attempts to choose a return type that
encompasses all of them, or signals an error if it can't.

.. index::
  pair: return; multiple values

Multiple return values
----------------------

Functions can also return more than one value. Because the comma operator has a low precedence,
you'll have to put parentheses around the list of return types if there is more than one::

  fn :int -> (int, int);          // A function returning two values
  fn :int -> (String, int, bool); // A function returning three values
  fn :int -> ();                  // A function returning no values (returns void)

What's nice about using the multiple-return-value syntax (as opposed to passing
arguments by reference, a technique often used in C++) is that the compiler can
know for certain that the variable has changed. This allows the compiler to do
additional optimizations::

  var a:String, b:int
  a = 10; // Dead code
  a, b = returnStringAndInt();

If ``a`` had been passed as a reference and filled in by the function, the
compiler wouldn't be able to know that the second line was dead code - because
the compiler can't determine whether ``a`` was actually filled in or not.

.. index::
  pair: function; arguments
  pair: keyword; arguments

Function arguments
------------------

In addition to regular positional arguments, functions support both
keyword and variadic arguments. Variadic arguments are indicated via an
ellipsis (``...``)::

  fn (format:int, args:int...);

Variadic arguments must be the last positional argument. The actual type of a
variadic argument is a variable-length array of the given type. So in the
example above, the actual type of ``args`` would be ``int[]``.

Default argument values are specified using ``=``::

  fn (format:String, npos:int=0, sep=false);

Function arguments use the same kind type deduction as :stmt:`var` and
:stmt:`let` statements: if the type of an argument is unspecified, the type will
be derived from the type of the default value.

Like Python, parameters can be referred to by name as well as position::

  print("Hello, World!\n", padding="");

Any parameter can be referred to by its keyword name. The normal mapping of
arguments to formal parameters is that positional arguments are assigned
first, in order, and then any keyword arguments are assigned to any remaining
unfilled parameters.

Sometimes it is useful to specify a parameter that is "keyword only" meaning
that it can only be specified via keyword instead of positionally. A semicolon
can be used to segregate regular positional parameters from keyword-only
parameters::

  def print (format:String; npos:int=0, sep=false);
  
  print("Hello world!", npos=1); // OK
  print("Hello world!", 1); // ERROR - too many positional arguments

In the above example, only the ``format`` argument will be filled in by
positional argument - to have additional positional arguments in this case
would be an error.

Functions as Arguments
----------------------

How would you declare the type of a function that takes another function as an
argument? Like this::

  fn (:fn i:int) -> int;
  
.. note:: The parentheses are needed because the precedence of the ``->``
  operator is higher than that of the :keyword:`fn` keyword.

.. index::
  pair: function; anonymous
  pair: function; literal

Anonymous Functions
-------------------

If a function type declaration is followed by a function body, the result
is an *anonymous function*::

  let square = fn (x:int) -> int { return x * x };

Here's an example of a function with no arguments and void return type being
used as a callback to a timer. As you can see, the syntax is minimal::

  timer.start(1000, fn { alarm.trigger(); });
