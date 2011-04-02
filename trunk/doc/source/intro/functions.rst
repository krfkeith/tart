.. highlight:: tart
  :linenothreshold: 30

.. index::
  pair: function; type
  single: def
  single: fn

Function Type Expressions
=========================

The keyword :kw:`fn` declares an expression having function type. It
is often used to declare an anonymous function (also known as a
'function literal'), but it can also be used to declare a variable
containing a pointer to a function.

The :kw:`fn` keyword is optionally followed by an argument list. Usually
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

If you don't declare a return type, it is assumed to be :ctype:`void`.

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
  operator is higher than that of the :kw:`fn` keyword.

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

.. index::
  pair: function; closure

Closures
--------

Function literals that are declared in a local scope automatically become closures,
meaning that they retain references to the local variables that were in scope
at the point where the function was defined::

  // Returns a function that increments an counter and returns its value each time
  // it is called.
  def createCounter() -> fn -> int {
    var counter = 0;
    return fn -> int { return counter++; }
  }

In the above example, the closure captures the :cdata:`counter` variable. The compiler
will create a special 'closure cell' object containing the variable. The closure cell
is allocated on the heap rather than on the stack, so that it can continue to exist
after the outer function has returned.

.. note::
  The compiler only creates closure cells for function parameters and variables declared
  with :kw:`var`. For values declared with :kw:`let`, the compiler instead
  creates a copy of the variable in the closure itself, which is more efficient,
  since it avoids the extra heap allocation. Because values declared with
  :kw:`let` cannot be changed, there's no need for the closure and the
  enclosing function to share a reference to the same variable. Of course, if the variable
  is a reference type, then the closure will have a copy of the *pointer*,
  not the object itself, which will still be shared.

.. index::
  pair: Function; interface

The Function Interface
----------------------

Expressions of function type (as declared using the :kw:`fn` keyword) can be treated
like objects that implement the :interface:`Function` interface. In fact, the
:kw:`fn` keyword is simply a shortcut - so for example, the declaration::

  var func:fn (:int, :int) -> String;
  
is actually equivalent to::

  var func:Function[String, int, int];

What this means is that any class that implements the :interface:`Function` interface can
be called like a function, and can be assigned to any variable of function type.
