.. highlight:: tart
  :linenothreshold: 30

.. index::
  single: let
  single: var

Declaring variables and functions
=================================

Variable Declarations
---------------------

In Tart, there are three keywords that are most often used to declare a named value:
:kw:`var`, :kw:`let`, and :kw:`def`.

 * :kw:`var` defines a mutable variable.
 * :kw:`let` defines an immutable variable.
 * :kw:`def` defines a function or property.

Let's start by looking at :kw:`let` and :kw:`var`. Both allow the
type of a declaration to be deduced from its initialization expression::

  // Type can be explicitly specified.
  let a1:int = 1;
  var a2:String = "Hello, World";

  // Or it can be automatically deduced from the initialization expression.
  let a1 = 1;
  var a2 = "Hello, World";

  // For a 'var', you don't have to specify a value, but for 'let' you do.
  var c1:int;
  let c2:int; // ERROR -- no value specified.
  var c2;     // ERROR -- no type and no way to infer it.

As you can see, you don't have to include the type if it can be inferred by
the compiler.

.. note:: In the case of :kw:`let`, the compiler may or may not allocate
    storage for the variable, depending on the circumstance.

.. index:: array

The type of a variable can be a complex type expression. For example, array
variables are declared using square brackets after the type name::

  // Allocate an array of 10 integers
  var a1:int[] = int[10];

Another example of a type expression is a *disjoint* or *union* type::

  // Variable can be either an integer or a String
  var value:int or String;

The :kw:`let` and :kw:`var` keywords can be used almost anywhere.
For example, they can be used inside a class to declare a class or struct
member, as we saw before. They can also be used to declare local variables::

  def square(x:int) {
    let x2 = x * x;
    return x2;
  }

Note that even though :kw:`let` declares an immutable value, it does not
have to be a compile-time constant. What :kw:`let` really means is "bind
this name to this value in this scope". Once bound, the binding cannot be
changed. (In fact, if you only use :kw:`let` and never use
:kw:`var`, you are essentially doing pure functional programming.)

Like many programming languages, you aren't allowed to put an assignment in
a conditional expression::

  var m:Match;
  if m = re_ident.match(s) { // ERROR - assignment not allowed here
    return m.group();
  }

You can, however, introduce a new variable inside a conditional expression
using :kw:`let` or :kw:`var` and assign to it. The scope of the
variable includes the body of the :stmt:`if`-statement::

  if let m = re_ident.match(s) {
     return m.group();
  }

In the case of the 'for...in' statement, there's an implicit :kw:`let`
for the iteration variable::

  for a in 1..10 {
    // do something
  }

.. index::
  single: def
  pair: function; declaring

Function Declarations
---------------------

You can declare variables of function type using the :kw:`fn` keyword, which introduces an
anonymous function type declaration. Here's how you might combine :kw:`let` and :kw:`fn`
to declare a function::

  let f1 = fn (x:int, x:int) -> int {
    sys.stdout.println("Hello, World!");
    return 1;
  }

The symbol ``->`` means "returns type". It's used to indicate the return type
of a function. So ``fn (x:int, y:int) -> int`` is actually the complete
type of the function.

Using the :kw:`let` keyword to define functions is kind of cumbersome though, so we
have :kw:`def` which is a shortcut::

  def f1(x:int, y:int) -> int {
    sys.stdout.println("Hello, World!");
    return 1;
  }

If you leave off the return type, the compiler assumes that the function's return type is
:ctype:`void`::

  def f1(x:int, y:int) {
    sys.stdout.println("Hello, World!");
  }

.. index::
  single: def
  pair: property; declaring

Property Declarations
---------------------

The :kw:`def` keyword can also be used to define a *property*. Properties
are like variables, except that they are implemented using functions. Whenever
you attempt to read from the property, the property's ``get`` function will be
invoked, and when you attempt to modify the value, the ``set`` function
will be called.::

  def myProp:int {
    get { return p; }
    set (value) { p = value; }
  }

  myProp = 1;   // Calls "myProp.set(1)"

.. topic:: Let, Var, and Def

  At this point, you may be wondering why have three keywords - :kw:`let`,
  :kw:`var` and :kw:`def` - when in fact with a smart compiler, one
  keyword could do the job. Indeed, an earlier version of Tart didn't use any
  keyword at all, just a punctuation symbol to introduce a new name. The problem
  with this earlier syntax is that the syntax was *too* regular - it was hard to
  tell one part of the program from another.

  Tart's syntax is designed for humans as well as machines. One consequence of
  this is that the grammar is deliberately inconsistent - not *greatly*
  inconsistent, but just enough to provide a certain amount of variety to make
  common coding idioms pop out visually. Similarly, punctuation characters are
  used judiciously, just enough so as to support quick visual recognition of the
  structure of the code.

  Lots of things can be omitted if they aren't needed. The goal is to have
  a syntax that allows the programmer to express their intent clearly, without
  a lot of clutter getting in the way. But not so terse and compressed as
  to look like line noise.

  In other words, the goal is to have something like "executable pseudo-code".
