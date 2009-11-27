.. highlight:: tart
  :linenothreshold: 30

.. index:: templates, parameterized types, pattern variable, generic types
.. index:: metaprogramming

Templates and Metaprogramming
=============================

Tart allows functions (including macros) and user-defined types (including classes, structs,
interfaces and protocols) to be templates. Tart templates are "pattern-based", in that the list of
template parameters for a type are specified with a list of patterns to be matched.

Here's a simple example of a template::

  class Example<[%T]> {
    var x:T;
  }
  
  var x:Example<[String]>;

The two-character sequence ``<[`` starts a template parameter list, and ``]>`` terminates the list.
The same syntax is used both for definition template parameters and explicitly invoking templates.

Each template parameter is a pattern to be matched. Patterns can either be a type expression (which
match a type), or a value expression (which match a constant value).

An identifier that begins with a percent-sign ``%`` is a *pattern variable*. By default a pattern
variables will match any type expression, and the matched expression will be bound to that variable.
Thus, in the above example, the pattern variable ``T`` will be bound to type :class:`String`.

Note that the percent-sign prefix is only used within the template parameter list. In the template
body, the variable is used without the prefix.

Pattern variables can be used within complex type expressions::

  class Example2<[List<[%T]>]> {}
  
  var x:Example2<[List<[String]>]>;
  var x:Example2<[String]>; // ERROR -- template argument does not match.
  
This example requires that the template argument be a List of some type ``T``. The ``T`` parameter
will be bound to the element type of the list.

.. index:: explicit specialization

Explicit specialization
-----------------------

A template argument list need not contain any pattern variables. This is generally only useful when
you want to create an explicit specialization of a generic type.::

  class Example3<[List<[%T]>]> { /* non-string version. */ }
  class Example3<[List<[String]>]> { /* string version. */ }

In the above example, if the template argument is :class:`String` it will use the second template;
if it is not, then it will use the first.

Function templates
------------------

For functions, the syntax is similar::

  def forall<[%T]>(Iterator<[T]>) { /* ... */ }

The example shows a function that takes an iterator of some type.

Implicit specialization
-----------------------

When a template is used, its arguments can be specified either explicitly via a template argument
list, or implicitly based on function arguments. For example::

  // Return the lesser of two values.
  def min<[%T]>(v0:T, v1:T) -> T;

  let x = min(1, 2);

In the above example, the template arguments did not need to be specified, as the compiler was able
to deduce the type of the pattern variable (``int`` in this case) by matching the types of the
function arguments.

Template parameters can also be deduced from the return type of a function. For example, we might
have a function :func:``emptyList`` which decides what kind of list to return based on what it is
being assigned to::

  namespace Collections {
    def emptyList<[%T]>() {
       return List<[T]>();
    }
  }

  let x:List<[String]> = Collections.emptyList();

Template instantiation
----------------------

What happens when a template is instantiated? The Tart compiler generates a clone of the template
for each unique combination of template arguments. This is similar to what C++ does.

The cloning of a template occurs before evaluation and type resolution. This
allows for a programming technique called *duck typing*. The term comes from
Python, and is inspired by the old adage "if it looks like a duck, walks like a
duck, and quacks like a duck, then it's a duck." In the context of templates it
means that if a template expects a class with a "quack()" method, then any class
having a quack() method will work, regardless of what its base classes are.

.. note:: This is different than Java's 'erasure' concept in which the
  template is compiled in such a way as to be type-neutral, meaning that the
  same code is used to operate on different types.  This greatly reduces
  code bloat, but unfortunately it severely limits what can be done with
  templates. When you declare a template parameter, you generally have to
  specify a base type or *upper bound* of the template argument. Only classes
  which are subtypes of the upper bound can be passed as template arguments,
  and the only methods you can call on that argument are those that are
  defined by the upper bound.

  For Tart, the problem of code bloat is addressed in a different way - by
  attempting to collapse copies template that generate identical code. (That
  is the eventual plan, anyway.)

Non-type template arguments
---------------------------

To declare a non-type template parameter, add a type specifier just as would be used to define the type of a variable::

  def func<[%N:int]>();
  
  func<[4]>();

The template argument must be a compile-time computable.

Template guard conditions
-------------------------

A guard condition is an additional restriction on the kind of value that can be bound to a template.
For example, suppose we wanted a template parameter to only bind to subclasses of "Node", so we
define a restriction using the subclass test operator ``<:``::

  class Visitor<[%T require %T <: Node]> { /* ... */ }
  
This also works with non-type arguments::

  class Visitor<[%A:int require %A <= 10]> { /* ... */ }

The "require" clause must come after all of the template arguments::

  class Visitor<[%A, %B require %A <: %B]> { /* ... */ }

However, the require clause can also introduce new pattern variables. Here's a more realistic
example, where we want to define a template that operates on all subclasses of iterators of a given
type::

  def forall<[%IterType require %IterType <: Iterator<[%T]>]>(iter:IterType);

Because expressions like this are so common, there is a shortcut syntax which allows us to omit the
"requires" keyword::

  def forall<[%IterType <: Iterator<[%T]>]>(iter:IterType);
  class Visitor<[%A:int <= 10]> { /* ... */ }

The general rule is as follows: A template argument consisting of a binary operator expression,
where the left-hand side of the expression is a pattern variable, will be expanded to that variable
plus a guard condition consisting of the original expression.

Thus:

.. code-block:: none

  %X <oper> <expression>
  
will be expanded to:

.. code-block:: none

  %X requires %X <oper> <expression>

More sophisticated guard conditions using protocols
---------------------------------------------------

Tart supports the notion of a *protocol*, which is a contract or that a type must adhere to. A type
is said to *conform* to a protocol if that type possesses all of the members of that protocol.

.. Note:: This use of the word "protocol" has nothing to do with network
  protocols, and has more in common with diplomatic protocols, meaning a
  set of guidelines for interaction between two parties.

A protocol is much like an interface, and in fact the syntax for declaring them is very similar::

  interface Iterator<[%T]> {
    def next() -> T;
  }

  protocol AbstractIterator<[%T]> {
    def next() -> T;
  }

Like an interface, a protocol can declare abstract methods, properties and types within its body.
Any non-abstract class which inherits from the protocol must supply an implementation of all of the
method and properties, otherwise an error will result.

However, there are some important differences between interfaces and protocols:

* A protocol declaration does not create a new type - you cannot declare a variable or function
  parameter whose type is the protocol.
* A protocol does not affect the runtime representation of a class in any way, unlike interfaces
  which create a dispatch table for each concrete type that implements the protocol.
* A class does not have to explicitly declare itself as implementing a protocol - any class that
  has the right set of methods and properties can be considered to conform to the protocol.
  (However, in most cases it will be useful to declare the protocol explicitly - both for purposes
  of documentation and support for automated code refactoring.)
* Protocols are not automatically polymorphic - if class A and class B both conform to a given
  protocol, this does not mean that they have a common base class - it merely means that they both
  conform to the same contract.

In other words, a protocol is purely a contract, a set of constraints that a given type must adhere
to. As such, it is a much weaker set of guarantees than an interface which creates a new type and an
inheritance relationship.

So why would you want to use a protocol rather than an interface? There are three advantages that
protocols have over interfaces:

* A protocol can be added to a class without modifying the original class definition, either
  implicitly or explicitly using 'extend'. (The 'extend' keyword will be discussed later.)
* A protocol incurs no additional runtime overhead.
* A protocol can be used with value types (structs) as well as reference types. Interfaces can
  only be used with reference types (classes).

Protocols will most often be used in template guard conditions::

  class IteratorWrapper<[%IterType <: AbstractIterator<[%T]>]> {}

What this example says is that the IteratorWrapper can only be used with types that conform to the
AbstractIterator protocol. Since the only constraint that is imposed by ``AbstractIterator`` is that
the class have a method ``next`` which returns a type T, what this guard condition effectively does
is test whether ``IterType`` has a ``next`` method, something that would be hard to do otherwise.

.. note::
  The "protocol" feature is similar in spirit to C++0x's "concepts" feature,
  although there are some differences.

Goals for Templates
-------------------

C++ templates are powerful, but many programmers find them to be complex. The
truth is that C++ templates aren't hard to understand because they are too
powerful - they are hard to understand because they are *not quite powerful
enough*. A lot of the complexity of C++ templates (as seen if you've ever
tried to read the source code to BOOST) comes from trying to get around their
limitations.

One goal of Tart's template system is to be able to generate new classes from
old. Say for example you want to take an abstract interface and generate a
mock implementation for testing. This mock implementation merely records all
of the method calls and plays them back later for verification. In Java, this
can be done via run-time creation of classes using reflection. In Tart, we do
this at compile time by using a template that invokes a mapping transformation
on the members of the class. The input to the template is a class (the
interface), and the output is a class (the mock implementation). For each
public member of the input class, we apply an inner template to it, producing
a mock implementation of that member; the concatenation of all of those
resulting members is the output class.

The same technique could be used to generate call stubs for RPC calls.
