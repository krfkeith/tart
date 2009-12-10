.. highlight:: tart
  :linenothreshold: 30

.. index::
  pair: generic; functions

Generic Functions
=================

Like C++, Tart supports compile-time overloading of functions. So for example,
suppose you have a :meth:`toString` function that does something different
depending on whether it is given an int, a float, or a string::

  def toString(val:int) -> String {
    // Convert int to string
  }

  def toString(val:float) -> String {
    // Convert float to string
  }

  def toString(val:String) -> String {
    return val;
  }

This works well, but only if the type of the argument is known at compile
time. What if we want to select the method based on the actual runtime type of
the object, as opposed to merely it's declared type?

We can tell Tart to dynamically dispatch based on the actual type of the
argument by using the ``virtual`` keyword. But wait -- it's not like C++! We
use the virtual keyword on the argument, to indicate that this argument is
dynamically dispatched::

  // Declare a generic function
  def toString(virtual val:object) -> String;

  // Specialize for some type. We don't need to say virtual again.
  overload toString(val:HashTable) -> String {
    // Convert HashTable to string
  }

Note that if the compiler can figure out at compile time which function would be
called, then it won't use multi-method dispatch. So for example, if I say
:samp:`toString(5)`, it will know to use the ``int`` version, since integers
can't be subclassed.

Also, it will only use dynamic dispatching for types which are subclasses of
the ``virtual`` argument. So since :ctype:`HashTable` is a subclass of
:ctype:`Object`, it will use dynamic dispatching for all arguments of type
:ctype:`HashTable`.

It gets even more interesting if you have a function where some arguments are
virtual and some are not. In this case, what happens is that it tries to find
all of the matching overloads at compile time based on the declared types of
the arguments. If one or more of those functions has some arguments that are
declared virtual, then it will then use dynamic dispatching at runtime to
select the correct specialization based on the runtime types of those
arguments.

