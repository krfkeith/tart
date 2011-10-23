.. highlight:: tart
  :linenothreshold: 30

.. index:: operators

Operators
=========

Tart supports a rich set of operators:

.. csv-table:: Table of Operator Precedence
   :header: "Precedence", "Operator", "Description"
   :widths: 4, 10, 8

   "0", ":oper:`::`", "(application defined)"
   "1", ":oper:`or`", "logical or"
   "2", ":oper:`and`", "logical 'and'"
   "3", ":oper:`in`, :oper:`not in`", "membership test"
   "4", ":oper:`isa`, :oper:`is`, ``is not``", "type testing and reference equality"
   "5", ":oper:`==`, :oper:`!=`, :oper:`>`, ``<``, :oper:`>=`, ``<=``, :oper:`>?`, ``<?``, :oper:`>=?`, ``<=?``", "relational operators"
   "6", ":oper:`|`", "bitwise 'or'"
   "7", ":oper:`^`", "bitwise 'xor'"
   "8", ":oper:`&`", "bitwise 'and'"
   "8", ":oper:`>>`, :oper:`<<`", "shift operators"
   "9", ":oper:`+`, :oper:`-`", "additive operators"
   "10", ":oper:`*`, :oper:`/`, :oper:`%`", "multiplicative operators"
   "11", ":oper:`**`", "exponentiation"
   "12", ":oper:`..`", "range operator"
   "13", ":oper:`-`, :oper:`not`, :oper:`++`, :oper:`--`", "unary prefix operators"
   "14", ":oper:`++`, :oper:`--`", "unary postfix operators"

.. index::
  pair: operator; overloading

Operator Overloading
--------------------

Tart supports a limited form of operator overloading. This is important for
things like complex number classes, or vectors - data types which need a way
to define mathematical operations which shouldn't be built in to the language.

Unlike C++, however there's no special syntax for operator overloading. In
fact the :oper:`+` operator is merely syntactic sugar for a call to the
function :meth:`infixAdd`. There are definitions of :meth:`infixAdd` for
all of the built-in number types, and new overloads for user-created types
can be added, either as static overloads or as generic functions. Of course, the
compiler will try to inline such functions whenever possible, so that ``1 + 2``
will still be a compile-time constant.

For example, we can define string concatenation with the :oper:`+` operator::

  def infixAdd(s0:String, s1:String) -> String {
    var result = String.concat(s0, s1);
  }
  
Refer to the language reference manual section on :ref:`operator_overloading` to see all
of the operator functions and their names.

Tart doesn't allow you to arbitrarily define new and strange operators like
some languages do. The reason is because the purpose of a programming language
is to communicate with other programmers as well as communication with machines.
Having a standardized set of punctuation symbols that every programmer can
instantly recognize is an essential part of creating a robust culture of
software developers, and the freedom to reinvent the language syntax hinders
that cultural formation.

But for people who want to experiment with DSLs (Domain Specific
Languages), Tart has a set of semantically unassigned operators - that is,
built-in operators such as :oper:`::` which have no predefined meaning. These
operators are available for general application use.

.. note::

  You might wonder what prevents people from redefining these unassigned
  operators, or any operator for that matter, in such a way as to make the
  code obfuscated and hard to understand. From a technical standpoint, the
  answer is "nothing, really" - that is always a danger when operator
  overloading is permitted. However, Tart defines a set of conventions
  for the use of operators that all users of the language are encouraged to follow.
  These conventions are defined in the language reference manual, in the
  section on :ref:`operator_overloading` 

.. index::
  pair: callable; objects

Callable Objects
----------------

To make an object callable, define a member function with no name::

  class Foo {
    def (s:String) {
      // ...
    }
  }

  f = Foo();
  f("Hey there");

.. index::
  pair: subscriptable; objects

Subscriptable Objects
---------------------

You can also overload the array subscript operator. The syntax is similar to
defining a property::

  class StringMap[%T] {
    def [key:String]:T {
      get { /* getter code */ }
      set { /* setter code */ }
    }
  }

  m = StringMap();
  let result = m["first"];
