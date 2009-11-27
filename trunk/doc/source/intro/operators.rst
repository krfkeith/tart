.. highlight:: tart
  :linenothreshold: 30

Operators
=========

Tart supports a rich set of operators:

.. csv-table:: Table of Operator Precedence
   :header: "Precedence", "Operator", "Description"
   :widths: 4, 10, 8

   "0", "``::``", "(application defined)"
   "1", "``or``", "logical or"
   "2", "``and``", "logical 'and'"
   "3", "``in``, ``not in``", "membership test"
   "4", "``as``, ``is``, ``is not``", "type testing and conversion"
   "5", "``==``, ``!=``, ``>``, ``<``, ``>=``, ``<=``, ``>?``, ``<?``, ``>=?``, ``<=?``", "relational operators"
   "6", "``|``", "bitwise 'or'"
   "7", "``^``", "bitwise 'xor'"
   "8", "``&``", "bitwise 'and'"
   "8", "``>>``, ``<<``", "shift operators"
   "9", "``+``, ``-``", "additive operators"
   "10", "``*``, ``/``, ``%``", "multiplicative operators"
   "11", "``**``", "exponentiation"
   "12", "``..``", "range operator"
   "13", "``-``, ``not``, ``++``, ``--``, ``typeof``", "unary prefix operators"
   "14", "``++``, ``--``", "unary postfix operators"

Operator Overloading
--------------------

Tart supports a limited form of operator overloading. This is important for
things like complex number classes, or vectors - data types which need a way
to define mathematical operations which shouldn't be built in to the language.

Unlike C++, however there's no special syntax for operator overloading. Quite
the converse - the '+' operator is merely syntactic sugar for a call to the
method :meth:`infixAdd`. The :meth:`infixAdd` functions defines overloads for
all of the appropriate built-in types, and new overloads for user-created types
can be added, either as static overloads or as generic functions. Of course, the
compiler will try to inline such functions whenever possible, so that ``1 + 2``
will still be a compile-time constant.

For example, we can define string concatenation with the '+' operator::

  def infixAdd(s0:String, s1:String) -> String {
    var result = String.concat(s0, s1);
  }

Tart doesn't allow you to arbitrarily define new and strange operators like
some languages do. The reason is because the purpose of a programming language is to communicate with other programmers as well as communication with machines.
Having a standardized set of punctuation symbols that every programmer can
instantly recognize is an essential part of creating a robust culture of
software developers, and the freedom to reinvent the language syntax hinders
that cultural formation.

But for people who want to experiment with DSLs (Domain Specific
Languages), Tart has a set of semantically unassigned operators - that is,
built-in operators such as ':=' which have no assigned meaning. These
operators are available for application use.

.. note::

  You might wonder what prevents people from redefining these unassigned
  operators, or any operator for that matter, in such a way as to make the
  code obfuscated and hard to understand. From a technical standpoint, the
  answer is "nothing, really" - that is always a danger when operator
  overloading is permitted.
  
  However, the Tart reference manual defines a "standard meaning / standard
  metaphor" for each built-in operator. For example, the standard meaning of
  '+' is "1) summation of two mathematical objects, 2) concatenation".
  
  Programmers are encouraged to stay within the boundaries of the standard
  metaphor when defining new behavior for an operator. It will be up to the
  individual programmers and their code reviewers to enforce this.

One set of operators defined by Tart are the 'possible relation' operators
which are used in interval arithmetic. So ``<=?`` means 'possibly less than'.

Not all operators are overloaded via generic functions. There are a few unary
operators that are defined as class methods.

The design criteria for whether an operator is implemented as a generic
function or a member function can be summarized in the following rule: "If
it's something done *with* the object, then it's a generic function; If it's
something the object *itself* does, then it's a member function." So for
example, "adding" is something that uses an object - one speaks of adding two
numbers, rather than (Smalltalk and Ruby notwithstanding) numbers adding
themselves together. On the other hand, the ability to "call" an object is
seen as a facility that the object itself provides.

To make an object callable, define a member function with no name::

  class Foo {
    def (s:String) {
      // ...
    }
  }

  f = Foo();
  f("Hey there");

You can also overload the array subscript operator. The syntax is similar to
defining a property::

  class StringMap<[%T]> {
    def [key:String]:T {
      get { /* getter code */ }
      set { /* setter code */ }
    }
  }

  m = StringMap();
  let result = m["first"];

