.. highlight:: tart
  :linenothreshold: 30

Expressions
===========

.. index::
  pair: expression; primary

Primary Expressions
-------------------

.. productionlist::
   primary: "(" `expression` ")" | `ident` | `literal`

Object Creation Expressions
---------------------------

.. productionlist::
   object_create: `typename` `argument_list`
   
Member Access Expressions
-------------------------

.. productionlist::
   get_member: `expression` "." `ident`
   
Function Invocation Expressions
-------------------------------

.. productionlist::
   function_call: `expression` `argument_list`
   
Index Dereference Expressions
-----------------------------

.. productionlist::
   index_deref: `expression` "[" `expression` ("," `expression`)* "]"
   
Anonymous Function Expressions
------------------------------

.. productionlist::
   function_expr: "fn" (`argument_list` | `argument`) ["->" (`type_list` | `type`)] `statement`
   
Operators
---------

.. _operator_overloading:

Operator Overloading
^^^^^^^^^^^^^^^^^^^^

Most operators can be overloaded by the application. Some operators have no
built-in semantics at all; Their meaning is determined entirely by the
application.

One of the risks of allowing operators to be overloaded is that the
readability of a program may be obscured if the overloaded semantics
of an operator don't fit in with its standard meaning, or if the
metaphor of an operator is stretched too far. An example of the latter
is using the "+" operator to "add" a record to a database. Although
the English word "add" describes both the process of arithmetic summation
and the act of inserting an item into a collection, it is commonly
understood that the plus ("+") symbol refers only to the former.

For this reason, the documentation for each overloadable operator will attempt
to describe the standard meaning of each operator. It is suggested that programs that
redefine operators stay mostly within the bounds of these standard meanings.

..
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

Operator Precedence
^^^^^^^^^^^^^^^^^^^

.. csv-table:: Table of Operator Precedence
   :header: "Precedence", "Operator", "Description"
   :widths: 4, 10, 8

   "0", ":oper:`::`", "(application defined)"
   "1", ":oper:`or`", "logical or"
   "2", ":oper:`and`", "logical 'and'"
   "3", ":oper:`in`, :oper:`not in`", "membership test"
   "4", ":oper:`isa`, :oper:`is`, ``is not``", "type testing and reference equality"
   "5", ":oper:`==`, :oper:`!=`, :oper:`>`, :oper:`<`, :oper:`>=`, :oper:`<=`, :oper:`>?`, :oper:`<?`, :oper:`>=?`, :oper:`<=?`", "relational operators"
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

Binary Operators
^^^^^^^^^^^^^^^^

.. operator:: \:\:

  :Override: ``Operator.defines()``
  :Standard meaning: Introduce a new definition.
  
  The `defines` or `double-colon` operator is the lowest-precedence binary operator.
  It has no built-in semantics, and is available for application use.

  The standard meaning of the double-colon operator is to introduce a new name or
  item into an environment. The canonical example is a grammar file for
  a parser generator, where the ``::`` operator defines a new production.

.. operator:: or

  :Override: *not overridable*

  The logical 'or' operator. Attempts to convert the left-hand side to a
  boolean value; if the result is ``true`` then the result of the operation
  is ``true`` and the right-hand side is not evaluated. Otherwise, if the
  left-hand side is ``false``, then the result of the operation will be
  the right hand side expression, after evaluating it and converting it
  to a boolean type.

  It is an error if either side cannot be converted to a boolean type.
  
.. operator:: and

  :Override: *not overridable*

  The logical 'and' operator. Attempts to convert the left-hand side to a
  boolean value; if the result is ``false`` then the result of the operation
  is ``false`` and the right-hand side is not evaluated. Otherwise, if the
  left-hand side is ``true``, then the result of the operation will be
  the right hand side expression, after evaluating it and converting it
  to a boolean type.

  It is an error if either side cannot be converted to a boolean type.
  
.. operator:: in

  :Override: :meth:`Collection.contains` method.
  :Standard meaning: Test whether an item is contained within a collection.
  
  The membership test operator. The expression ``a in b`` is simply syntactic sugar for::

    b.contains(a)

.. operator:: not in

  :Override: :meth:`Collection.contains` method.
  
  This operator is simply the complement of the membership test operator. The expression
  ``a not in b`` is syntactic sugar for::

    not b.contains(a)

.. operator:: as

  :Override: ``Operator.asType<Type>``
  :Standard meaning: Treat an object reference as though it were a reference to another,
    compatible type, without modifying or transforming the original object.
  :Alternate meaning: Transform a value type into a different value type.
  
  The ``as`` operator is used to do dynamic casting. The expression ``value as type`` attempts
  to convert ``value`` to ``type``. The ``type`` is parsed as a type expression.
  
  The behavior of the ``as`` operator depends on whether the type on the right-hand side
  is a nullable type:

  * If it is nullable, then ``as`` functions as a dynamic cast: It either
    succeeds in casting the value to the specified type, or it returns a ``null`` pointer if
    the cast failed.
  * If the type on the right-hand side is not nullable, then the ``as`` operator will
    instead raise an exception if the cast fails.
    
  For example::

    def visit(node:Node) {
      // Cast the node to a container, if it succeeds then visit its children.
      if let container = node as ContainerNode? {
        // Since we tested the type for non-null, it's no longer nullable within
        // the if body.
        for child in container.children:
          visit(child);
      }
    }
    
  We could also have written::
  
    def visit(node:Node) {
      // Test to see if the node is a container
      if node is ContainerNode {
        // Convert to ContainerNode type. Since we did the test
        // above, it won't throw an exception.
        for child in (node as ContainerNode).children:
          visit(child);
      }
    }
    
.. operator:: is

  :Override: ``Operator.asType<Type>``
  :Standard meaning: Returns true whether the specified object can be cast to another type.
  
  The `is` operator tests whether an object can be cast to a given type. To put it another way,
  ``is`` returns ``true`` if the ``as`` operator would be able to succeed.
  
.. operator:: is not

  :Override: ``Operator.asType<Type>``

  This is simply the complement of the `is` operator. It's included because "not is" sounds
  wrong in English.
  
.. operator:: ==

  :Override: ``Operator.equal()`` function.
  :Standard meaning: Test for equivalence of two values.
  
  The `double-equals` operator tests whether two objects are *equivalent*, meaning
  that they have the same value, although they may in fact be distinct objects.
  For example, two strings which have the same text will be equal even though they
  are different strings.

.. operator:: ===

  :Override: *not overridable*
  
  The `triple-equals` operator tests two values for reference equality, in other words
  whether two objects of reference type are in fact the same object. (For value types,
  the reference equals operator functions exactly like the regular equals operator.)
  
.. operator:: !=

  :Override: ``Operator.unequal()`` function.
  :Standard meaning: Test for non-equivalence of two values.
  
  The :oper:`\!=` or `not-equals` operator is simply the complement of the
  :oper:`==` operator. It returns ``true`` if two objects are not equivalent.

.. operator:: >

  :Override: ``Operator.greater()`` function.
  :Standard meaning: Test which of two values has the greater value.
  :Alternate meaning: Test the natural ordering of two values.
  
  The `greater than` operator returns ``true`` if the left hand side's value is
  greater than the right hand side.

.. operator:: <
  
  :Override: ``Operator.less()`` function.
  :Standard meaning: Test which of two values has the lesser value.
  :Alternate meaning: Test the natural ordering of two values.

  The `less than` operator returns ``true`` if the left hand side's value is
  less than the right hand side.

.. operator:: >=

  :Override: ``Operator.greaterOrEqual()`` function.
  :Standard meaning: Test which of two values has the greater or equal value.
  :Alternate meaning: Test the natural ordering of two values.

  The `greater than or equal to` operator returns ``true`` if the left hand side's value is
  greater than or equal to that of the right hand side.

.. operator:: <=

  :Override: ``Operator.lessOrEqual()`` function.
  :Standard meaning: Test which of two values has the less or equal value.
  :Alternate meaning: Test the natural ordering of two values.

  The `less than or equal to` operator returns ``true`` if the left hand side's value is
  less than or equal to that of the right hand side.

.. operator:: >?

  :Override: ``Operator.possiblyGreater()`` function.
  :Standard meaning: Test whether one uncertain value may be greater than another.

  The `possibly greater than` operator returns ``true`` if the left
  hand side's value might be greater than that of the right
  hand side. This operator is intended for use with value types that
  represent uncertain quantities (for example, interval arithmetic.)

.. operator:: <?
  
  :Override: ``Operator.possiblyLess()`` function.
  :Standard meaning: Test whether one uncertain value may be less than another.

  The `possibly less than` operator returns ``true`` if the left
  hand side's value might be less than that of the right
  hand side. This operator is intended for use with value types that
  represent uncertain quantities (for example, interval arithmetic.)

.. operator:: >=?

  :Override: ``Operator.possiblyGreaterOrEqual()`` function.
  :Standard meaning: Test whether one uncertain value may be greater or equal to another.

  The `possibly greater than or equal to` operator returns ``true`` if the left
  hand side's value might be greater than or equal to that of the right
  hand side. This operator is intended for use with value types that
  represent uncertain quantities (for example, interval arithmetic.)

.. operator:: <=?

  :Override: ``Operator.possiblyLessOrEqual()`` function.
  :Standard meaning: Test whether one uncertain value may be less than or equal to another.

  The `possibly less than or equal to` operator returns ``true`` if the left
  hand side's value might be less than or equal to that of the right
  hand side. This operator is intended for use with value types that
  represent uncertain quantities (for example, interval arithmetic.)

.. operator:: |

  :Override: ``Operator.bitOr()`` function.
  :Standard meaning: The union of two values or logical sets.
  
  The `union` or :oper:`|` operator returns the bitwise-or of two integer values.

.. operator:: ^

  :Override: ``Operator.bitXor()`` function.
  :Standard meaning: The exclusive-or of two bit sets.
  :Alternate meaning: The difference of two logical sets.

  The `xor` or :oper:`^` operator returns the bitwise exclusive-or of two integer values.

.. operator:: &

  :Override: ``Operator.bitAnd()`` function.
  :Standard meaning: The intersection of two values or logical sets.

  The `intersection` or :oper:`&` operator returns the bitwise-and of two integer values.

.. operator:: >>

  :Override: ``Operator.rshift()`` function.

  The `right-shift logical` operator takes two arguments, both of which must be of integer type.
  The bits of the first argument are shifted in the direction of the least significant bit
  by an offset in bits equal to the second argument. The bits formally in the most
  significant position are replaced with zero bits if the left hand side is unsigned,
  or sign bits it is signed. The second argument is always treated as unsigned,
  regardless of its type.
  
.. operator:: <<

  :Override: ``Operator.lshift()`` function.

  The `left-shift` operator takes two arguments, both of which must be of integer type.
  The bits of the first argument are shifted in the direction of the most significant bit
  by an offset in bits equal to the second argument. The second argument is
  always treated as unsigned, regardless of its type.
  
.. operator:: +

  :Override: ``Operator.add()`` function.
  :Standard meaning: The sum of two mathematical objects.
  :Alternate meaning: The concatenation of two values.
  
  For numeric values, the `plus` operator returns their arithmetic sum.
  For string values, it returns the concatenation of the two strings (specifically, it calls
  :meth:`String.concat()`.)

.. operator:: -

  :Override: ``Operator.subtract()`` function.
  :Standard meaning: The difference of two mathematical objects.

  For numeric values, the `minus` operator returns their arithmetic difference.

.. operator:: *

  :Override: ``Operator.multiply()`` function.
  :Standard meaning: The product of two mathematical objects.
  :Standard meaning: The cross product of two sets.

  For numeric values, the `multiply` operator returns their arithmetic product.

.. operator:: /

  :Override: ``Operator.divide()`` function.
  :Standard meaning: The division of one mathematical object by another.

  For numeric values, the `divide` operator returns the quotient of the left hand side
  when divided by the right hand side.

.. operator:: %

  :Override: ``Operator.modulus()`` function.
  :Standard meaning: The mathematical modulus of one object by another.

  For integer values, the `modulus` operator returns the integer remainder of the left hand side
  when divided by the right hand side.

.. operator:: **

  :Override: *not overridable*

.. operator:: ..

  :Override: *not overridable*
  
  The `range` operator defines an interval between two values by constructing a new
  :class:`tart.core.Range` object out of its two arguments, both of which must
  be of the same type.
  
  Because :class:`Range` is a template, you can construct a range of almost any object
  type. However, it will generally only be meaningful to create a range from objects
  whose type defines some kind of ordering.  

Unary Operators
^^^^^^^^^^^^^^^

Postfix Operators
^^^^^^^^^^^^^^^^^

Constant Expressions
--------------------

Evaluation Order
----------------
