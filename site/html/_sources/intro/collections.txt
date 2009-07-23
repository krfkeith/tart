.. highlight:: tart
  :linenothreshold: 30

.. index:: collections

Collections
===========

The Tart core library contains a rich set of collection classes which include a variety of sequence,
map, and set collections.

======================= ==============================================================
Collection Class        Description
======================= ==============================================================
:ctype:`Collection`     Interface for collections that can be enumerated and appended.
:ctype:`List`           Interface for appendable sequences.
:ctype:`Map`            Interface for map types.
:ctype:`Set`            Interface for set types.
:ctype:`Array`          A fixed-length array of elements.
:ctype:`ArrayList`      A list which is backed by an array.
:ctype:`ImmutableList`  A read-only List.
:ctype:`ImmutableMap`   A read-only map.
:ctype:`ImmutableSet`   A read-only set.
:ctype:`HashMap`        A map that is implemented by a hash table.
:ctype:`HashSet`        A set that is implemented by a hash table.
:ctype:`EnumSet`        A set whose values are restricted to a given enumeration.
:ctype:`EnumMap`        A map whose keys are restricted to a given enumeration.
======================= ==============================================================

.. digraph:: foo

  ranksep = 0.5;
  node [shape="box", fontsize=9, height=.3, margin="0.2,0.1"];
  edge [dir=back];

  "Collection" -> "Array";
  "Collection" -> "List";
  "Collection" -> "Set";
  "Collection" -> "Map";
  "List" -> "ArrayList";
  "List" -> "ImmutableList";
  "Map" -> "ImmutableMap";
  "Map" -> "HashMap";
  "Map" -> "EnumMap";
  "Set" -> "ImmutableSet";
  "Set" -> "HashSet";
  "Set" -> "EnumSet";
  "Collection" [peripheries=2]
  "List" [peripheries=2]
  "Set" [peripheries=2]
  "Map" [peripheries=2]

See the :ref:`collections-library` section for more information about the collection classes.

.. index::
  pair: literal; collection
  pair: literal; array
  pair: literal; list
  pair: literal; set

Collection Literal Syntax
-------------------------

All of the list and set collections in the core collections library have a constructor which takes
a variable-length list of initializers::

  // ArrayList of integers
  let a0 = ArrayList(1, 2, 3);
  
  // Immutable list of strings.
  let a1 = ImmutableList("Hydrogen", "Helium", "Lithium");
  
  // Hash set of strings, with explicitly specified type.
  let a2 = HashSet<[String]>("Clubs", "Spades", "Hearts", "Diamonds");
  
In most cases, the type of the collection can be deduced either from the items in the initializer
list, or the type of the variable being assigned to (if specified). However, you can also specify
the type explicitly, as seen in the last line in the previous example.

In some cases, the collection takes additional parameters as keyword-only arguments::

  // ArrayList of integers with initial size 3, reserved size 10.
  let a0 = ArrayList(1, 2, 3, reserve=10);
  
There is also a shortcut syntax, which can be used when the list type is deducible::

  let a0:int[] = {1, 2, 3};
  let a1:ImmutableList<[String]> = {"Hydrogen", "Helium", "Lithium"};
  let a1:HashSet<[String]> = {"Clubs", "Spades", "Hearts", "Diamonds"};
  
The initializer-list syntax (consisting of a comma-separated list, surrounded by braces) is
automatically transformed by the compiler into the corresponding constructor call. This also
works for function arguments::

  def append(childNodes:List<Element>);
  
  append({
    SpanElement(),
    AnchorElement(),
    DivElement()});
    
However, in most cases it is simpler to use variadic arguments::

  def append(childNodes:Element...);
  
  append(
    SpanElement(),
    AnchorElement(),
    DivElement());

The list and set classes also have a :meth:`from` method, which takes an Iterable instead of
a list of items::

  // Fill up an array with the squares of the numbers from 0 to 9.
  let a0 = ArrayList.from(x * x for x in 0 .. 10);

.. index::
  pair: literal; map

Map Literals
------------

Map literals are handled a little differently, because the key and the value may not be the same
type. The :oper:`->` operator is used to indicate a key/value pair. This special meaning of the
:oper:`->` operator is only valid within a map initializer list - otherwise it is used to indicate
a function return type.

  let f:HashMap = {
    "Apple" -> 1,
    "Cherry" -> 2,
    "Lemon" -> 3 };

This is translated by the compiler into the following code::

  let f:HashMap<[String, int]> = HashMap<[String, int]>(
    HashMap<[String, int]>.Entry("Apple", 1),
    HashMap<[String, int]>.Entry("Cherry", 2),
    HashMap<[String, int]>.Entry("Lemon", 3));
    
In other words, it converts the initializer list into an array of "Entry" structures, each
containing a key/value pair, which is then passed to the map's constructor. The collection
may or may not actually use the "Entry" class internally - it is free to choose whatever internal
representation is convenient.

.. index:: generator expressions, comprehensions

Generator expressions and comprehensions
----------------------------------------

Tart supports "generator expressions" similar to those found in Python. A generator expression
produces an iterator, which can be passed directly to the :meth:`from` method of a collection::

  let s0 = ImmutableSet<[int]>.from(x * x for x in 0 .. 10);
  
The initializer-list syntax is also supported, as is the map key/value operator::
  
  let s0:ImmutableSet<[int]> = {x * x for x in 0 .. 10};
  let m0:ImmutableMap<[int, int]> = {x -> x * x for x in 0 .. 10};

Sequence Unpacking
------------------

Tart supports a Python-like ability to unpack variables from a sequence::

  let a, b = [1, 2];
  let a:int, b:int = [1, 2];

The last variable in the unpacking assignment can be a variadic argument, meaning it scoops up all
the remaining values::

  let a:int, b:int... = [1, 2, 3, 4];

As with function arguments, the '...' syntax changes the type of the argument into an array of the
explicitly declared type. So the type of ``b`` is actually ``int[]``, and would in the above example
be assigned the value ``[2, 3, 4]``.

Variable unpacking works with regular assignment as well, allowing for the Python 'swap idiom' to
exchange the values of two variables::

  a, b = b, a;

The sequence-unpacking syntax is what allows you to return multiple values from a function::

  def returnStringAndInt() -> (String, int) {
    return "Hello", 12;
  }

  let a:String, b:int = returnStringAndInt();

.. note:: Unlike Python, a function that returns multiple values does not actually
  create a 'tuple' object for the return results. Instead, the values are returned
  either in registers or on the stack - the exact mechanism is dependent on the
  platform ABI.
