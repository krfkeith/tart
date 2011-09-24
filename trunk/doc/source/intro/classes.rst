.. highlight:: tart
  :linenothreshold: 30

.. index:: classes
  single: struct
  single: class
  single: interface
  single: protocol
  single: private
  single: public
  single: protected
  single: friend
  pair: declaring; types

User-defined types: Composites
==============================

The :kw:`class` keyword is one of the four keywords that are used to define a new
aggregate type. The others are :kw:`struct`, :kw:`interface` and :kw:`protocol`.

* A :kw:`class` is *reference* type, meaning it is always passed around by reference.
  When a value of class type is assigned to a variable, all that gets copied is a pointer to
  the object, not the object's contents.

* A :kw:`struct` is a *value* type. That means that normally when you assign one struct
  variable to another, it does a member-wise copy of the entire thing.

* An :kw:`interface` defines an pure abstract type which can only contain methods, types
  and constants. Like Java and C#, Tart does not support multiple inheritance, but does allow
  classes to derive from multiple interfaces. See the section on :ref:`interfaces`.
 
* A :kw:`protocol` represents a contract which a type may conform to. A class or struct
  is said to *support* the protocol if that class or struct defines all of the method signatures
  that are defined by the protocol. Template arguments can be constrained to only match types
  which support a specified protocol. Classes may declare explicitly that they support a protocol,
  or the support can be determined implicitly.
 
  Protocols will be discussed further in the section on :ref:`protocols`.

.. index::
  pair: class; definition
  pair: members; private

Declaring a new class
---------------------

The :kw:`class` keyword defines a new class. Here is a sample of a class definition::

  // Declare a class. The base class is 'ListNode'.
  class Shape : ListNode {
    // Declare an enumeration
    enum Style {
      Filled,
      Hollow,
    }

    // Private member variables.
    private {
      // Allow the unit test access to these vars.
      friend ShapeTest;

      // Some variables.
      var fillStyle:Style = Style.Filled;
      var fillColor:Color;

      // A variable with parameterized type.
      var children:List[Shape];
    }
  
    // A member function.
    def draw(dc:DrawContext) {
      dc.setFillStyle(fillStyle);
      for child in children {
        child.draw(dc);
      }
    }

    // A method with variable number of arguments
    // The 'children' argument's actual type is Shape[]
    final def addChildren(children:Shape...) {
      // Explicitly qualified self.children to disambiguate
      // from same-named parameter.
      self.children.extend(children);
    }
  }

A couple of things are worth noticing in this example:

**Dynamic dispatch is the default**: Like most object-oriented languages (other than C++), all
functions are dynamically overridable (``virtual`` in C++ parlance) unless declared ``final``.

**Private members:** You can declare a block of variables as :kw:`private`, rather than having
to put the word :kw:`private` in front of every variable name. Within a :kw:`private` or
:kw:`protected` block, you can declare :kw:`friend` classes that have direct access to
just these class members.

.. note:: The last point deserves some additional explanation: With C++, you can
  only declare a :kw:`friend` of an entire class. In Java, you can't declare a
  :kw:`friend` at all. Both of these feature choices cause programmers to expose
  too much encapsulated data. In the case of C++, you can't expose a private
  member without exposing everything. In Java, not having the ability to
  expose private data to certain classes causes Java programmers to declare
  class members ``public`` far more than the would otherwise.
  
.. warning:: ``friend`` is not implemented in the current release.

.. index::
  pair: class; members

Defining class members
----------------------

There is no restriction on the kinds of declarations which can appear in the body of a
:kw:`class` or :kw:`struct`, but most commonly they will be variable or method
declarations.

.. index:: public; private; protected; visibility

Member Visibility
^^^^^^^^^^^^^^^^^

A member definition can have one of three visibility levels:

  * ``public``, which is visible from everywhere;
  * ``protected``, which is visible only within the body of the class and its subclasses.
  * ``private``, which is visible only within the body of the class itself.

.. index::
  pair: member; variables

Member Variables
^^^^^^^^^^^^^^^^

A variable declared within a class body is a class instance variable, unless it the
``static`` modifier is present, in which case it is a static variable.

Variable declarations can include an optional initialization expression. If present, the
variable will be initialized to this value when the class instance is created, instead of
the built-in default for that type.

.. code-block:: tart

  class Point {
    var x:int = 0;
    var y:int = 0;
  }
  
Variables may be defined with either :kw:`let` or :kw:`var`. Variables defined with :kw:`let`
are constant, and may not be changed once initialized.

.. index::
  pair: class; methods
  single: def
  single: undef

Properties
^^^^^^^^^^

A *property* is a member definition that has a *getter* and *setter* function, and is otherwise
accessed like a variable. Reading from the property calls the getter function, and writing to
it calls the setter function. It is not required that both the getter and setter be defined,
although at least one of them must be.

Properties are defined with the :kw:`def` keyword, followed by a colon and the type of the
property::

  class Rectangle {
    // Some member variables
    var left:int = 0;
    var top:int = 0;
    var right:int = 0;
    var bottom:int = 0;

    // Width property    
    def width:int {
      get { return right - left; }
      set { right = left + value; }
    }

    // Height property    
    def height:int {
      get { return bottom - top; }
      set { bottom = top + value; }
    }
  }
  
  r = Rectangle(0, 0, 10, 10);
  
  // Calls the getter method of the rectangle's 'width' property
  let w = r.width;
  
  // Calls the setter method of the rectangle's 'height' property
  r.height = 3;
  
In the example above, we define two properties ``width`` and ``height`` which are synthesized
from the coordinates of the rectangle. In the case of the ``get`` method, the return statement
returns the value of the property.

For ``set`` methods, the implicit pseudo-variable ``value`` contains the value being assigned
to the property.
  
.. index::
  pair: class; methods
  pair: instance; methods
  pair: overriding; methods
  single: def

Methods
^^^^^^^

In Tart, the term *Method* and *Function* are interchangeable - usually "Function" is used
when speaking of global functions, and "Method" when speaking of member functions.

The :kw:`def` keyword defines a new method, as previously describe in the section on
:ref:`functionDeclarations`.

Methods defined in a class are automatically inherited by subclasses of that class. An
inherited method definition can be overridden using the :kw:`override` keyword. The subclass
method definition must have the same name and type signature as the base class method
being overridden::

   class Base {
     def doAction(code:ActionCode) -> ResultCode {
       // Base class definition
     }
   }

   class Subclass : Base {
     override doAction(code:ActionCode) -> ResultCode {
       // Subclass definition
     }
   }

The :kw:`def` keyword cannot be used to override a base class method - it is an error
to use :kw:`def` to define a method with the same name and type signature as a method
in a base class.

The :kw:`override` keyword is only required when overriding methods defined in a base class.
It is *not* required (and is not allowed) when implementing a method defined in an interface.
(Implementing an interface is not considered "inheritance".)

Methods overridden using :kw:`override` use dynamic dispatching, much like the way
virtual functions work in C++. The decision of which method body gets executed when a method
gets called is based on the real type of the object, not it's declared type at that point
in the code.

.. index:: undef

An inherited method definition can also be *undefined* using the :kw:`undef` keyword.
An undefined method simply throws an :class:`UnsupportedOperationError` exception, and
is a convenient way to signal that a subclass does not support an operation that is defined
in the base class::

  class Base {
    def caption -> String {
      return "Howdy!";
    }
  }
  
  class Subclass : Base {
    undef caption -> String;
  }

Unlike :kw:`override`, :kw:`undef` works with methods defined in interfaces as well as base
classes. There is an additional use
for :kw:`undef`, which is to suppress the creation of implicit methods which would be added by the
compiler, such as the default constructor. For example, suppose there is some data type which
is never intended to be constructed within Tart code, but only created by some external C
library::

  struct TimeStamp {
    var value:uint64;

    // Tell the compiler not to generate a default constructor.    
    undef construct;
  }

.. index:: final

A method may be declared :kw:`final`, informing the compiler that it is not permitted to
override that method in subclasses. This will in some cases allow the compiler to make additional
optimizations, avoiding the need for a dynamic dispatch::

  class Circle {
    final def draw(dc:DrawContext) {
      // Code to draw a circle
    }
  }

The :kw:`final` keyword can also be applied to an entire class, which means that the class
cannot be subclassed at all.

.. index:: abstract

A method declared as :kw:`abstract` has no method body. A class containing an abstract method
must also be declared abstract::

  abstract class Shape {
    abstract def draw(dc:DrawContext);
  }

Abstract classes cannot be directly instantiated, only subclasses of the abstract class can be
instantiated, and only if those subclasses are themselves concrete (i.e. not abstract).
A concrete subclass of an abstract class must provide concrete definitions for all of the
inherited abstract methods.

.. index:: self

The ``self`` parameter
^^^^^^^^^^^^^^^^^^^^^^

Within a class method, the special variable :cdata:`self` refers to the current instance.
You do not need to declare the  :cdata:`self` variable explicitly.

Instance variables can be referred to directly by name, you do not need to qualify them
with :cdata:`self` unless you also have a local variable or parameter with the same name.
:cdata:`self` is often used in constructors::

  def construct(x:int, y:int) {
    self.x = x;
    self.y = y;
  }
  
In most cases the :cdata:`self` parameter works exactly like other, explicitly declared
parameters. The exception to this rule is the :cdata:`self` parameter of :kw:`struct` methods.
Normally when the type of a function parameter is a :kw:`struct` type, the value that is
passed to that function is
a *copy* of the struct, however in the case of :cdata:`self`, what gets passed is a *pointer* to
the struct. If this were not true, it would be impossible to write methods that modify
struct members, since the method could only modify the temporary copy. Note, however,
that if you assign the :cdata:`self` parameter to another variable, the variable will
still get a copy.

.. index::
  pair: struct; members

Struct members
--------------

Method definitions in structs are always implicitly :kw:`final` and cannot be overridden.
The reason for  this is because structs do not have a hidden dispatch table like classes do,
so the decision of which method body gets executed has to be determined at compile time using
only the static type information at the calling site.

.. index::
  pair: class; new instance
  pair: struct; new instance

Creating a new class instance
-----------------------------

The syntax for creating a new class instance is the same as in Python, which is
to call the class as if it were a function::

  var s = Rectangle(10, 10, 20, 20);

The creation syntax is the same regardless of whether an object is a value or reference type. If
it's a value (:kw:`struct`) type, the new object will be constructed in place; If it's a
reference (:kw:`class`) type, then space for the object will be allocated on the global heap,
and a pointer to the object assigned to the variable.

.. note::
  .. compound::

    Value (:kw:`struct`) types can never exist on the heap except as instance variables
    inside other, reference types. The reason for this is fairly involved, but the short explanation
    is this: The garbage collector only deals with reference types. Every reference type has
    a special, invisible header field that is used by the garbage collector to track the status
    of the object. Structures, integers, floats, tuples, and other value types don't have any
    invisible fields, they are just 'plain old data' or 'POD' types.
    
    Tart also restricts you from having long-lived pointers to these types, because there's no
    way to track the lifetime of these objects. If you need to keep a long-lived copy of a value
    type around, it has to be wrapped inside another object. You can declare a value type as an
    instance member of a class, or you can use the provided :ctype:`ValueRef` or :ctype:`MutableRef`
    helper classes which will automatically wrap ('auto-box') value types when needed.

.. index:: constructors
  pair: function; construct

Constructors
------------

A *constructor method* is responsible for initializing the members of a class instance
when it is created. The constructor method is named :func:`construct`, and must not declare a
return value.

Here's an example of a class with several constructors::

  struct Rectangle {
    var x:int;
    var y:int;
    var width:int;
    var height:int;
  
    // Default constructor
    def construct {
      x = y = width = height = 0;
    }
    
    // Constructor that takes some arguments
    def construct(x:int, y:int, width:int, height:int) {
      self.x = x;
      self.y = y;
      self.width = width;
      self.height = height;
    }

    // Constructor that takes another Rectangle.
    def construct(from:Rectangle) {
      x = from.x;
      y = from.y;
      width = from.width;
      height = from.height;
    }
  }

When you create a new instance of a type by calling the type as a function, the compiler attempts to
locate a constructor method having the same signature as the calling arguments. This search
happens in several steps:

* First, the compiler looks for a constructor method named :func:`construct` having the
  appropriate signature.
  
  If the class has one or more constructor methods, but none of them match the input arguments,
  then the result is an error. Similarly, if there are constructors but none of them are accessible
  to the caller, then this is also an error.
  
* If the class has no constructor methods at all, then the compiler will look for a static
  member function called :func:`create` and call it instead.
  
  The :func:`create` function is not required to return a newly created object every time - it
  can return the same object as a previous call. It is also not required to return an object whose
  type is the same as the class, but it should at least be a subtype.
  
  The :func:`create` function is responsible for both allocating the memory for the object and
  initializing it completely.
  
  The :func:`create` function is often used to implement a custom allocation strategy for a type.
  For example, the :class:`String` class uses the :func:`create` function to allow string objects
  to be variably sized - so that both the fixed-length header part and the variable-length
  character array can be combined into a single memory allocation.

  .. note:: There is nothing special about this function other than the fact that the compiler
    calls it automatically.
   
  .. todo:: It may be prefereable to have an attribute to mark the factory function instead of
    using a special name like 'create'.

* If the class has no constructors, and no :func:`create` function, then the compiler will attempt
  to synthesize a default constructor.

.. index:: initialization
  pair: member; variable

Any member variable which is not initialized by the constructor will be set to
its default value. If the default value for the member variable has not been
specified, then it uses the default value for that type.
  
.. index::
  pair: constructor; default
  pair: keyword; arguments

Default constructors
^^^^^^^^^^^^^^^^^^^^

If the object has no :func:`construct` method and no :func:`create` method, then the compiler
will attempt to create a default constructor. The default constructor takes no arguments,
and will initialize every field to its default value. If any fields cannot be initialized
to a default, then the compiler will emit an error.

.. 
  If the object has no :func:`construct` method and no :func:`create` method, and every field within
  the object is either public or has a default value, then a default constructor will be generated
  by the compiler.
  
  The default constructor takes an argument for each public field, where the argument name is the same
  as the field name. If the field has a default value, then the parameter is optional and has a
  default value the same as the default value of the field. If the field does not have a default
  value, then the corresponding parameter is required and does not have a default value.
  
  If the object has a private field that does not have a default value, then the object cannot
  be default-constructed.
  
  Thus, if you call the default constructor with no arguments, then all member variables, both public
  and private, will get their default values. If you supply *some* arguments, then the member
  variables corresponding to those will be initialized to the supplied values, and the rest of the
  members will get their default values.
  
  Returning to our earlier example::
  
    // A basic structure declaration
    struct Point {
      var x:int;
      var y:int;
    }
  
    p = Point(x=10, y=10); // Initialize using keyword arguments.
    
  .. note:: Because required arguments must come before optional arguments, the order of function
    parameters cannot always be the same as the order in which the member fields which were declared.
    Fields which don't have a default value (and are therefore required) will be placed before
    fields which do have a default value (and are therefore optional). Other than that, the parameters
    will be in the same order as the fields were declared.
    
    You can insure that the default constructor parameters are always in the same order as the members
    by making sure that member fields with no default are always declared first.

.. index::
  pair: construct; super
  single: constructor
  single: construction

Constructor Execution
^^^^^^^^^^^^^^^^^^^^^

The compiler will attempt to ensure that all member fields are initialized during the execution of
the constructor. Fields which have no defaults must be assigned in the constructor, whereas fields
which do have defaults (either explicitly specified or implicit based on the type) may not be.
If the compiler detects that any field is not fully initialized, then it will report an error.

Member variables that are declared with :kw:`let` cannot be assigned a value after the
constructor has finished execution. In most cases, these kinds of variables will be constants
anyway, meaning that their values are fixed at compile time. However, Tart also allows
:kw:`let` variables to be assigned in the constructor, in which case they are constant for
the life of that instance (similar to the way "final" is used in Java). Note that such variables
may only be assigned once in the constructor body.

Constructors can call the superclass constructor by using the syntax ``super()``.

Constructors can also invoke other constructors of the same class, by explicitly calling
:func:`construct`.

.. note:: The compiler assumes that all constructors are complete, meaning that they initialize
  every variable that must be initialized. That means if a constructor calls another constructor,
  then when that call returns the compiler presumes that all fields have been initialized, and
  any assignments which occur after that point are re-assignments, overwriting values that
  were assigned during the call.
  
  This means that once you delegate to another constructor, you cannot assign to any
  :kw:`let`-defined members afterwards.

.. index::
  pair: array; constructor
  pair: array; literal

Creating Array Instances
------------------------

The syntax for constructing a new array instance is the type name, followed
by the dimension of the array in square brackets::

  // Allocate an array of 10 rectangles.
  let a = Rectangle[10];
  
If the element type of the array is a reference (:kw:`class`) type, then what is actually
allocated is an array of pointers to that type. If it's a value (:kw:`struct`) type, then
the array actually contains embedded instances of that type.

The array constructor call is actually implemented as a call to the Array
template class. So the example above is transformed by the compiler into this::

  // Allocate an array of 10 rectangles.
  let a = Array[Rectangle](10);

If you want to supply a list of initial values for the array, you can either use an array literal
or the static :func:`of` method of the Array class. Both are equivalent::

  // Deduce the type of the array from the list of values
  var x = ["One", "Two", "Three"];
  var y = Array.of(1, 2, 3);
  
  // Deduce the type of the array from the list of values and the
  // type of the variable being assigned to.
  var x:String[] = ["One", "Two", "Three"];
  var y:int[] = Array.of(1, 2, 3);
  
  // The 'of' method also allows you to explicitly specify the array type.
  // Most concrete container types in Tart have an 'of' method.
  var y = Array[uint32].of(1, 2, 3);
  
.. note::
  Within Tart expressions, square brackets '[]' can have several possible meanings:
  
    * Empty brackets after a type name indicate an array type, such as ``int[]``.
    * Non-empty brackets after a type name indicate a template argument list, such as
      ``Array[int]``.
    * Non-empty brackets after a variable name indicate an element lookup operation, such
      as an array element dereference, or a hash table lookup, for example ``words[10]``.
      Any class can support lookup operations using this syntax by declaring an
      'indexer' method using special syntax.
    * A bracket that does not follow a symbol name or expression is used to indicate
      the beginning of an array literal.

  .. note::
    Almost all of the Tart language grammar is *context-free*, which basically means
    that a parser doesn't need to understand the semantic meanings of the symbols
    in order to correctly parse the source text. This overloading of the square brackets
    is the single exception to this rule, which was unavoidable due to the fact that
    ASCII only has a limited number of bracketing delimiters - ``[]``, ``()`` and ``{}``.
    Java and C++ get around this by using *angle-brackets* - ``<`` and ``>`` - however, this
    causes even greater grammatical ambiguities due to confusion with the less-than
    and greater-than operators, and makes the parser even more complicated and less
    context-free.
    
    
.. _interfaces:

Interfaces
----------

Tart supports "interface inheritance", which means that a class can only have one base class,
but can derive from any number of interfaces::

  // A sample base class
  class Widget {
    def draw(dc:DrawContext) {
      // Method body...
    }
  }

  // An interface
  interface Selectable {
    def select(selected:bool);
  }

  // Another interface
  interface HasEventListeners {
    def addListener(handler:fn(e:Event));
  }

  // A class that derives from one base class and two interfaces.
  class Window : Widget, Selectable, HasEventListeners {
    // Override method from base class
    override draw(dc:DrawContext) {
      // Draw the window...
    }

    def select(selected:bool) {
      // Implement select
    }

    def addListener(handler:fn(e:Event)) {
      // Implement addListener.
    }
  }

Interfaces can only contain a retricted set of definitions:

  * Interface methods (methods with no body).
  * Type definitions
  * Named constants

A class that derives from an interface type must provide definitions of all of the methods in
the interface, unless that class is declared abstract. Any missing definitions will be flagged
by the compiler.

.. _protocols:

Protocols
---------

A *protocol* is a kind of abstract type that defines a *contract* which another type can
support. An example would be a "HasToString" protocol::

  protocol HasToString {
    def toString -> String;
  }

What this says is that in order to support the ``HasToString`` protocol, a class, interface
or struct must define
a :meth:`toString` method that returns a String. There are two ways that a type can support
a protocol, *explicit* and *implicit*. Explicitly supporting a protocol is done by declaring
the protocol in the list of base types::

  class A : HasToString {
    // It would be an error to declare support for HasToString and then
    // fail to define a toString() method.
    def toString -> String {
      return "Hi there";
    }
  }

You can also support a protocol implicitly simply by adding the required methods to your type,
without declaring the protocol as a base type. Moreover, this applies even to classes that
know nothing about the protocol - as long as they have the right set of methods, they support
the protocol. So for example, any class that has a ``toString -> String`` method supports the
``HasToString`` protocol, even if that class were written before ``HasToString`` ever existed!

Protocols can only be used as matching constraints - you can't declare a variable or a
function parameter whose type is a protocol. (Although, maybe someday...)

Protocols are primarily used in instantiating templates. You can define a template and then
add constraints to its template parameters, such that a parameter can only be bound to a type
that supports the proper protocol.

Note that protocols exist only at compile time, and declararing a protocol as a base type
has no effect on the generated code for that type.

Extending types
---------------

.. warning:: :kw:`extend` is not implemented in the current release.

The :kw:`extend` keyword allows you to add additional methods to a user-defined type::

  /* Add an additional method to the String class. */
  extend String {
    static def toUpperCase() { /* ... */ }
  }

Note however, that you can't actually change the runtime representation of a type this way. The
reason is simple: The :kw:`extend` declaration may not be visible everywhere in the program. If
you extend class :class:`String`, some modules may only see the original, unextended class, while
other modules will see the extended version of the class. In order for all of the code to
interoperate, the runtime implementation of the class must be the same, regardless of the extension.

This means that the extension can only add certain kinds of things to a type, namely:

* Static methods or properties.
* Final methods or properties.
* Inner types and type aliases.
* Protocol inheritance declarations.

The last is worthy of some note: Since by definition adding a protocol to a class does not affect
the runtime implementation of the class in any way, it is permissible to add a protocol in an
extension. This allows protocols to be added to a class after the fact::

  protocol Serializable { /* ... */ }

  // Make the string class serializable.
  extend String : Serializable { /* ... */ }

Extensions follow the same scoping rules as other declarations, meaning that they are only in effect
if the scope in which they are declared is active. For example, we can define an extension inside a
namespace::

  class Foo { /* ... */ }

  namespace JSONUtils {
    extend Foo {
      final def convertToJSon() -> String { /* ... */ }
    }

    // Foo.convertToJSon is visible here
    let f = Foo();
    let s = f.convertToJSon();
  }

  // Foo.convertToJSon is no longer visible

  import namespace JSONUtils;

  // And now it's visible again.

It does not matter whether the extension is visible via the same "path" as the original class, so
long as it is in a currently active scope. However, the extension does not apply if the extended
class is hidden by another declaration with the same name::

  class Foo { /* ... */ }
  extend Foo { /* ... */ }

  namespace JSONUtils {
    /** A different class Foo, unrelated to the extended one. */
    class Foo {}
  }

Extensions can be templates if the class that they are extending is also a template. If the original
class is a template and the extension is a specialization of that template, then the extension only
applies when using the specialization::

  class Foo[%S] { /* ... */ }
  
  // Only extended for strings
  extend Foo[String] { /* ...*/ }
