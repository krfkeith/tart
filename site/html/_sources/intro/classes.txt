.. highlight:: tart
  :linenothreshold: 30

.. index:: classes
  keyword: struct
  keyword: class
  keyword: interface
  keyword: protocol
  keyword: private
  keyword: public
  keyword: protected
  keyword: friend
  pair: declaring; types
  
Classes
=======

The :keyword:`class` keyword is one of the four keywords that are used to define a new
aggregate type. The others are :keyword:`struct`, :keyword:`interface` and :keyword:`protocol`.

* A :keyword:`class` is *reference* type - it is always passed around by reference. When you assign
  one class instance variable to another, all that gets copied is a pointer to the object.

* A :keyword:`struct` is a *value* type. That means that normally when you assign one struct
  instance to another, it does a member-wise copy of the entire thing.

* An :keyword:`interface` defines an pure abstract type which can only contain methods, types
  and constants. Like Java and C#, Tart does not support multiple inheritance, but does allow
  classes to derive from multiple interfaces.
 
* A :keyword:`protocol` represents a contract which a type may conform to. A class or struct
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

The :keyword:`class` keyword defines a new class. Here is a sample of a class definition::

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
      var fillStyle:Style;
      var fillColor:Color;

      // A variable with parameterized type.
      var children:List<[Shape]>;
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

**Private members:** You can declare a block of variables as :keyword:`private`, rather than having
to put the word :keyword:`private` in front of every variable name. Within a :keyword:`private` or
:keyword:`protected` block, you can declare :keyword:`friend` classes that have direct access to
just these class members.

.. note:: The last point deserves some additional explanation: With C++, you can
  only declare a :keyword:`friend` of an entire class. In Java, you can't declare a
  :keyword:`friend` at all. Both of these feature choices cause programmers to expose
  too much encapsulated data. In the case of C++, you can't expose a private
  member without exposing everything. In Java, not having the ability to
  expose private data to certain classes causes Java programmers to declare
  class members ``public`` far more than the would otherwise.

**Dynamic dispatch is the default**: Like most object-oriented languages (other than C++), all
functions are dynamically overridable (``virtual`` in C++ parlance) unless declared ``final``.

.. index::
  pair: class; new instance

Creating a new class instance
-----------------------------

The syntax for creating a new class instance is the same as in Python, which is
to call the class as if it were a function::

  var s = Rectangle(10, 10, 20, 20);

The creation syntax is the same regardless of whether an object is a value or reference type. If
it's a value type, the new object will be constructed in place; If it's a reference type, then space
for the object will be allocated on the global heap, and a pointer to the object returned.

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
--------------------

If the object has no constructor and no :func:`create` method, and every field within the object is
either public or has a default value, then a default constructor will be generated by the compiler.
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

Constructor Execution
---------------------

The compiler will attempt to ensure that all member fields are initialized during the execution of
the constructor. Fields which have no defaults must be assigned in the constructor, whereas fields
which do have defaults (either explicitly specified or implicit based on the type) may not be.
If the compiler detects that any field is not fully initialized, then it will report an error.

Member variables that are declared with :keyword:`let` cannot be assigned a value after the
constructor has finished execution. In most cases, these kinds of variables will be constants
anyway, meaning that their values are fixed at compile time. However, Tart also allows
:keyword:`let` variables to be assigned in the constructor, in which case they are constant for
the life of that instance (similar to the way "final" is used in Java). Note that such variables
may only be assigned once in the constructor body.

Constructors can call the superclass constructor by using the syntax::

  super.construct();

Constructors can also invoke other constructors of the same class, by explicitly calling
:func:`construct`.

.. note:: The compiler assumes that all constructors are complete, meaning that they initialize
  every variable that must be initialized. That means if a constructor calls another constructor,
  then when that call returns the compiler presumes that all fields have been initialized, and
  any assignments which occur after that point are re-assignments, overwriting values that
  were assigned during the call.
  
  This means that once you delegate to another constructor, you cannot assign to any
  :keyword:`let`-defined members afterwards.

Array constructors
------------------

The syntax for constructing a new array instance is the type name, followed
by the dimension of the array in square brackets::

  // Allocate an array of 10 rectangles.
  let a = Rectangle[10];
  
If the element type of the array is a reference type, then what is actually
allocated is an array of pointers to that type. If it's a value type, then
the array actually contains embedded instances of that type.

The array constructor call is actually implemented as a call to the Array
template class. So the example above is transformed by the compiler into this::

  // Allocate an array of 10 rectangles.
  let a = Array<[Rectangle]>(10);

.. note::
  The '<[' token is used to indicate the start of a template argument list.

Extending types
---------------

The :keyword:`extend` keyword allows you to add additional methods to a user-defined type::

  /* Add an additional method to the String class. */
  extend String {
    static def toUpperCase() { /* ... */ }
  }

Note however, that you can't actually change the runtime representation of a type this way. The
reason is simple: The :keyword:`extend` declaration may not be visible everywhere in the program. If
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

  class Foo<[%S]> { /* ... */ }
  
  // Only extended for strings
  extend Foo<[String]> { /* ...*/ }
