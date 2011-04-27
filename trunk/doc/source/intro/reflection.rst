.. highlight:: tart
  :linenothreshold: 30
  
.. index:: reflection

Types and Reflection
====================

Tart supports reflection as a standard part of the language. You can get the reflection
information for a type by passing the name of the type to the :meth:`of` method of class
:class:`Type`::

  import tart.reflect.Type;

  let t:Type = Type.of(String);
  sys.stdout.println("isClass = ", t.isClass);
  sys.stdout.println("isIntegerType = ", t.isIntegerType);

For composite types such as classes and structs, you can use the class
:class:`CompositeType`, which has additional information about the type::

  let t:CompositeType = CompositeType.of(String);
  sys.stdout.println("name = ", t.name);
  sys.stdout.println("qualified name = ", t.qualifiedName);
  
The :class:`CompositeType` type implements a large set of methods for discovering things
about types, including properties, methods, constructors, and so on.

Some examples of using the reflection functions::

  // Lookup a property by name
  let p:Property = CompositeType.of(List).getProperty("length");
  
  // List all property names
  for p in CompositeType.of(List).properties {
    sys.stdout.println(p.name);
  }
  
.. index::
  pair: @Reflect; attribute

The @Reflect Attribute
----------------------

The compiler can generate a set of reflection tables that contains all of the
information about a classes methods, properties, fields, and so on. However, this information
can be quite large, especially when dealing with templates (each unique template instantiation
gets its own copy of the reflection tables.) Because of this, the default behavior for the
compiler is to only generate minimal information for the class - that is, information about
the name, base classes, and type parameters. Information about the methods, properties,
fields, and other class members is not generated unless you specifically ask for it.

You can tell the compiler to generate the more detailed information by adding the ``@Reflect``
attribute to the class::

  @Reflect class SomeClass {
    // All class members will be reflected.
  }

This annotation is inheritable, meaning that it applies to all subclasses as well.

Because of this, you need to plan ahead when using reflection. As a practical matter,
most uses of reflection start from a known type. For example, a common use of reflection
is to find all test methods in a unit test. These unit tests normally inherit from some
base testing class such as :class:`Test`. Adding the ``@Reflect`` annotation to the
:class:`Test` class insures that all unit tests will have complete reflection information.

.. note:: The compiler will also generate complete reflection information for any class
  member that is annotated with an attribute. This does not include compile-time-only
  attributes which are not retained in the compiler's output.
