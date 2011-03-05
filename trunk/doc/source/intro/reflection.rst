.. highlight:: tart
  :linenothreshold: 30

Types and Reflection
====================

Tart supports reflection as a standard part of the language. Type literals are
represented using the typeof operator::

  let t:Type = typeof(Integer);
  sys.stdout.println("Name = ", t.name);
  sys.stdout.println("isArray = ", t.isArray);
  sys.stdout.println("isRefType = ", t.isRefType);

The :ctype:`Type` type implements a large set of methods for discovering things about types,
including properties, methods, constructors, and so on. Type literals are constants, and many of
their properties are also constants, meaning that you can use those properties in compile-time
expressions.

Some examples of using the reflection functions::

  // Lookup a property by name
  let p:PropertyDescriptor = List.type.getProperty("length");
  let list:List[String] = Collections.newArrayList();
  let length = p.getValue(list);
  
  // List all property names
  for p in List.type.properties {
    sys.stdout.println(p.name);
  }
