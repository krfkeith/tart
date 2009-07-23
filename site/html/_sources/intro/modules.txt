.. highlight:: tart
  :linenothreshold: 30

Modules and Packages
====================

Each individual Tart source file corresponds to a module. Modules can import
definitions from other modules via the :stmt:`import` statement::

   import tart.collections.ArrayList;

The compiler reads the imported definitions from the compiled code - it does
not need to parse the original source file for the imported module.

Modules are arranged into *packages*, which are directories of modules. The
compiler maintains a *classpath* variable which lists all of the directories
to search for modules. Each entry in the class path represents the top of the
package hierarchy.

In addition to explicit imports using the :stmt:`import` statement, a
definition can also be imported implicitly if one of the following are true:

*  The name of the definition is given as a fully-qualified package path.
*  The definition is in the same package as the current module.
*  The definition is in the ``tart.core`` package.

Structure of a Module
---------------------

When a module is imported, the compiler loads the module definitions into
memory. It then examines the top-level namespace of the module for any
public definitions that have the same name as the module itself. (Because of
overloading, there can be more than one definition with the same name.) These
definitions are then introduced into the namespace of the importing module.

What this means is that a module can only export symbols that are (a) public,
and (b) have the same name as the module itself. A module can have any number
of *private* symbols at the top level; These are not visible externally.

Namespaces and the Import Statement
-----------------------------------

Oftentimes, it will be desirable to create a module that has a mix of different
definitions with different names. The easiest way to do this is to put the
symbols in a namespace which has the same name as the module::

  namespace Stuff {
    def example1() { /* ... */ }
    def example2() { /* ... */ }
  }

The :stmt:`import` statement has two forms. The normal form causes the imported
symbol to be added to the current scope::

  // Make the name 'Stuff' available in the global scope
  import Stuff;
  
  def main() {
    Stuff.example1();
  }

The second form of the :stmt:`import` statement is the "namespace" form. Rather
than defining the definition in the current scope, it causes the *contents* of
the specified namespace to be added to the current scope::

  // Make the contents of 'Stuff' available in the global scope
  import namespace Stuff;
  
  def main() {
    example1();
  }

This second for is useful for operator definitions and other symbols that
affect compilation without being specifically invoked by name.

Transitive Namespaces
---------------------

Namespaces can contain local import statements::

  namespace Foo {
    import namespace Bar;
    import namespace Baz;
  }

Thus, importing ``Foo`` will also import the contents of Bar and Baz.
