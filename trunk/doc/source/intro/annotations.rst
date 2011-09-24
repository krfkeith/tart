.. highlight:: tart
  :linenothreshold: 30

.. index:: annotations, attributes, effects

Attributes
==========

Methods, variables, classes, and other definitions in Tart can be annotated with an
arbitrary number of *attributes*. An attribute is signified by an @ sign, and comes
immediately before a declaration::

  // Declare a thread-local variable
  @ThreadLocal var count:int;

  // Declare this function as the entry point of the program
  @EntryPoint def main(args:String[]) -> int {
    runApp();
    return 0;
  }
  
Attributes can take arguments, as in the case of the `@Extern` attribute, which allows you
to declare an external reference to a C-language function. The argument to `@Extern` allows
you to specify the linkage name of the external function::

  @Extern("create_String")
  def createString(length:int) -> String;

Although the standard library defines many different attribute types, in practice there are
only a few that you will encounter on a regular basis:

  =========== ===========================================================
  Attribute   Meaning
  =========== ===========================================================
  EntryPoint  Marks the entry point of a program
  Extern      Indicates a definition external to the current module
  Flags       Used to create a `flags` enumeration
  Reflect     Tells the compiler to emit detailed reflection information.
  ThreadLocal Indicates a thread-local variable.
  =========== ===========================================================

.. index::
  pair: attribute; propagation

Different attribute types have different rules for *propagating*. There are three
main modes of propagation:

  * **Subtype** propagation - attributes from a base class are copied to any subclasses.
  * **Member** propagation - attributes from a class are copied to the members of the class.
  * **Caller** propagation - attributes on a function are propagated to the functions's caller.

The last type is useful for implementing *effect* attributes. For example, you could define
a ``@Throws`` attribute which propagates to any callers::

  // Any function that calls 'lookup' will also get a @Throws attribute.
  @Throws(ArrayBoundsException)
  def lookup(index:int) {
    return table[index];
  }
  
.. index::
  pair: attribute; retention

Attributes also have a *retention* property, which says whether or not the attribute should
be retained in the final executable. There are various APIs available for discovering at runtime
what attributes are associated with a particular function or variable.

..
  There are also special kinds of annotations that remove effects: Just like
  try/catch can remove the effect of a 'throws' in Java, you can have statements
  that remove effects from a block of code. You can also have annotations that
  verify that a particular effect is present or absent::
  
    @AssertEffect(threadSafe)
    def threadSafeFunc() {
        // m.withLock removes the 'not threadsafe' effect.
        m.withLock({
           notThreadSafeFunc();
        });
    }
  
  So if you really do want to say "This function throws no exceptions other than
  DivideByZeroException", and make sure that none of the functions that it is
  calling can throw that exception either, you can do so - and if that
  constraint is violated, a compilation error will let you know about it.
  
  One of the big reasons for the "Effects" feature is that it allows you to
  reason about concurrent programs. Tart doesn't have any special syntax for
  threading or synchronization - those are just library functions. What it does
  have is a way to make meaningful statements about the concurrent behavior of a
  function or class, and act on those assertions later.
