.. highlight:: tart
  :linenothreshold: 30

Overview
========

.. epigraph::  "Whenever a programmer has the urge to create a new
  programming language, the proper response is to lie down on the
  couch and wait until the feeling passes." -- Anonymous

**Tart** is a general-purpose, strongly-typed programming language.

Tart is intended for high-performance applications such as audio synthesis applications, computer
games, real-time video processing, and simulation.

Tart borrows many ideas from other programming languages, including C, C++, Python, Java, C#, D,
Haskell, Ruby, Scala and many others. It also includes a number of features not seen in any of these
languages.

Tart is a statically-typed language that has been designed for high efficiency. Tart allows you to
get "close to the metal", in the sense that you get access to the details of the native platform. At
the same time, Tart is a very high level language which supports things like type deduction,
multi-method dispatch and metaprogramming.

Tart is designed to be easy to learn, but its also designed to be "expert-friendly". Like a musical
instrument, Tart enables a true master to create "virtuoso performances" of excellence and creative
power.

Motivations
-----------

Why create a new programming language, when there are already so many good ones out there?

The answer to this question is complex, because there's no single motivating reason that stands out
from all the others. Some of these motivations are:

* The desire for a language that would combine the simplicity and
  readability of Python with the power of static typing and
  template metaprogramming, as well as modern language features
  such as closures and generic functions.
* The desire for a compiler that compiles to highly efficient
  native code instead of a virtual instruction set.
* The desire for a language which would fulfill the same role as
  C++, but designed from scratch with the benefit of hindsight.
* The desire for a language which would fulfill the same role as
  Java, but more concise and requiring less verbose boilerplate.

Features of Tart
----------------

* Strong static typing. Tart's type system is optimized for
  expressiveness and power. Types can be automatically inferred in many
  cases, which means that the code isn't cluttered up by redundant
  type declarations.
* Supports a variety of programming styles: object-oriented, functional
  programming, imperative programming, generic programming, metaprogramming,
  and so on.
* Built-in support for reflection and type inspection.
* Built-in support for garbage collection.
* Support for closures and anonymous functions.
* Able to interoperate with libraries written in other languages through
  the platform ABI using a foreign function interface (FFI).
* Able to be compiled directly to native code. Although there may also
  be versions of the Tart compiler that are targeted at virtual machines
  such as the JVM and CLR, Tart doesn't require a VM.
* Support for concurrency and multiprocessor programming.
* Supports operator overloading via generic functions. Writing a new '+'
  operator is simply a matter of writing a new specialization for
  ``infixAdd``.
* Regular, unambiguous syntax that is easy to write parsers for. It will
  be easy to support Tart in IDEs and refactoring tools. At the same time,
  Tart's syntax has been designed with humans in mind - the syntax provides
  just enough visual variety to allow easy recognition of common coding
  idioms.
* Direct support for Unicode. Tart's source files are unicode files, and
  unicode characters are allowed in identifiers.

Things that Tart doesn't do
---------------------------

Tart can't do everything! Here's some things that some other languages do that
Tart doesn't:

* Tart doesn't support run-time compilation of code or creation of
  classes, although some implementations may provide this as an
  extension. Many of the kinds of things you would want to do with
  run-time class creation (such as automatic creation of mock
  implementation classes for testing) can be done at compile time
  with Tart's template system.
* Tart doesn't have a preprocessor like C and C++. It has macros,
  but they operate on the AST level.
* Tart doesn't allow C-style pointer math. It's more like Java/C# in
  this respect.
* Tart isn't finished.

Tart is still in an early development stage. That means that some aspects
of the language may change. At some point the design of Tart will be
"frozen", but for the moment there's no guarantee that the code you write
today will be compatible with future compilers.

About the name "Tart"
---------------------

The name "Tart" was chosen because it has the following attributes:

* Its a 4-letter word that is easy to say and remember.
* Like some other successful programming languages, it shares its name
  with a tasty food product.
* It's a fruitful (pun intended) source of metaphor, which will be
  important for all those future tech magazine writers who are looking
  for a new angle for their latest article.
* The name "tart" also implies promiscuity - which is appropriate,
  because Tart will interoperate with just about anybody.
* Unlike many great name ideas, "tart" isn't taken yet.

Now, you may ask: Do the letters "TART" stand for anything? The answer is:
Yes they do. In fact they stand for a lot of different things. And you,
the Tart users will get to decide what some of those things are. You
see, each release of the Tart compiler will include (and be named
after) a new definition of what "Tart" stands for. This definition
will be selected from suggestions sent in by Tart users. So start
working on those backronyms!
