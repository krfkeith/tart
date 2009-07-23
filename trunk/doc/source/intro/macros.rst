.. highlight:: tart
  :linenothreshold: 30

Macros
======

Tart macros are functions which execute in the compiler rather than at
runtime. They are introduced with the :keyword:`macro` keyword::

  macro MyMacro(t:Type) -> Type {
    // ...
  }

One thing that is interesting about macros is that the arguments are not
evaluated before calling the macro. In other words, when you call a macro with
an argument of "1 + 2", it does not pass the value "3", but rather the
unevaluated expression "1 + 2" (technically what gets passed is an AST
fragment.) These expressions will be substituted inline in the body of the
macro. This means that you can control how many times (if at all) the
expression and its associated side effects are evaluated.

Take for example the :macro:`cond` macro, which is part of the Tart core
library::

  macro cond<[%T]>(condition:bool, trueVal:T, falseVal:T) -> T {
    if condition { return trueVal; }
    else { return falseVal; }
  }

The :macro:`cond` macro operates exactly like the ternary ? operator in C.

.. note:: The name "cond" is inspired by the conceptually similar LISP macro
  of the same name.) Because it's a macro, the side effects of ``trueVal`` will
  occur only if the condition is true, and the side effects of ``falseVal``
  will occur only if the condition is false.

Another thing to notice is that the :macro:`cond` macro is a template as well
as a macro. More about templates in the next section.

There are a number of built-in functions that give you access to the attributes
of the AST. For example, :func:`stringify` converts an AST node into its string
representation. This is used by the :macro:`assert` macro, which is another
part of the Tart core library::

  macro assert<[%T]>(expression:T) {
    if not expression {
      throw AssertionError(stringify(expression));
    }
  }
