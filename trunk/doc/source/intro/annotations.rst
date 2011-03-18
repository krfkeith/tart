.. highlight:: tart
  :linenothreshold: 30

.. index:: annotations, effects

Annotations
===========

Tart uses the Java syntax for declaring annotations::

  @Throws(ArrayBoundsException)
  def lookup(index:int) {
      return table[index];
  }

However, in this case ``throws`` isn't just a normal annotation, it's an
'effect'. Effects are special, in that they are 'viral' - they propagate to
the caller, much like the way checked exceptions work in Java::

  // This function implicitly has the 'throws(ArrayBoundsException)' effect,
  // which it inherits by virtue of the fact that it called lookup().
  def lookup2() {
      // Calling a function with an effect gives us that effect too.
      return lookup(0);
  }

As you may notice, there are two big differences between effects and Java
checked exceptions. The first is that effects aren't hard-wired into the
language - you can invent your own kinds of effect classes. The second
difference is that you don't have to explicitly declare the effects in the
signature of the calling function - instead it's calculated for you.

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
