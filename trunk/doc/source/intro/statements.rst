.. highlight:: tart
  :linenothreshold: 30

Statements
==========

.. statement:: if
.. statement:: else
.. statement:: do
.. statement:: while
.. statement:: repeat

Basic control-flow statements
-----------------------------

Tart has all the usual statements that you would expect:
:stmt:`if`/:stmt:`else`, :stmt:`for`, :stmt:`while`,
:stmt:`do`/:stmt:`while`, as well as a few new ones such
as :stmt:`repeat`.

Here are a few examples::

  // if/else statement
  if a > 2 {
    ++a;
  } else {
    --a;
  }

  // while loop, showing a test expression that also declares a variable.
  while let m = re.match(str) {
    console.stdout.writeLn(m.group(1));
  }

  // do/while loop
  do {
    let x = buffer[i++];
  } while i < length;
  
  // 'repeat' is a simple 'do-forever' statement.
  repeat {
    let s = console.stdin.readLn();
    break if s == null;
  }
  
The syntax is a little bit different from C or Java: You aren't required to
put parentheses around the test expression, but the braces around the statement
body are always required.

.. statement:: for

Tart supports both forms of :stmt:`for` (the C++ and Python forms), just
like Java & JavaScript do::

  // C-style for loop
  var total:int = 0;
  for (var i:int = 0; i < 10; ++i) {
    total += i;
  }

  // Python-style for-loop
  var fact = 1;
  for n in range(10) {
    fact *= n;
  }
    
.. statement:: break
.. statement:: continue

Break and Continue Statements
-----------------------------

Because the :stmt:`break` and :stmt:`continue` statements
are so often used conditionally, the Perl syntax of *post-statement conditions*
is supported for these statement types::

  break if a > 10;
  continue if a < 10 and a not in 0..5;
  
.. statement:: switch

Switch statements
-----------------

Tart has a C-style 'switch' statement::

  switch input {
    case 'a' {
      // ...
    }
    
    case 'b' {
      // ...
    }

    case 'c'
    case 'd' {
      // ...
    }
    
    else {
      // The default case
    }
  }
  
The braces around the case body are required. There is no need for a 'break'
statement, as execution does not 'fall through' from one case body to the next.

.. statement:: match

Match statements
----------------

There is also a :stmt:`match` statement which is used to differentiate
based on the type of the input expression::

  match input {
    as str:String {
      // ...
    }
    
    as w:Widget {
      // ...
    }
    
    default {
      // The default case
    }
  }
  
If the input value is one of the types specified, then the value is bound to
the corresponding variable (``str`` in the case of a :class:`String` in the example
above) and that variable will be available within the scope of the case body.
If the input does not match any of the types listed, then the :kw:`else` case
will be executed, or if there is no :kw:`else` case then the entire statement
is skipped.

.. statement:: with

The "with" statement
--------------------

.. warning: This statement is not implemented in the current release.

Another useful statement is the :stmt:`with` statement::

  // Declare a new variable 'fh' and assign an open file handle to it.
  with fh = File.open("unicode.txt) {
    // Do something with fh.
    // It will be closed when the block exits.
  }
  
The :stmt:`with` statement can be used to guarantee that the appropriate cleanup
code is called after you are finished with an object. In the above example,
the file handle ``fh`` will be closed upon exit from the :stmt:`with` block,
regardless of how the block was existed (even if via :stmt:`return` or an
exception.)

The :stmt:`with` statement can also influence the set of effect annotations
that propagate outward from within the contained block. Similar to the way a
:stmt:`try` statement can filter out an exception effect, a :stmt:`with`
statement that acquires and then releases a mutex could potentially
remove a 'thread-unsafe' effect.

.. statement:: try
.. statement:: catch
.. statement:: finally

Exception Statements
--------------------

Exceptions are very similar to Python and Java::

  try {
    do_stuff();
  } catch e:NumberFormatException {
    // Exception handler
  } catch e:AssertionFailedException {
    // Exception handler
  } finally {
    // Cleanup
  }

.. todo:: This section needs to be fleshed out.
