Statements and Control Structures
=================================

.. productionlist::
   stmt:  `expr_stmt`
       :| `assign_stmt`
       :| `return_stmt`
       :| `decl_stmt`
       :| `with_stmt`
       :| `switch_stmt`
       :| `match_stmt`
       :| `if_stmt`
       :| `while_stmt`
       :| `do_while_stmt`
       :| `for_stmt`
       :| `for_in_stmt`
       :| `break_stmt`
       :| `continue_stmt`
       :| `try_stmt`
       :| `import_stmt`

Statement blocks
----------------

A statement block is a list of statements, separated by semicolons. The last statement in the
block does not need to end with a semicolon.

.. productionlist:: Statement block
   stmt_block: "{" [ `stmt` (";" `stmt`)* ] "}"

Simple Statements
-----------------

.. statement:: expression

Expression Statement
^^^^^^^^^^^^^^^^^^^^

.. productionlist::
   expr_stmt: `expression`

An expression statement evaluates an expression. Typically this will be an expression with
side-effects.

.. statement:: assignment

Assignment Statement
^^^^^^^^^^^^^^^^^^^^

.. productionlist::
   assign_stmt: `assign` | `aug_assign`
   assign: `lvalue` "=" (`lvalue` "=")* `expression`
   aug_assign: `lvalue` `aug_assign_oper` `expression`
   aug_assign_oper: "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "^=" | ">>=" | "<<="
   
The assignment statement has two forms: *regular* and *augmented* assignment.

The regular assignment statement can assign an expression to one or more variables::

  // Assign 1 to both a and b.
  a = b = 1;

The augmented assignment statement reads the value of a variable, operates on the value, and then
stores the value back into the variable::

  // Add 1 to a
  a += 1;

.. statement:: return

Return Statement
^^^^^^^^^^^^^^^^

.. productionlist::
   return_stmt: "return" [`expression`]

The :stmt:`return` statement returns from the current function. A return value may be supplied.
The type of the return value must be convertible to the return type of the current function.

To return more than one value, use the comma (:oper:`,`) operator to construct a tuple::

  def pair(a:int, b:int) -> (int, int) {
    return a, b;
  }

Compound Statements
-------------------

.. statement:: declaration

Declaration Statement
^^^^^^^^^^^^^^^^^^^^^

.. productionlist::
   decl_stmt: `declaration`

A declaration statement declares a variable in the current scope.

.. statement:: with

With Statement
^^^^^^^^^^^^^^

.. warning:: This statement is not implemented in the current release.

.. productionlist::
   with_stmt: "with" `with_var` ("," `with_var`)* `stmt_block`
   with_var: `ident` [ ":" `type` ] "=" `expression`
   
The :stmt:`with` statement is used to explicitly control the lifetime of a value. It can be used
to guarantee that the appropriate cleanup code is called after you are finished with an object.
For example, you could use it to insure that a file handle gets closed after you are finished
writing to the file::

  // Declare a new variable 'fh' and assign an open file handle to it.
  with fh = File.open("unicode.txt) {
    // Do something with fh.
    // It will be closed when the block exits.
  }

In the above example, the file handle ``fh`` will be closed upon exit from the
:stmt:`with` block, regardless of how the block was exited (even if via :stmt:`return` or
an exception.)

..
  The :stmt:`with` statement can also influence the set of effect annotations
  that propagate outward from within the contained block. Similar to the way a
  :stmt:`try` statement can filter out an exception effect, a :stmt:`with`
  statement that acquires and then releases a mutex could potentially
  remove a 'thread-unsafe' effect.

Conditional Statements
----------------------

.. statement:: if

If Statement
^^^^^^^^^^^^

.. productionlist::
   if_stmt:"if" `expression` `stmt_block` [ "else" `stmt_block` ]

.. statement:: switch
.. statement:: case

Switch Statement
^^^^^^^^^^^^^^^^

.. productionlist::
   switch_stmt: "switch" `expression` "{" (`case_stmt` | `else_stmt`)* "}"
   case_stmt: "case" `expression` ( "case" `expression` )* `stmt_block`
   else_stmt: "else" `stmt_block`

.. statement:: match
.. statement:: as

Match Statement
^^^^^^^^^^^^^^^

.. productionlist::
   match_stmt: `match1_stmt` | `match1_stmt`
   match1_stmt: "match" `expression` "{" (`as_stmt` | `else_stmt`)* "}"
   match2_stmt: "match" `expression` "as" `ident` ":" `expression` `stmt_block` (`else_stmt`)?
   as_stmt: "as" `ident` ":" `type` `stmt_block`
   else_stmt: "else" `stmt_block`

A :stmt:`match` statement takes different actions based on the type of a value. It has two forms,
short and long.

The long form of the :stmt:`match` statement looks similar to a :stmt:`switch` statement::

  match n {
    as s:String { Debug.writeLn(s); }
    as i:int { return i * 2; }
    else { fail("Unrecognized type!"); }
  }

In the example above, if ``n`` can be a :class:`String`, an :ctype:`int`, or some other type.
If it's a :class:`String`, the value will be bound to the variable ``s``, and the corresponding
:stmt:`as` block executed. If the value is an integer, then it would bind the value to the variable
``i`` and execute the second block. If it's neither of those types, then the :stmt:`else` block
would be executed. (If there is no :stmt:`else` block, then none of the :stmt:`as` blocks are 
executed and control just falls through to the end.)

Note that unlike the :stmt:`switch` statement, the :stmt:`as` type tests are done in the order
that they are declared. The example given above is equivalent to the following::

  if n isa String {
    let s = typecast[String](n);
    Debug.writeLn(s);
  } else if n isa int {
    let i = typecast[int](n);
    return i * 2;
  } else {
    fail("Unrecognized type!");
  }

The short form of the :stmt:`match` statement is used when you only have a single type test::

  // Short form without 'else'
  match n as s:String {
    Debug.writeLn(s);
  }
  
  // Short form with 'else'
  match n as i:int {
    return i * 2;
  } else {
    fail("Unrecognized type!");
  }
  
.. index::
  pair: looping; statements

Looping Statements
------------------

.. statement:: while

While Statement
^^^^^^^^^^^^^^^

.. productionlist::
   while_stmt:"while" `expression` `statement`

.. statement:: do/while

Do / While Statement
^^^^^^^^^^^^^^^^^^^^

.. statement:: for

For Statement
^^^^^^^^^^^^^

For .. In Statement
^^^^^^^^^^^^^^^^^^^

.. statement:: break

Break Statement
^^^^^^^^^^^^^^^

.. productionlist::
   break_stmt:"break" [if `expression`]

.. statement:: continue

Continue Statement
^^^^^^^^^^^^^^^^^^

.. productionlist::
   continue_stmt:"continue" [if `expression`]

Exception Handling Statements
-----------------------------

.. statement:: throw

Throw Statement
^^^^^^^^^^^^^^^

.. productionlist::
   throw_stmt:"throw" [ `expression` ] ;

.. statement:: try
.. statement:: catch
.. statement:: finally

Try/Catch Statement
^^^^^^^^^^^^^^^^^^^

.. productionlist::
   try_stmt:try_stmt1 | try_stmt2
   try_stmt1:"try" `statement`
            :("except" [`ident` ":" `type`] `statement`)+
            :["else" `statement`]
            :["finally" `statement`]
   try_stmt2:"try" `statement`
            :"finally" `statement`

Compilation Directives
----------------------

Compilation directives are statements that don't generate executable code or
data directly, but influence how subsequent code is compiled.

.. statement:: import

Import Statement
^^^^^^^^^^^^^^^^
