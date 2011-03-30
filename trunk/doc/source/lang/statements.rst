Statements and Control Structures
=================================

Simple Statements
-----------------

.. index::
  pair: empty; statement

The Empty Statement
^^^^^^^^^^^^^^^^^^^

.. productionlist::
   empty_stmt: ";"

.. index::
  pair: expression; statement

Expression Statement
^^^^^^^^^^^^^^^^^^^^

.. productionlist::
   expr_stmt: `expression` ";"

.. index::
  pair: assignment; statement

Assignment Statement
^^^^^^^^^^^^^^^^^^^^

.. productionlist::
   assign_stmt: `ident` "=" (`ident` "=")* `expression` ";"

.. index::
  pair: return; statement

Return Statement
^^^^^^^^^^^^^^^^

.. productionlist::
   return_stmt: "return" [`expression`] ["if" `expression`] ";"

Compound Statements
-------------------

.. index::
  pair: block; statement

Block Statement
^^^^^^^^^^^^^^^

   block_stmt: "{" (`statement`)* "}"

.. index::
  pair: with; statement

With Statement
^^^^^^^^^^^^^^

Conditional Statements
----------------------

.. index::
  pair: if; statement

If Statement
^^^^^^^^^^^^

.. productionlist::
   if_stmt: "if" `expression` `statement` [ "else" `statement` ]

.. index::
  pair: case; statement

Case Statement
^^^^^^^^^^^^^^

Looping Statements
------------------

.. index::
  pair: while; statement

While Statement
^^^^^^^^^^^^^^^

.. productionlist::
   while_stmt: "while" `expression` `statement`

.. index::
  pair: do .. while; statement

Do / While Statement
^^^^^^^^^^^^^^^^^^^^

.. index::
  pair: for; statement

For Statement
^^^^^^^^^^^^^

.. index::
  pair: for .. in; statement

For .. In Statement
^^^^^^^^^^^^^^^^^^^

.. index::
  pair: break; statement

Break Statement
^^^^^^^^^^^^^^^

.. productionlist::
   break_stmt: "break" [if `expression`] ";"

.. index::
  pair: continue; statement

Continue Statement
^^^^^^^^^^^^^^^^^^

.. productionlist::
   continue_stmt: "continue" [if `expression`] ";"

Exception Handling Statements
-----------------------------

.. index::
  pair: throw; statement

Throw Statement
^^^^^^^^^^^^^^^

.. productionlist::
   throw_stmt: "throw" [ `expression` ] ;

.. index::
  pair: try .. catch; statement

Try/Catch Statement
^^^^^^^^^^^^^^^^^^^

.. productionlist::
   try_stmt: try_stmt1 | try_stmt2
   try_stmt1: "try" `statement`
            : ("except" [`ident` ":" `type`] `statement`)+
            : ["else" `statement`]
            : ["finally" `statement`]
   try_stmt2: "try" `statement`
            : "finally" `statement`

Compilation Directives
----------------------

Compilation directives are statements that don't generate executable code or
data directly, but influence how subsequent code is compiled.

.. index::
  pair: import; statement

Import Statement
^^^^^^^^^^^^^^^^

