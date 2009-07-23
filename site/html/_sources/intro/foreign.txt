.. highlight:: tart
  :linenothreshold: 30

Calling C functions
===================

Tart makes it easy to call library functions written in C. Unlike Java,
you don't need to write a special "wrapper" around the C library.
Instead, you declare these functions directly in Tart using the :attr:`Extern`
attribute::

  [Extern("img_open")]
  def OpenImage(name:NativePointer<[ubyte]>, length:int);
  
The argument to ``Extern`` is the C linkage name of the function. This causes
the Tart compiler to emit a reference to that name in the module being
compiled. Note that within Tart programs, you still use the declared
function name (``OpenImage`` in the example.)

The :attr:`Extern` attribute can be applied to class methods as well
as standalone functions. If the method isn't static, then the ``self``
parameter will be passed as the first argument.

The Tart compiler doesn't do anything special to the arguments being
passed to the library function. In other words, it passes each argument
exactly the same way as it would if calling a function written in Tart.
For primitive types, such as integers or native arrays, the memory
layout is no different than the corresponding C type. Tart structs are also
similar in memory layout to their C counterparts, so it should be possible
to pass them to C library functions as well.

For object types, such as :class:`String`, Tart passes a pointer to
the object. Unfortunately, most native libraries won't know how to
interpret a Tart class. Currently the only way around this is to write
a wrapper around the native function. Note that this wrapper could be
written in C, or it could be written in Tart, whichever is most convenient.
For a C wrapper, you would import a header that defines the various
Tart types that you need to work with. For a Tart wrapper, you would
convert the arguments into types that the native C function understands.

At some point, there will most likely be support for automatic translation
of arguments, probably using some kind of per-parameter annotation on the
external function declaration.

.. note:: the issue of interaction between garbage collection and
  native functions has not been adequately dealt with in the current
  design (since the collector hasn't been written yet.) Initially,
  a non-copying mark & sweep collector will be used, however a
  copying collector may be more efficient especially for young
  generation objects. The compiler will need to auto-generate
  wrapper code that locks objects in memory to handle this case.
