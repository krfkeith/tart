.. highlight:: tart
  :linenothreshold: 30

Some Examples of Tart Code
==========================

Here's some examples of Tart code to get you started.

Let's start by declaring a new type. We'll create a new structure, named
``Point``, which contains two integers ``x`` and ``y``::

  // A basic structure declaration
  struct Point {
    var x:int;
    var y:int;
  }
  
  var p = Point(10, 10);
  
The colon character is used in Tart to mean "of type". So ``x:int`` means
a variable named 'x' of type ``int``.

Once a type is declared, you can call that type by name to create a new
instance of the type. So the expression ``Point(10, 10)`` creates a new
``Point`` instance.

In the example, we assign the new ``Point`` object to a variable named ``p`` which is defined using
the :keyword:`var` keyword. Since the compiler knows what type is being assigned to ``p``, there is
not need to specify the variable type. We could just as easily have declared ``p`` like this::

  var p:Point = Point(10, 10);
  
.. note:: If you are familiar with C++, you may notice that the ``Point`` type does not have a
  constructor definitions. This causes the compiler to generate a default constructor that has one
  argument for each non-private member of the type. So in the above example, the default
  constructor has the type signature ``Point(x:int = 0, y:int = 0)``.

Here's a somewhat more interesting example::

  // An interface that defines a mapping between a key type and a value type.
  class Scrollbar : Widget {
    // Member variables
    private {
      var val:int;
      var minVal:int;
      var maxVal:int;
    }

    // Constructor
    def construct(var:minValue, var:maxValue) {
      // Initialize member variables from constructor args.
      minVal = val = minValue;
      maxVal = maxValue;
    }

    // Attach widget to parent
    def attach(parent:Widget) {
      parent.addChild(self);
    }

    // Getter and setter for 'value' field.
    def value:int {
      get { return val; }
      set (n:int) {
        val = n;
        repaint();
      }
    }
  }

The example is somewhat contrived, but it does illustrate a number of important points:

* Members are public by default. The :keyword:`private` modifier can be used to declare individual
  members as private, or you can use the block syntax to declare a whole group of members private.
* The constructor method for a type is always called :meth:`construct`.
* Class methods are declared with the :keyword:`def` keyword.
* You can declare named properties which have custom getters and setters that are defined with
  :keyword:`get` and :keyword:`set`.

.. note:: In real code, you would probably want to use a naming convention - such as a trailing
  underscore - for private member variables. Such style conventions are not required by Tart,
  however, and therefore are omitted from this simple example.
