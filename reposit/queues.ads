package Queues is

  generic
    -- Last in first out
    -- management of a stack of objects to be defined: type Object is...;
    -- size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Lifo is new Queues.Lifo (Size, Object);
    Size : Positive;
    type Item is private;
  package Lifo is

    subtype No_Range is Positive range 1 .. Size;

    -- Push an item
    procedure Push (X : in Item);

    -- Pop an item
    procedure Pop (X : out Item);

    -- Read without popping an item
    -- 1 gives the first to pop etc...
    -- Lifo_Not if No > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Read without popping an item
    -- 1 gives the last to pop etc...
    -- Lifo_Not if No > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;

    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Lifo_Full : exception;
    Lifo_Empty : exception;
    -- Raised by look if no is > number of items in the stack
    Lifo_Not : exception;
  end Lifo;

  generic
    -- First in first out
    -- management of a stack of objects to be defined: type Object is...;
    -- size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Fifo is new Queues.Fifo (Size, Object);
    Size : Positive;
    type Item is private;
  package Fifo is

    subtype No_Range is Positive range 1 .. Size;

    -- Push an item
    procedure Push (X : in Item);

    -- Pop an item
    procedure Pop (X : out Item);

    -- Read without popping an item
    -- 1 gives the last pushed etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- Read without popping an item
    -- 1 gives the first to be popped etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;


    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Fifo_Full : exception;
    Fifo_Empty : exception;
    -- Raised by look if no is > number of items in the stack
    Fifo_Not : exception;

  end Fifo;

  generic
    -- First in first out with Priority
    -- management of a stack of objects to be defined: type Object is...;
    -- each object associated to a priority:
    -- subtpye Priority is Integer range ...;
    -- size of the stack: Size : Positive := ...;
    -- Instanciation:
    --  package My_Prio is new Queues.Prio (Size, Object, Priority);
    Size : Positive;
    type Item is private;
    type Priority is range <>;
  package Prio is

    subtype No_Range is Positive range 1 .. Size;

    -- Insert an item
    procedure Push (X : in Item; P : in Priority := Priority'Last);

    -- Pop an item
    procedure Pop (X : out Item);

    -- Read without popping an item
    -- 1 gives the last to be popped (lowest priority) etc...
    -- Prio_Not if No > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- Read without popping an item
    -- 1 gives the first to be popped (highest priority) etc...
    -- Prio_Not if No > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;

    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Prio_Full : exception;
    Prio_Empty : exception;
    -- Raised by look if no is > number of items in the stack
    Prio_Not : exception;

  end Prio;

  generic
    -- Circular buffer
    -- management of a buffer of objects to be defined: type Object is...;
    -- size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Fifo is new Queues.Circ (Size, Object);
    Size : Positive;
    type Item is private;
  package Circ is

    subtype No_Range is Positive range 1 .. Size;

    -- Push an item
    procedure Push (X : in Item);

    -- pop an item
    procedure Pop (X : out Item);

    -- Read without popping an item
    -- 1 gives the last pushed etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- Read without popping an item
    -- 1 gives the first to be popped etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;


    -- Exceptions raised when popping if stack is empty
    Circ_Empty : exception;
    -- Raised by look if no is > number of items in the stack
    Circ_Not : exception;

  end Circ;

end Queues;

