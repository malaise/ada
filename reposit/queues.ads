package Queues is

  generic
    -- Last in first out
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION: package MY_LIFO is new QUEUES.LIFO (SIZE, OBJECT);
    Size : Positive;
    type Item is private;
  package Lifo is

    subtype No_Range is Positive range 1 .. Size;

    -- push an item
    procedure Push (X : in Item);

    -- pop an item
    procedure Pop (X : out Item);

    -- read without popping an item
    -- 1 gives the first to pop etc...
    -- LIFO_NOT if NO > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- read without popping an item
    -- 1 gives the last to pop etc...
    -- LIFO_NOT if NO > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;

    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Lifo_Full : exception;
    Lifo_Empty : exception;
    -- raised by look if no is > number of items in the stack
    Lifo_Not : exception;
  end Lifo;

  generic
    -- First in first out
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION: package MY_FIFO is new QUEUES.FIFO (SIZE, OBJECT);
    Size : Positive;
    type Item is private;
  package Fifo is

    subtype No_Range is Positive range 1 .. Size;

    -- push an item
    procedure Push (X : in Item);

    -- pop an item
    procedure Pop (X : out Item);

    -- read without popping an item
    -- 1 gives the last pushed etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- read without popping an item
    -- 1 gives the first to be popped etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;


    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Fifo_Full : exception;
    Fifo_Empty : exception;
    -- raised by look if no is > number of items in the stack
    Fifo_Not : exception;

  end Fifo;

  generic
    -- First in first out with Priority
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- each object associated to a priority:
    -- subtpye PRIORITY is INTEGER range ...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION:
    --  package MY_PRIO is new QUEUES.PRIO (SIZE, OBJECT, PRIORITY);
    Size : Positive;
    type Item is private;
    type Priority is range <>;
  package Prio is

    subtype No_Range is Positive range 1 .. Size;

    -- insert an item
    procedure Push (X : in Item; P : in Priority := Priority'Last);

    -- pop an item
    procedure Pop (X : out Item);

    -- read without popping an item
    -- 1 gives the last to be popped (lowest priority) etc...
    -- PRIO_NOT if NO > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- read without popping an item
    -- 1 gives the first to be popped (highest priority) etc...
    -- PRIO_NOT if NO > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;

    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Prio_Full : exception;
    Prio_Empty : exception;
    -- raised by look if no is > number of items in the stack
    Prio_Not : exception;

  end Prio;

  generic
    -- Circular buffer
    -- management of a buffer of objects to be defined: type OBJECT is...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION: package MY_FIFO is new QUEUES.CIRC (SIZE, OBJECT);
    Size : Positive;
    type Item is private;
  package Circ is

    subtype No_Range is Positive range 1 .. Size;

    -- push an item
    procedure Push (X : in Item);

    -- pop an item
    procedure Pop (X : out Item);

    -- read without popping an item
    -- 1 gives the last pushed etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure Look_Last (X : out Item; No : in No_Range := 1);

    -- read without popping an item
    -- 1 gives the first to be popped etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure Look_First (X : out Item; No : in No_Range := 1);

    -- Make room by removing the last to be popped
    procedure Discard_Last;


    -- exceptions raised when popping if stack is empty
    Circ_Empty : exception;
    -- raised by look if no is > number of items in the stack
    Circ_Not : exception;

  end Circ;

end Queues;

