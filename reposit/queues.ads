package QUEUES is

  generic
    -- Last in first out
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION: package MY_LIFO is new QUEUES.LIFO (SIZE, OBJECT);
    SIZE : POSITIVE;
    type ITEM is private;
  package LIFO is

    subtype NO_RANGE is POSITIVE range 1 .. SIZE;

    -- push an item
    procedure PUSH (X : in ITEM);

    -- pop an item
    procedure POP (X : out ITEM);

    -- read without popping an item
    -- 1 gives the first to pop etc...
    -- LIFO_NOT if NO > number of items in the stack
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1);

    -- read without popping an item
    -- 1 gives the last to pop etc...
    -- LIFO_NOT if NO > number of items in the stack
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1);

    -- Make room by removing the last to be popped
    procedure DISCARD_LAST;

    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    LIFO_FULL : exception;
    LIFO_EMPTY : exception;
    -- raised by look if no is > number of items in the stack
    LIFO_NOT : exception;
  end LIFO;

  generic
    -- First in first out
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION: package MY_FIFO is new QUEUES.FIFO (SIZE, OBJECT);
    SIZE : POSITIVE;
    type ITEM is private;
  package FIFO is

    subtype NO_RANGE is POSITIVE range 1 .. SIZE;

    -- push an item
    procedure PUSH (X : in ITEM);

    -- pop an item
    procedure POP (X : out ITEM);

    -- read without popping an item
    -- 1 gives the last pushed etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1);

    -- read without popping an item
    -- 1 gives the first to be popped etc...
    -- FIFO_NOT if NO > number of items in the stack
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1);

    -- Make room by removing the last to be popped
    procedure DISCARD_LAST;


    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    FIFO_FULL : exception;
    FIFO_EMPTY : exception;
    -- raised by look if no is > number of items in the stack
    FIFO_NOT : exception;

  end FIFO;

  generic
    -- First in first out with Priority
    -- management of a stack of objects to be defined: type OBJECT is...;
    -- each object associated to a priority:
    -- subtpye PRIORITY is INTEGER range ...;
    -- size of the stack: SIZE : POSITIVE := ...;
    -- INSTANCIATION:
    --  package MY_PRIO is new QUEUES.PRIO (SIZE, OBJECT, PRIORITY);
    SIZE : POSITIVE;
    type ITEM is private;
    type PRIORITY is range <>;
  package PRIO is

    subtype NO_RANGE is POSITIVE range 1 .. SIZE;

    -- insert an item
    procedure PUSH (X : in ITEM; P : in PRIORITY := PRIORITY'LAST);

    -- pop an item
    procedure POP (X : out ITEM);

    -- read without popping an item
    -- 1 gives the last to be popped (lowest priority) etc...
    -- PRIO_NOT if NO > number of items in the stack
    procedure LOOK_LAST (X : out ITEM; NO : in NO_RANGE := 1);

    -- read without popping an item
    -- 1 gives the first to be popped (highest priority) etc...
    -- PRIO_NOT if NO > number of items in the stack
    procedure LOOK_FIRST (X : out ITEM; NO : in NO_RANGE := 1);

    -- Make room by removing the last to be popped
    procedure DISCARD_LAST;

    -- exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    PRIO_FULL : exception;
    PRIO_EMPTY : exception;
    -- raised by look if no is > number of items in the stack
    PRIO_NOT : exception;

  end PRIO;

end QUEUES;

