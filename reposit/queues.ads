package Queues is

  generic
    -- Last in - first out
    -- Management of a stack of objects to be defined: type Object is...;
    -- Size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Lifo is new Queues.Lifo (Size, Object);
    Size : Positive;
    type Item is private;
  package Lifo is
    type Lifo_Type is tagged private;

    subtype Len_Range is Natural range 0 .. Size;
    subtype No_Range is Positive range 1 .. Size;

    -- Number of Items in Lifo
    function Length (Queue : Lifo_Type) return Len_Range;

    -- Push an item
    procedure Push (Queue : in out Lifo_Type; X : in Item);

    -- Pop an item
    procedure Pop (Queue : in out Lifo_Type; X : out Item);
    function Pop (Queue : in out Lifo_Type) return Item;

    -- Read without popping an item
    -- 1 gives the first to pop etc...
    -- Lifo_Not if No > number of items in the stack
    procedure Look_First (Queue : in out Lifo_Type;
                          X : out Item; No : in No_Range := 1);
    function Look_First (Queue : in out Lifo_Type; No : in No_Range := 1)
                        return Item;

    -- Read without popping an item
    -- 1 gives the last to pop etc...
    -- Lifo_Not if No > number of items in the stack
    procedure Look_Last (Queue : in out Lifo_Type;
                         X : out Item; No : in No_Range := 1);
    function Look_Last (Queue : in out Lifo_Type; No : in No_Range := 1)
                       return Item;

    -- Make room by removing the last to be popped
    procedure Discard_Last (Queue : in out Lifo_Type);

    -- Clear the whole stack
    procedure Clear (Queue : in out Lifo_Type);


    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Lifo_Full : exception;
    Lifo_Empty : exception;
    -- Raised by look if No is > number of items in the stack
    Lifo_Not : exception;
    type Pile_Type is array (No_Range) of Item;
  private
    type Lifo_Type is tagged record
      Pile : Pile_Type;
      -- Ptr is the last pushed except if stack is empty
      --  then it is 0.
      Ptr : Len_Range := 0;
    end record;
  end Lifo;

  generic
    -- First in - first out
    -- Management of a queue of objects to be defined: type Object is...;
    -- Size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Fifo is new Queues.Fifo (Size, Object);
    Size : Positive;
    type Item is private;
  package Fifo is

    type Fifo_Type is tagged private;

    subtype Len_Range is Natural range 0 .. Size;
    subtype No_Range is Positive range 1 .. Size;

    -- Number of Items in Fifo
    function Length (Queue : Fifo_Type) return Len_Range;

    -- Push an item
    procedure Push (Queue : in out Fifo_Type; X : in Item);

    -- Pop an item
    procedure Pop (Queue : in out Fifo_Type; X : out Item);
    function Pop (Queue : in out Fifo_Type) return Item;

    -- Read without popping an item
    -- 1 gives the last pushed etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_Last (Queue : in out Fifo_Type;
                         X : out Item; No : in No_Range := 1);
    function Look_Last (Queue : in out Fifo_Type; No : in No_Range := 1)
                       return Item;

    -- Read without popping an item
    -- 1 gives the first to be popped etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_First (Queue : in out Fifo_Type;
                          X : out Item; No : in No_Range := 1);
    function Look_First (Queue : in out Fifo_Type; No : in No_Range := 1)
                        return Item;

    -- Make room by removing the last to be popped
    procedure Discard_Last (Queue : in out Fifo_Type);

    -- Clear the whole queue
    procedure Clear (Queue : in out Fifo_Type);

    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Fifo_Full : exception;
    Fifo_Empty : exception;
    -- Raised by look if No is > number of items in the stack
    Fifo_Not : exception;
  private
    subtype Ptr_Range is Natural range 0 .. Size - 1;
    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full
    type File_Type is array (Ptr_Range) of Item;
    type Fifo_Type is tagged record
      Full : Boolean := False;
      File : File_Type;
      Ptr_In  : Ptr_Range := 0;
      Ptr_Out : Ptr_Range := 0;
    end record;
  end Fifo;

  generic
    -- First in - first out with Priority
    -- Management of a queue of objects to be defined: type Object is...;
    -- Each object associated to a priority:
    -- subtpye Priority is Integer range ...;
    -- Size of the stack: Size : Positive := ...;
    -- Instanciation:
    --  package My_Prio is new Queues.Prio (Size, Object, Priority);
    Size : Positive;
    type Item is private;
    type Priority is range <>;
  package Prio is
    type Prio_Type is tagged private;

    subtype Len_Range is Natural range 0 .. Size;
    subtype No_Range is Positive range 1 .. Size;

    -- Number of Items in Prio
    function Length (Queue : Prio_Type) return Len_Range;

    -- Insert an item
    procedure Push (Queue : in out Prio_Type;
                    X : in Item; P : in Priority := Priority'Last);

    -- Pop an item
    procedure Pop (Queue : in out Prio_Type; X : out Item);
    function Pop (Queue : in out Prio_Type) return Item;

    -- Read without popping an item
    -- 1 gives the last to be popped (lowest priority) etc...
    -- Prio_Not if No > number of items in the stack
    procedure Look_Last (Queue : in out Prio_Type;
                         X : out Item; No : in No_Range := 1);
    function Look_Last (Queue : in out Prio_Type; No : in No_Range := 1)
                       return Item;

    -- Read without popping an item
    -- 1 gives the first to be popped (highest priority) etc...
    -- Prio_Not if No > number of items in the stack
    procedure Look_First (Queue : in out Prio_Type;
                          X : out Item; No : in No_Range := 1);
    function Look_First (Queue : in out Prio_Type; No : in No_Range := 1)
                        return Item;

    -- Make room by removing the last to be popped
    procedure Discard_Last (Queue : in out Prio_Type);

    -- Clear the whole queue
    procedure Clear (Queue : in out Prio_Type);


    -- Exceptions raised during push if the stack is full
    --  or when popping if stack is empty
    Prio_Full : exception;
    Prio_Empty : exception;
    -- Raised by look if No is > number of items in the stack
    Prio_Not : exception;
  private
    subtype Typ_Ptr is Natural range 0 .. Size - 1;
    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full
    type Cell is record
      Prio : Priority;
      Data : Item;
    end record;
    type File_Type is array (Typ_Ptr) of Cell;
    type Prio_Type is tagged record
      File : File_Type;
      Ptr_In  : Typ_Ptr := 0;
      Ptr_Out : Typ_Ptr := 0;
      Full : Boolean := False;
    end record;
  end Prio;

  generic
    -- Circular buffer
    -- Management of a buffer of objects to be defined: type Object is...;
    -- Size of the stack: Size : Positive := ...;
    -- Instanciation: package My_Fifo is new Queues.Circ (Size, Object);
    Size : Positive;
    type Item is private;
  package Circ is
    type Circ_Type is tagged private;

    subtype Len_Range is Natural range 0 .. Size;
    subtype No_Range is Positive range 1 .. Size;

    -- Number of Items in Circ
    function Length (Queue : Circ_Type) return Len_Range;

    -- Push an item
    procedure Push (Queue : in out Circ_Type; X : in Item);

    -- Pop an item
    procedure Pop (Queue : in out Circ_Type; X : out Item);
    function Pop (Queue : in out Circ_Type) return Item;

    -- Read without popping an item
    -- 1 gives the last pushed etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_Last (Queue : in out Circ_Type;
                         X : out Item; No : in No_Range := 1);
    function Look_Last (Queue : in out Circ_Type; No : in No_Range := 1)
                       return Item;

    -- Read without popping an item
    -- 1 gives the first to be popped etc...
    -- Fifo_Not if No > number of items in the stack
    procedure Look_First (Queue : in out Circ_Type;
                          X : out Item; No : in No_Range := 1);
    function Look_First (Queue : in out Circ_Type; No : in No_Range := 1)
                        return Item;

    -- Make room by removing the last to be popped
    procedure Discard_Last (Queue : in out Circ_Type);

    -- Clear the whole buffer
    procedure Clear (Queue : in out Circ_Type);


    -- Exceptions raised when popping if stack is empty
    Circ_Empty : exception;
    -- Raised by look if No is > number of items in the stack
    Circ_Not : exception;

  private
    subtype Typ_Ptr is Natural range 0 .. Size - 1;
    -- Ptr_In  points to the last pushed
    -- Ptr_Out points to the first to pop
    --  fifo full  is raised if and only if ptr_in  = ptr_out and full
    --  fifo empty is raised if and only if ptr_in  = ptr_out and not full
    type File_Type is array (Typ_Ptr) of Item;
    type Circ_Type is tagged record
      File : File_Type;
      Ptr_In  : Typ_Ptr := 0;
      Ptr_Out : Typ_Ptr := 0;
      Full : Boolean := False;
    end record;

  end Circ;

end Queues;

