-- Queue of items that are kept only for a limited time
private with Ada.Finalization;
private with Long_Long_Limited_List, Chronos.Passive_Timers;
with Perpet, Virtual_Time;
generic
  -- Size of the queue, 0 for infinite
  type Item is private;
package Queues.Timed is
  type Timed_Type (Size : Len_Range) is tagged limited private;

  -- Assign a virtual clock to the queue (the queue does not register but each
  --  Item will have a passive timer on this clock.
  -- By default, clock is null (real time).
  -- The Queue must be empty, otherwise the exception Timed_Not_Empty is raised
  procedure Attach (Queue : in out Timed_Type;
                    Clock : in Virtual_Time.Clock_Access);

  -- Remove expired items and add this one, which will expire after Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Natural_Duration);

  -- Remove expired items and add this one, which will expire after Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Delta_Rec);

  -- Remove expired items
  procedure Expire (Queue : in out Timed_Type);

  -- Remove expired items and read the first item pushed that matches
  --  criteria
  -- Exceptions raised by Equal are propagated as Equal_Error
  procedure Read (Queue  : in out Timed_Type;
                  Crit   : in Item;
                  Equal  : not null access function (X, Criteria : Item)
                                           return Boolean;
                  X      : out Item;
                  Found  : out Boolean);

  -- Remove all items if any (no exception)
  -- Leave the clock attached, if any
  procedure Clear (Queue : in out Timed_Type);

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- May raise Timed_Empty
  procedure Pop (Queue : in out Timed_Type; X : out Item);
  function Pop (Queue : in out Timed_Type) return Item;

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- Set Done to False if the queue was empty (and X is not set)
  procedure Pop (Queue : in out Timed_Type; X : out Item; Done : out Boolean);
  function Pop (Queue : in out Timed_Type; Done : out Boolean) return Item;

  -- Exceptions raised during push if the stack is full
  --  or during pop if the stack is empty
  Timed_Full, Timed_Empty : exception;
  -- Exception raised when setting the clock on a non-empty stack
  Timed_Not_Empty : exception;
  -- If Equal raises an exception
  Equal_Error : exception;
private
  -- Item and its expiration time
  type Timer_Access is access Chronos.Passive_Timers.Passive_Timer;
  type Loc_Item is record
    Timer : Timer_Access;
    Data : Item;
  end record;

  -- Use Dynamic_List instead of array because need of removing "random" items
  -- List will always have current pos set to first
  procedure Set (To : out Loc_Item; Val : in Loc_Item);
  package Item_List_Mng is new Long_Long_Limited_List (Loc_Item, Set);
  type Timed_Type (Size : Len_Range) is
      new Ada.Finalization.Limited_Controlled with record
    Clock : Virtual_Time.Clock_Access := null;
    List : Item_List_Mng.List_Type;
  end record;

  -- Destructor
  overriding procedure Finalize (Queue: in out Timed_Type) renames Clear;

end Queues.Timed;

