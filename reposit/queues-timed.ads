-- Queue of items that are kept only for a limited time
with Perpet, Dynamic_List, Virtual_Time, Chronos.Passive_Timers;
generic
  -- Size of the queue, 0 for infinite
  Size : Natural;
  type Item is private;
package Queues.Timed is
  type Timed_Type is tagged limited private;

  subtype Len_Range is Natural range 0 .. Size;

  -- Assign a virtual clock to the queue (the queue does not register but each
  --  Item will have a chrono on this clock.
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
  procedure Read (Queue  : in out Timed_Type;
                  Crit   : in Item;
                  Equal  : access function (X, Criteria : Item) return Boolean;
                  X      : out Item;
                  Found  : out Boolean);

  -- Remove all items if any (no exception)
  procedure Clear (Queue : in out Timed_Type);

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- May raise Timed_Empty
  procedure Pop (Queue : in out Timed_Type; X : out Item);

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- Set Done to False if the queue was empty (and X is not set)
  procedure Pop (Queue : in out Timed_Type; X : out Item; Done : out Boolean);

  -- Exceptions raised during push if the stack is full
  --  or during pop if the stack is empty
  Timed_Full, Timed_Empty : exception;
  -- Exception raised when setting the clock on a non-empty stack
  Timed_Not_Empty : exception;
private
  -- Item and its expiration time
  type Timer_Access is access Chronos.Passive_Timers.Passive_Timer;
  type Loc_Item is record
    Timer : Timer_Access;
    Data : Item;
  end record;

  -- Use Dynamic_List instead of array because need of removing "random" items
  -- List will always have current pos set to first
  package Item_Dyn_List_Mng is new Dynamic_List (Loc_Item);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  type Timed_Type is tagged limited record
    Clock : Virtual_Time.Clock_Access := null;
    List : Item_List_Mng.List_Type;
  end record;

end Queues.Timed;

