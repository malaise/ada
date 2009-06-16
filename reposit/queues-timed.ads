-- Queue of items that are kept only for a limited time
with Perpet, Dynamic_List, Virtual_Time, Passive_Timers;
generic
  -- Size of the queue, 0 for infinite
  Size : Natural;
  type Item is private;
package Queues.Timed is
  type Timed_Type is tagged limited private;

  subtype Len_Range is Natural range 0 .. Size;

  -- Remove obsolete items and add this one, which will expire at Expdate
  procedure Push (Queue : in out Timed_Type;
                  X       : in Item;
                  Expdate : in Virtual_Time.Time;
                  Clock   : in Virtual_Time.Clock_Access := null);

  -- Remove obsolete items an add this one, which will expire after
  --  Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Delta_Rec;
                  Clock    : in Virtual_Time.Clock_Access := null);

  -- Remove obsolete items
  procedure Expire (Queue : in out Timed_Type);

  -- Remove all items if any (no exception)
  procedure Clear (Queue : in out Timed_Type);

  -- Remove obsolete items and retrieve (and also remove)
  --  a non expired item, may raise Timed_Empty
  procedure Pop (Queue : in out Timed_Type; X : out Item);

  -- Remove obsolete items and retrieve (and also remove)
  --  a non expired item, sets Done to False if the
  --  queue was empty (and X is not set)
  procedure Pop (Queue : in out Timed_Type; X : out Item; Done : out Boolean);

  -- Suspend removal of obsolete items
  procedure Freeze (Queue : in out Timed_Type);

  -- Re-activate removal of obsolete items
  procedure Unfreeze (Queue : in out Timed_Type);

  -- Exceptions raised during push if the stack is full
  --  or during pop if the stack is empty
  Timed_Full, Timed_Empty : exception;
private
  -- Item and its expiration time
  type Timer_Access is access Passive_Timers.Passive_Timer;
  type Loc_Item is record
    Timer : Timer_Access;
    Data : Item;
  end record;

  -- Use Dynamic_List instead of array because need of removing "random" items
  -- List will always have current pos set to first
  package Item_Dyn_List_Mng is new Dynamic_List (Loc_Item);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  type Timed_Type is tagged limited record
    Frozen : Boolean := False;
    List : Item_List_Mng.List_Type;
  end record;

end Queues.Timed;

