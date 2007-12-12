-- Queue of item that are kept only for a limited time
with Ada.Calendar;
with Perpet, Dynamic_List;
generic
  -- Size of the queue, 0 for infinite
  Size : Natural;
  type Item is private;
package Queues.Timed is
  type Timed_Type is tagged limited private;

  -- Remove obsolete items and add this one that will expire at Expdate
  procedure Push (Queue : in out Timed_Type;
                  X : in Item; Expdate : in Ada.Calendar.Time);

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X : in Item; Lifetime : in Perpet.Delta_Rec);

  -- Remove obsolete items
  procedure Expire (Queue : in out Timed_Type);

  -- Remove all items if any (no exception)
  procedure Clear (Queue : in out Timed_Type);

  -- Remove obsolete items and retrieve (and also remove)
  --  the first to expire item,
  procedure Pop (Queue : in out Timed_Type; X : out Item);

  -- Exceptions raised during push if the stack is full
  --  or when popping if stack is empty
  Timed_Full, Timed_Empty : exception;
private
    -- Item and its expiration time
  type Loc_Item is record
    Exp : Ada.Calendar.Time;
    Data : Item;
  end record;
   -- Use Dynamic_List instead of array because need for sorting and removing
  --  "random" items
  -- List will always be sorted and with current pos set to first
  package Item_Dyn_List_Mng is new Dynamic_List (Loc_Item);
  package Item_List_Mng renames Item_Dyn_List_Mng.Dyn_List;
  subtype Lt is Item_List_Mng.List_Type;
  type Timed_Type is new Item_List_Mng.List_Type with null record;
  
end Queues.Timed;

