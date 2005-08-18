-- Queue of item that are kept only for a limited time
with Ada.Calendar;
with Perpet;
generic
  -- Size of the queue, 0 for infinite
  Size : Natural;
  type Item is private;
package Queues.Timed is

  -- Remove obsolete items and add this one that will expire at Expdate
  procedure Push (X : in Item; Expdate : in Ada.Calendar.Time);

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (X : in Item; Lifetime : in Perpet.Delta_Rec);

  -- Remove obsolete items
  procedure Expire;

  -- Remove all items if any (no exception)
  procedure Clear;

  -- Remove obsolete items and retrieve (and also remove)
  --  the first to expire item,
  procedure Pop (X : out Item);
  function Pop return Item;

  -- Exceptions raised during push if the stack is full
  --  or when popping if stack is empty
  Timed_Full, Timed_Empty : exception;

end Queues.Timed;

