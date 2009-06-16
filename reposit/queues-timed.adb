with Ada.Unchecked_Deallocation;
with Timers;
-- Child package of Queues
package body Queues.Timed is

  procedure Free_Timer is new Ada.Unchecked_Deallocation (
    Object => Passive_Timers.Passive_Timer,
    Name => Timer_Access);

  -- Check length before pushing (raise Timed_Full if list lenght is size)
  procedure Check_Length (Queue : in Timed_Type) is
  begin
    if Size /= 0 and then Queue.List.List_Length = Size then
      raise Timed_Full;
    end if;
  end Check_Length;

  -- Remove obsolete items an add this one that will expire at Expdate
  procedure Push (Queue : in out Timed_Type;
                  X       : in Item;
                  Expdate : in Virtual_Time.Time;
                  Clock   : in Virtual_Time.Clock_Access := null) is
    Item : Loc_Item;
  begin
    -- Make room first
    Expire (Queue);
    -- Check length vs. size
    Check_Length (Queue);
    -- Init and insert record
    Item.Data := X;
    Item.Timer := new Passive_Timers.Passive_Timer;
    Item.Timer.Start ( (Timers.Delay_Exp, Clock, Timers.No_Period, Expdate) );
    Queue.List.Insert (Item);
  end Push;

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Delta_Rec;
                  Clock    : in Virtual_Time.Clock_Access := null) is
    Item : Loc_Item;
  begin
    -- Make room first
    Expire (Queue);
    -- Check length vs. size
    Check_Length (Queue);
    -- Init and insert record
    Item.Data := X;
    Item.Timer := new Passive_Timers.Passive_Timer;
    Item.Timer.Start ( (Timers.Delay_Del, Clock, Timers.No_Period, Lifetime) );
    Queue.List.Insert (Item);
  end Push;

  -- Remove obsolete items
  procedure Expire (Queue : in out Timed_Type) is
    Item : Loc_Item;
    Done : Boolean;
  begin
    if Queue.Frozen then
      return;
    end if;
    -- List is always with current pos set to first
    loop
      -- Nothing when list is/becomes empty
      exit when Queue.List.Is_Empty;
      Queue.List.Read (Item, Item_List_Mng.Current);
      if Item.Timer.Has_Expired then
        -- Delete timer that has expired
        Queue.List.Delete (Done => Done);
        Item.Timer.Stop;
        Free_Timer (Item.Timer);
      else
        Done := Queue.List.Check_Move;
        if Done then
          Queue.List.Move_To;
        end if;
      end if;
      -- End of list
      exit when not Done;
    end loop;
    if not Queue.List.Is_Empty then
      Queue.List.Rewind;
    end if;
  end Expire;

  -- Remove all items
  procedure Clear (Queue : in out Timed_Type) is
    Item : Loc_Item;
  begin
    -- List is always with current pos set to first
    loop
      -- Nothing when list is/becomes empty
      exit when Queue.List.Is_Empty;
      Queue.List.Read (Item, Item_List_Mng.Current);
      -- Delete timer
      Queue.List.Delete;
      Item.Timer.Stop;
      Free_Timer (Item.Timer);
    end loop;
    -- Clear and deallocate
    Queue.List.Delete_List;
  end Clear;

  -- Remove obsolete items and retrieve (and also remove)
  --  the first to expire item,
  procedure Pop (Queue : in out Timed_Type; X : out Item) is
    Done : Boolean;
  begin
    Pop (Queue, X, Done);
    if not Done then
      raise Timed_Empty;
    end if;
  end Pop;

  -- Remove obsolete items and retrieve (and also remove)
  --  a non expired item, may raise Timed_Empty
  procedure Pop (Queue : in out Timed_Type; X : out Item; Done : out Boolean) is
    Item : Loc_Item;
  begin
    -- Expire any obsolete
    Expire (Queue);
    -- Check list is not empty
    if Queue.List.Is_Empty then
      Done := False;
      return;
    end if;
    -- Get first item
    -- Moving to next should always be Ok because progressing from first and
    --  always getting. If there is no next record it is because getting
    --  the last one (no exception)!
    Queue.List.Get (Item);
    Item.Timer.Stop;
    Free_Timer (Item.Timer);
    X := Item.Data;
    Done := True;
  end Pop;

  -- Suspend removal of obsolete items
  procedure Freeze (Queue : in out Timed_Type) is
  begin
    Queue.Frozen := True;
  end Freeze;

  -- Re-activate removal of obsolete items
  procedure Unfreeze (Queue : in out Timed_Type) is
  begin
    Queue.Frozen := False;
  end Unfreeze;

end Queues.Timed;

