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

    -- Assign a virtual clock to the queue (the queue does not register but each
  --  Item will have a chrono on this clock.
  -- By default, clock is null (real time).
  -- The Queue must be empty, otherwise tje exception Timed_Not_Empty is raised
  procedure Attach (Queue : in out Timed_Type;
                    Clock : in Virtual_Time.Clock_Access) is
  begin
    if not Queue.List.Is_Empty then
      raise Timed_Not_Empty;
    end if;
    Queue.Clock := Clock;
  end Attach;

  -- Remove obsolete items an add this one
  procedure Add_Item (Queue : in out Timed_Type;
                      X     : in Item;
                      Timer : out Timer_Access) is
    Item : Loc_Item;
  begin
    -- Make room first
    Expire (Queue);
    -- Check length vs. size
    Check_Length (Queue);
    -- Init record and timer access
    Item.Data := X;
    Item.Timer := new Passive_Timers.Passive_Timer;
    Timer := Item.Timer;
    -- Append record and rewind
    Queue.List.Rewind (False, Item_List_Mng.Prev);
    Queue.List.Insert (Item);
    Queue.List.Rewind;
  end Add_Item;

  procedure Push (Queue    : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Natural_Duration) is
    Timer : Timer_Access;
  begin
    Add_Item (Queue, X, Timer);
    Timer.Start ( (Timers.Delay_Sec, Queue.Clock, Timers.No_Period, Lifetime) );
  end Push;

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (Queue    : in out Timed_Type;
                  X        : in Item;
                  Lifetime : in Perpet.Delta_Rec) is
    Timer : Timer_Access;
  begin
    Add_Item (Queue, X, Timer);
    Timer.Start ( (Timers.Delay_Del, Queue.Clock, Timers.No_Period, Lifetime) );
  end Push;

  -- Remove obsolete items
  procedure Expire (Queue : in out Timed_Type) is
    Item : Loc_Item;
    Moved : Boolean;
  begin
    -- List is always with current pos set to first
    loop
      -- Nothing when list is/becomes empty
      exit when Queue.List.Is_Empty;
      Queue.List.Read (Item, Item_List_Mng.Current);
      if Item.Timer.Has_Expired then
        -- Delete timer that has expired
        Queue.List.Delete (Moved => Moved);
        Item.Timer.Stop;
        Free_Timer (Item.Timer);
      else
        Moved := Queue.List.Check_Move;
        if Moved then
          Queue.List.Move_To;
        end if;
      end if;
      -- End of list
      exit when not Moved;
    end loop;
    Queue.List.Rewind (False);
  end Expire;

   -- Remove expired items and read the first item pushed that matches
  --  criteria
  procedure Read (Queue  : in out Timed_Type;
                  Crit   : in Item;
                  Equal  : access function (X, Criteria : Item) return Boolean;
                  X      : out Item;
                  Found  : out Boolean) is
    Litem : Loc_Item;
    function Lequal (Current, Criteria : Loc_Item) return Boolean is
    begin
      return Equal (Current.Data, Criteria.Data);
    end Lequal;
  begin
    -- Make room first
    Expire (Queue);
    -- Search
    Litem.Data := Crit;
    Queue.List.Search_Match (Found, Lequal'Access, Litem,
                             From => Item_List_Mng.Absolute);
    -- Read
    if Found then
      Queue.List.Read (Litem, Item_List_Mng.Current);
      X := Litem.Data;
    end if;
    Queue.List.Rewind (False);
  end Read;

  -- Remove all items
  procedure Clear (Queue : in out Timed_Type) is
    Litem : Loc_Item;
  begin
    -- List is always with current pos set to first
    loop
      -- Nothing when list is/becomes empty
      exit when Queue.List.Is_Empty;
      Queue.List.Read (Litem, Item_List_Mng.Current);
      -- Delete timer
      Queue.List.Delete;
      Litem.Timer.Stop;
      Free_Timer (Litem.Timer);
    end loop;
    -- Clear and deallocate
    Queue.List.Delete_List;
  end Clear;

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- May raise Timed_Empty
  procedure Pop (Queue : in out Timed_Type; X : out Item) is
    Done : Boolean;
  begin
    Pop (Queue, X, Done);
    if not Done then
      raise Timed_Empty;
    end if;
  end Pop;

  -- Retrieve (and also remove) a non expired item
  -- Does not expire items
  -- Items are retrieved in the order there where pushed
  -- Set Done to False if the queue was empty (and X is not set)
  procedure Pop (Queue : in out Timed_Type; X : out Item; Done : out Boolean) is
    Litem : Loc_Item;
  begin
    -- Check list is not empty
    if Queue.List.Is_Empty then
      Done := False;
      return;
    end if;
    -- Get first item
    -- Moving to next should always be Ok because progressing from first and
    --  always getting. If there is no next record it is because getting
    --  the last one (no exception)!
    Queue.List.Get (Litem);
    Litem.Timer.Stop;
    Free_Timer (Litem.Timer);
    X := Litem.Data;
    Done := True;
  end Pop;

end Queues.Timed;

