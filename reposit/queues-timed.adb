with Dynamic_List;
-- Child package of Queues
package body Queues.Timed is

  -- Sort by chrono expiration
  function "<" (X1, X2 : Loc_Item) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return X1.Exp < X2.Exp;
  end "<";
  procedure Sort is new Item_List_Mng.Sort ("<");

  -- Check length before pushing (raise Timed_Full if list lenght is size)
  procedure Check_Length (Queue : in Timed_Type) is
  begin
    if Size /= 0 and then Item_List_Mng.List_Length(Lt(Queue)) = Size then
      raise Timed_Full;
    end if;
  end Check_Length;

  -- Remove obsolete items an add this one that will expire at Expdate
  procedure Push (Queue : in out Timed_Type;
                  X : in Item; Expdate : in Ada.Calendar.Time) is
  begin
    -- Make room first
    Expire (Queue);
    -- Check length vs. size
    Check_Length (Queue);
    -- Insert record
    Item_List_Mng.Insert (Lt(Queue), (Expdate, X));
    -- Re-sort list. This also sets current pos to first
    Sort (Lt(Queue));
  end Push;

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (Queue : in out Timed_Type;
                  X : in Item; Lifetime : in Perpet.Delta_Rec) is
    Exp : Ada.Calendar.Time;
    use Perpet, Ada.Calendar; -- For both "+"
  begin
    -- Compute expiration time
    Exp := Ada.Calendar.Clock + Lifetime.Days + Lifetime.Secs;
    Push (Queue, X, Exp);
  end Push;

  -- Remove obsolete items
  procedure Expire (Queue : in out Timed_Type) is
    Now : Ada.Calendar.Time;
    L : Loc_Item;
    use type Ada.Calendar.Time;
  begin
    Now := Ada.Calendar.Clock;
    -- List is always sorted and with current pos set to first
    loop
      -- Nothing if empty list
      exit when Item_List_Mng.Is_Empty (Lt(Queue));
      -- Stop expiring when Exp is in future
      Item_List_Mng.Read (Lt(Queue), L, Item_List_Mng.Current);
      exit when L.Exp > Now;
      -- Moving to next should always be Ok because progressing from first and
      --  always deleting. If there is no next record it is because deleting
      --  the last one (no exception)!
      Item_List_Mng.Delete (Lt(Queue));
    end loop;
    -- List is still sorted and with current pos set to first
  end Expire;

  -- Remove all items
  procedure Clear (Queue : in out Timed_Type) is
  begin
    -- Clear and deallocate
    Item_List_Mng.Delete_List (Lt(Queue));
  end Clear;

  -- Remove obsolete items and retrieve (and also remove)
  --  the first to expire item,
  procedure Pop (Queue : in out Timed_Type; X : out Item) is
    L : Loc_Item;
  begin
    -- Expire any obsolete
    Expire (Queue);
    -- Check list is not empty
    if Item_List_Mng.Is_Empty (Lt(Queue)) then
      raise Timed_Empty;
    end if;
    -- Get first item
    -- Moving to next should always be Ok because progressing from first and
    --  always getting. If there is no next record it is because getting
    --  the last one (no exception)!
    Item_List_Mng.Get (Lt(Queue), L);
    X := L.Data;
  end Pop;

end Queues.Timed;

