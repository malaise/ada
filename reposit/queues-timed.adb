with Dynamic_List;
-- Child package of Queues
package body Queues.Timed is

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
  Item_List : Item_List_Mng.List_Type;
  -- Sort by chrono expiration
  function "<" (X1, X2 : Loc_Item) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return X1.Exp < X2.Exp;
  end "<";
  procedure Sort is new Item_List_Mng.Sort ("<");

  -- Check length before pushing (raise Timed_Full if list lenght is size)
  procedure Check_Length is
  begin
    if Size /= 0 and then Item_List_Mng.List_Length(Item_List) = Size then
      raise Timed_Full;
    end if;
  end Check_Length;

  -- Remove obsolete items an add this one that will expire at Expdate
  procedure Push (X : in Item; Expdate : in Ada.Calendar.Time) is
  begin
    -- Make room first
    Expire;
    -- Check length vs. size
    Check_Length;
    -- Insert record
    Item_List_Mng.Insert (Item_List, (Expdate, X));
    -- Re-sort list. This also sets current pos to first
    Sort (Item_List);
  end Push;

  -- Remove obsolete items an add this one that will expire after
  --  Lifetime
  procedure Push (X : in Item; Lifetime : in Perpet.Delta_Rec) is
    Exp : Ada.Calendar.Time;
    use Perpet, Ada.Calendar; -- For both "+"
  begin
    -- Compute expiration time
    Exp := Ada.Calendar.Clock + Lifetime.Days + Lifetime.Secs;
    Push (X, Exp);
  end Push;

  -- Remove obsolete items
  procedure Expire is
    Now : Ada.Calendar.Time;
    L : Loc_Item;
    use type Ada.Calendar.Time;
  begin
    Now := Ada.Calendar.Clock;
    -- List is always sorted and with current pos set to first
    loop
      -- Nothing if empty list
      exit when Item_List_Mng.Is_Empty (Item_List);
      -- Stop expiring when Exp is in future
      Item_List_Mng.Read (Item_List, L, Item_List_Mng.Current);
      exit when L.Exp > Now;
      -- Moving to next should always be Ok because progressing from first and
      --  always deleting. If there is no next record it is because deleting
      --  the last one (no exception)!
      Item_List_Mng.Delete (Item_List);
    end loop;
    -- List is still sorted and with current pos set to first
  end Expire;

  -- Remove all items
  procedure Clear is
  begin
    -- Clear and deallocate
    Item_List_Mng.Delete_List (Item_List);
  end Clear;

  -- Remove obsolete items and retrieve (and also remove)
  --  the first to expire item,
  function Pop return Item is
    L : Loc_Item;
  begin
    -- Expire any obsolete
    Expire;
    -- Check list is not empty
    if Item_List_Mng.Is_Empty (Item_List) then
      raise Timed_Empty;
    end if;
    -- Get first item
    -- Moving to next should always be Ok because progressing from first and
    --  always getting. If there is no next record it is because getting
    --  the last one (no exception)!
    Item_List_Mng.Get (Item_List, L);
    return L.Data;
  end Pop;

  procedure Pop (X : out Item) is
  begin
    X := Pop;
  end Pop;

end Queues.Timed;

