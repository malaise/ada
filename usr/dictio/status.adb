with Timers, Dynamic_List;
with Debug;
package body Status is

  Current_Status : Status_List := Starting;
  Stable_Status : Stable_Status_List := Dead;
  Status_Cb : New_Status_Callback := null;

  -- List of pending statuses
  type Pending_Rec is record
    Tid : Timers.Timer_Id;
    Status : Status_List;
  end record;
  function Tid_Match (R1, R2 : Pending_Rec) return Boolean is
    use type Timers.Timer_Id;
  begin
    return R1.Tid = R2.Tid;
  end Tid_Match;
  package Pending_List_Mng is new Dynamic_List(Pending_Rec);
  Pending_List : Pending_List_Mng.List_Type;
  procedure Search_Tid is new Pending_List_Mng.Search (Tid_Match);

  function Timer_Cb (Id : Timers.Timer_Id) return Boolean is
    Rec : Pending_Rec;
    Prev_Status : constant Status_List := Current_Status;
  begin
    -- Look for status in pending list
    Rec.Tid := Id;
    begin
      Search_Tid (Pending_List, Rec, From_Current => False);
    exception
      when Pending_List_Mng.Not_In_List =>
        if Debug.Level_Array(Debug.Status) then
          Debug.Put ("Status: Timer " & Timers.Image (Id)
                   & " not found in pending list");
        end if;
        return False;
    end;
    -- Read status and delete rec
    if Pending_List_Mng.Get_Position (Pending_List) = 1 then
      Pending_List_Mng.Get (Pending_List, Rec, Pending_List_Mng.Next);
    else
      Pending_List_Mng.Get (Pending_List, Rec, Pending_List_Mng.Prev);
    end if;

    -- Handle status change
    if Current_Status /= Dead and then Rec.Status /= Current_Status then
      if Debug.Level_Array(Debug.Status) then
        Debug.Put ("Status: " & Prev_Status'Img & " -> " & Rec.Status'Img);
      end if;
      Current_Status := Rec.Status;
      case Current_Status is
        when Starting | Init | Dead =>
          Stable_Status := Dead;
        when Slave | Master =>
          Stable_Status := Current_Status;
        when Fight =>
          null;
      end case;
      if Status_Cb /= null then
        Status_Cb.all (Prev_Status, Current_Status);
      end if;
    end if;
    return False;
  end Timer_Cb;

  procedure Set (Status : in Status_List) is
    Tid : Timers.Timer_Id;
    T : Timers.Delay_Rec;
  begin
    -- Create immediate timer (so we go back to main loop)
    T.Delay_Seconds := 0.0;
    Tid := Timers.Create (T, Timer_Cb'Access);
    
    -- Append pending status for expiration
    if not Pending_List_Mng.Is_Empty (Pending_List) then
      Pending_List_Mng.Move_To (Pending_List, Pending_List_Mng.Prev, 0, False);
    end if;
    Pending_List_Mng.Insert (Pending_List, (Tid, Status));
  end Set;

  function Get return Status_List is
  begin
    return Current_Status;
  end Get;

  procedure Set (New_Status_Cb : in New_Status_Callback) is
  begin
    Status_Cb := New_Status_Cb;
  end Set;

  function Get_Stable return Stable_Status_List is
  begin
    return Stable_Status;
  end Get_Stable;

end Status;

