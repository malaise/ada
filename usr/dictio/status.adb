with Timers;
with Debug;
package body Status is

  Current_Status : Status_List := Starting;
  Stable_Status : Stable_Status_List := Dead;
  Status_Cb : New_Status_Callback := null;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    Prev_Status : constant Status_List := Current_Status;
    New_Status : Status_List;
  begin

    begin
      New_Status := Status_List'Val(Data);
    exception
      when others =>
        if Debug.Level_Array(Debug.Status) then
          Debug.Put ("Status: Cb with invalid status " & Data'Img);
        end if;
        return False;
    end;

    -- Handle status change
    if Current_Status /= Dead and then New_Status /= Current_Status then
      if Debug.Level_Array(Debug.Status) then
        Debug.Put ("Status: " & Prev_Status'Img & " -> " & New_Status'Img);
      end if;
      Current_Status := New_Status;
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

  procedure Set (Status : in Status_List;
                 Immediate : in Boolean := False) is
    Dummy : Boolean;
    Tid : Timers.Timer_Id;
    T : Timers.Delay_Rec;
  begin
    if Immediate then
      Dummy := Timer_Cb (Timers.No_Timer, Status_List'Pos(Status));
    else
      -- Create immediate timer with status (so we go back to main loop)
      T.Delay_Seconds := 0.0;
      Tid := Timers.Create (T, Timer_Cb'Access, Status_List'Pos(Status));
    end if;
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

