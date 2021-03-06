with Timers, Any_Def;
with Dictio_Debug;
package body Status is

  Current_Status : Status_List := Starting;
  Stable_Status : Stable_Status_List := Dead;
  Status_Cb : New_Status_Callback := null;
  Tid : Timers.Timer_Id;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    Prev_Status : constant Status_List := Current_Status;
    New_Status : Status_List;
  begin

    if not Tid.Exists and then Id.Exists then
      -- Called as a timer Cb after an immediate status set
      return False;
    end if;

    begin
      New_Status := Status_List'Val(Data.Lint);
    exception
      when others =>
        Dictio_Debug.Put (Dictio_Debug.Status,
                          "Status: Cb with invalid status "
                        & Any_Def.Image (Data));
        return False;
    end;

    -- Handle status change
    if Current_Status /= Dead and then New_Status /= Current_Status then
      Dictio_Debug.Put (Dictio_Debug.Status,
                        "Status: " & Prev_Status'Img & " -> "
                      & New_Status'Img);
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
    T : Timers.Delay_Rec;
  begin
    if Immediate then
      Dummy := Timer_Cb (Timers.No_Timer, (Any_Def.Lint_Kind,
                                           Status_List'Pos(Status)));
      -- No action on expiration of pendig timer
      Tid.Delete_If_Exists;
    else
      -- Create immediate timer with status (so we go back to main loop)
      T.Delay_Seconds := 0.0;
      Tid.Create (T, Timer_Cb'Access, (Any_Def.Lint_Kind,
                                       Status_List'Pos(Status)));
    end if;
  end Set;

  function Get return Status_List is (Current_Status);

  procedure Set (New_Status_Cb : in New_Status_Callback) is
  begin
    Status_Cb := New_Status_Cb;
  end Set;

  function Get_Stable return Stable_Status_List is (Stable_Status);

end Status;

