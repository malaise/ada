with Timers, Normal;
with Debug, Parse, Local_Host_Name, Nodes, Errors, Versions, Intra_Dictio;
package body Fight_Mng is

  Tid : Timers.Timer_Id := Timers.No_Timer;
  Per : Timers.Timer_Id := Timers.No_Timer;

  Last_Status : Status.Status_List;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  function Perio_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  Fight_Actions : Fight_Action;

  function In_Fight return Boolean is
    use type Timers.Timer_Id;
  begin
    return Tid /= Timers.No_Timer;
  end In_Fight;

  procedure Start (New_Status : in Status.Status_List;
                   Timeout : in Duration;
                   Actions : in Fight_Action) is
    T : Timers.Delay_Rec;
    use type Status.Status_List;
  begin
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: start");
    end if;

    -- Init
    Nodes.Init_List;
    Fight_Actions := Actions;
    Last_Status := Status.Get;

    -- Change status
    Status.Set (New_Status, Immediate => True);
    if New_Status /= Last_Status then
      Intra_Dictio.Send_Status;
    end if;

    -- End of fight timer
    T.Delay_Seconds := Timeout;
    Tid := Timers.Create (T, Timer_Cb'Access);

    -- Periodical timer to send Last_Status
    Intra_Dictio.Send_Status (Last_Status);
    T.Delay_Seconds := Timeout / 4;
    T.Period := Timeout / 4;
    Per := Timers.Create (T, Perio_Cb'Access);
  end Start;


  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Sync : in Boolean;
                   Prio : in Args.Prio_Str;
                   Diff : in Boolean;
                   Extra : in String := "") is
    use type Status.Status_List;
  begin
    if not In_Fight then
      if Debug.Level_Array(Debug.Fight) then
        Debug.Put ("Fight.Event: Not in fight");
      end if;
      return;
    end if;
    declare
      Vers : constant String
           := Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Ver);
    begin
      if Vers /= "" and Vers /= Versions.Intra then
        Debug.Put_Error ("ERROR. Fight: version mismatch. Received "
                       & Vers & " while being " & Versions.Intra);
        raise Errors.Exit_Error;
      end if;
    end;

    Nodes.Set (From, Stat, Sync, Prio);
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: received Stat: " & Stat'Img
               & "  From: " & Parse (From));
    end if;
  end Event;


  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    Result : Nodes.Check_Result_List;
    use type Timers.Timer_Id;
  begin
    Result := Nodes.Check;
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: ends " & Result'Img);
    end if;
    Tid := Timers.No_Timer;
    if Per /= Timers.No_Timer then
      Timers.Delete (Per);
      Per := Timers.No_Timer;
    end if;

    -- This may restart a fight
    Status.Set (Fight_Actions(Result));
    return False;
  end Timer_Cb;

  function Perio_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    use type Status.Status_List;
  begin
    if In_Fight and then Last_Status /= Status.Fight then
      if Debug.Level_Array(Debug.Fight) then
        Debug.Put ("Fight: send status " & Last_Status'Img);
      end if;
      Intra_Dictio.Send_Status (Last_Status);
    end if;
    return False;
  end Perio_Cb;

end Fight_Mng;

