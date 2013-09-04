with Timers;
with Dictio_Debug, Errors, Versions, Intra_Dictio;
package body Fight_Mng is

  Tid : Timers.Timer_Id;
  Per : Timers.Timer_Id;

  Last_Status : Status.Status_List;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  function Perio_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  Fight_Actions : Fight_Action;

  function In_Fight return Boolean is
  begin
    return Tid.Exists;
  end In_Fight;

  procedure Start (New_Status : in Status.Status_List;
                   Timeout : in Duration;
                   Actions : in Fight_Action) is
    T : Timers.Delay_Rec;
    use type Status.Status_List;
  begin
    Dictio_Debug.Put (Dictio_Debug.Fight, "Start");

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
    Tid.Create (T, Timer_Cb'Access);

    -- Periodical timer to send Last_Status
    Intra_Dictio.Send_Status (Last_Status);
    T.Delay_Seconds := Timeout / 4;
    T.Period := Timeout / 4;
    Per.Create (T, Perio_Cb'Access);
  end Start;


  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Sync : in Boolean;
                   Prio : in Args.Prio_Str;
                   Extra : in String := "") is
    use type Status.Status_List;
  begin
    if not In_Fight then
      Dictio_Debug.Put (Dictio_Debug.Fight, "Not in fight");
      return;
    end if;
    declare
      Vers : constant String
           := Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Ver);
    begin
      if Vers /= "" and then Vers /= Versions.Intra then
        Dictio_Debug.Put_Fatal (Dictio_Debug.Fight,
            "Version mismatch. Received "
          & Vers & " while being " & Versions.Intra);
        raise Errors.Exit_Error;
      end if;
    end;

    Nodes.Set (From, Stat, Sync, Prio);
    Dictio_Debug.Put (Dictio_Debug.Fight, "Received status from: " & From.Image
                                        & "/" & Stat'Img & "-" & Prio);
  end Event;


  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
    Result : Nodes.Check_Result_List;
  begin
    Result := Nodes.Check;
    Dictio_Debug.Put (Dictio_Debug.Fight, "Ends " & Result'Img);
    Per.Delete_If_Exists;

    -- This may restart a fight
    Status.Set (Fight_Actions(Result));
    return False;
  end Timer_Cb;

  function Perio_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
    use type Status.Status_List;
  begin
    if In_Fight and then Last_Status /= Status.Fight then
      Dictio_Debug.Put (Dictio_Debug.Fight, "Send status " & Last_Status'Img);
      Intra_Dictio.Send_Status (Last_Status);
    end if;
    return False;
  end Perio_Cb;

end Fight_Mng;

