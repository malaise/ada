with Timers;
with Debug, Parse, Local_Host_Name, Nodes, Errors, Versions, Intra_Dictio;
package body Fight_Mng is

  Tid : Timers.Timer_Id := Timers.No_Timer;

  function Timer_Cb (Id : Timers.Timer_Id) return Boolean;

  Fight_Actions : Fight_Action;

  function In_Fight return Boolean is
    use type Timers.Timer_Id;
  begin
    return Tid /= Timers.No_Timer;
  end In_Fight;

  procedure Start (Timeout : in Duration; Actions : in Fight_Action) is
    T : Timers.Delay_Rec;
  begin
    Fight_Actions := Actions;

    T.Delay_Seconds := Timeout;
    Tid := Timers.Create (T, Timer_Cb'Access);

    Nodes.Init_List;
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: start");
    end if;
  end Start;


  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Sync : in Boolean;
                   Diff : in Boolean;
                   Extra : in String := "") is
    use type Status.Status_List;
  begin
    if not In_Fight then
      return;
    end if;
    if Extra /= "" and then Extra(1) = Intra_Dictio.Extra_Ver
    and then Extra(2 .. Extra'Last) /= Versions.Intra then
      Debug.Put ("Fight: version mismatch. Received " & Extra
               & " while being " & Versions.Intra);
      raise Errors.Exit_Error;
    end if;
    Nodes.Set (From, Stat, Sync);
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: received Stat: " & Stat'Img
               & "  From: " & Parse (From));
    end if;
  end Event;


  function Timer_Cb (Id : Timers.Timer_Id) return Boolean is
    Result : Nodes.Check_Result_List;
  begin
    Result := Nodes.Check;
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Fight: ends " & Result'Img);
    end if;
    Status.Set (Fight_Actions(Result));
    Tid := Timers.No_Timer;
    return False;
  end Timer_Cb;

end Fight_Mng;

