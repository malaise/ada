with Timers, Event_Mng, Dynamic_List, Sys_Calls;
with Status, Intra_Dictio, Data_Base, Parse, Debug;
package body Sync_Mng is

  Sending_Sync : Boolean := False;

  Timer_Id : Timers.Timer_Id := Timers.No_Timer;
  Sync_Has_Been_Received : Boolean;
  Nb_Syn_Received : Natural := 0;

  Sync_Timeout : constant Duration := 1.0;


  function Timer_Active return Boolean is
    use type Timers.Timer_Id; 
  begin
    return Timer_Id /= Timers.No_Timer;
  end Timer_Active;

  procedure Cancel_Timer is
  begin
    if Timer_Active then
      Timers.Delete (Timer_Id);
      Timer_Id := Timers.No_Timer;
    end if;
  end Cancel_Timer;

  function Timer_Rec_Cb (Id : in Timers.Timer_Id;
                         Data : Timers.Timer_Data) return Boolean is
  begin
    if Sync_Has_Been_Received then
      -- Still in sync
      Sync_Has_Been_Received := False;
    else
      Cancel_Timer;
      if Debug.Level_Array(Debug.Sync) then
        Debug.Put ("Sync: End, received " & Nb_Syn_Received'Img & " sync");
      end if;
    end if;
    return False;
  end Timer_Rec_Cb;

  procedure Start is
  begin
    Cancel_Timer;
    Nb_Syn_Received := 0;
    Intra_Dictio.Send_Status;
    Timer_Id := Timers.Create ( (Timers.Delay_Sec,
                                 Sync_Timeout,
                                 2 * Sync_Timeout),
                                Timer_Rec_Cb'access);
    Sync_Has_Been_Received := False;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Start");
    end if;
  end Start;

  procedure Cancel is
  begin
    Cancel_Timer;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Cancel");
    end if;
  end Cancel;

  procedure Sync_Received is
  begin
    if Timer_Active then
      Sync_Has_Been_Received := True;
      Nb_Syn_Received := Nb_Syn_Received + 1;
    end if;
  end Sync_Received;

  function In_Sync return Boolean is
  begin
    return Timer_Active or else Sending_Sync;
  end In_Sync;

  ------------------------------------------------------------

  Max_Retry : constant := 3;
  First_Timeout : constant Natural := 100;
  Timeout_Factor : constant := 2;

  Delay_Per_Kb : Natural := 0;

  procedure Set_Delay is
    Default_Delay_Per_Kb : constant Natural := 1;
    Val : String (1 .. 4);
    Set, Trunc : Boolean;
    Len : Natural;
  begin
    if Delay_Per_Kb /= 0 then
      return;
    end if;
    Sys_Calls.Getenv ("DICTIO_DELAY_PER_KB", Set, Trunc, Val, Len);
    if not Set or else Len = 0 or else Trunc then
      Delay_Per_Kb := Default_Delay_Per_Kb;
    else
      begin
        Delay_Per_Kb := Positive'Value (Val(1 .. Len));
      exception
        when others =>
          Delay_Per_Kb := Default_Delay_Per_Kb;
      end;
    end if;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Delay per Kb set to" & Delay_Per_Kb'Img & " ms");
    end if;
  end Set_Delay;

  package Sync_List_Mng is new Dynamic_List (Tcp_Util.Host_Name);
  Sync_List : Sync_List_Mng.List_Type;

  procedure Do_Sync;

  function Timer_Sen_Cb (Id : Timers.Timer_Id;
                         Data : Timers.Timer_Data) return Boolean is
  begin
    Do_Sync;
    return False;
  end Timer_Sen_Cb;
  
  procedure Send (To : Tcp_Util.Host_Name) is
    Tid : Timers.Timer_Id := Timers.No_Timer;
  begin
    if Sending_Sync then
      -- Reject new dest if already syncing
      return;
    end if;
    if Sync_List_Mng.Is_Empty (Sync_List) then
      -- First dest, arm timer
      Tid := Timers.Create ( (Timers.Delay_Sec,
                              Timers.No_Period,
                              Sync_Timeout),
                              Timer_Sen_Cb'access);
    end if;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Adding dest " & Parse (To));
    end if;
    Sync_List_Mng.Insert (Sync_List, To);
  end Send;
      

  procedure Do_Sync is
    Item : Data_Base.Item_Rec;
    Result : Intra_Dictio.Reply_Result_List;
    Timeout : Natural;
    Dest : Tcp_Util.Host_Name;
    Bytes_Sent : Natural;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List;
  begin
    Set_Delay;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Sending " & Natural'Image(Data_Base.Nb_Item)
               & " items");
    end if; 

    Sending_Sync := True;
    Data_Base.Read_First (Item);
    Bytes_Sent := 0;

    Items:
    while Item /= Data_Base.No_Item loop

      Sync_List_Mng.Move_To (Sync_List, Sync_List_Mng.Next, 0, False);
      Dests:
      loop
        Sync_List_Mng.Read (Sync_List, Dest, Sync_List_Mng.Current);

        Timeout := First_Timeout;
        Retries:
        for I in 1 .. Max_Retry loop
          Result := Intra_Dictio.Send_Sync_Data (Dest, Item);
          -- Ok or Error or too many Overflows.
          exit Retries when Result /= Intra_Dictio.Overflow
                            or else I = Max_Retry;
          if Debug.Level_Array(Debug.Sync) then
            Debug.Put ("Sync: Overflow to " & Parse (Dest));
          end if; 
          Event_Mng.Wait (Timeout);
          -- Increase timeout for next retry
          Timeout := Timeout * Timeout_Factor;
        end loop Retries;

        if Result /= Intra_Dictio.Ok then
          -- Give up with this destination if too many overflows or other error
          if Debug.Level_Array(Debug.Sync) then
            Debug.Put ("Sync: Giving up " & Parse (Dest) & " due to " & Result'Img);
          end if;
          if Sync_List_Mng.Get_Position (Sync_List) = 1 then
            Sync_List_Mng.Delete (Sync_List, Sync_List_Mng.Next);
          else
            Sync_List_Mng.Delete (Sync_List, Sync_List_Mng.Prev);
          end if;
          exit Items when Sync_List_Mng.Is_Empty (Sync_List);
        else
          -- Flow limitation
          Bytes_Sent := Bytes_Sent + 110 + Item.Data_Len;
          if Bytes_Sent >= 1024 then
            Bytes_Sent := 0;
            Event_Mng.Wait (Delay_Per_Kb);
          else
            Event_Mng.Wait (0);
          end if;
        end if;

        if Sync_List_Mng.Get_Position (Sync_List)
        /= Sync_List_Mng.List_Length (Sync_List) then
          -- Next Dest
          Sync_List_Mng.Move_To (Sync_List);
        else
          exit Dests;
        end if;

      end loop Dests;

      Data_Base.Read_Next (Item);

    end loop Items;

    if not Sync_List_Mng.Is_Empty (Sync_List) then
      Sync_List_Mng.Delete_List (Sync_List, True);
    end if;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Done");
    end if; 
    Sending_Sync := False;
  end Do_Sync;

end Sync_Mng;

