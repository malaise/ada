with Timers, Event_Mng, Dynamic_List, Sys_Calls;
with Status, Intra_Dictio, Data_Base, Parse, Debug, Errors, Online_Mng;
package body Sync_Mng is


  Timer_Id : Timers.Timer_Id := Timers.No_Timer;
  Sync_Has_Been_Received : Boolean;
  Nb_Syn_Received : Natural := 0;


  -- When receiving a Alive message, slave (if not sync) sends its status and
  -- starts sync: wait Sync_Init_Timeout then Sync_Timeout then Sync_Timeout
  --  until no (more) sync received
  -- When receiving a sync request, master waits Sync_Listen_Timeout for
  --  other requests then starts sync

  -- Delay in master for building list of slave nodes to sync
  -- A small value because all waiting unsynced slaves send their sync request
  --   when they receive an alive message
  Sync_Listen_Timeout : constant Duration := 0.3;

  -- Delay after which end of sync reception sequence if no sync received
  -- Large enough to be sure than we don't cancel a running sync
  Sync_Timeout : constant Duration := 1.0;

  -- Delay for first check of end of sync reception sequence
  -- More than Sync_Listen_Timeout so we don't cancel a sync while master
  --  is listening
  -- Smaller than Alive_Period so that all unsync slaves will not be in sync
  --   at next alive message and will reauest sync
  Sync_Init_Timeout : constant Duration
                    := (Sync_Listen_Timeout + Online_Mng.Alive_Period) / 2;


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
    Intra_Dictio.Reply_Status;
    Timer_Id := Timers.Create ( (Timers.Delay_Sec,
                                 Sync_Timeout,
                                 Sync_Init_Timeout),
                                Timer_Rec_Cb'access);
    Sync_Has_Been_Received := False;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Start");
    end if;
  end Start;


  procedure Sync_Received is
  begin
    if Timer_Active then
      Sync_Has_Been_Received := True;
      Nb_Syn_Received := Nb_Syn_Received + 1;
    end if;
  end Sync_Received;

  ------------------------------------------------------------

  type Send_Status_List is (Init, Send, Stop);
  Sending_Status : Send_Status_List := Stop;

  Max_Retry : constant := 3;
  First_Timeout : constant Natural := 100;
  Timeout_Factor : constant := 2;

  Tid : Timers.Timer_Id := Timers.No_Timer;

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
  procedure Sync_Search is new Sync_List_Mng.Search;

  procedure Do_Sync;

  function Timer_Sen_Cb (Id : Timers.Timer_Id;
                         Data : Timers.Timer_Data) return Boolean is
  begin
    Do_Sync;
    return False;
  end Timer_Sen_Cb;
  
  procedure Send (To : Tcp_Util.Host_Name) is
  begin
    if Sending_Status = Send then
      -- Reject new dest if already syncing
      if Debug.Level_Array(Debug.Sync) then
        Debug.Put ("Sync: Rejecting dest " & Parse (To) & " cause sending");
      end if;
      return;
    end if;
    if Sync_List_Mng.Is_Empty (Sync_List) then
      -- First dest, arm timer
      Tid := Timers.Create ( (Timers.Delay_Sec,
                              Timers.No_Period,
                              Sync_Listen_Timeout),
                              Timer_Sen_Cb'access);
      Sending_Status := Init;
    end if;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Adding dest " & Parse (To));
    end if;
    begin
      Sync_Search (Sync_List, To, From_Current => False);
    exception
      when Sync_List_Mng.Not_In_List =>
        Sync_List_Mng.Insert (Sync_List, To);
    end;
  end Send;

  procedure Do_Sync is
    Item : Data_Base.Item_Rec;
    Reply_Result : Intra_Dictio.Reply_Result_List;
    Ovf_Timeout, Curr_Timeout : Natural;
    Dest : Tcp_Util.Host_Name;
    Bytes_Sent : Natural;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List,
             Event_Mng.Out_Event_List;
  begin

    Set_Delay;

    -- Check sync not cancelled during init
    if Sending_Status /= Init then
      if Debug.Level_Array(Debug.Sync) then
        Debug.Put ("Sync: cancelling due to " & Sending_Status'Img);
      end if;
      return;
    end if;

    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Sending " & Natural'Image(Data_Base.Nb_Item)
               & " items");
    end if; 

    Sending_Status := Send;
    Data_Base.Read_First (Item);
    Bytes_Sent := 0;

    Items:
    while Item /= Data_Base.No_Item loop

      Sync_List_Mng.Move_To (Sync_List, Sync_List_Mng.Next, 0, False);
      Dests:
      loop
        Sync_List_Mng.Read (Sync_List, Dest, Sync_List_Mng.Current);

        Ovf_Timeout := First_Timeout;
        Retries:
        for I in 1 .. Max_Retry loop
          -- Try to send
          Reply_Result := Intra_Dictio.Send_Sync_Data (Dest, Item);
          if Reply_Result = Intra_Dictio.Overflow then
            Curr_Timeout := Ovf_Timeout;
            -- Increase timeout for next retry
            Ovf_Timeout := Ovf_Timeout * Timeout_Factor;
            if Debug.Level_Array(Debug.Sync) then
              Debug.Put ("Sync: Overflow to " & Parse (Dest));
            end if; 
          else
            -- Ok or error
            Curr_Timeout := 0;
          end if;
          -- Wait a bit / check event
          Event_Mng.Wait (Curr_Timeout);

          if Sending_Status /= Send then
            if Debug.Level_Array(Debug.Sync) then
              Debug.Put ("Sync: Sending cancelled");
            end if; 
            exit Items;
          end if;

          -- Ok or Error or too many Overflows.
          exit Retries when Reply_Result /= Intra_Dictio.Overflow
                            or else I = Max_Retry;
        end loop Retries;

        if Reply_Result /= Intra_Dictio.Ok then
          -- Give up with this destination if too many overflows or other error
          if Debug.Level_Array(Debug.Sync) then
            Debug.Put ("Sync: Giving up " & Parse (Dest) & " due to "
                     & Reply_Result'Img);
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
    Sending_Status := Stop;
  end Do_Sync;

  ------------------------------------------------------------

  function In_Sync return Boolean is
  begin
    return Timer_Active or else Sending_Status = Send;
  end In_Sync;

  procedure Cancel is
  begin
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Cancel");
    end if;
    if Timer_Active then
      Cancel_Timer;
    end if;
    if Sending_Status = Init then
      Timers.Delete (TId);
    end if;
    Sending_Status := Stop;
  end Cancel;

end Sync_Mng;

