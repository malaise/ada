with As.U; use As.U;
with Timers, Event_Mng, Dynamic_List, Environ;
with Intra_Dictio, Data_Base, Dictio_Debug, Online_Mng, Args;
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
    pragma Unreferenced (Id, Data);
  begin
    if Sync_Has_Been_Received then
      -- Still in sync
      Sync_Has_Been_Received := False;
    else
      Cancel_Timer;
      if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
        Dictio_Debug.Put ("Sync: End, received " & Nb_Syn_Received'Img & " sync");
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
                                 null,
                                 Sync_Timeout,
                                 Sync_Init_Timeout),
                                Timer_Rec_Cb'Access);
    Sync_Has_Been_Received := False;
    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      Dictio_Debug.Put ("Sync: Start");
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

  -- Timer between first request and real syn (Init)
  --  or for flow limitation (Send)
  Tid : Timers.Timer_Id := Timers.No_Timer;

  Delay_Per_Kb : Natural := 0;

  procedure Init is
    Default_Delay_Per_Kb : constant Natural := 1;
    Dur : Duration;
  begin
    begin
      Dur := Duration(Default_Delay_Per_Kb) / 1000;
      Environ.Get_Dur ("DICTIO_DELAY_PER_KB", Dur);
      Delay_Per_Kb := Positive (Dur * 1000.0);
    exception
      when others =>
        Delay_Per_Kb := Default_Delay_Per_Kb;
    end;
    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      Dictio_Debug.Put ("Sync.Init: Delay per Kb set to " & Delay_Per_Kb'Img & " ms");
    end if;
  end Init;

  package Sync_Dyn_List_Mng is new Dynamic_List (Tcp_Util.Host_Name);
  package Sync_List_Mng renames Sync_Dyn_List_Mng.Dyn_List;
  Sync_List : Sync_List_Mng.List_Type;
  procedure Sync_Search is new Sync_List_Mng.Search (As.U."=");


  function Timer_Sen_Cb (Id : Timers.Timer_Id;
                         Data : Timers.Timer_Data) return Boolean;

  procedure Send (To : Tcp_Util.Host_Name) is
    Found : Boolean;
  begin
    if Sending_Status = Send then
      -- Reject new dest if already syncing
      if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
        Dictio_Debug.Put ("Sync: Rejecting dest " & To.Image
                        & " cause sending");
      end if;
      return;
    end if;
    if Sync_List.Is_Empty then
      -- First dest, arm timer
      Tid := Timers.Create ( (Timers.Delay_Sec,
                              null,
                              Timers.No_Period,
                              Sync_Listen_Timeout),
                              Timer_Sen_Cb'Access);
      Sending_Status := Init;
    end if;
    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      Dictio_Debug.Put ("Sync: Adding dest " & To.Image);
    end if;
    Sync_Search (Sync_List, Found, To, From => Sync_List_Mng.Absolute);
    if not Found then
      Sync_List.Insert (To);
    end if;
  end Send;

  procedure Do_Sync_Bus is
    Item : Data_Base.Item_Rec;
    Bytes_Sent : Natural;
    Reply_Result : Intra_Dictio.Reply_Result_List;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List;
  begin


    Data_Base.Read_First (Item);
    Bytes_Sent := 0;

    Items:
    while Item /= Data_Base.No_Item loop

      -- Send item
      Reply_Result := Intra_Dictio.Send_Sync_Data ("", Item);
      if Reply_Result /= Intra_Dictio.Ok then
        if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
          Dictio_Debug.Put ("Sync: Bus send error " & Reply_Result'Img);
        end if;
      end if;

      -- Flow limitation
      Bytes_Sent := Bytes_Sent + 110 + Item.Data_Len;
      if Bytes_Sent >= 1024 then
        Bytes_Sent := 0;
        Event_Mng.Pause (Delay_Per_Kb);
      else
        Event_Mng.Pause (0);
      end if;

      -- Check if not cancelled
      if Sending_Status /= Send then
        if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
          Dictio_Debug.Put ("Sync: Sending cancelled");
        end if;
        exit Items;
      end if;

      -- Next item
      Data_Base.Read_Next (Item);

    end loop Items;

  end Do_Sync_Bus;

  procedure Do_Sync_Channel is
    Item : Data_Base.Item_Rec;
    Reply_Result : Intra_Dictio.Reply_Result_List;
    Ovf_Timeout, Curr_Timeout : Natural;
    Dest : Tcp_Util.Host_Name;
    Bytes_Sent : Natural;
    Moved : Boolean;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List,
             Event_Mng.Out_Event_List;
  begin


    Data_Base.Read_First (Item);
    Bytes_Sent := 0;

    Items:
    while Item /= Data_Base.No_Item loop

      Sync_List.Rewind;
      Dests:
      loop
        Sync_List.Read (Dest, Sync_List_Mng.Current);

        Ovf_Timeout := First_Timeout;
        Retries:
        for I in 1 .. Max_Retry loop
          -- Try to send
          Reply_Result := Intra_Dictio.Send_Sync_Data (Dest.Image, Item);
          if Reply_Result = Intra_Dictio.Overflow then
            Curr_Timeout := Ovf_Timeout;
            -- Increase timeout for next retry
            Ovf_Timeout := Ovf_Timeout * Timeout_Factor;
            if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
              Dictio_Debug.Put ("Sync: Overflow to " & Dest.Image);
            end if;
          else
            -- Ok or error
            Curr_Timeout := 0;
          end if;
          -- Wait a bit / check event
          Event_Mng.Pause (Curr_Timeout);

          if Sending_Status /= Send then
            if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
              Dictio_Debug.Put ("Sync: Sending cancelled");
            end if;
            exit Items;
          end if;

          -- Ok or Error or too many Overflows.
          exit Retries when Reply_Result /= Intra_Dictio.Overflow
                            or else I = Max_Retry;
        end loop Retries;

        if Reply_Result /= Intra_Dictio.Ok then
          -- Give up with this destination if too many overflows or other error
          if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
            Dictio_Debug.Put ("Sync: Giving up " & Dest.Image & " due to "
                     & Reply_Result'Img);
          end if;
          Sync_List.Delete (Sync_List_Mng.Prev, Moved);
          exit Items when Sync_List.Is_Empty;
        else
          -- Flow limitation
          Bytes_Sent := Bytes_Sent + 110 + Item.Data_Len;
          if Bytes_Sent >= 1024 then
            Bytes_Sent := 0;
            Event_Mng.Pause (Delay_Per_Kb);
          else
            Event_Mng.Pause (0);
          end if;
        end if;

        if Sync_List.Check_Move then
          -- Next Dest
          Sync_List.Move_To;
        else
          exit Dests;
        end if;

      end loop Dests;

      Data_Base.Read_Next (Item);

    end loop Items;

  end Do_Sync_Channel;

  function Timer_Sen_Cb (Id : Timers.Timer_Id;
                         Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
    use type Args.Channel_Mode_List;
  begin
    Tid := Timers.No_Timer;

    -- Check sync not cancelled during init
    if Sending_Status /= Init then
      if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
        Dictio_Debug.Put ("Sync: cancelling due to " & Sending_Status'Img);
      end if;
      return False;
    end if;

    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      Dictio_Debug.Put ("Sync: Sending " & Natural'Image(Data_Base.Nb_Item)
               & " items");
    end if;

    -- Send items
    Sending_Status := Send;
    if Args.Get_Mode = Args.Channel then
      Do_Sync_Channel;
    else
      Do_Sync_Bus;
    end if;

    -- Done
    if not Sync_List.Is_Empty then
      Sync_List.Delete_List (True);
    end if;
    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      Dictio_Debug.Put ("Sync: Done");
    end if;
    Sending_Status := Stop;
    return False;
  end Timer_Sen_Cb;

  ------------------------------------------------------------

  function In_Sync return Boolean is
  begin
    return Timer_Active or else Sending_Status = Send;
  end In_Sync;

  procedure Cancel is
    use type Timers.Timer_Id;
  begin
    if Dictio_Debug.Level_Array(Dictio_Debug.Sync) then
      if Timer_Active or else Sending_Status /= Stop then
        Dictio_Debug.Put ("Sync: Cancel");
      end if;
    end if;
    if Timer_Active then
      Cancel_Timer;
    end if;
    if Sending_Status = Init then
      Tid := Timers.No_Timer;
      Timers.Delete (Tid);
    end if;
    Sending_Status := Stop;
  end Cancel;

end Sync_Mng;

