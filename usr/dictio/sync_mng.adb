with As.U, Timers, Event_Mng, Dynamic_List, Environ;
with Intra_Dictio, Data_Base, Dictio_Debug, Online_Mng;
package body Sync_Mng is

  Timer_Id : Timers.Timer_Id;
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
  begin
    return Timer_Id.Exists;
  end Timer_Active;

  procedure Cancel_Timer is
  begin
    Timer_Id.Delete_If_Exists;
  end Cancel_Timer;

  function Timer_Rec_Cb (Unused_Id : Timers.Timer_Id;
                         Unused_Data : Timers.Timer_Data) return Boolean is
  begin
    if Sync_Has_Been_Received then
      -- Still in sync
      Sync_Has_Been_Received := False;
    else
      Cancel_Timer;
      Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: End, received "
                                         & Nb_Syn_Received'Img & " sync");
    end if;
    return False;
  end Timer_Rec_Cb;

  procedure Start is
  begin
    Cancel_Timer;
    Nb_Syn_Received := 0;
    Intra_Dictio.Reply_Status;
    Timer_Id.Create ( (Timers.Delay_Sec,
                       null,
                       Sync_Timeout,
                       Sync_Init_Timeout),
                         Timer_Rec_Cb'Access);
    Sync_Has_Been_Received := False;
    Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Start");
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

  -- Timer between first request and real syn (Init)
  --  or for flow limitation (Send)
  Tid : Timers.Timer_Id;

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
    Dictio_Debug.Put (Dictio_Debug.Sync, "Sync.Init: Delay per Kb set to "
                                       & Delay_Per_Kb'Img & " ms");
  end Init;

  package Sync_Dyn_List_Mng is new Dynamic_List (Tcp_Util.Host_Name);
  package Sync_List_Mng renames Sync_Dyn_List_Mng.Dyn_List;
  Sync_List : Sync_List_Mng.List_Type;
  function Sync_Search is new Sync_List_Mng.Search (As.U."=");


  function Timer_Sen_Cb (Unused_Id : Timers.Timer_Id;
                         Unused_Data : Timers.Timer_Data) return Boolean;

  procedure Send (To : Tcp_Util.Host_Name) is
  begin
    if Sending_Status = Send then
      -- Reject new dest if already syncing
      Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Rejecting dest " & To.Image
                                         & " cause sending");
      return;
    end if;
    if Sync_List.Is_Empty then
      -- First dest, arm timer
      Tid.Create ( (Timers.Delay_Sec,
                    null,
                    Timers.No_Period,
                    Sync_Listen_Timeout),
                      Timer_Sen_Cb'Access);
      Sending_Status := Init;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Adding dest " & To.Image);
    if not Sync_Search (Sync_List, To, From => Sync_List_Mng.Absolute) then
      Sync_List.Insert (To);
    end if;
  end Send;

  procedure Do_Sync_Bus is
    Item : Data_Base.Item_Rec;
    Bytes_Sent : Natural;
    Reply_Result : Intra_Dictio.Reply_Result_List;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List;
  begin


    Item := Data_Base.Read_First;
    Bytes_Sent := 0;

    Items:
    while Item /= Data_Base.No_Item loop

      -- Send item
      Reply_Result := Intra_Dictio.Send_Sync_Data ("", Item);
      if Reply_Result /= Intra_Dictio.Ok then
        Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Bus send error "
                                           & Reply_Result'Img);
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
        Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Sending cancelled");
        exit Items;
      end if;

      -- Next item
      Item := Data_Base.Read_Next;

    end loop Items;

  end Do_Sync_Bus;

  function Timer_Sen_Cb (Unused_Id : Timers.Timer_Id;
                         Unused_Data : Timers.Timer_Data) return Boolean is
  begin

    -- Check sync not cancelled during init
    if Sending_Status /= Init then
      Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: cancelling due to "
                                         & Sending_Status'Img);
      return False;
    end if;

    Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Sending "
                                       & Natural'Image(Data_Base.Nb_Item)
                                       & " items");

    -- Send items
    Sending_Status := Send;
    Do_Sync_Bus;

    -- Done
    if not Sync_List.Is_Empty then
      Sync_List.Delete_List (True);
    end if;
    Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Done");
    Sending_Status := Stop;
    return False;
  end Timer_Sen_Cb;

  ------------------------------------------------------------

  function In_Sync return Boolean is
  begin
    return Timer_Active or else Sending_Status = Send;
  end In_Sync;

  procedure Cancel is
  begin
    if Timer_Active or else Sending_Status /= Stop then
      Dictio_Debug.Put (Dictio_Debug.Sync, "Sync: Cancel");
    end if;
    Cancel_Timer;
    if Sending_Status = Init then
      Tid.Delete_If_Exists;
    end if;
    Sending_Status := Stop;
  end Cancel;

end Sync_Mng;

