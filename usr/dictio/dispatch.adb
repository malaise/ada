with Tcp_Util, Event_Mng;
with Status, Data_Base, Intra_Dictio, Init_Mng, Online_Mng, Client_Mng, Args,
     Sync_Mng, Debug, Versions, Parse, Fight_Mng;
package body Dispatch is

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
                       Sync : in Boolean;
                       From : in Tcp_Util.Host_Name;
                       Kind : in Character;
                       Item : in Data_Base.Item_Rec);


  Signal_Received : Boolean := False;
  procedure Signal is
  begin
    Signal_Received := True;
  end Signal;

  procedure Handle_New_Status
        (Prev_Status, New_Status : in Status.Status_List);
  Prev_Status : Status.Status_List := Status.Starting;

  procedure Init is
    use type Event_Mng.Out_Event_List;
  begin
    Args.Init;
    Status.Set (Handle_New_Status'Access);
    Event_Mng.Set_Sig_Callback (Signal'Access);
    Intra_Dictio.Init;

    while Event_Mng.Wait (100) loop
      null;
    end loop;

    Intra_Dictio.Set_Read_Cb (New_Intra'Access);
    Init_Mng.Start;
  end Init;


  procedure Run is
  begin
    while not Signal_Received loop
      Event_Mng.Wait (-1);
    end loop;
  end;

  procedure Quit is 
  begin
    Status.Set (Status.Dead);
  end Quit; 

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
                       Sync : in Boolean;
                       From : in Tcp_Util.Host_Name;
                       Kind : in Character;
                       Item : in Data_Base.Item_Rec) is
    use type Status.Status_List;
  begin
    if Kind = Intra_Dictio.Stat_Kind then
      if Diff and then Stat /= Status.Master
              and then Stat /= Status.Slave
              and then Stat /= Status.Dead then
        if Debug.Level_Array(Debug.Fight) then
          Debug.Put ("Dispatch: reply status to: " & Parse(From) & "/" & Stat'Img);
        end if;
        Intra_Dictio.Reply_Status (Intra_Dictio.Extra_Ver & Versions.Intra);
      end if;
      case Status.Get is
        when Status.Starting | Status.Dead =>
          return;
        when Status.Init | Status.Fight =>
          Fight_Mng.Event (From, Stat, Sync, Diff,
                          Item.Data(1 .. Item.Data_Len));
        when Status.Slave | Status.Master =>
          Online_Mng.Event (From, Stat, Sync, Diff,
                            Item.Data(1 .. Item.Data_Len));
      end case;
    else
      -- Data or sync
      -- Transfer to notify and data base
      Client_Mng.Modified (Kind, Item);
    end if;
  end New_Intra;

  procedure Handle_New_Status
        (Prev_Status, New_Status : in Status.Status_List) is
    use type Status.Status_List;
  begin
    case New_Status is
      when Status.Starting =>
        if not Signal_Received then
          Init_Mng.Start;
        end if;
      when Status.Init =>
        null;
      when Status.Dead =>
        if Prev_Status = Status.Master then
          Intra_Dictio.Send_Status;
        end if;
        Sync_Mng.Cancel;
        Client_Mng.Quit;
        Intra_Dictio.Quit;
      when Status.Slave | Status.Master =>
        -- Start client
        Online_Mng.Start (Prev_Status = Status.Init);
      when Status.Fight =>
        null;
    end case;
    Client_Mng.New_Status;
  end Handle_New_Status;

end Dispatch;

