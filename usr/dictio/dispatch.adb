with Tcp_Util, Event_Mng;
with Status, Data_Base, Intra_Dictio, Init_Mng, Online_Mng, Client_Mng, Args,
     Sync_Mng, Debug, Versions, Parse, Fight_Mng, Errors;
package body Dispatch is

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
                       Sync : in Boolean;
                       Prio : in Args.Prio_Str;
                       From : in Tcp_Util.Host_Name;
                       Kind : in Character;
                       Item : in Data_Base.Item_Rec);


  procedure Signal is
  begin
    if Debug.Level_Array(Debug.Fight) then
      Debug.Put ("Dispatch: signal received");
    end if;
    raise Errors.Exit_Signal;
  end Signal;

  procedure Handle_New_Status
        (Prev_Status, New_Status : in Status.Status_List);
  Prev_Status : Status.Status_List := Status.Starting;

  procedure Init is
    use type Event_Mng.Out_Event_List;
  begin
    Args.Init;
    Event_Mng.Set_Sig_Callback (Signal'Access);
    Intra_Dictio.Init;

    Event_Mng.Pause (200);

    Status.Set (Handle_New_Status'Access);
    Intra_Dictio.Set_Read_Cb (New_Intra'Access);
    Status.Set (Status.Init);
  end Init;


  procedure Run is
  begin
    Event_Mng.Pause (Event_Mng.Infinite_Ms);
  end;

  procedure Quit is 
    use type Status.Status_List;
  begin
    Status.Set (Status.Dead);
    while Status.Get /= Status.Dead loop
      Event_Mng.Wait (10);
    end loop;
  end Quit; 

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
                       Sync : in Boolean;
                       Prio : in Args.Prio_Str;
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
          Debug.Put ("Dispatch: reply status to: " & Parse(From)
                   & "/" & Stat'Img & "-" & Prio);
        end if;
        Intra_Dictio.Reply_Status (Intra_Dictio.Extra_Ver & Versions.Intra);
      end if;
      case Status.Get is
        when Status.Starting | Status.Dead =>
          return;
        when Status.Init | Status.Fight =>
          Fight_Mng.Event (From, Stat, Sync, Prio, Diff,
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
        Status.Set (Status.Init);
      when Status.Init =>
        Init_Mng.Start;
      when Status.Dead =>
        if Prev_Status = Status.Master then
          Intra_Dictio.Send_Status;
        end if;
        Sync_Mng.Cancel;
        Client_Mng.New_Status;
        Client_Mng.Quit;
        Intra_Dictio.Quit;
      when Status.Slave | Status.Master =>
        -- Start client
        Online_Mng.Start (Prev_Status = Status.Init);
      when Status.Fight =>
        null;
    end case;
    if New_Status /= Status.Dead then
      Client_Mng.New_Status;
    end if;
  end Handle_New_Status;

end Dispatch;

