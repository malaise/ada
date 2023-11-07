with Socket_Util, Event_Mng;
with Status, Data_Base, Intra_Dictio, Online_Mng, Client_Mng, Args, Nodes,
     Sync_Mng, Dictio_Debug, Versions, Parse, Fight_Mng, Errors;
package body Dispatch is

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
                       Sync : in Boolean;
                       Prio : in Args.Prio_Str;
                       From : in Socket_Util.Host_Name;
                       Kind : in Character;
                       Item : in Data_Base.Item_Rec);


  procedure Signal is
  begin
    Dictio_Debug.Put (Dictio_Debug.Fight, "Dispatch: signal received");
    raise Errors.Exit_Signal;
  end Signal;

  procedure Handle_New_Status
        (Prev_Status, New_Status : in Status.Status_List);

  procedure Init is
  begin
    Args.Init;
    Event_Mng.Set_Sig_Term_Callback (Signal'Access);
    Intra_Dictio.Init;

    Event_Mng.Pause (200);

    Sync_Mng.Init;
    Status.Set (Handle_New_Status'Access);
    Intra_Dictio.Set_Read_Cb (New_Intra'Access);
    Status.Set (Status.Init);
  end Init;


  procedure Run is
  begin
    Event_Mng.Pause (Event_Mng.Infinite_Ms);
  end Run;

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
                       From : in Socket_Util.Host_Name;
                       Kind : in Character;
                       Item : in Data_Base.Item_Rec) is
    use type Status.Status_List;
  begin
    if Kind = Intra_Dictio.Stat_Kind then
      if Diff and then Stat /= Status.Master
              and then Stat /= Status.Slave
              and then Stat /= Status.Dead then
        Dictio_Debug.Put (Dictio_Debug.Fight, "Dispatch: reply status to: "
                   & From.Image & "/" & Stat'Img & "-" & Prio);
        Intra_Dictio.Reply_Status (Intra_Dictio.Extra_Ver & Versions.Intra);
      end if;
      case Status.Get is
        when Status.Starting | Status.Dead =>
          return;
        when Status.Init | Status.Fight =>
          Fight_Mng.Event (From, Stat, Sync, Prio,
                          Item.Data(1 .. Item.Data_Len));
        when Status.Slave | Status.Master =>
          Online_Mng.Event (From, Stat, Sync,
                            Item.Data(1 .. Item.Data_Len));
      end case;
    elsif Kind = Intra_Dictio.Sync_Kind then
      Dictio_Debug.Put (Dictio_Debug.Client_Data,
                        "Dispatch: receive sync " & Parse(Item.Name));
      -- Sync: Store and notify
      Sync_Mng.Sync_Received;
      Client_Mng.Modified (Item);
    else
      -- Data: Store and notify
      Dictio_Debug.Put (Dictio_Debug.Client_Data,
                        "Dispatch: receive data " & Parse(Item.Name));
      Client_Mng.Modified (Item);
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
        Fight_Mng.Start (Status.Init, 2.0,
          (Nodes.Many_Master_Master => Status.Starting,
           Nodes.Many_Master_Slave  => Status.Starting,
           Nodes.One_Master_Master  => Status.Slave,
           Nodes.One_Master_Slave   => Status.Slave,
           Nodes.All_Init_Master    => Status.Master,
           Nodes.All_Init_Slave     => Status.Slave,
           Nodes.No_Master_Master   => Status.Master,
           Nodes.No_Master_Slave    => Status.Starting) );
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

