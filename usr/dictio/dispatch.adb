with Tcp_Util, Event_Mng;
with Status, Data_Base, Intra_Dictio, Init_Mng, Online_Mng, Client_Mng, Args,
     Sync_Mng;
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

  Prev_Status : Status.Status_List := Status.Starting;
  procedure Handle_New_Status;

  procedure Init is
    use type Event_Mng.Out_Event_List;
  begin
    Args.Init;
    Status.Set (Handle_New_Status'Access);
    Event_Mng.Set_Sig_Callback (Signal'Access);
    Intra_Dictio.Init;
    Intra_Dictio.Set_Read_Cb (New_Intra'Access);

    while Event_Mng.Wait (100) loop
      null;
    end loop;
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
    Current_Status : constant Status.Status_List := Status.Get;
  begin
    if Kind = Intra_Dictio.Stat_Kind then
      case Current_Status is
        when Status.Starting | Status.Dead =>
          return;
        when Status.Init =>
          Init_Mng.Event (From, Stat, Sync, Diff,
                          Item.Data(1 .. Item.Data_Len));
        when Status.Slave | Status.Master | Status.Fight =>
          Online_Mng.Event (From, Stat, Sync, Diff,
                            Item.Data(1 .. Item.Data_Len));
      end case;
    else
      -- Data or sync
      -- Transfer to notify and data base
      Client_Mng.Modified (Kind, Item);
    end if;
  end New_Intra;

  procedure Handle_New_Status is
    New_Status : constant Status.Status_List := Status.Get;
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
        Client_Mng.Quit;
        Intra_Dictio.Quit;
      when Status.Slave | Status.Master =>
        Online_Mng.Start;
        if Prev_Status = Status.Init
        and then New_Status = Status.Slave then
          -- Start client, allowing after sync
          Client_Mng.Start (True);
        else
          -- Start client immediately
          Client_Mng.Start (False);
        end if;
      when Status.Fight =>
        null;
    end case;
    Client_Mng.New_Status;
    Prev_Status := New_Status;
  end Handle_New_Status;

end Dispatch;

