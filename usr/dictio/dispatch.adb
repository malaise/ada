with Tcp_Util, X_Mng;
with Status, Data_Base, Intra_Dictio, Init_Mng, Online_Mng, Client_Mng, Args;
package body Dispatch is

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
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
  begin
    Args.Init;
    Status.Set (Handle_New_Status'Access);
    X_Mng.X_Set_Signal (Signal'Access);
    Intra_Dictio.Init;
    Intra_Dictio.Set_Read_Cb (New_Intra'Access);

    while X_Mng.Select_No_X (100) loop
      null;
    end loop;
    Init_Mng.Start;
  end Init;


  procedure Run is
    Dummy : Boolean;
  begin
    while not Signal_Received loop
      Dummy := X_Mng.Select_No_X (-1);
    end loop;
  end;

  procedure Quit is 
  begin
    Status.Set (Status.Dead);
  end Quit; 

  procedure New_Intra (Diff : in Boolean;
                       Stat : in Status.Status_List;
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
          Init_Mng.Event (From, Stat, Diff);
        when Status.Slave | Status.Master | Status.Fight =>
          Online_Mng.Event (From, Stat, Diff);
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
        if Prev_Status = Status.Init
        and then New_Status = Status.Slave then
          -- Request sync
          Intra_Dictio.Send_Status;
        end if;
        Online_Mng.Start;
        Client_Mng.Start;
      when Status.Fight =>
        null;
    end case;
    Prev_Status := New_Status;
  end Handle_New_Status;

end Dispatch;

