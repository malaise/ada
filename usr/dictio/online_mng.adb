with As.U, Timers;
with Dictio_Debug, Intra_Dictio, Local_Host_Name, Nodes,
     Fight_Mng, Sync_Mng, Data_Base, Client_Mng;

package body Online_Mng is

  Tid : Timers.Timer_Id;

  -- Ever requested a sync?
  Ever_Synced : Boolean := False;

  function Timer_Cb (Unused_Id : Timers.Timer_Id;
                     Unused_Data : Timers.Timer_Data) return Boolean;

  Fight_Actions : constant Fight_Mng.Fight_Action :=
    (Nodes.Many_Master_Master => Status.Master,
     Nodes.Many_Master_Slave  => Status.Slave,
     Nodes.One_Master_Master  => Status.Master,
     Nodes.One_Master_Slave   => Status.Slave,
     Nodes.All_Init_Master    => Status.Master,
     Nodes.All_Init_Slave     => Status.Master,
     Nodes.No_Master_Master   => Status.Master,
     Nodes.No_Master_Slave    => Status.Slave);

  procedure Delete_Timer is
  begin
    Tid.Delete_If_Exists;
  end Delete_Timer;

  procedure Start_Slave_Timeout is
    T : Timers.Delay_Rec;
  begin
    T.Delay_Seconds := Alive_Period * 3.0;
    Tid := Timers.Create (T, Timer_Cb'Access);
  end Start_Slave_Timeout;

  No_Master : constant Socket_Util.Host_Name := As.U.Asu_Null;
  Current_Master : Socket_Util.Host_Name := No_Master;

  procedure Reset_Master is
  begin
    Current_Master := No_Master;
  end Reset_Master;

  procedure Start (First : in Boolean) is
    T : Timers.Delay_Rec;
    use type Status.Status_List;
  begin
    Reset_Master;
    if Status.Get = Status.Slave then
      Start_Slave_Timeout;
      if First then
        Ever_Synced := False;
      end if;
    elsif Status.Get = Status.Master then
      -- Master
      Current_Master := As.U.Tus (Local_Host_Name.Get);
      T.Delay_Seconds := 0.0;
      T.Period := Alive_Period;
      Tid.Create (T, Timer_Cb'Access);
      Status.Sync := True;
      Ever_Synced := True;
      Client_Mng.Start;
    end if;

    Dictio_Debug.Put (Dictio_Debug.Online, "Start as " & Status.Get'Img
                                         & " " & First'Img);
  end Start;

  procedure Start_Fight is
  begin
    Reset_Master;
    if Sync_Mng.In_Sync then
      Sync_Mng.Cancel;
    end if;
    Delete_Timer;
    Fight_Mng.Start (Status.Fight, 1.0, Fight_Actions);
  end Start_Fight;

  procedure Event (From  : in Socket_Util.Host_Name;
                   Stat  : in Status.Status_List;
                   Sync  : in Boolean;
                   Extra : in String := "") is
    Crc : constant String
        := Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Crc);
    use type As.U.Asu_Us, Status.Status_List;
  begin
    if Status.Get = Status.Slave then
      if Stat = Status.Master then

        -- Receive a Master while slave, check Crc and restart timer
        Current_Master := From;
        if Crc /= "" and then not Sync_Mng.In_Sync then
          if not Ever_Synced then
            -- Never synced and not syncing (init). Sync.
            Dictio_Debug.Put (Dictio_Debug.Online, "Syncing from: "
                                                 & From.Image);
            Data_Base.Reset;
            Status.Sync := False;
            Ever_Synced := True;
            Sync_Mng.Start;
          elsif Crc /= Data_Base.Get_Crc then
            -- Invalid Crc. Re-sync.
            Dictio_Debug.Put (Dictio_Debug.Online, "Crc error. Received "
                       & Crc
                       & " from: " & From.Image
                       & ", got " & Data_Base.Get_Crc);
            Data_Base.Reset;
            Status.Sync := False;
            Sync_Mng.Start;
          elsif not Status.Sync then
            -- Crc Ok and not synced
            Dictio_Debug.Put (Dictio_Debug.Online, "Crc OK, synced");
            Status.Sync := True;
            Client_Mng.Start;
          end if;
        end if;

        Delete_Timer;
        Start_Slave_Timeout;
      elsif Stat = Status.Dead then
        -- Receive a Dead while slave, start Fight
        Dictio_Debug.Put (Dictio_Debug.Online, "Fight due to death of: "
                                             & From.Image );
        Start_Fight;
        return;
      else
        -- Receive other status, check that this is not from previously known master
        if Current_Master /= No_Master and then From = Current_Master then
          Dictio_Debug.Put (Dictio_Debug.Online,
              "Fight due to master new status " & From.Image & "/" & Stat'Img);
          Start_Fight;
          return;
        end if;
      end if;

    elsif Status.Get = Status.Master then

      if Stat = Status.Master
      and then Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Crc) /= "" then
        -- An alive message (not a fight reply nor fight info)
        Dictio_Debug.Put (Dictio_Debug.Online, "Fight cause another master: "
                                             & From.Image );
        Start_Fight;
        return;
      elsif Stat = Status.Slave and then not Sync
      and then not Sync_Mng.In_Sync then
        -- Synchronise slave which is not Synchronised
        Dictio_Debug.Put (Dictio_Debug.Online, "Syncing " & From.Image );
        Sync_Mng.Send (From);
      end if;

    end if;

    if Stat = Status.Fight then
      Dictio_Debug.Put (Dictio_Debug.Online, "Fight cause fight from: "
                 & From.Image );
      Start_Fight;
      return;
    end if;

  end Event;

  function Timer_Cb (Unused_Id : Timers.Timer_Id;
                     Unused_Data : Timers.Timer_Data) return Boolean is
    use type Status.Status_List;
  begin
    if Status.Get = Status.Master then
      -- Send alive message
      -- With Crc if stable
      if Client_Mng.Stable then
        Intra_Dictio.Send_Status (Intra_Dictio.Extra_Crc & Data_Base.Get_Crc);
      else
        Intra_Dictio.Send_Status;
      end if;
    elsif Status.Get /= Status.Dead then
      -- No alive message
      Dictio_Debug.Put (Dictio_Debug.Online, "Fight due to alive timeout");
      Start_Fight;
    end if;
    return False;
  end Timer_Cb;

end Online_Mng;

