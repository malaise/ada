with Timers;
with Debug, Parse, Intra_Dictio, Local_Host_Name, Nodes,
     Fight_Mng, Sync_Mng, Data_Base, Sync_Mng, Versions,
     Client_Mng;

package body Online_Mng is

  Tid : Timers.Timer_Id := Timers.No_Timer;

  -- Ever requested a sync?
  Ever_Synced : Boolean := False;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

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
    use type Timers.Timer_Id;
  begin
    if Tid /= Timers.No_Timer then
      Timers.Delete (Tid);
    end if;
    Tid := Timers.No_Timer;
  end Delete_Timer;

  procedure Start_Slave_Timeout is
    T : Timers.Delay_Rec;
  begin
    T.Delay_Seconds := Alive_Period * 3.0;
    Tid := Timers.Create (T, Timer_Cb'Access);
  end Start_Slave_Timeout;

  No_Master : constant Tcp_Util.Host_Name := (others => ' ');
  Current_Master : Tcp_Util.Host_Name := No_Master;

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
      Local_Host_Name.Get (Current_Master);
      T.Delay_Seconds := 0.0;
      T.Period := Alive_Period;
      Tid := Timers.Create (T, Timer_Cb'Access);
      Status.Sync := True;
      Ever_Synced := True;
      Client_Mng.Start;
    end if;
  
    if Debug.Level_Array(Debug.Online) then
      Debug.Put ("Online: start as " & Status.Get'Img & " " & First'Img);
    end if;
  end Start;

  procedure Start_Fight is
    use type Timers.Timer_Id;
  begin
    Reset_Master;
    if Sync_Mng.In_Sync then
      Sync_Mng.Cancel;
    end if;
    Delete_Timer;
    Fight_Mng.Start (Status.Fight, 1.0, Fight_Actions);
  end Start_Fight;

  procedure Event (From  : in Tcp_Util.Host_Name;
                   Stat  : in Status.Status_List;
                   Sync  : in Boolean;
                   Diff  : in Boolean;
                   Extra : in String := "") is
    use type Status.Status_List;
    Crc : constant String
        := Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Crc);
  begin
    if Status.Get = Status.Slave then
      if Stat = Status.Master then

        -- Receive a Master while slave, check Crc and restart timer
        Current_Master := From;
        if Crc /= "" and then not Sync_Mng.In_Sync then
          if not Ever_Synced then
            -- Never synced and not syncing (init). Sync.
            if Debug.Level_Array(Debug.Online) then
              Debug.Put ("Online: Syncing from: " & Parse(From));
            end if;
            Data_Base.Reset;
            Status.Sync := False;
            Ever_Synced := True;
            Sync_Mng.Start;
          elsif Crc /= Data_Base.Get_Crc then
            -- Invalid Crc. Re-sync.
            if Debug.Level_Array(Debug.Online) then
              Debug.Put ("Online: Crc error. Received " & Crc
                       & " from: " & Parse(From)
                       & ", got " & Data_Base.Get_Crc);
            end if;
            Data_Base.Reset;
            Status.Sync := False;
            Sync_Mng.Start;
          elsif not Status.Sync then
            -- Crc OK and not synced
            if Debug.Level_Array(Debug.Online) then
              Debug.Put ("Online: Crc OK, synced");
            end if;
            Status.Sync := True;
            Client_Mng.Start;
          end if;
        end if;

        Delete_Timer;
        Start_Slave_Timeout;
      elsif Stat = Status.Dead then
        -- Receive a Dead while slave, start Fight
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: fight due to death of: "
                   & Parse(From) );
        end if;
        Start_Fight;
        return;
      else
        -- Receive other status, check that this is not from previously known master
        if Current_Master /= No_Master and then From = Current_Master then
          if Debug.Level_Array(Debug.Online) then
            Debug.Put ("Online: fight due to master new status "
                       & Parse(From) & "/" & Stat'Img);
          end if;
          Start_Fight;
          return;
        end if;
      end if;

    elsif Status.Get = Status.Master then

      if Stat = Status.Master
      and then Intra_Dictio.Extra_Of (Extra, Intra_Dictio.Extra_Crc) /= "" then
        -- An alive message (not a fight reply nor fight info)
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: fight cause another master: " & Parse(From) );
        end if;
        Start_Fight;
        return;
      elsif Stat = Status.Slave and then not Sync
      and then not Sync_Mng.In_Sync then
        -- Synchronise slave which is not Synchronised
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: syncing " & Parse(From) );
        end if;
        Sync_Mng.Send (From);
      end if;

    end if;

    if Stat = Status.Fight then
      if Debug.Level_Array(Debug.Online) then
        Debug.Put ("Online: fight cause fight from: "
                 & Parse(From) );
      end if;
      Start_Fight;
      return;
    end if;

  end Event;

  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
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
      if Debug.Level_Array(Debug.Online) then
        Debug.Put ("Online: fight due to alive timeout");
      end if;
      Tid := Timers.No_Timer;
      Start_Fight;
    end if;
    return False;
  end Timer_Cb;

end Online_Mng;

