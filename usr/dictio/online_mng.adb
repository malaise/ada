with Timers;
with Debug, Parse, Intra_Dictio, Local_host_Name, Nodes,
     Fight_Mng, Sync_Mng, Data_Base, Sync_Mng, Versions,
     Client_Mng;

package body Online_Mng is

  Tid : Timers.Timer_Id := Timers.No_Timer;

  function Timer_Cb (Id : Timers.Timer_Id) return Boolean;

  Alive_Period : constant Duration := 1.0;

  Fight_Actions : constant Fight_Mng.Fight_Action :=
    (Nodes.Many_Master_Master => Status.Master,
     Nodes.Many_Master_Slave  => Status.Slave,
     Nodes.One_Master_Master  => Status.Master,
     Nodes.One_Master_Slave   => Status.Slave,
     Nodes.All_Init_Master    => Status.Master,
     Nodes.All_Init_Slave     => Status.Master,
     Nodes.No_Master_Master   => Status.Master,
     Nodes.No_Master_Slave    => Status.Slave);

  procedure Start_Slave_Timeout is
    T : Timers.Delay_Rec;
  begin
    T.Delay_Seconds := Alive_Period * 3.0;
    Tid := Timers.Create (T, Timer_Cb'Access);
  end Start_Slave_Timeout;


  procedure Start (First : in Boolean) is
    T : Timers.Delay_Rec;
    use type Status.Status_List;
    use type Timers.Timer_Id;
  begin
    if Status.Get = Status.Slave then
      Start_Slave_Timeout;
      if First then
        Sync_Mng.Start;
      end if;
    else
      -- Master
      T.Delay_Seconds := 0.0;
      T.Period := Alive_Period;
      Tid := Timers.Create (T, Timer_Cb'Access);
      Status.Sync := True;
      if First then
        Client_Mng.Start;
      end if;
    end if;
  
    if Debug.Level_Array(Debug.Online) then
      Debug.Put ("Online: start as " & Status.Get'Img);
    end if;
  end Start;

  procedure Start_Fight is
  begin
    Status.Set (Status.Fight);
    Intra_Dictio.Send_Status;
    Fight_Mng.Start (1.0, Fight_Actions);
  end Start_Fight;

  procedure Event (From  : in Tcp_Util.Host_Name;
                   Stat  : in Status.Status_List;
                   Sync : in Boolean;
                   Diff  : in Boolean;
                   Extra : in String := "") is
    use type Status.Status_List;
  begin
    if Status.Get = Status.Fight then
      Fight_Mng.Event (From, Stat, Sync, Diff, Extra);
    elsif Status.Get = Status.Slave then
      if Stat = Status.Master then
        -- Receive a Master while slave, check Crc and restart timer
        if not Sync_Mng.In_Sync
        and then Extra /= ""
        and then Extra(1) = Intra_Dictio.Extra_Crc then
          if Extra(2 .. Extra'Last) /= Data_Base.Get_Crc then
            -- Crc Error: Re sync
            if Debug.Level_Array(Debug.Online) then
              Debug.Put ("Online: Crc error. Received >" & Extra
                       & "< from: " & Parse(From)
                       & ", got >" & Data_Base.Get_Crc & "<");
            end if;
            -- Invalid Crc. Re-sync.
            Data_Base.Reset;
            Status.Sync := False;
            Sync_Mng.Start;
          else
            -- Crc OK:
            if not Status.Sync then
              if Debug.Level_Array(Debug.Online) then
                Debug.Put ("Online: Crc OK, synced.");
              end if;
              Status.Sync := True;
              Client_Mng.Start;
            end if;
          end if;
        end if;

        Timers.Delete (Tid);
        Start_Slave_Timeout;
      elsif Stat = Status.Dead then
        -- Receive a Dead while slave, start Fight
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: fight due to death of: "
                   & Parse(From) );
        end if;
        Timers.Delete (Tid);
        Start_Fight;
      end if;
    elsif Status.Get = Status.Master then
      if Stat = Status.Master then
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: fight cause another master: "
                   & Parse(From) );
        end if;
        Timers.Delete (Tid);
        Start_Fight;
      elsif Stat = Status.Slave and then not Sync_Mng.In_Sync then
        -- Synchronise initialising slave
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: sync " & Parse(From) );
        end if;
        Sync_Mng.Send;
        if Debug.Level_Array(Debug.Online) then
          Debug.Put ("Online: end of sync " & Parse(From) );
        end if;
      end if;
    end if;
    if Status.Get /= Status.Fight and then Stat = Status.Fight then
      if Debug.Level_Array(Debug.Online) then
        Debug.Put ("Online: fight cause fight from: "
                 & Parse(From) );
      end if;
      Timers.Delete (Tid);
      Start_Fight;
    end if;


    if Diff and then (Stat = Status.Starting or else Stat = Status.Fight) then
      if Debug.Level_Array(Debug.Online) then
        Debug.Put ("Online: reply status to: " & Parse(From) );
      end if;
      Intra_Dictio.Reply_Status (Intra_Dictio.Extra_Ver & Versions.Intra);
    end if;
  end Event;

  function Timer_Cb (Id : Timers.Timer_Id) return Boolean is
    use type Status.Status_List;
  begin
    if Status.Get = Status.Master then
      -- Send alive message with Crc
      Intra_Dictio.Send_Status (Intra_Dictio.Extra_Crc & Data_Base.Get_Crc);
    else
      -- No alive message
      if Debug.Level_Array(Debug.Online) then
        Debug.Put ("Online: fight due to alive timeout");
      end if;
      Start_Fight;
    end if;
    return False;
  end Timer_Cb;

end Online_Mng;

