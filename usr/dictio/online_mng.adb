with Timers;
with Debug, Parse, Intra_Dictio, Local_host_Name, Nodes, Fight_Mng, Sync_Mng;
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


  procedure Start is
    T : Timers.Delay_Rec;
    use type Status.Status_List;
    use type Timers.Timer_Id;
  begin
    if Status.Get = Status.Slave then
      Start_Slave_Timeout;
    else
      T.Delay_Seconds := 0.0;
      T.Period := Alive_Period;
      Tid := Timers.Create (T, Timer_Cb'Access);
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

  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Diff : in Boolean) is
    use type Status.Status_List;
  begin
    if Status.Get = Status.Fight then
      Fight_Mng.Event (From, Stat);
    elsif Status.Get = Status.Slave then
      if Stat = Status.Master then
        -- Receive a Master while slave, restart timer
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
      elsif Stat = Status.Slave then
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
      Intra_Dictio.Reply_Status;
    end if;
  end Event;

  function Timer_Cb (Id : Timers.Timer_Id) return Boolean is
    use type Status.Status_List;
  begin
    if Status.Get = Status.Master then
      -- Send alive message
      Intra_Dictio.Send_Status;
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

