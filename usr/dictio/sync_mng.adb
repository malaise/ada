with Timers, Event_Mng;
with Status, Intra_Dictio, Data_Base, Debug;
package body Sync_Mng is

  Sending_Sync : Boolean := False;

  Timer_Id : Timers.Timer_Id := Timers.No_Timer;
  Sync_Has_Been_Received : Boolean;
  Nb_Syn_Received : Natural := 0;
  Eos_Cb : End_Of_Sync_Callback := null;


  function Timer_Active return Boolean is
    use type Timers.Timer_Id; 
  begin
    return Timer_Id /= Timers.No_Timer;
  end Timer_Active;

  procedure Cancel_Timer is
  begin
    if Timer_Active then
      Timers.Delete (Timer_Id);
      Timer_Id := Timers.No_Timer;
    end if;
  end Cancel_Timer;

  function Timer_Cb (Id : in Timers.Timer_Id) return Boolean is
  begin
    if Sync_Has_Been_Received then
      -- Still in sync
      Sync_Has_Been_Received := False;
    else
      Cancel_Timer;
      if Eos_Cb /= null then
        Eos_Cb.all;
      end if;
      if Debug.Level_Array(Debug.Sync) then
        Debug.Put ("Sync: End, received " & Nb_Syn_Received'Img & " sync");
      end if;
    end if;
    return False;
  end Timer_Cb;

  procedure Start (End_Of_Sync_Cb : End_Of_Sync_Callback := null) is
  begin
    Cancel_Timer;
    Eos_Cb := End_Of_Sync_Cb;
    Nb_Syn_Received := 0;
    Timer_Id := Timers.Create ( (Timers.Delay_Sec, 1.0, 1.0),
                                Timer_Cb'access);
    Sync_Has_Been_Received := False;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Start");
    end if;
  end Start;

  procedure Cancel is
  begin
    Cancel_Timer;
    Eos_Cb := null;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Cancel");
    end if;
  end Cancel;

  procedure Sync_Received is
  begin
    if Timer_Active then
      Sync_Has_Been_Received := True;
      Nb_Syn_Received := Nb_Syn_Received + 1;
    end if;
  end Sync_Received;

  function In_Sync return Boolean is
  begin
    return Timer_Active or else Sending_Sync;
  end In_Sync;



  Max_Retry : constant := 3;
  First_Timeout : constant Natural := 100;
  Timeout_Factor : constant := 2;
  
  procedure Send is
    Item : Data_Base.Item_Rec;
    Result : Intra_Dictio.Reply_Result_List;
    Timeout : Natural;
    Nb_Items : Natural;
    use type Data_Base.Item_Rec, Intra_Dictio.Reply_Result_List;
  begin
    Sending_Sync := True;
    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Sending " & Natural'Image(Data_Base.Nb_Item)
               & " items");
    end if; 
    Data_Base.Read_First (Item);
    Nb_Items := 0;

    Items:
    while Item /= Data_Base.No_Item loop
      Timeout := First_Timeout;

      Retries:
      for I in 1 .. Max_Retry loop
        Result := Intra_Dictio.Reply_Sync_Data (Item);
        -- Ok or Error or too many Overflows.
        exit Retries when Result /= Intra_Dictio.Overflow
                          or else I = Max_Retry;
        if Debug.Level_Array(Debug.Sync) then
          Debug.Put ("Sync: Overflow");
        end if; 
        Event_Mng.Wait (Timeout);
        -- Increase timeout for next retry
        Timeout := Timeout * Timeout_Factor;
      end loop Retries;

      if Result = Intra_Dictio.Ok then
        Nb_Items := Nb_Items + 1;
        Event_Mng.Wait (1);
      else
        -- Give up if too many overflows or other error
        if Debug.Level_Array(Debug.Sync) then
          Debug.Put ("Sync: Giving up due to " & Result'Img);
        end if; 
        exit Items;
      end if;

      Data_Base.Read_Next (Item);

    end loop Items;

    if Debug.Level_Array(Debug.Sync) then
      Debug.Put ("Sync: Sent " & Nb_Items'Img & " items");
    end if; 
    Sending_Sync := False;
  end Send;

end Sync_Mng;

