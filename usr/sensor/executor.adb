with Event_Mng, Timers, Any_Def;
with Rules, Filters;
package body Executor is

  -- Exit request handler
  Do_Exit : Boolean := False;
  procedure Exit_Handler is
  begin
    Do_Exit := True;
  end Exit_Handler;

  -- The timer callbackl
  function Expire (Dummy_Id : in Timers.Timer_Id;
                   Data : in Timers.Timer_Data) return Boolean is
  begin
     -- Retrieve the filter index from data and read it
     -- Exec a shell that makes tail on the file and grep -E the regex
     -- If found, check each line of the result in the history
     -- If not found in the history, store in the history and execute rule
    return False;
  end Expire;

  -- Init the executor (arms timers and sets handlers)
  procedure Init is
    Filter : Filters.Filter_Rec;
    Data : Any_Def.Any (Any_Def.Inte_Kind);
    Appointment : Timers.Delay_Rec  (Timers.Delay_Sec);
    Dummy_Id : Timers.Timer_Id;
  begin
    -- Set sigterm callback
    Event_Mng.Set_Sig_Term_Callback (Exit_Handler'Access);
    -- Arm the timers, each associated to a filter
    Appointment.Delay_Seconds := 0.0;
    for I in 1 .. Filters.Get_Number loop
      -- Retrieve the filter
      Filter := Filters.Get_Filter (I);
      Appointment.Period := Filter.Period;
      -- Arm the timer
      Data.Inte := I;
      Dummy_Id := Timers.Create (Appointment, Expire'Access, Data);
    end loop;
  end Init;

  -- Has exit been requested
  function Exit_Requested return Boolean is
  begin
    return Do_Exit;
  end Exit_Requested;

end Executor;

