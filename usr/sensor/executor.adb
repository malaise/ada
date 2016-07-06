with Event_Mng;
package body Executor is

  -- Exit request handler
  Do_Exit : Boolean := False;
  procedure Exit_Handler is
  begin
    Do_Exit := True;
  end Exit_Handler;

  -- Init the executor (arms timers and sets handlers)
  procedure Init is
  begin
    Event_Mng.Set_Sig_Term_Callback (Exit_Handler'Access);
  end Init;

  -- Has exit been requested
  function Exit_Requested return Boolean is
  begin
    return Do_Exit;
  end Exit_Requested;

end Executor;

