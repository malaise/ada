with Sys_Calls;
with Debug, Dispatch, Errors;
procedure Dictio is
begin
  Sys_Calls.Set_Exit_Code (Errors.Init_Error);
  Debug.Init;
  Dispatch.Init;

  Sys_Calls.Set_Exit_Code (Errors.Runtime_Error);
  Dispatch.Run;

  Dispatch.Quit;
exception
  when Errors.Exit_Signal =>
    Sys_Calls.Set_Exit_Code (Errors.Signal);
    Dispatch.Quit;
  when Errors.Exit_Error =>
    Dispatch.Quit;
end Dictio;

