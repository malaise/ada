with Ada.Exceptions;
with Sys_Calls;
with Dictio_Debug, Dispatch, Errors;
procedure Dictio is
begin
  Sys_Calls.Set_Exit_Code (Errors.Init_Error);
  Dictio_Debug.Init;
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
  when Error:others =>
    Dictio_Debug.Put ("Runtime exception "
             & Ada.Exceptions.Exception_Name(Error));
end Dictio;

