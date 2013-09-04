with Ada.Exceptions;
with Basic_Proc;
with Dictio_Debug, Dispatch, Errors;
procedure Dictio is
begin
  Basic_Proc.Set_Exit_Code (Errors.Init_Error);
  Dictio_Debug.Init;
  Dispatch.Init;

  Basic_Proc.Set_Exit_Code (Errors.Runtime_Error);
  Dispatch.Run;

  Dispatch.Quit;
exception
  when Errors.Exit_Signal =>
    Basic_Proc.Set_Exit_Code (Errors.Signal);
    Dispatch.Quit;
  when Errors.Exit_Error =>
    Dispatch.Quit;
  when Error:others =>
    Basic_Proc.Put_Line_Error ("Runtime exception "
                             & Ada.Exceptions.Exception_Name(Error));
end Dictio;

