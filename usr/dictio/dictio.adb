with Debug, Dispatch, Errors;
procedure Dictio is
begin
  Debug.Init;
  Dispatch.Init;
  Dispatch.Run;
  Dispatch.Quit;
exception
  when Errors.Exit_Error  =>
    Dispatch.Quit;
end Dictio;

