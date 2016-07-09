package body Debug is

  procedure Log (Msg : in String) is
  begin
    Logger.Log_Debug (Msg);
  end Log;

end Debug;

