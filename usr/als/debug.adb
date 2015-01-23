with Trace.Loggers;
package body Debug is

  Logger : Trace.Loggers.Logger;

  procedure Log (Message : in String) is
  begin
    Logger.Init ("Als");
    Logger.Log_Debug (Message);
  end Log;

end Debug;

