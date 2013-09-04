with Trace;
package body Debug is

  Logger : Trace.Logger;

  function Set return Boolean is
  begin
    if not Logger.Is_Init then
      Logger.Init;
    end if;
    return Logger.Debug_On;
  end Set;

  procedure Log (Str : in String) is
  begin
    if not Logger.Is_Init then
      Logger.Init;
    end if;
    Logger.Log_Debug (Str);
  end Log;

end Debug;

