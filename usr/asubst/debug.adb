with Trace;
package body Debug is

  Init : Boolean := False;
  Logger : Trace.Logger;

  function Set return Boolean is
  begin
    if not Init then
      Logger.Activate;
      Init := True;
    end if;
    return Logger.Debug_On;
  end Set;

  procedure Log (Str : in String) is
  begin
    if not Init then
      Logger.Activate;
      Init := True;
    end if;
    Logger.Log_Debug (Str);
  end Log;

end Debug;

