with Trace.Loggers;
package body Log is

  Def_Logger : Trace.Loggers.Logger;
  Sub_Logger : Trace.Loggers.Logger;
  Sea_Logger : Trace.Loggers.Logger;
  Rep_Logger : Trace.Loggers.Logger;

  procedure Init is
  begin
    Def_Logger.Init ("");
    Sub_Logger.Init ("Substit");
    Sea_Logger.Init ("Search");
    Rep_Logger.Init ("Replace");
  end Init;

  procedure Def (Message : in String; Severity : Severities := Debug) is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    Def_Logger.Log (Severity, Message);
  end Def;
  procedure Sub (Message : in String; Severity : Severities := Debug) is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    Sub_Logger.Log (Severity, Message);
  end Sub;
  procedure Sea (Message : in String; Severity : Severities := Debug) is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    Sea_Logger.Log (Severity, Message);
  end Sea;
  procedure Rep (Message : in String; Severity : Severities := Debug) is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    Rep_Logger.Log (Severity, Message);
  end Rep;

  function Def_Debug return Boolean is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    return Def_Logger.Debug_On;
  end Def_Debug;
  function Sub_Debug return Boolean is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    return Sub_Logger.Debug_On;
  end Sub_Debug;
  function Sea_Debug return Boolean is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    return Sea_Logger.Debug_On;
  end Sea_Debug;
  function Rep_Debug return Boolean is
  begin
    if not Def_Logger.Is_Init then
      Init;
    end if;
    return Rep_Logger.Debug_On;
  end Rep_Debug;

end Log;

