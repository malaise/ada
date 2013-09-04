with Mixed_Str;
package body Dictio_Debug is

  procedure Init is
  begin
    for I in Loggers'Range loop
      Loggers(I).Init (Mixed_Str (I'Img));
    end loop;
  end Init;

  -- Put debug
  procedure Put (Level : in Level_List; Msg : in String) is
  begin
    Loggers(Level).Log_Debug (Msg);
  end Put;

  -- Put info, warning, error or fatal
  procedure Put_Info (Level : in Level_List; Msg : in String) is
  begin
    Loggers(Level).Log_Info (Msg);
  end Put_Info;
  procedure Put_Warning (Level : in Level_List; Msg : in String) is
  begin
    Loggers(Level).Log_Warning (Msg);
  end Put_Warning;
  procedure Put_Error (Level : in Level_List; Msg : in String) is
  begin
    Loggers(Level).Log_Error (Msg);
  end Put_Error;
  procedure Put_Fatal (Level : in Level_List; Msg : in String) is
  begin
    Loggers(Level).Log_Fatal (Msg);
  end Put_Fatal;

end Dictio_Debug;

