with Sys_Calls, Argument, Normal;
with Errors, Debug;
package body Args is

  subtype Prio_Range is Positive range 1 .. 255;
  Prio : Prio_Str;

  procedure Usage is
  begin
    Debug.Put_Error ("Usage: "
                   & Argument.Get_Program_Name
                   & " -c<channel_name> -f<channel_file> -p<port> -P<prio>");
    raise Errors.Exit_Error;
  end Usage;

  procedure Init is
  begin
    declare
      S1 : constant String := Get_Channel_Name;
      S2 : constant String := Get_Dest_File;
      S3 : constant String := Get_Client_Port;
      P  : constant Prio_Range
         := Prio_Range'Value (Argument.Get_Parameter(1, "P"));
    begin
      Prio := Normal (P, 3, Gap => '0');
      return;
    end;
  exception
    when Argument.Argument_not_Found =>
      Debug.Put_Error ("ERROR. Argument not found");
      Usage;
    when others =>
      Debug.Put_Error ("ERROR. Invalid argument");
      Usage;
  end Init;

  function Get_Channel_Name return String is
  begin
    return Argument.Get_Parameter(1, "c");
  end Get_Channel_Name;

  function Get_Dest_File return String is
  begin
    return Argument.Get_Parameter(1, "f");
  end Get_Dest_File;

  function Get_Client_Port return String is
  begin
    return Argument.Get_Parameter(1, "p");
  end Get_Client_Port;

  function Get_Prio return Prio_Str is
  begin
    return Prio;
  end Get_Prio;

end Args;

