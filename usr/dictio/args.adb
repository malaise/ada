with Sys_Calls, Argument;
with Errors, Debug;
package body Args is

  procedure Usage is
  begin
    Debug.Put_Error ("Usage: "
                   & Argument.Get_Program_Name
                   & " -c<channel_name> -f<channel_file> -p<port>");
    raise Errors.Exit_Error;
  end Usage;

  procedure Init is
  begin
    declare
      S1 : constant String := Get_Channel_Name;
      S2 : constant String := Get_Dest_File;
      S3 : constant String := Get_Client_Port;
    begin
      return;
    end;
  exception
    when Argument.Argument_not_Found =>
      Debug.Put_Error ("ERROR. Argument not found");
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

end Args;

