with Ada.Text_Io;
with Debug, Lower_Str, Argument, X_Mng, Text_Handler;

with Space, Connection, Human, File;

procedure Chess is

  Color : Space.Color_List;
  Server_Name : Text_Handler.Text (512);
  File_Name : Text_Handler.Text (File.Max_File_Name_Len);
  Invalid_Argument : exception;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
           & " [ { -D<debug_flag> } ] -c<color> [ -s<server_name> [ -f<file_name>] ]");
  end Usage;

  procedure Parse_Debug (Flag : in String; Option : in Debug.Debug_List) is
    Debug_Key : constant String := "D";
  begin
    if Argument.Get_Parameter (1, Debug_Key & Flag) = "" then
      Debug.Set (Option, True);
    else
      raise Invalid_Argument;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      null;
  end Parse_Debug;

begin
  -- All is key
  begin
    Argument.Get_Parameter (Server_Name, 1, Argument.Not_Key);
    raise Invalid_Argument;
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;
  
  Parse_Debug ("M", Debug.Moves);
  Parse_Debug ("T", Debug.Think);
  Parse_Debug ("N", Debug.No_Check);
  Parse_Debug ("C", Debug.Connection);
  Parse_Debug ("H", Debug.Human);

  if Lower_Str (Argument.Get_Parameter (1, "c")) = "white" then
    Color := Space.White;
  elsif Lower_Str (Argument.Get_Parameter (1, "c")) = "black" then
    Color := Space.black;
  end if;
  begin
    Argument.Get_Parameter (Server_Name, 1, "s");
  exception
    when Argument.Argument_Not_Found =>
      null;
  end;
  begin
    Argument.Get_Parameter (File_Name, 1, "f");
    if not Text_Handler.Empty (Server_Name) then
      -- No file name if client
      raise Invalid_Argument;
    end if;
  exception
    when  Argument.Argument_Not_Found =>
     null;
  end;
  Human.Play (Text_Handler.Value (Server_Name),
              Text_Handler.Value (File_Name),
              Color);
 
exception
  when Invalid_Argument | Argument.Argument_Not_Found =>
    Usage;
  when Connection.Color_Error =>
    Ada.Text_Io.Put_Line ("Server uses " & Lower_Str (Argument.Get_Parameter (1, "c"))
                                         & ". Try alternate color.");
  when Connection.Busy_Error =>
    Ada.Text_Io.Put_Line ("Server is busy.");
  when Human.Load_Error =>
    Ada.Text_Io.Put_Line ("Load error.");
  when File.File_Error =>
    Ada.Text_Io.Put_Line ("File I/O error.");
  when File.Format_Error =>
    Ada.Text_Io.Put_Line ("File format error.");
end Chess;

