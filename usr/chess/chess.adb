with Ada.Text_Io;
with Debug, Lower_Str, Argument, X_Mng, Text_Handler;

with Space, Connection, Human, File;

procedure Chess is

  Mode : Human.Play_Mode;
  Color : Space.Color_List;
  Name : Text_Handler.Text (File.Max_File_Name_Len);
  Invalid_Argument : exception;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
           & " [ { -D<debug_flag> } ] <mode>");
    Ada.Text_Io.Put_Line (" Modes:");
    Ada.Text_Io.Put_Line ("  server: -S -c<color> [ -f<file_name> ]");
    Ada.Text_Io.Put_Line ("  client: -C -c<color> -s<server_host>");
    Ada.Text_Io.Put_Line ("  both:   -B [ -f<file_name> ]");
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

  use type Human.Play_Mode;

begin
  -- All is key
  begin
    Argument.Get_Parameter (Name, 1, Argument.Not_Key);
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

  -- Parse play mode
  begin
    if Argument.Get_Parameter (1, "S") = "" then
      Mode := Human.Server;
    else
      raise Invalid_Argument;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      begin 
        if Argument.Get_Parameter (1, "C") = "" then
          Mode := Human.Client;
        else
          raise Invalid_Argument;
        end if;
      exception
        when Argument.Argument_Not_Found =>
          begin
            if Argument.Get_Parameter (1, "B") = "" then
              Mode := Human.Both;
            else
              raise Invalid_Argument;
            end if;
          exception
            when Argument.Argument_Not_Found =>
              raise Invalid_Argument;
          end;
      end;
  end;
  
  -- Color if not both
  if Mode /= Human.Both then
    if Lower_Str (Argument.Get_Parameter (1, "c")) = "white" then
      Color := Space.White;
    elsif Lower_Str (Argument.Get_Parameter (1, "c")) = "black" then
      Color := Space.black;
    else
      raise Invalid_Argument;
    end if;
  else
    Color := Space.White;
    begin
      Argument.Get_Parameter (Name, 1, "c");
      raise Invalid_Argument;
    exception
      when Argument.Argument_Not_Found =>
        null;
      when others =>
        raise Invalid_Argument;
    end;
  end if;

  -- Parse file name / server name
  if Mode /= Human.Client then
    begin
      Argument.Get_Parameter (Name, 1, "s");
      raise Invalid_Argument;
    exception
      when Argument.Argument_Not_Found =>
        null;
    end;
    begin
      Argument.Get_Parameter (Name, 1, "f");
    exception
      when Argument.Argument_Not_Found =>
        Text_Handler.Empty (Name);
    end;
  else
    begin
      Argument.Get_Parameter (Name, 1, "f");
      raise Invalid_Argument;
    exception
      when Argument.Argument_Not_Found =>
        null;
    end;
    begin
      Argument.Get_Parameter (Name, 1, "s");
      if Text_Handler.Empty (Name) then
        raise Invalid_Argument;
      end if;
    exception
      when  Argument.Argument_Not_Found =>
       raise Invalid_Argument;
    end;
  end if;

  Human.Play (Mode, Color, Text_Handler.Value (Name));
 
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

