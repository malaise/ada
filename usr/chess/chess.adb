with Ada.Text_Io;
with Debug, Lower_Str, Argument, X_Mng, Text_Handler, Tcp_Util;

with Space, Connection, Human, File;

procedure Chess is

  Mode : Human.Play_Mode;
  Color : Space.Color_List;
  Name : Text_Handler.Text (File.Max_File_Name_Len);
  Init : Text_Handler.Text (File.Max_File_Name_Len);
  Tmp_Txt : Text_Handler.Text (File.Max_File_Name_Len);
  Wait : Boolean;
  Port : Tcp_Util.Remote_Port;
  Invalid_Argument : exception;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
           & " [ { -D<debug_flag> } ] <mode>");
    Ada.Text_Io.Put_Line ("  <mode> ::= <server> | <client> | <both>");
    Ada.Text_Io.Put_Line ("    <server> ::= -S -c<color> <port> [ <replay_file> ]");
    Ada.Text_Io.Put_Line ("    <client> ::= -C -c<color> <port> -s<server_host>");
    Ada.Text_Io.Put_Line ("    <both>   ::= -B [ <init_option> ]");
    Ada.Text_Io.Put_Line ("      <color> ::=  white | black");
    Ada.Text_Io.Put_Line ("      <port>  ::=  -P<port_name> | -p<port_num>");
    Ada.Text_Io.Put_Line ("      <init_option> ::= <replay_file> | <setup_file>");
    Ada.Text_Io.Put_Line ("        <replay_file> ::= -f<file_name> [ -w ]");
    Ada.Text_Io.Put_Line ("        <setup_file>  ::= -i<init_file>");
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

  -- Port if not both
  Port := (Kind => Tcp_Util.Port_Num_Spec, Num => 0);
  if Mode /= Human.Both then
    declare
      Len : Natural;
    begin
      Port.Num := Tcp_Util.Port_Num'Value(Argument.Get_Parameter(Param_Key => "p"));
    exception
      when Argument.Argument_Not_Found =>
        Port := (Kind => Tcp_Util.Port_Name_Spec, Name => (others => ' '));
        begin
          Argument.Get_Parameter(Port.Name, Len, Param_Key => "P");
        exception
          when others =>
            raise Invalid_Argument;
        end;
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

  -- Wait
  if Mode = Human.Client or else Text_Handler.Empty (Name) then
    begin
      Argument.Get_Parameter (Tmp_Txt, 1, "w");
      raise Invalid_Argument;
    exception
      when Argument.Argument_Not_Found =>
        Wait := False;
    end;
  else
    begin
      Argument.Get_Parameter (Tmp_Txt, 1, "w");
      if not Text_Handler.Empty (Tmp_Txt) then
        raise Invalid_Argument;
      end if;
      Wait := True;
    exception
      when Argument.Argument_Not_Found =>
        Wait := False;
    end;
  end if;

  -- Init file
  if Mode = Human.Both and then Text_Handler.Empty (Name) then
    begin
      Argument.Get_Parameter (Init, 1, "i");
    exception
      when Argument.Argument_Not_Found =>
        Text_Handler.Empty (Init);
    end;
  else
    begin
      Argument.Get_Parameter (Init, 1, "i");
      raise Invalid_Argument;
    exception
      when Argument.Argument_Not_Found =>
        null;
    end;
  end if;


  Human.Play (Mode, Color, Text_Handler.Value (Name), Port,
              Text_Handler.Value (init), Wait);
 
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

