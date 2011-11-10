with As.U, Lower_Str, Argument, Tcp_Util, Basic_Proc;
with Debug, Space, Connection, Human, File, Screen;

procedure Chess is

  Mode : Human.Play_Mode;
  Color : Space.Color_List;
  Name : As.U.Asu_Us;
  Init : As.U.Asu_Us;
  Tmp_Txt : As.U.Asu_Us;
  Wait : Boolean;
  Port : Tcp_Util.Remote_Port;
  Invalid_Argument : exception;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
           & " [ { -D<debug_flag> } ] <mode>");
    Basic_Proc.Put_Line_Output ("  <mode> ::= <server> | <client> | <both>");
    Basic_Proc.Put_Line_Output ("    <server> ::= -S -c<color> <port> [ <replay_file> ]");
    Basic_Proc.Put_Line_Output ("    <client> ::= -C -c<color> <port> -s<server_host>");
    Basic_Proc.Put_Line_Output ("    <both>   ::= -B [ <init_option> ]");
    Basic_Proc.Put_Line_Output ("      <color> ::=  white | black");
    Basic_Proc.Put_Line_Output ("      <port>  ::=  -P<port_name> | -p<port_num>");
    Basic_Proc.Put_Line_Output ("      <init_option> ::= <replay_file> | <setup_file>");
    Basic_Proc.Put_Line_Output ("        <replay_file> ::= -f<file_name> [ -w ]");
    Basic_Proc.Put_Line_Output ("        <setup_file>  ::= -i<init_file>");
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
      Color := Space.Black;
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
    begin
      Port.Num := Tcp_Util.Port_Num'Value(Argument.Get_Parameter
                     (Param_Key => "p"));
    exception
      when Argument.Argument_Not_Found =>
        Port := (Kind => Tcp_Util.Port_Name_Spec, Name => As.U.Asu_Null);
        begin
          Argument.Get_Parameter(Port.Name, Param_Key => "P");
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
        Name.Set_Null;
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
      if Name.Is_Null then
        raise Invalid_Argument;
      end if;
    exception
      when  Argument.Argument_Not_Found =>
       raise Invalid_Argument;
    end;
  end if;

  -- Wait
  if Mode = Human.Client or else Name.Is_Null then
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
      if not Tmp_Txt.Is_Null then
        raise Invalid_Argument;
      end if;
      Wait := True;
    exception
      when Argument.Argument_Not_Found =>
        Wait := False;
    end;
  end if;

  -- Init file
  if Mode = Human.Both and then Name.Is_Null then
    begin
      Argument.Get_Parameter (Init, 1, "i");
    exception
      when Argument.Argument_Not_Found =>
        Init.Set_Null;
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


  Human.Play (Mode, Color, Name.Image, Port, Init.Image, Wait);

exception
  when Invalid_Argument | Argument.Argument_Not_Found =>
    Usage;
  when Connection.Color_Error =>
    Basic_Proc.Put_Line_Output ("Server uses "
            & Lower_Str (Argument.Get_Parameter (1, "c"))
            & ". Try alternate color.");
  when Connection.Busy_Error =>
    Basic_Proc.Put_Line_Output ("Server is busy.");
  when Human.Load_Error =>
    Basic_Proc.Put_Line_Output ("Load error.");
  when File.File_Error =>
    Basic_Proc.Put_Line_Output ("File I/O error.");
  when File.Format_Error =>
    Basic_Proc.Put_Line_Output ("File format error.");
  when Screen.Font_Too_Small =>
    Basic_Proc.Put_Line_Output ("Current font is too small.");
  when Screen.Font_Too_Big =>
    Basic_Proc.Put_Line_Output ("Current font is too big.");
end Chess;

