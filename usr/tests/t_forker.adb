with Ada.Characters.Latin_1;

with Basic_Proc, Argument, Lower_Str, Socket, Forker, Ip_Addr, Tcp_Util;

procedure T_Forker is

  Soc : Socket.Socket_Dscr;
  Host : Tcp_Util.Remote_Host;
  Port : Socket.Port_Num;

  Buff : String (1 .. 500);
  Len  : Natural;

  Req_Num : Forker.Command_Number;
  Req : Forker.Request_Rec;
  Rep : Forker.Report_Rec;

  procedure My_Send is new Socket.Send (Forker.Request_Rec);
  procedure My_Receive is new Socket.Receive (Forker.Report_Rec,
                                              Forker.Report_Size);

  Cause : Forker.Exit_Cause_List;
  Code  : Natural;


  procedure Cat_Str (To : in out String; Str : in String) is
    C : Character;
  begin
    C := ' ';
    for I in To'Range loop
      if C = Ada.Characters.Latin_1.Nul
      and then To(I) = Ada.Characters.Latin_1.Nul then
        To(I .. I + Str'Length - 1) := Str;
        To(I + Str'Length + 1) := Ada.Characters.Latin_1.Nul;
        To(I + Str'Length + 2) := Ada.Characters.Latin_1.Nul;
        return;
      end if;
      C := To(I);
    end loop;
    raise Program_Error;
  end Cat_Str;

  use type Socket.Port_Num, Tcp_Util.Remote_Host_List, Forker.Command_Number;
begin

  -- Basic_Proc.Put_Line_Output ("Req_size: " & Integer'Image(Forker.Request_Rec'Size));
  -- Basic_Proc.Put_Line_Output ("Rep_size: " & Integer'Image(Forker.Report_Rec'Size));

  if Argument.Get_Nbre_Arg /= 2 then
    Basic_Proc.Put_Line_Output ("ERROR: Two args <dest> <port_name/num> expected.");
    Basic_Proc.Put_Line_Output ("  <dest> ::= <host_name/addr> | <lan_addr>");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  Soc.Open (Socket.Udp);

  Host := Ip_Addr.Parse (Argument.Get_Parameter(1));
  begin
    Port := Socket.Port_Num'Value (Argument.Get_Parameter(2));
  exception
    when others =>
      Port := 0;
  end;

  if Port = 0 then
    if Host.Kind = Tcp_Util.Host_Name_Spec then
      Soc.Set_Destination_Name_And_Service (False,
         Argument.Get_Parameter(1), Argument.Get_Parameter(2));
    else
      Soc.Set_Destination_Host_And_Service (Host.Id, Argument.Get_Parameter(2));
    end if;
  else
    if Host.Kind = Tcp_Util.Host_Name_Spec then
      Soc.Set_Destination_Name_And_Port (False,
         Argument.Get_Parameter(1), Port);
    else
      Soc.Set_Destination_Host_And_Port (Host.Id, Port);
    end if;
  end if;
  Soc.Link_Dynamic;
  Soc.Set_Blocking (Socket.Blocking_Send);
  Basic_Proc.Put_Line_Output (
      "My host is "
    & Socket.Host_Name_Of(Socket.Local_Host_Id)
    & " and my port is "
    & Socket.Port_Num'Image(Soc.Get_Linked_To));

  Req_Num := 0;
  loop
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Output("Start Kill Exit Ping Read (s k e p r) ? ");
    Basic_Proc.Get_Line (Buff, Len);

    if Len = 1 and then Buff(1) = 's' then
      -- Start
      Basic_Proc.Put_Line_Output ("Number =" & Req_Num'Img);
      Req := (Kind => Forker.Start_Request,
              Start_Data => Forker.Init_Start);
      Req.Start_Data.Number := Req_Num;

      Req_Num := Req_Num + 1;

      loop
        Basic_Proc.Put_Output ("Command ? ");
        Basic_Proc.Get_Line (Buff, Len);
        Req.Start_Data.Command(1 .. Len) := Buff(1 .. Len);
        exit when Len /= 0;
      end loop;

      loop
        Basic_Proc.Put_Output ("Argument (Empty to end) ? ");
        Basic_Proc.Get_Line (Buff, Len);
        exit when Len = 0;
        Cat_Str (Req.Start_Data.Command, Buff(1 .. Len));
      end loop;

      for N in Positive loop
        Basic_Proc.Put_Output ("Environ (Empty to end) ? ");
        Basic_Proc.Get_Line (Buff, Len);
        exit when Len = 0;
        if N = 1 then
          Req.Start_Data.Environ(1 .. Len) := Buff(1 .. Len);
        else
          Cat_Str (Req.Start_Data.Environ, Buff(1 .. Len));
        end if;
      end loop;

      Basic_Proc.Put_Output ("Current dir ? ");
      Basic_Proc.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Current_Dir(1 .. Len) := Buff(1 .. Len);
      end if;

      Basic_Proc.Put_Output ("Output flow ? ");
      Basic_Proc.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Output_Flow(1 .. Len) := Buff(1 .. Len);
        loop
          Basic_Proc.Put_Output ("  Append (t or [f]) ? ");
          Basic_Proc.Get_Line (Buff, Len);
          if Len = 0 then
            exit;
          elsif Len = 1 and then Buff(1) = 't' then
            Req.Start_Data.Append_Output := Forker.True;
            exit;
          elsif Len = 1 and then Buff(1) = 'f' then
            exit;
          end if;
        end loop;
      end if;

      Basic_Proc.Put_Output ("Error flow ? ");
      Basic_Proc.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Error_Flow(1 .. Len) := Buff(1 .. Len);
        loop
          Basic_Proc.Put_Output ("  Append (t or [f]) ? ");
          Basic_Proc.Get_Line (Buff, Len);
          if Len = 0 then
            exit;
          elsif Len = 1 and then Buff(1) = 't' then
            Req.Start_Data.Append_Error := Forker.True;
            exit;
          elsif Len = 1 and then Buff(1) = 'f' then
            exit;
          end if;
        end loop;
      end if;

      My_Send (Soc, Req);

    elsif Len = 1 and then Buff(1) = 'k' then
      Req := (Kind => Forker.Kill_Request,
              Kill_Data => (Number => 0, Signal => 0) );
      loop
        Basic_Proc.Put_Output ("Number ? ");
        Basic_Proc.Get_Line (Buff, Len);
        begin
          Req.Kill_Data.Number := Forker.Command_Number'Value(Buff(1 .. Len));
          exit;
        exception
          when others => null;
        end;
      end loop;

      loop
        Basic_Proc.Put_Output ("Signal ? ");
        Basic_Proc.Get_Line (Buff, Len);
        begin
          Req.Kill_Data.Signal := Natural'Value(Buff(1 .. Len));
          exit;
        exception
          when others => null;
        end;
      end loop;

      My_Send (Soc, Req);

    elsif Len = 1 and then Buff(1) = 'e' then
      Req := (Kind => Forker.Forker_Exit_Request,
              Forker_Exit_Data => (Exit_Code => 0) );
      loop
        Basic_Proc.Put_Output ("Exit code ? ");
        Basic_Proc.Get_Line (Buff, Len);
        begin
          Req.Forker_Exit_Data.Exit_Code := Natural'Value(Buff(1 .. Len));
          exit;
        exception
          when others => null;
        end;
      end loop;

      My_Send (Soc, Req);

    elsif Len = 1 and then Buff(1) = 'p' then
      Req := (Kind => Forker.Ping_Request);

      My_Send (Soc, Req);

    elsif Len = 1 and then Buff(1) = 'r' then
      begin
        My_Receive (Soc, Rep, Len, False);
      exception
        when Socket.Soc_Would_Block =>
          Len := 0;
          Basic_Proc.Put_Line_Output ("No report");
      end;
      if Len /= 0 then
        case Rep.Kind is
          when Forker.Start_Report =>
            Basic_Proc.Put_Line_Output ("Start: command"
              & Rep.Start_Result.Number'Img
              & " Pid " & Rep.Start_Result.Started_Pid'Img);
          when Forker.Kill_Report =>
            Basic_Proc.Put_Line_Output ("Kill: command"
              & Rep.Kill_Result.Number'Img
              & " Pid " & Rep.Kill_Result.Killed_Pid'Img);
          when Forker.Exit_Report =>
            Forker.Decode_Exit (Rep.Exit_Result.Status, Cause, Code);
            Basic_Proc.Put_Line_Output ("Exit: command"
              & Rep.Exit_Result.Number'Img
              & " Pid" & Rep.Exit_Result.Exit_Pid'Img
              & " Status" & Rep.Exit_Result.Status'Img
              & ": Cause " & Lower_Str(Cause'Img)
              & " Code" & Code'Img);
          when Forker.Forker_Exit_Report =>
            Basic_Proc.Put_Line_Output ("Forker exited");
          when Forker.Pong_Report =>
            Basic_Proc.Put_Line_Output ("Pong");
        end case;
      end if;

    end if;

  end loop;

end T_Forker;

