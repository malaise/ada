with Ada.Text_Io, Ada.Characters.Latin_1;

with Sys_Calls;
with Argument;
with Lower_Str;
with Socket;
with Forker;

procedure T_Forker is

  Soc : Socket.Socket_Dscr;
  Port : Socket.Port_Num;

  Buff : String (1 .. 500);
  Len  : Natural;

  Req_Num : Natural;
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

  use type Socket.Port_Num;
begin

  -- Ada.Text_Io.Put_Line ("Req_size: " & Integer'Image(Forker.Request_Rec'Size));
  -- Ada.Text_Io.Put_Line ("Rep_size: " & Integer'Image(Forker.Report_Rec'Size));

  if Argument.Get_Nbre_Arg /= 2 then
    Ada.Text_Io.Put_Line ("Error. Two args <hostname> <port_name/num> expected.");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;

  Socket.Open (Soc, Socket.Udp);
  Socket.Link_Dynamic (Soc);
  begin
    Port := Socket.Port_Num'Value (Argument.Get_Parameter(2));
  exception
    when others =>
      Port := 0;
  end;
  if Port = 0 then
    Socket.Set_Destination_Name_And_Service (Soc, False,
       Argument.Get_Parameter(1), Argument.Get_Parameter(2));
  else
    Socket.Set_Destination_Name_And_Port (Soc, False,
       Argument.Get_Parameter(1), Port);
  end if;
  Socket.Set_Blocking (Soc, False);
  Ada.Text_Io.Put_Line (
      "My host is "
    & Socket.Host_Name_Of(Socket.Local_Host_Id)
    & " and my port is "
    & Socket.Port_Num'Image(Socket.Get_Linked_To (Soc)));

  Req_Num := 0;
  loop
    Ada.Text_Io.New_Line;
    Ada.Text_Io.Put("Start Kill Exit Ping Read (s k e p r) ? ");
    Ada.Text_Io.Get_Line (Buff, Len);

    if Len = 1 and then Buff(1) = 's' then
      -- Start
      Ada.Text_Io.Put_Line ("Number =" & Natural'Image(Req_Num));
      Req := (Kind => Forker.Start_Request,
              Start_Data => Forker.Init_Start);
      Req.Start_Data.Number := Req_Num;

      Req_Num := Req_Num + 1;

      loop
        Ada.Text_Io.Put ("Command ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
        Req.Start_Data.Command(1 .. Len) := Buff(1 .. Len);
        exit when Len /= 0;
      end loop;

      loop
        Ada.Text_Io.Put ("Argument (Empty to end) ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
        exit when Len = 0;
        Cat_Str (Req.Start_Data.Command, Buff(1 .. Len));
      end loop;

      for N in Positive loop
        Ada.Text_Io.Put ("Environ (Empty to end) ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
        exit when Len = 0;
        if N = 1 then
          Req.Start_Data.Environ(1 .. Len) := Buff(1 .. Len);
        else
          Cat_Str (Req.Start_Data.Environ, Buff(1 .. Len));
        end if;
      end loop;

      Ada.Text_Io.Put ("Current dir ? ");
      Ada.Text_Io.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Current_Dir(1 .. Len) := Buff(1 .. Len);
      end if;

      Ada.Text_Io.Put ("Output flow ? ");
      Ada.Text_Io.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Output_Flow(1 .. Len) := Buff(1 .. Len);
        loop
          Ada.Text_Io.Put ("  Append (t or [f]) ? ");
          Ada.Text_Io.Get_Line (Buff, Len);
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

      Ada.Text_Io.Put ("Error flow ? ");
      Ada.Text_Io.Get_Line (Buff, Len);
      if Len /= 0 then
        Req.Start_Data.Error_Flow(1 .. Len) := Buff(1 .. Len);
        loop
          Ada.Text_Io.Put ("  Append (t or [f]) ? ");
          Ada.Text_Io.Get_Line (Buff, Len);
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
        Ada.Text_Io.Put ("Number ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
        begin
          Req.Kill_Data.Number := Natural'Value(Buff(1 .. Len));
          exit;
        exception
          when others => null;
        end;
      end loop;

      loop
        Ada.Text_Io.Put ("Signal ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
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
        Ada.Text_Io.Put ("Exit code ? ");
        Ada.Text_Io.Get_Line (Buff, Len);
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
          Ada.Text_Io.Put_Line ("No report");
      end;
      if Len /= 0 then
        case Rep.Kind is
          when Forker.Start_Report =>
            Ada.Text_Io.Put_Line ("Start: command"
              & Natural'Image(Rep.Start_Result.Number)
              & " Pid "
              & Forker.Pid_Result'Image(Rep.Start_Result.Started_Pid));
          when Forker.Kill_Report =>
            Ada.Text_Io.Put_Line ("Kill: command"
              & Natural'Image(Rep.Kill_Result.Number)
              & " Pid "
              & Forker.Pid_Result'Image(Rep.Kill_Result.Killed_Pid));
          when Forker.Exit_Report =>
            Forker.Decode_Exit (Rep.Exit_Result.Status, Cause, Code);
            Ada.Text_Io.Put_Line ("Exit: command"
              & Natural'Image(Rep.Exit_Result.Number)
              & " Pid" & Forker.Pid_Result'Image(Rep.Exit_Result.Exit_Pid)
              & " Status" & Integer'Image(Rep.Exit_Result.Status)
              & ": Cause " & Lower_Str(Forker.Exit_Cause_List'Image(Cause))
              & " Code" & Natural'Image(Code));
          when Forker.Forker_Exit_Report =>
            Ada.Text_Io.Put_Line ("Forker exited");
          when Forker.Pong_Report =>
            Ada.Text_Io.Put_Line ("Pong");
        end case;
      end if;

    end if;

  end loop;

end T_Forker;

