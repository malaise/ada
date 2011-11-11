with Argument, Mixed_Str, Event_Mng, Basic_Proc;
with Fifos;
procedure T_Fifos is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                        & "  -s | -c      <fifo_name>");
  end Usage;

  Server : Boolean := False;
  Signal : Boolean := False;
  Connected : Boolean := False;

  -- Same as t_tcp_util
  type Test_Rec is record
    Len : Positive;
    Num : Positive := 1;
    Str : String (1 .. 80);
    Dummy : String (1 .. 11_000);
  end record;

  package Test_Fifo is new Fifos.Fifo (Test_Rec);
  Fid : Test_Fifo.Fifo_Id;

  procedure Send (Id : Test_Fifo.Fifo_Id; Str : in String) is
    Msg : Test_Rec;
    Res : Fifos.Send_Result_List;
  begin
    Msg.Str (1 .. Str'Length) := Str;
    Msg.Len := Str'Length;
    Res := Test_Fifo.Send (Id, Msg);
    Basic_Proc.Put_Line_Output ("Send result: " & Mixed_Str(Res'Img));
  end Send;


  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Test_Fifo.Fifo_Id;
                     Connected : in Boolean) is
    use type Test_Fifo.Fifo_Id;
  begin
    Basic_Proc.Put_Output ("Fifo " & Fifo_Name
      & " kind " & Fifos.Fifo_Kind_List'Image(Test_Fifo.Fifo_Kind (Id))
      & " is");
    if not Connected then
      Basic_Proc.Put_Output (" not");
    end if;
    Basic_Proc.Put_Line_Output (" connected");

    if not Server
    and then Connected
    and then Fid /= Test_Fifo.No_Fifo
    and then Fid /= Id then
      Basic_Proc.Put_Line_Output ("Ooops in Fid");
      Signal := True;
      return;
    end if;

    if not Server
    and then Connected then
      Send (Id, "Ah que coucou!");
    end if;
    T_Fifos.Connected := Connected;
  end Conn_Cb;

  procedure Rece_Cb (Id      : in Test_Fifo.Fifo_Id;
                     Message : in Test_Rec;
                     Length  : in Fifos.Message_Length) is
  begin
    Basic_Proc.Put_Line_Output ("Received >"
                        & Message.Str(1 .. Message.Len)
                        & "<  Len:" & Length'Img & ". Waiting then replying");
    Event_Mng.Wait(1_000);
    if Connected then
      Send (Id, "Ah que coucou aussi!");
    end if;
  end Rece_Cb;

  procedure Ovfl_Cb (Id      : in Test_Fifo.Fifo_Id) is
    pragma Unreferenced (Id);
  begin
    Basic_Proc.Put_Line_Output ("End of overflow");
  end Ovfl_Cb;


  procedure Sign_Cb is
  begin
    Signal := True;
  end Sign_Cb;


  use type Test_Fifo.Fifo_Id;
begin
  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  if Argument.Get_Parameter = "-s" then
    Server := True;
  elsif Argument.Get_Parameter = "-c" then
    Server := False;
  else
    Usage;
    return;
  end if;

  Event_Mng.Set_Sig_Term_Callback (Sign_Cb'Unrestricted_Access);

  Fid := Test_Fifo.Open (Argument.Get_Parameter(Occurence => 2),
                         not Server,
                         Conn_Cb'Access,
                         Rece_Cb'Access,
                         Ovfl_Cb'Access);

  Basic_Proc.Put_Line_Output ("Fifo " & Argument.Get_Parameter(Occurence => 2)
    & " kind " & Fifos.Fifo_Kind_List'Image(Test_Fifo.Fifo_Kind (Fid))
    & " is open");

  loop
    pragma Warnings (Off, "variable ""*"" is not modified in loop body");
    exit when Signal;
    pragma Warnings (On,  "variable ""*"" is not modified in loop body");
    Event_Mng.Wait (-1);
  end loop;

  if Server then
    Basic_Proc.Put_Line_Output ("Closing accepting Fifo");
    Test_Fifo.Close (Fid);
    Basic_Proc.Put_Line_Output ("Closing all Fifos");
    Test_Fifo.Close_All;
  else
    Basic_Proc.Put_Line_Output ("Closing connected Fifo");
    Test_Fifo.Close (Fid);
  end if;

  Basic_Proc.Put_Line_Output ("Done.");

end T_Fifos;

