with Ada.Text_Io;
with Argument, Mixed_Str, Event_Mng;
with Fifos;
procedure T_Fifos is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
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
    Ada.Text_Io.Put_Line ("Send result: " & Mixed_Str(Res'Img));
  end Send;
   

  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Test_Fifo.Fifo_Id;
                     Connected : in Boolean) is
    use type Test_Fifo.Fifo_Id;
  begin
    Ada.Text_Io.Put ("Fifo " & Fifo_Name
      & " kind " & Fifos.Fifo_Kind_List'Image(Test_Fifo.Fifo_Kind (Id))
      & " is");
    if not Connected then
      Ada.Text_Io.Put (" not");
    end if;
    Ada.Text_Io.Put_Line (" connected");

    if not Server
    and then Connected
    and then Fid /= Test_Fifo.No_Fifo
    and then Fid /= Id then
      Ada.Text_Io.Put_Line ("Ooops in Fid");
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
    Ada.Text_Io.Put_Line ("Received >"
                        & Message.Str(1 .. Message.Len)
                        & "<  Len:" & Length'Img & ". Waiting then replying");
    Event_Mng.Wait(1_000);
    if Connected then
      Send (Id, "Ah que coucou aussi!");
    end if;
  end Rece_Cb;

  procedure Ovfl_Cb (Id      : in Test_Fifo.Fifo_Id) is
  begin
    Ada.Text_Io.Put_Line ("End of overflow");
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
                         Conn_Cb'Unrestricted_Access,
                         Rece_Cb'Unrestricted_Access,
                         Ovfl_Cb'Unrestricted_Access);

  Ada.Text_Io.Put_Line ("Fifo " & Argument.Get_Parameter(Occurence => 2)
    & " kind " & Fifos.Fifo_Kind_List'Image(Test_Fifo.Fifo_Kind (Fid))
    & " is open");

  while not Signal loop
    Event_Mng.Wait (-1);
  end loop;

  if Server then
    Ada.Text_Io.Put_Line ("Closing accepting Fifo");
    Test_Fifo.Close (Fid);
    Ada.Text_Io.Put_Line ("Closing all Fifos");
    Test_Fifo.Close_All;
  else
    Ada.Text_Io.Put_Line ("Closing connected Fifo");
    Test_Fifo.Close (Fid);
  end if;

  Ada.Text_Io.Put_Line ("Done.");

end T_Fifos;

