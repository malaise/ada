-- Usage: relay <mode> <fifo>
-- <mode> ::= -c | -s
-- -c : connects to server  for data to relay
-- -s : accepts connections for data to relay
-- Each data is send to client which sent las data received
with Ada.Text_Io;

with Text_Handler, Sys_Calls, Argument, Async_Stdin, Mixed_Str,
     Event_Mng, Fifos;
procedure Pipe is

  -- Message type
  Max_Data_Size : constant := 1024;
  subtype Message_Type is String (1 .. Max_Data_Size);
  package Pipe_Fifo is new Fifos.Fifo (Message_Type);
  Fid : Pipe_Fifo.Fifo_Id;

  -- Message
  Message : Message_Type;
  Len : Natural;

  -- Mode
  Server : Boolean;

  -- End of processing
  Done : Boolean := False;

  -- Sig callback
  procedure Sig_Callback is
  begin
    Done := True;
  end Sig_Callback;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                            & " <mode> <fifo>");
    Sys_Calls.Put_Line_Error ("<mode> ::= -s | -c");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  procedure Send (Str : in String) is
    Msg : Message_Type;
    Res : Fifos.Send_Result_List;
    use type Fifos.Send_Result_List, Pipe_Fifo.Fifo_Id;
  begin
    if Fid = Pipe_Fifo.No_FIfo then
      Sys_Calls.Put_Line_Error ("Send result: Not open");
      return;
    end if;
    Msg (1 .. Str'Length) := Str;
    Res := Pipe_Fifo.Send (FId, Msg, Str'Length);
    if Res /= Fifos.Ok then
      Sys_Calls.Put_Line_Error ("Send result: " & Mixed_Str(Res'Img));
    end if;
  end Send;

  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Pipe_Fifo.Fifo_Id;
                     Connected : in Boolean) is
  begin
    if not Server then
      Fid := Id;
    end if;
  end Conn_Cb;

  procedure Ovfl_Cb (Id      : in Pipe_Fifo.Fifo_Id) is
  begin
    null;
  end Ovfl_Cb;

  procedure Rece_Cb (Id      : in Pipe_Fifo.Fifo_Id;
                     Message : in Message_Type;
                     Length  : in Fifos.Message_Length) is
  begin
    if Server then
      Fid := Id;
    end if;
    if Length = 1 and then Message(1) = Ascii.Cr then
      Ada.Text_Io.New_Line;
    else
      Ada.Text_Io.Put_Line (Message(1 .. Length));
    end if;
  end Rece_Cb;


  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
  begin
    if Len >= 1 and then Str(Str'Length) = Ascii.Eot then
      -- End of transmission
      Len := Len - 1;
      Done := True;
    end if;
    -- Skip Lf but avoid empty message
    if Len > 1 and then (Str(Len) = Ascii.Lf
                 or else Str(Len) = Ascii.Cr) then
      Len := Len - 1;
    end if;
    -- Particular case of only return
    if Len = 1 and then Str(Len) = Ascii.Lf then
      Message(1) := Ascii.Cr;
    else
      Message (1 .. Len) := Str (1 .. Len);
    end if;
    if Len > 0 then
      Send (Message(1 .. Len));
    end if;
    return True;
  end Stdin_Cb;

  -- Read data from not a tty (file?)
  procedure Get_No_Tty is
  begin
    begin
      Ada.Text_Io.Get_Line (Message, Len);
    exception
      when Ada.Text_Io.End_Error =>
        Done := True;
        return;
    end;

    if Len = 0 then
      Message(1) := Ascii.Cr;
      Len := 1;
    end if;
    Send (Message(1 .. Len));
  end Get_No_Tty;

begin

  -- 2 arguments
  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Store mode
  if Argument.Get_Parameter (Occurence => 1) = "-s" then
    Server := True;
  elsif Argument.Get_Parameter (Occurence => 1) = "-c" then
    Server := False;
  else
    Usage;
    return;
  end if;

  -- Init connection
  Fid := Pipe_Fifo.Open (Argument.Get_Parameter(Occurence => 2),
                         not Server,
                         Conn_Cb'Unrestricted_Access,
                         Rece_Cb'Unrestricted_Access,
                         Ovfl_Cb'Unrestricted_Access);
  if Server then
    Fid := Pipe_Fifo.No_Fifo;
  end if;

  -- Init stdin
  Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, Max_Data_Size);
  Event_Mng.Set_Sig_Callback (Sig_Callback'Unrestricted_Access);

  -- Main loop
  loop
     Event_Mng.Wait (-1);
     exit when Done;
  end loop;

  -- Restore stdin
  Async_Stdin.Set_Async;

end Pipe;

