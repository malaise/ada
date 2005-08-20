-- Usage: pipe [ -n ] <mode> <fifo>
-- -n for no carriage return after message reception
-- <mode> ::= -c | -s
-- -c : connects to server  for data to relay
-- -s : accepts connections for data to relay
-- -C or -S for remanent (otherwise exit on fifo disconnection or end of input)
-- Each data is send to client which sent las data received
with Ada.Text_Io, Ada.Characters.Latin_1;

with Text_Handler, Sys_Calls, Argument, Async_Stdin, Mixed_Str,
     Event_Mng, Fifos;
procedure Pipe is

  -- Message type
  Max_Data_Size : constant := 1024;
  subtype Message_Type is String (1 .. Max_Data_Size);
  package Pipe_Fifo is new Fifos.Fifo (Message_Type);
  Fid, Acc_Id : Pipe_Fifo.Fifo_Id;

  -- Message
  Message : Message_Type;
  Len : Natural;

  -- Mode
  Server : Boolean;
  Remanent : Boolean;
  Put_Cr : Boolean;

  -- End of processing
  Done : Boolean := False;

  -- Next argument
  Next_Arg : Positive;

  -- Sig callback
  procedure Sig_Callback is
  begin
    Done := True;
  end Sig_Callback;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                            & " [ -n ] <mode> <fifo>");
    Sys_Calls.Put_Line_Error (
     "-n                     no cariage return after message is put");
    Sys_Calls.Put_Line_Error (
     "<mode> ::= -s | -c     exiting when fifo disconnects");
    Sys_Calls.Put_Line_Error (
     "<mode> ::= -S | -C     remaining when fifo disconnects");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  procedure Send (Str : in String) is
    Msg : Message_Type;
    Res : Fifos.Send_Result_List;
    use type Fifos.Send_Result_List, Pipe_Fifo.Fifo_Id;
  begin
    if Fid = Pipe_Fifo.No_Fifo then
      Sys_Calls.Put_Line_Error ("Send result: Not open");
      return;
    end if;
    Msg (1 .. Str'Length) := Str;
    Res := Pipe_Fifo.Send (Fid, Msg, Str'Length);
    if Res /= Fifos.Ok then
      Sys_Calls.Put_Line_Error ("Send result: " & Mixed_Str(Res'Img));
    end if;
  end Send;

  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Pipe_Fifo.Fifo_Id;
                     Connected : in Boolean) is
    use type Pipe_Fifo.Fifo_Id;
  begin
    if Server then
      if Connected then
        -- Accept client
        Fid := Id;
        if not Remanent then
          -- No more client if not remanent
          Pipe_Fifo.Close (Acc_Id);
        end if;
      else
        -- Disconnection, exit if not remanent
        if not Remanent then
          Done := True;
        else
          Fid := Pipe_Fifo.No_Fifo;
        end if;
      end if;
    else
      -- Client
      if Connected then
        Fid := Id;
      else
        if Remanent then
          Fid := Pipe_Fifo.No_Fifo;
        else
          Done := True;
        end if;
      end if;
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
    if Length = 1 and then Message(1) = Ada.Characters.Latin_1.Cr then
      Async_Stdin.New_Line_Out;
    else
      Async_Stdin.Put_Out (Message(1 .. Length));
      if Put_Cr then
        Async_Stdin.New_Line_Out;
      end if;
    end if;
  end Rece_Cb;


  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
  begin
    if Len = 0 then
      if not Remanent then
        Done := True;
      else
        Async_Stdin.Set_Async;
      end if;
      return True;
    end if;
    if Len >= 1 and then Str(Str'Length) = Ada.Characters.Latin_1.Eot then
      -- End of transmission
      Len := Len - 1;
      if not Remanent then
        Done := True;
      else
        Async_Stdin.Set_Async;
      end if;
    end if;
    -- Skip Lf but avoid empty message
    if Len > 1 and then (Str(Len) = Ada.Characters.Latin_1.Lf
                 or else Str(Len) = Ada.Characters.Latin_1.Cr) then
      Len := Len - 1;
    end if;
    -- Particular case of only return
    if Len = 1 and then Str(Len) = Ada.Characters.Latin_1.Lf then
      Message(1) := Ada.Characters.Latin_1.Cr;
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
        if not Remanent then
          Done := True;
        else
          Async_Stdin.Set_Async;
        end if;
        return;
    end;

    if Len = 0 then
      Message(1) := Ada.Characters.Latin_1.Cr;
      Len := 1;
    end if;
    Send (Message(1 .. Len));
  end Get_No_Tty;

  use type Fifos.Fifo_State_List, Event_Mng.Out_Event_List;

begin

  -- Get optional -n
  begin
    if Argument.Get_Parameter (1, "n") /= "" then
      Usage;
      return;
    end if;
    -- -n set, check and fix Next_Arg to mode
    if Argument.Get_Nbre_Arg /= 3 then
      Usage;
      return;
    end if;
    Next_Arg := Argument.Get_Position (1, "n");
    if Next_Arg = 1 then
      Next_Arg := 2;
    elsif Next_Arg = 3 then
      Next_Arg := 1;
    else
      Usage;
      return;
    end if;
    Put_Cr := False;
  exception
    when Argument.Argument_Not_Found =>
      -- -n not set, check and fix Next_Arg to mode
      if Argument.Get_Nbre_Arg /= 2 then
        Usage;
        return;
      end if;
      Next_Arg := 1;
      Put_Cr := True;
  end;


  -- Store mode
  if Argument.Get_Parameter (Occurence => Next_Arg) = "-s" then
    Server := True;
    Remanent := False;
  elsif Argument.Get_Parameter (Occurence => Next_Arg) = "-c" then
    Server := False;
    Remanent := False;
  elsif Argument.Get_Parameter (Occurence => Next_Arg) = "-S" then
    Server := True;
    Remanent := True;
  elsif Argument.Get_Parameter (Occurence => Next_Arg) = "-C" then
    Server := False;
    Remanent := True;
  else
    Usage;
    return;
  end if;

  -- Init connection
  Fid := Pipe_Fifo.Open (Argument.Get_Parameter(Occurence => Next_Arg + 1),
                         not Server,
                         Conn_Cb'Unrestricted_Access,
                         Rece_Cb'Unrestricted_Access,
                         Ovfl_Cb'Unrestricted_Access);
  if Server then
    -- Fid will be the one of last message received
    Acc_Id := Fid;
    Fid := Pipe_Fifo.No_Fifo;
  else
    -- Let connection establish
    Event_Mng.Wait (100);
    loop
      -- Wait until connected or nothing happens
      exit when Pipe_Fifo.Fifo_State (Fid) = Fifos.Connected
      or else Event_Mng.Wait (500) = Event_Mng.No_Event;
    end loop;
  end if;



  -- Init stdin
  Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, Max_Data_Size);
  Event_Mng.Set_Sig_Term_Callback (Sig_Callback'Unrestricted_Access);

  -- Main loop
  loop
     Event_Mng.Wait (-1);
     exit when Done;
  end loop;

  -- Restore stdin
  Async_Stdin.Set_Async;

  -- Close
  Pipe_Fifo.Close_All;

end Pipe;

