-- Usage: pipe <mode> <fifo>
-- <mode> ::= -c | -s
-- -c : connects to server  for data to relay
-- -s : accepts connections for data to relay
-- -C or -S for remanent (otherwise exit on fifo disconnection or end of input)
-- Each data is send to client which sent last data received
with Ada.Text_Io, Ada.Characters.Latin_1;

with Text_Handler, Basic_Proc, Argument, Async_Stdin, Mixed_Str, Event_Mng;
with Fifos;
with Io_Flow;
procedure Pipe is

  -- Message type, same as Mcd
  Max_Data_Size : constant := Io_Flow.Max_Message_Len;
  subtype Message_Type is Io_Flow.Message_Type;

  -- The fifo
  package Pipe_Fifo is new Fifos.Fifo (Message_Type);
  Fid, Acc_Id : Pipe_Fifo.Fifo_Id;

  -- Message
  Message : Message_Type;

  -- Mode
  Server : Boolean;
  Remanent : Boolean;

  -- End of processing
  Done : Boolean := False;

  -- Sig callback
  procedure Sig_Callback is
  begin
    Done := True;
  end Sig_Callback;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                                        & " <mode> <fifo>");
    Basic_Proc.Put_Line_Error (
     "<mode> ::= -s | -c     exiting when fifo disconnects");
    Basic_Proc.Put_Line_Error (
     "<mode> ::= -S | -C     remaining when fifo disconnects");
    Basic_Proc.Set_Error_Exit_Code;
  end Usage;

  procedure Send (Str : in String) is
    Res : Fifos.Send_Result_List;
    use type Fifos.Send_Result_List, Pipe_Fifo.Fifo_Id;
  begin
    if Fid = Pipe_Fifo.No_Fifo then
      Basic_Proc.Put_Line_Error ("Send result: Not open");
      return;
    end if;
    Message (1 .. Str'Length) := Str;
    Res := Pipe_Fifo.Send (Fid, Message, Str'Length);
    if Res /= Fifos.Ok then
      Basic_Proc.Put_Line_Error ("Send result: " & Mixed_Str(Res'Img));
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
    Async_Stdin.Put_Out (Message(1 .. Length));
  end Rece_Cb;


  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
  begin
    if Len = 0 then
      -- Error
      if not Remanent then
        Done := True;
      else
        Async_Stdin.Set_Async;
      end if;
      return True;
    elsif Str(Str'Last) = Ada.Characters.Latin_1.Eot then
      -- End of transmission
      Send (Str(Str'First .. Len-1));
      if not Remanent then
        Done := True;
      else
        Async_Stdin.Set_Async;
      end if;
    else
      Send (Str(Str'First .. Len));
      if Str(Str'Last) /= Ada.Characters.Latin_1.Lf then
        Async_Stdin.New_Line_Out;
      end if;
    end if;
    return True;
  end Stdin_Cb;

  use type Fifos.Fifo_State_List, Event_Mng.Out_Event_List;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Store mode
  if Argument.Get_Parameter (Occurence => 1) = "-s" then
    Server := True;
    Remanent := False;
  elsif Argument.Get_Parameter (Occurence => 1) = "-c" then
    Server := False;
    Remanent := False;
  elsif Argument.Get_Parameter (Occurence => 1) = "-S" then
    Server := True;
    Remanent := True;
  elsif Argument.Get_Parameter (Occurence => 1) = "-C" then
    Server := False;
    Remanent := True;
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
    -- Fid will be the one of last message received
    Acc_Id := Fid;
    Fid := Pipe_Fifo.No_Fifo;
    -- Let pending connection establish
    Event_Mng.Wait (100);
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

