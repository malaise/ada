with Ada.Text_Io, Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Text_Handler, Event_Mng, Sys_Calls, Async_Stdin;
with Fifos;
with Input_Dispatcher, Debug, Mcd_Mng;
package body Io_Flow is

  -- Init, get fifo name or leave empty for stdin
  Init_Done : Boolean := False;
  Fifo_Name : Text_Handler.Text (Fifos.Max_Fifo_Name_Len);

  -- Data read from stdin tty or fifo
  Max_Data_Len : constant := Input_Dispatcher.Max_String_Lg;
  subtype Input_Type is String (1 .. Max_Data_Len);
  Input_Data : Text_Handler.Text (Input_Type'Length);

  -- Fifo
  package Mcd_Fifos is new Fifos.Fifo (Input_Type);
  Fifo_Msg : Input_Type;
  Acc_Id, Client_Id : Mcd_Fifos.Fifo_Id := Mcd_Fifos.No_Fifo;
  procedure Open_Fifo;

  -- Stdin
  Stdin_Is_A_Tty : Boolean := False;
  function Stdin_Cb (Str : in String) return Boolean;

  ----------------------------------------------------
  -- Init fifo or stdin
  ----------------------------------------------------
  procedure Init is
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    if Init_Done then
      return;
    end if;
    Init_Done  := True;
    -- Get fifo name argument if set
    begin
      Argument.Get_Parameter (Fifo_Name, 1, "f");
    exception
      when Argument.Argument_Not_Found =>
        -- Stdin
        Text_Handler.Empty (Fifo_Name);
        if Debug.Debug_Level_Array(Debug.Parser) then
          Ada.Text_Io.Put_Line ("Flow: init on stdin");
        end if;
        Stdin_Is_A_Tty :=
          Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
        if Stdin_Is_A_Tty then
          Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, Max_Data_Len);
        end if;
        return;
    end;
    -- Fifo
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: init on fifo "
                          & Text_Handler.Value (Fifo_Name));
    end if;
    Open_Fifo;
    if Text_Handler.Empty (Fifo_Name) then
      raise Fifo_Error;
    end if;

  end Init;

  ----------------------------------------------------
  -- Get data from fifo or stdin
  ----------------------------------------------------
  procedure Next_Line (Str : out String;
                       Len : out Natural) is
    Evt : Event_Mng.Out_Event_List;
    use type Event_Mng.Out_Event_List;
  begin
    Init;
    if Text_Handler.Empty (Fifo_Name) and then not Stdin_Is_A_Tty then
      -- Get next non empty line from Stdin
      loop
        begin
          Ada.Text_Io.Get_Line (Str, Len);
          exit when Len /= 0;
        exception
          when Ada.Text_Io.End_Error =>
            -- Set Len to 0 and the end of Stdin
            Len := 0;
            exit;
        end;
      end loop;
      return;
    else
      -- Get next data on TTY stdy or Fifo
      loop
        Text_Handler.Empty (Input_Data);
        if Debug.Debug_Level_Array(Debug.Parser) then
          Ada.Text_Io.Put_Line ("Flow: Waiting on fifo/tty");
        end if;
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Fd_Event
        and then not Text_Handler.Empty (Input_Data) then
          -- New string
          Len := Text_Handler.Length (Input_Data);
          Str(1 .. Len) := Text_Handler.Value (Input_Data);
          exit;
        elsif Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Len := 0;
          exit;
        end if;
      end loop;
    end if;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: Next_Line -> " & Str(1 .. Len));
    end if;
  end Next_Line;

  ----------------------------------------------------
  -- Put data on fifo or stdout
  ----------------------------------------------------
  procedure Put (Str : in String) is
    Res : Fifos.Send_Result_List;
    use type Mcd_Fifos.Fifo_Id, Fifos.Send_Result_List;
  begin
    if Text_Handler.Empty (Fifo_Name) then
      Ada.Text_Io.Put (Str);
    elsif Client_Id /= Mcd_Fifos.No_Fifo then
      if Str'Length > Fifo_Msg'Length then
        raise Mcd_Mng.String_Len;
      end if;
      Fifo_Msg(1 .. Str'Length) := Str;
      Res := Mcd_Fifos.Send (Client_Id, Fifo_Msg, Str'Length);
      if Res /= Fifos.Ok and then Res /= Fifos.Overflow then
        Mcd_Fifos.Close (Client_Id);
        Open_Fifo;
      end if;
    end if;
  exception
    when Fifos.In_Overflow =>
      Mcd_Fifos.Close (Client_Id);
      Open_Fifo;
  end Put;

  procedure New_Line is
  begin
    if Text_Handler.Empty (Fifo_Name) then
      Ada.Text_Io.New_Line;
    else
      Put ("" & Ada.Characters.Latin_1.Cr);
    end if;
  end New_Line;

  Closing : Boolean := False;
  procedure Close is
    use type Mcd_Fifos.Fifo_Id;
  begin
    if Text_Handler.Empty (Fifo_Name) then
      if Stdin_Is_A_Tty then
        Async_Stdin.Set_Async;
      end if;
      return;
    end if;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: Closing fifo");
    end if;
    Closing := True;
    if Client_Id /= Mcd_Fifos.No_Fifo then
      Mcd_Fifos.Close (Client_Id);
    end if;
    if Acc_Id /= Mcd_Fifos.No_Fifo then
      Mcd_Fifos.Close (Acc_Id);
    end if;
  end Close;

  ----------------------------------------------------
  -- Fifo callbacks
  ----------------------------------------------------
  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Mcd_Fifos.Fifo_Id;
                     Connected : in Boolean) is
  begin
    if Connected then 
      if Debug.Debug_Level_Array(Debug.Parser) then
        Ada.Text_Io.Put_Line ("Flow: Client accepted");
      end if;
      -- Accept client and stop accepting
      Mcd_Fifos.Close (Acc_Id);
      Client_Id := Id;
    else
      -- Client disconnects, allow new client
      if Debug.Debug_Level_Array(Debug.Parser) then
        Ada.Text_Io.Put_Line ("Flow: Client has disconnected");
      end if;
      Client_Id := Mcd_Fifos.No_Fifo;
      if not Closing then
        Open_Fifo;
      end if;
    end if;
  end Conn_Cb;

  procedure Rece_Cb (Id      : in Mcd_Fifos.Fifo_Id;
                     Message : in Input_Type;
                     Length  : in Fifos.Message_Length) is
  begin
    if Length = 1
    and then (Message(1) = Ada.Characters.Latin_1.Cr
              or else Message(1) = Ada.Characters.Latin_1.Lf) then
      return;
    end if;
    Text_Handler.Set (Input_Data, Message(1 .. Length));
  end Rece_Cb;

  procedure Open_Fifo is
  begin
    if Text_Handler.Empty (Fifo_Name) then
      if Debug.Debug_Level_Array(Debug.Parser) then
        Ada.Text_Io.Put_Line ("Flow: Opening empty fifo discarded");
      end if;
      return;
    end if;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: Opening fifo "
                          & Text_Handler.Value (Fifo_Name));
    end if;
    Acc_Id := Mcd_Fifos.Open (Text_Handler.Value (Fifo_Name),
                              False,
                              Conn_Cb'Access,
                              Rece_Cb'Access,
                              null);
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: Fifo open");
    end if;
  exception
    when Error:others =>
      if Debug.Debug_Level_Array(Debug.Parser) then
        Ada.Text_Io.Put_Line ("Flow: Fifo open error "
                            & Ada.Exceptions.Exception_Name (Error));
      end if;
      Text_Handler.Empty (Fifo_Name);
  end Open_Fifo;

  ----------------------------------------------------
  -- Stdin callback
  ----------------------------------------------------
  function Stdin_Cb (Str : in String) return Boolean is
  begin
    if Str = "" then
      -- Error or end
      Text_Handler.Empty (Input_Data);
      return True;
    elsif Str(Str'Last) = Ada.Characters.Latin_1.Cr
    or else  Str(Str'Last) = Ada.Characters.Latin_1.lf then
      -- Skip Cr/lf
      Text_Handler.Set (Input_Data, Str(1 .. Natural'Pred(Str'Last)));
    else
      Text_Handler.Set (Input_Data, Str);
    end if;
    return True;
  end Stdin_Cb;

end Io_Flow;

