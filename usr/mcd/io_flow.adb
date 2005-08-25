with Ada.Text_Io, Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Text_Handler, Event_Mng, Sys_Calls, Async_Stdin;
with Fifos;
with Input_Dispatcher, Debug, Mcd_Mng;
package body Io_Flow is

  package Unb renames Ada.Strings.Unbounded;

  -- Init, get fifo name or leave empty for stdin
  Init_Done : Boolean := False;
  Fifo_Name : Text_Handler.Text (Fifos.Max_Fifo_Name_Len);

  -- Data read from fifo
  Max_Message_Len : constant := 10240;
  subtype Message_Type is String (1 .. Max_Message_Len);

  -- Data read from stdin tty
  Input_Data : Unb.Unbounded_String;

  -- Fifo
  package Mcd_Fifos is new Fifos.Fifo (Message_Type);
  Acc_Id, Client_Id : Mcd_Fifos.Fifo_Id := Mcd_Fifos.No_Fifo;
  procedure Open_Fifo (Active : in Boolean);

  -- Stdin
  Stdio_Is_A_Tty : Boolean := False;
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
      -- Fifo
      if Debug.Debug_Level_Array(Debug.Parser) then
        Async_Stdin.Put_Line_Err ("Flow: init on fifo "
                            & Text_Handler.Value (Fifo_Name));
      end if;
      Open_Fifo (True);
      return;
    exception
      when Argument.Argument_Not_Found =>
        -- No fifo arg. Continue on Stdin
        null;
    end;

    -- Stdin
    Text_Handler.Empty (Fifo_Name);
    if Debug.Debug_Level_Array(Debug.Parser) then
      Async_Stdin.Put_Line_Err ("Flow: init on stdio");
    end if;
    -- Set stdin/out asynchronous if it is a Tty
    Stdio_Is_A_Tty :=
        Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin)  = Sys_Calls.Tty
      and then 
        Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty;
    if Stdio_Is_A_Tty then
      Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, 0);
      if Debug.Debug_Level_Array(Debug.Parser) then
        Async_Stdin.Put_Line_Err ("Flow: stdio is a tty");
      end if;
    end if;
    return;

  end Init;

  ----------------------------------------------------
  -- Get data from fifo or stdin
  ----------------------------------------------------
  procedure Next_Line (Str : out Ada.Strings.Unbounded.Unbounded_String) is
    Evt : Event_Mng.Out_Event_List;
    S : String (1 .. 1024);
    L : Natural;
    use type Event_Mng.Out_Event_List;
    use type Mcd_Fifos.Fifo_Id;
  begin
    Init;
    if Text_Handler.Empty (Fifo_Name) and then not Stdio_Is_A_Tty then
      Input_Data := Unb.To_Unbounded_String ("");
      -- Get next non empty line from Stdin (not a tty)
      loop
        begin
          -- Get a chunk of L'Len characters
          Ada.Text_Io.Get_Line (S, L);
          if L /= 0 then
            -- Append the read chunk to result
            Unb.Append (Input_Data, S(1 .. L));
          end if;
          -- Done when Get_Line returns less that L'Len => End of line
          -- but skip empty lines
          exit when L /= S'Last and then Unb.Length (Input_Data) /= 0;
        exception
          when Ada.Text_Io.End_Error =>
            -- Set Len to 0 and the end of Stdin
            Input_Data := Unb.To_Unbounded_String ("");
            exit;
        end;
      end loop;
    else
      -- Get next data on Tty stdin or Fifo
      loop
        Input_Data := Unb.To_Unbounded_String ("");
        if Debug.Debug_Level_Array(Debug.Parser) then
          Async_Stdin.Put_Line_Err ("Flow: Waiting on fifo/tty");
        end if;
        Evt := Event_Mng.Wait (Event_Mng.Infinite_Ms);

        if Evt = Event_Mng.Fd_Event
        and then Unb.To_String (Input_Data) /= "" then
          -- New string
          exit;
        elsif Evt = Event_Mng.Signal_Event then
          -- Give up on signal
          Input_Data := Unb.To_Unbounded_String ("");
          exit;
        end if;
      end loop;
    end if;
    Str := Input_Data;
    -- Allow input data to be overwritten
    if Client_Id /= Mcd_Fifos.No_Fifo then
      Mcd_Fifos.Activate (Client_Id, True);
    elsif Stdio_Is_A_Tty then
      Async_Stdin.Activate (True);
    end if;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Async_Stdin.Put_Line_Err ("Flow: Next_Line -> " & Unb.To_String (Str));
    end if;
  end Next_Line;

  ----------------------------------------------------
  -- Put data on fifo or stdout
  ----------------------------------------------------
  Put_Message : Message_Type;
  procedure Put (Str : in String) is
    Res : Fifos.Send_Result_List;
    Active : Boolean;
    use type Mcd_Fifos.Fifo_Id, Fifos.Send_Result_List;
  begin
    if Text_Handler.Empty (Fifo_Name) then
      -- Put on stdout (tty or not)
      Async_Stdin.Put_Out (Str);
    elsif Client_Id /= Mcd_Fifos.No_Fifo then
      -- Send on fifo
      Active := Mcd_Fifos.Is_Active (Client_Id);
      if Str'Length > Message_Type'Length then
        raise Mcd_Mng.String_Len;
      end if;
      Put_Message(1 .. Str'Length) := Str;
      Res := Mcd_Fifos.Send (Client_Id, Put_Message, Str'Length);
      if Res /= Fifos.Ok and then Res /= Fifos.Overflow then
        Mcd_Fifos.Close (Client_Id);
        Open_Fifo (Active);
      end if;
    end if;
  exception
    when Fifos.In_Overflow =>
      Mcd_Fifos.Close (Client_Id);
      Open_Fifo (Active);
  end Put;

  procedure New_Line is
  begin
    if Text_Handler.Empty (Fifo_Name) then
      -- Put on stdout (tty or not)
      Async_Stdin.New_Line_Out;
    else
      -- Send on fifo (Lf is the Unix standard)
      Put ("" & Ada.Characters.Latin_1.Lf);
    end if;
  end New_Line;

  procedure Put_Line (Str : in String) is
  begin
    Put (Str);
    New_Line;
  end Put_Line;

  Closing : Boolean := False;
  procedure Close is
    use type Mcd_Fifos.Fifo_Id;
  begin
    if Text_Handler.Empty (Fifo_Name) then
      if Stdio_Is_A_Tty then
        -- Reset tty blocking
        Async_Stdin.Set_Async;
      end if;
      return;
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Closing fifo");
    end if;
    Closing := True;
    if Client_Id /= Mcd_Fifos.No_Fifo then
      Mcd_Fifos.Close (Client_Id);
    end if;
    if Acc_Id /= Mcd_Fifos.No_Fifo then
      Mcd_Fifos.Close (Acc_Id);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Closed fifo");
    end if;
  end Close;

  ----------------------------------------------------
  -- Fifo callbacks
  ----------------------------------------------------
  procedure Conn_Cb (Fifo_Name : in String;
                     Id        : in Mcd_Fifos.Fifo_Id;
                     Connected : in Boolean) is
    Active : Boolean;
  begin
    if Connected then 
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Client accepted");
      end if;
      -- Accept one client and stop accepting others
      Mcd_Fifos.Close (Acc_Id);
      Client_Id := Id;
    else
      -- Client disconnected, allow new client (except if we are closing)
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Client has disconnected");
      end if;
      Active := Mcd_Fifos.Is_Active (Client_Id);
      Client_Id := Mcd_Fifos.No_Fifo;
      if not Closing then
        Open_Fifo (Active);
      end if;
    end if;
  end Conn_Cb;

  procedure Rece_Cb (Id      : in Mcd_Fifos.Fifo_Id;
                     Message : in Message_Type;
                     Length  : in Fifos.Message_Length) is
  begin
    if Length = 1
    and then (Message(1) = Ada.Characters.Latin_1.Cr
              or else Message(1) = Ada.Characters.Latin_1.Lf) then
      return;
    end if;
    Input_Data := Unb.To_Unbounded_String (Message(1 .. Length));
    -- Prevent Input_Data to be overwritten by freezing fifo
    Mcd_Fifos.Activate (Client_Id, False);
  end Rece_Cb;

  procedure Open_Fifo (Active : in Boolean) is
  begin
    if Text_Handler.Empty (Fifo_Name) then
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Opening empty fifo discarded");
      end if;
      raise Fifo_Error;
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Opening fifo "
                          & Text_Handler.Value (Fifo_Name));
    end if;
    Acc_Id := Mcd_Fifos.Open (Text_Handler.Value (Fifo_Name),
                              False,
                              Conn_Cb'Access,
                              Rece_Cb'Access,
                              null);
    if not Active then
      Mcd_Fifos.Activate (Acc_Id, False);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Fifo open");
    end if;
  exception
    when Error:others =>
      if Debug.Debug_Level_Array(Debug.Flow) then
        Async_Stdin.Put_Line_Err ("Flow: Fifo open error "
                            & Ada.Exceptions.Exception_Name (Error));
      end if;
      raise Fifo_Error;
  end Open_Fifo;

  ----------------------------------------------------
  -- Stdin callback
  ----------------------------------------------------
  function Stdin_Cb (Str : in String) return Boolean is
    use type Mcd_Fifos.Fifo_Id;
  begin
    if Str = "" then
      -- Error or end
      Input_Data := Unb.To_Unbounded_String (Str);
      return True;
    elsif Str(Str'Last) = Ada.Characters.Latin_1.Cr
    or else  Str(Str'Last) = Ada.Characters.Latin_1.Lf then
      -- Skip Cr/lf
      Input_Data := Unb.To_Unbounded_String (Str(1 .. Natural'Pred(Str'Last)));
    else
      Input_Data := Unb.To_Unbounded_String (Str);
    end if;
    -- Prevent overwritting of Input_Data by freezing Stdin
    Async_Stdin.Activate (False);
    if Debug.Debug_Level_Array(Debug.Flow) then
      Async_Stdin.Put_Line_Err ("Flow: Stdin_Cb set >"
                           & Unb.To_String (Input_Data)
                           & "<");
    end if;
    return True;
  end Stdin_Cb;

end Io_Flow;

