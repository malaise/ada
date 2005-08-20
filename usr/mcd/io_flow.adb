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
  procedure Open_Fifo;

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
    exception
      when Argument.Argument_Not_Found =>
        -- Stdin
        Text_Handler.Empty (Fifo_Name);
        if Debug.Debug_Level_Array(Debug.Parser) then
          Ada.Text_Io.Put_Line ("Flow: init on stdio");
        end if;
        Stdio_Is_A_Tty :=
            Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin)  = Sys_Calls.Tty
          and then 
            Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty;
        if Stdio_Is_A_Tty then
          Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, 0);
          if Debug.Debug_Level_Array(Debug.Parser) then
            Ada.Text_Io.Put_Line ("Flow: stdio is a tty");
          end if;
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
  procedure Next_Line (Str : out Ada.Strings.Unbounded.Unbounded_String) is
    Evt : Event_Mng.Out_Event_List;
    S : String (1 .. 1024);
    L : Natural;
    use type Event_Mng.Out_Event_List;
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
      -- Get next data on TTY stdin or Fifo
      loop
        Input_Data := Unb.To_Unbounded_String ("");
        if Debug.Debug_Level_Array(Debug.Parser) then
          Ada.Text_Io.Put_Line ("Flow: Waiting on fifo/tty");
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
    if Debug.Debug_Level_Array(Debug.Parser) then
      Ada.Text_Io.Put_Line ("Flow: Next_Line -> " & Unb.To_String (Str));
    end if;
  end Next_Line;

  ----------------------------------------------------
  -- Put data on fifo or stdout
  ----------------------------------------------------
  Put_Message : Message_Type;
  procedure Put (Str : in String) is
    Res : Fifos.Send_Result_List;
    use type Mcd_Fifos.Fifo_Id, Fifos.Send_Result_List;
  begin
    if Text_Handler.Empty (Fifo_Name) then
      Ada.Text_Io.Put (Str);
    elsif Client_Id /= Mcd_Fifos.No_Fifo then
      if Str'Length > Message_Type'Length then
        raise Mcd_Mng.String_Len;
      end if;
      Put_Message(1 .. Str'Length) := Str;
      Res := Mcd_Fifos.Send (Client_Id, Put_Message, Str'Length);
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
      if Stdio_Is_A_Tty then
        Async_Stdin.Set_Async;
      end if;
      return;
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
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
      if Debug.Debug_Level_Array(Debug.Flow) then
        Ada.Text_Io.Put_Line ("Flow: Client accepted");
      end if;
      -- Accept client and stop accepting
      Mcd_Fifos.Close (Acc_Id);
      Client_Id := Id;
    else
      -- Client disconnects, allow new client
      if Debug.Debug_Level_Array(Debug.Flow) then
        Ada.Text_Io.Put_Line ("Flow: Client has disconnected");
      end if;
      Client_Id := Mcd_Fifos.No_Fifo;
      if not Closing then
        Open_Fifo;
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
  end Rece_Cb;

  procedure Open_Fifo is
  begin
    if Text_Handler.Empty (Fifo_Name) then
      if Debug.Debug_Level_Array(Debug.Flow) then
        Ada.Text_Io.Put_Line ("Flow: Opening empty fifo discarded");
      end if;
      return;
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Ada.Text_Io.Put_Line ("Flow: Opening fifo "
                          & Text_Handler.Value (Fifo_Name));
    end if;
    Acc_Id := Mcd_Fifos.Open (Text_Handler.Value (Fifo_Name),
                              False,
                              Conn_Cb'Access,
                              Rece_Cb'Access,
                              null);
    if Debug.Debug_Level_Array(Debug.Flow) then
      Ada.Text_Io.Put_Line ("Flow: Fifo open");
    end if;
  exception
    when Error:others =>
      if Debug.Debug_Level_Array(Debug.Flow) then
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
      Input_Data := Unb.To_Unbounded_String (Str);
      return True;
    elsif Str(Str'Last) = Ada.Characters.Latin_1.Cr
    or else  Str(Str'Last) = Ada.Characters.Latin_1.lf then
      -- Skip Cr/lf
      Input_Data := Unb.To_Unbounded_String (Str(1 .. Natural'Pred(Str'Last)));
    else
      Input_Data := Unb.To_Unbounded_String (Str);
    end if;
    if Debug.Debug_Level_Array(Debug.Flow) then
      Ada.Text_Io.Put_Line ("Flow: Stdin_Cb set >"
                           & Unb.To_String (Input_Data)
                           & "<");
    end if;
    return True;
  end Stdin_Cb;

end Io_Flow;

