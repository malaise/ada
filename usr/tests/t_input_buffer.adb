with Basic_Proc, Text_Char, Argument, Input_Buffer;
procedure T_Input_Buffer is

  -- Last chat shall be line feed
  procedure Notifier (Str : in String) is
  begin
    Basic_Proc.Put_Line_Output (Str(Str'First .. Str'Last - 1));
  end Notifier;

  -- Report an error
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Flow of chars
  File : Text_Char.File_Type;
  Char : Character;

  -- Slice to be sent to Buffer
  Buf_Size : constant := 21;
  subtype Buf_Range is Natural range 0 .. Buf_Size;
  Buf_Len : Buf_Range;
  Buf : String (1 .. Buf_Range'Last);

  -- Input buffer
  Buffer : Input_Buffer.Buffer;

begin

  -- Parse argument
  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " <file_name>       use '-' for stdin.");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Open flow
  if Argument.Get_Parameter (1) = "-" then
    File.Open_All ("");
  else
    File.Open_All (Argument.Get_Parameter (1));
  end if;

  -- Init buffer, delimiter is line feed
  Buffer.Set (Notifier'Unrestricted_Access);

  -- "infinite" Loop
  Buf_Len := 0;
  Buf := (others => ' ');
  loop
    if Buf_Len = Buf'Last then
      -- Flush a slice
      Buffer.Push (Buf);
      Buf_Len := 0;
    end if;
    begin
      Char := File.Get;
      Buf_Len := Buf_Len + 1;
      Buf(Buf_Len) := Char;
    exception
      when Text_Char.End_Error =>
        -- End of flow: Flush, done
        Buffer.Push (Buf (Buf'First .. Buf_Len));
        exit;
    end;
  end loop;

  -- Add tail if any
  declare
    Tail : constant String := Buffer.Tail;
  begin
    if Tail /= "" then
      Basic_Proc.Put_Line_Error ("Warning: Tail is >" & Buffer.Tail & "<");
    end if;
    Basic_Proc.Put_Output (Tail);
  end;

  -- Close
  File.Close_All;

  -- Test Clean versus Tail
  Buffer.Clean;
  if Buffer.Tail /= "" then
    Error ("Tail should be empty after clean. >" & Buffer.Tail &"<");
    return;
  end if;
  Buffer.Push ("foo");
  if Buffer.Tail = "" then
    Error ("Tail should not be empty after Push. >" & Buffer.Tail &"<");
    return;
  end if;
  Buffer.Clean;
  if Buffer.Tail /= "" then
    Error ("Tail should be empty after push then clean. >" & Buffer.Tail &"<");
    return;
  end if;

  -- Test destructor
  declare
    B1, Unused : Input_Buffer.Buffer;
  begin
    B1.Set (Notifier'Unrestricted_Access);
  end;

  -- Test reset;
  Buffer.Reset;
  if Buffer.Is_Set then
    Error ("Buffer should not be set after Reset");
    return;
  end if;

end T_Input_Buffer;

