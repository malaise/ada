with Basic_Proc, Sys_Calls, Text_Char, Argument, Input_Buffer;
procedure T_Input_Buffer is

  -- Last chat shall be line feed
  procedure Notifier (Str : in String) is
  begin
    Basic_Proc.Put_Line_Output (Str(Str'First .. Str'Last - 1));
  end Notifier;

  -- Flow of chars
  Fd : Sys_Calls.File_Desc;
  File : Text_Char.File_Type;
  Char : Character;

  -- Slice to be sent to Buffer
  Buf_Size : constant := 21;
  subtype Buf_Range is Natural range 0 .. Buf_Size;
  Buf_Len : Buf_Range;
  Buf : String (1 .. Buf_Range'Last);

  -- Input buffer
  Buffer : Input_Buffer.Buffer;

  use type Sys_Calls.File_Desc;
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
    Fd := Sys_Calls.Stdin;
  else
    Fd := Sys_Calls.Open (Argument.Get_Parameter (1), Sys_Calls.In_File);
  end if;
  File.Open (Fd);

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
      Basic_Proc.Put_Line_Error ("Warning: Tail is >" & Tail & "<");
    end if;
    Basic_Proc.Put_Output (Tail);
  end;

  -- Close
  File.Close;
  if Fd /= Sys_Calls.Stdin then
    Sys_Calls.Close (Fd);
  end if;

  -- Test destructor
  declare
    B1, B2 : Input_Buffer.Buffer;
    pragma Unreferenced (B2);
  begin
    B1.Set (Notifier'Unrestricted_Access);
  end;

end T_Input_Buffer;

