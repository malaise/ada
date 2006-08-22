with Ada.Text_Io;
with Argument, Text_Char, Sys_Calls, Rnd, Queues;
procedure T_Text_Char is
  package Q is new Queues.Circ (4, Character);

  Fd : Sys_Calls.File_Desc;
  File : Text_Char.File_Type;
  C, U : Character;
  Nundo : Natural;
  Failed : exception;

begin
  -- One arg
  if Argument.Get_Nbre_Arg /= 1 then
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <file_name>");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;

  -- Open file for Text_Line
  begin
    Fd := Sys_Calls.Open (Argument.Get_Parameter, Sys_Calls.In_File);
  exception
    when Sys_Calls.Name_Error =>
      Sys_Calls.Put_Line_Error ("Error: Cannot open file " &
          Argument.Get_Parameter);
      Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & "<file_name>");
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  -- This should work ok
  Text_Char.Open (File, Fd);

  -- Loop of read
  Rnd.Randomize;
  loop
    C := Text_Char.Get (File);
    Ada.Text_Io.Put (C);
    Q.Push (C);
    exit when Text_Char.End_Of_File (File);
    -- Check unget average each 4 chars
    if Rnd.Int_Random (0, 3) = 0 then
      -- Undo 1 to 4 characters
      Nundo := Rnd.Int_Random (1, 4);
      if Nundo > Q.Length then
        -- Don't undo more characters than read
        Nundo := Q.Length;
      end if;
      -- Undo some chars
      for I in 1 .. Nundo loop
        Q.Look_Last (U, I);
        Text_Char.Unget (File, U);
      end loop;
      -- Re-read the chars and check
      for I in reverse 1 .. Nundo loop
        Text_Char.Get (File, C);
        Q.Look_Last (U, I);
        if C /= U then
          Sys_Calls.New_Line_Error;
          Sys_Calls.Put_Line_Error ("Got after unget " & C & " should be " & U);
          raise Failed;
        end if;
      end loop;
    end if;
  end loop;

  -- Done: close
  Text_Char.Close (File);
  Sys_Calls.Close (Fd);

end T_Text_Char;

