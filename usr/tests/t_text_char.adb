with Argument, Text_Char, Sys_Calls, Rnd, Queues;
procedure T_Text_Char is
  package Q is new Queues.Circ (4, Character);
  Circ : Q.Circ_Type;

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
    File.Open_All (Argument.Get_Parameter);
  exception
    when Text_Char.Name_Error =>
      Sys_Calls.Put_Line_Error ("Error: Cannot open file " &
          Argument.Get_Parameter);
      Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & "<file_name>");
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;

  -- Loop of read
  Rnd.Gen.Randomize;
  loop
    C := Text_Char.Get (File);
    Sys_Calls.Put_Output (C);
    Q.Push (Circ, C);
    exit when Text_Char.End_Of_File (File);
    -- Check unget average each 4 chars
    if Rnd.Gen.Int_Random (0, 3) = 0 then
      -- Undo 1 to 4 characters
      Nundo := Rnd.Gen.Int_Random (1, 4);
      if Nundo > Q.Length (Circ) then
        -- Don't undo more characters than read
        Nundo := Q.Length (Circ);
      end if;
      -- Undo some chars
      for I in 1 .. Nundo loop
        Q.Look_Last (Circ, U, I);
        Text_Char.Unget (File, U);
      end loop;
      -- Re-read the chars and check
      for I in reverse 1 .. Nundo loop
        Text_Char.Get (File, C);
        Q.Look_Last (Circ, U, I);
        if C /= U then
          Sys_Calls.New_Line_Error;
          Sys_Calls.Put_Line_Error ("Got after unget " & C & " should be " & U);
          raise Failed;
        end if;
      end loop;
    end if;
  end loop;

  -- Done: close
  File.Close_All;

end T_Text_Char;

