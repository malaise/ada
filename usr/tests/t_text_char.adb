with Argument, Text_Char, Sys_Calls, Rnd, Queues, As.U;
procedure T_Text_Char is
  Qsize : constant := 5;
  package Q is new Queues.Circ (Character);
  Circ : Q.Circ_Type(Qsize);

  File : Text_Char.File_Type;
  C, U : Character;
  Str : As.U.Asu_Us;
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
    -- Read line every 10 read
    if Rnd.Gen.Int_Random (0, 10) = 0 then
      Str := File.Get;
      Sys_Calls.Put_Output (Str.Image);
      for I in 1 .. Str.Length loop
        Circ.Push (Str.Element(I));
      end loop;
    else
      C := File.Get;
      Sys_Calls.Put_Output (C);
      Circ.Push (C);
    end if;
    exit when File.End_Of_File;

    -- Check unget average each 4 chars
    if Rnd.Gen.Int_Random (0, 4) = 0 then
      -- Undo 1 to Q size characters
      Nundo := Rnd.Gen.Int_Random (1, Qsize);
      if Nundo > Q.Length (Circ) then
        -- Don't undo more characters than read
        Nundo := Q.Length (Circ);
      end if;

      -- Undo some chars or a string
      if Rnd.Gen.Int_Random (0, 1) = 0 then
        -- N chars
        for I in 1 .. Nundo loop
          Circ.Look_Last (U, I);
          File.Unget (U);
        end loop;
      else
        -- A string of length N
        Str.Set_Null;
        for I in 1 .. Nundo loop
          Str.Append (Circ.Look_Last (I));
        end loop;
        File.Unget (Str.Image);
      end if;

      -- Re-read the chars and check
      for I in reverse 1 .. Nundo loop
        File.Get (C);
        Circ.Look_Last (U, I);
        if C /= U then
          Sys_Calls.New_Line_Error;
          Sys_Calls.Put_Line_Error ("Got after unget " & C
                                  & " should be " & U);
          raise Failed;
        end if;
      end loop;

    end if;
  end loop;

  -- Done: close
  File.Close_All;

end T_Text_Char;

