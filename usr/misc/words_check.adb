-- Check that file (arg 1) is made of lines
--  each with one word of lower case letters and a Line_Feed
with Sys_Calls, Argument, Text_Line, My_Math;
procedure Words_Check is

  -- Abnormal error exit code
  Error_Code : constant Natural := 2;

  -- In file fd and txt file
  In_Fd : Sys_Calls.File_Desc;
  In_Txt : Text_Line.File_Type;

  -- Current line number
  Line_No : My_Math.Inte;

  -- Allowed characters
  subtype Letter_Range is Character range 'a' .. 'z';

begin

  if Argument.Get_Nbre_Arg /= 1 then
    Sys_Calls.Put_Line_Error ("Error: Missing argument.");
    Sys_Calls.Set_Exit_Code (Error_Code);
    return;
  end if;

  -- Open file
  begin
    In_Fd := Sys_Calls.Open (Argument.Get_Parameter (Occurence => 1),
                             Sys_Calls.In_File);
  exception
    when Sys_Calls.Name_Error =>
      Sys_Calls.Put_Line_Error ("Error: Cannot open file "
                              & Argument.Get_Parameter (Occurence => 1) & ".");
      Sys_Calls.Set_Exit_Code (Error_Code);
      return;
  end;
  Text_Line.Open (In_Txt, Text_Line.In_File, In_Fd);

  -- Read and check lines
  Line_No := 1;
  loop
    declare
      Word : constant String := Text_Line.Get (In_Txt);
      Last : constant Natural := Word'Length;
    begin
      -- End of file?
      exit when Last = 0;
      if Last = 1 and then Word(1) = Text_Line.Line_Feed then
        -- Line must not be empty, report and next line
        Sys_Calls.Put_Line_Error ("Error: Empty line No" & Line_No'Img & ".");
        Sys_Calls.Set_Error_Exit_Code;
      elsif  Word(Last) /= Text_Line.Line_Feed then
        -- Line must be terminated by Line_Feed
        Sys_Calls.Put_Line_Error ("Error: Line No" & Line_No'Img
                                & " must be terminated by a Line_Feed.");
        Sys_Calls.Set_Error_Exit_Code;
      else
        for I in 1 .. Last - 1 loop
          if Word(I) not in Letter_Range then
            -- Invalid character, report and next word/line
            Sys_Calls.Put_Line_Error ("Error: In valid character in "
                    &  Word(1 .. Last - 1) & " at line No" & Line_No'Img & ".");
            Sys_Calls.Set_Error_Exit_Code;
            exit;
          end if;
        end loop;
      end if;
    end;
    -- Next line
    Line_No := Line_No + 1;
  end loop;

  -- Done. Close
  Text_Line.Close (In_Txt);
  Sys_Calls.Close (In_Fd);

end Words_Check;

