with Ada.Exceptions;
with Argument, My_Math, Sys_Calls, Normal, Mixed_Str, Upper_Str, Hexa_Utils;
-- Analyse the frequency of each byte of stdin
procedure Anal_Freq is
  -- Put all letters or only non-null
  Put_All : Boolean;
  -- Number of char read and anb of occurence of each
  Nb_Read : My_Math.Inte := 0;
  Table : array (Character) of My_Math.Inte := (others => 0);
  -- Char read and its pos
  Char : Character;
  Pos : Natural;
  -- Result of read
  Res : Natural;
  -- Percentage of appearance
  Percent : Natural;

begin

  if Argument.Get_Nbre_Arg = 0 then
    Put_All := False;
  elsif Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter (1) = "-a"
    or else Argument.Get_Parameter (1) = "--all") then
    Put_All := True;
  else
    Sys_Calls.Put_Line_Error ("Usage : " & Argument.Get_Program_Name
     & "[ -a | --all ]");
    Sys_Calls.Put_Line_Error (
     "Puts percentage of appearance of letters of stdin");
    Sys_Calls.Put_Line_Error ("Non null percentages or all letters.");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;


  -- Read chars
  begin
    loop
      Res := Sys_Calls.Read (Sys_Calls.Stdin, Char'Address, 1);
      exit when Res = 0;
      -- Update counts
      Nb_Read := Nb_Read + 1;
      Table(Char) := Table(Char) + 1;
    end loop;
  exception
    when Sys_Calls.System_Error =>
      -- End of read flow
      null;
  end;

  -- Display result
  Sys_Calls.Put_Line_Output ("Read " & Nb_Read'Img & " characters:");
  Sys_Calls.Put_Line_Output ("C Dec Hx   %  NbOcc");
  Sys_Calls.Put_Line_Output ("-------------------");
  for I in Table'Range loop
    if Table(I) /= 0 or else Put_All then
      begin
        if I >= ' ' and then I <= '~' then
          Sys_Calls.Put_Output (I);
        else
          Sys_Calls.Put_Output (' ');
        end if;
        Pos := Character'Pos (I);
        Sys_Calls.Put_Output (' ' & Normal (Pos, 3, Gap => '0') & ' ');
        Sys_Calls.Put_Output (Upper_Str (Hexa_Utils.Image (Pos, 2)));
        Percent := Natural (Table(I) * 100 / Nb_Read);
        if Percent < 100 then
          Sys_Calls.Put_Output ("  " & Normal (Percent, 2, Gap => '0'));
        else
          Sys_Calls.Put_Output (" 100");
        end if;
        Sys_Calls.Put_Output (' ' & Table(I)'Img);
        Sys_Calls.New_Line_Output;
      exception
        when Error:others =>
          Sys_Calls.Set_Error_Exit_Code;
          Sys_Calls.Put_Line_Output ("Exception "
           & Mixed_Str (Ada.Exceptions.Exception_Name (Error)));
      end;
    end if;
  end loop;

end Anal_Freq;

