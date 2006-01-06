with Ada.Text_Io, Ada.Exceptions;
with Argument, My_Math, Sys_Calls, Normal, Mixed_Str;
-- Analyse the frquency of each byte of stdin
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
  -- P in hexa: 16#xx#
  Str16 : String (1 .. 6);
  -- Percentage of appearance
  Percent : Natural;

  package Int_Io is new Ada.Text_Io.Integer_Io (Natural);
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
  Ada.Text_Io.Put_Line ("Read " & Nb_Read'Img & " characters:");
  Ada.Text_Io.Put_Line ("C Dec Hx   %  NbOcc");
  Ada.Text_Io.Put_Line ("-------------------");
  for I in Table'Range loop
    if Table(I) /= 0 or else Put_All then
      begin
        if I >= ' ' and then I <= '~' then
          Ada.Text_Io.Put (I);
        else
          Ada.Text_Io.Put (' ');
        end if;
        Pos := Character'Pos (I);
        Int_Io.Put (Str16, Pos, Base => 16);
        if Str16(4) = '#' then
          -- 16#x# i.o. 16#0x#
          Str16(4) := '0';
        end if;
        Ada.Text_Io.Put (' ' & Normal (Pos, 3, Gap => '0')
                       & ' ' & Str16(4..5));
        Percent := Natural (Table(I) * 100 / Nb_Read);
        if Percent < 100 then
          Ada.Text_Io.Put ("  " & Normal (Percent, 2, Gap => '0'));
        else
          Ada.Text_Io.Put (" 100");
        end if;
        Ada.Text_Io.Put (' ' & Table(I)'Img);
        Ada.Text_Io.New_Line;
      exception
        when Error:others =>
          Sys_Calls.Set_Error_Exit_Code;
          Ada.Text_Io.Put_Line ("Exception "
           & Mixed_Str (Ada.Exceptions.Exception_Name (Error)));
      end;
    end if;
  end loop;

end Anal_Freq;

