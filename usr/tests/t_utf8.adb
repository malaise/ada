with Images, Basic_Proc, Utf_8, Rnd, Key_Pressed, Argument;
procedure T_Utf8 is
  function Image16 is new Images.Int_Image16 (Natural);

  Str : constant String := "aàâeéèêëiîïoôuùü";
  Ucodes : constant array (Positive range <>) of Utf_8.Unicode_Number
         := (16#61#, 16#E0#, 16#E2#,
             16#65#, 16#E9#, 16#E8#, 16#EA#, 16#EB#,
             16#69#, 16#EE#, 16#EF#,
             16#6F#, 16#F4#,
             16#75#, 16#F9#, 16#FC#);

  U1, U2 : Utf_8.Unicode_Number;

  Nb_Loops, Id_Loop : Natural;
begin
  Nb_Loops := 0;
  if Argument.Get_Nbre_Arg = 1 then
    begin
      Nb_Loops := Natural'Value (Argument.Get_Parameter(1));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " [ <nb_loops> ]");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  end if;

  Basic_Proc.Put_Line_Output (Str);
  for I in Ucodes'Range loop
    declare
      Ustr : constant String := Utf_8.Encode(Ucodes(I));
    begin
      Basic_Proc.Put_Output (Ustr);
      for J in Ustr'Range loop
        Basic_Proc.Put_Output (" " & Image16 (Character'Pos (Ustr(J))));
      end loop;
    end;
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.New_Line_Output;

  Rnd.Randomize;
  Key_Pressed.Open (False);
  Id_Loop := 0;
  loop
    U1 := Rnd.Int_Random (Utf_8.Unicode_Number'First,
                          Utf_8.Unicode_Number'Last);
    Basic_Proc.Put_Output (Image16 (U1));
    Basic_Proc.Put_Output (" ->");
    declare
      Str : constant Utf_8.Sequence := Utf_8.Encode (U1);
    begin
      for I in Str'Range loop
        Basic_Proc.Put_Output (" " & Image16 (Character'Pos (Str(I))));
      end loop;
      Basic_Proc.Put_Output (" -> ");

      U2 := Utf_8.Decode (Str);
      Basic_Proc.Put_Output (Image16 (U2));
      if U1 /= U2 then
        Basic_Proc.Put_Line_Output (" Bug");
        Basic_Proc.Set_Error_Exit_Code;
        exit;
      else
        Basic_Proc.Put_Line_Output (" OK");
      end if;
    end;
    exit when Key_Pressed.Key_Pressed;
    Id_Loop := Id_Loop + 1;
    exit when Id_Loop = Nb_Loops;
  end loop;

  Basic_Proc.Flush_Output;
  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
    raise;
end T_Utf8;

