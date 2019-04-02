with Ada.Wide_Text_Io;
with Images, Basic_Proc, Utf_16, Rnd, Key_Pressed, Argument;
procedure T_Utf16 is
  package Image16 is new Images.Int_Image16 (Natural);

  Str : constant String := "aàâeéèêëiîïoôuùü"; --## rule line off Char
  Ucodes : constant array (Positive range <>) of Utf_16.Unicode_Number
         := (16#61#, 16#E0#, 16#E2#,
             16#65#, 16#E9#, 16#E8#, 16#EA#, 16#EB#,
             16#69#, 16#EE#, 16#EF#,
             16#6F#, 16#F4#,
             16#75#, 16#F9#, 16#FC#);

  U1, U2 : Utf_16.Unicode_Number;

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
  for I of Ucodes loop
    declare
      Ustr : constant Wide_String := Utf_16.Encode(I);
    begin
      Ada.Wide_Text_Io.Put (Ustr);
      for J of Ustr loop
        Basic_Proc.Put_Output (" " & Image16.Image (Wide_Character'Pos (J)));
      end loop;
    end;
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.New_Line_Output;

  Rnd.Gen.Randomize;
  Key_Pressed.Open (False);
  Id_Loop := 0;
  loop
    begin
      U1 := Rnd.Gen.Int_Random (Utf_16.Unicode_Number'First,
                                Utf_16.Unicode_Number'Last);
      declare
        Str : constant Utf_16.Word := Utf_16.Encode (U1);
      begin
        Basic_Proc.Put_Output (Image16.Image (U1));
        Basic_Proc.Put_Output (" ->");
        for C of Str loop
          Basic_Proc.Put_Output (" " & Image16.Image (Wide_Character'Pos (C)));
        end loop;
        Basic_Proc.Put_Output (" -> ");

        U2 := Utf_16.Decode (Str);
        Basic_Proc.Put_Output (Image16.Image (U2));
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
    exception
      when Utf_16.Excluded_Non_Character =>
        null;
    end;
  end loop;

  Basic_Proc.Flush_Output;
  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
    raise;
end T_Utf16;

