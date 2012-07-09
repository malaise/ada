with Int_Io, Basic_Proc, Utf_8, Rnd, Key_Pressed;
procedure T_Utf8 is

  procedure Put (N : Natural) is
    Str : String (1 .. 10) := (others => ' ');
  begin
    Int_Io.Put (Str, N, Base => 16);
    for I in reverse Str'Range loop
      if Str(I) /= ' ' then
        Basic_Proc.Put_Output (Str(1 .. I));
        return;
      end if;
    end loop;
  end Put;

  Str : constant String := "aàâeéèêëiîïoôuùü";
  Ucodes : constant array (Positive range <>) of Utf_8.Unicode_Number
         := (16#61#, 16#E0#, 16#E2#,
             16#65#, 16#E9#, 16#E8#, 16#EA#, 16#EB#,
             16#69#, 16#EE#, 16#EF#,
             16#6F#, 16#F4#,
             16#75#, 16#F9#, 16#FC#);

  U1, U2 : Utf_8.Unicode_Number;
begin
  Basic_Proc.Put_Line_Output (Str);
  for I in Ucodes'Range loop
    declare
      Ustr : constant String := Utf_8.Encode(Ucodes(I));
    begin
      Basic_Proc.Put_Output (Ustr);
      for J in Ustr'Range loop
        Put (Character'Pos (Ustr(J)));
      end loop;
    end;
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.New_Line_Output;

  Rnd.Randomize;
  Key_Pressed.Open (False);
  loop
    U1 := Rnd.Int_Random (Utf_8.Unicode_Number'First,
                          Utf_8.Unicode_Number'Last);
    Put (U1);
    Basic_Proc.Put_Output (" -> ");
    declare
      Str : constant Utf_8.Sequence := Utf_8.Encode (U1);
    begin
      for I in Str'Range loop
        Put (Character'Pos (Str(I)));
      end loop;
      Basic_Proc.Put_Output (" -> ");

      U2 := Utf_8.Decode (Str);
      Put (U2);
      if U1 /= U2 then
        Basic_Proc.Put_Line_Output (" Bug");
        Basic_Proc.Set_Error_Exit_Code;
        exit;
      else
        Basic_Proc.Put_Line_Output (" OK");
      end if;
    end;
    exit when Key_Pressed.Key_Pressed;
  end loop;

  Basic_Proc.Flush_Output;
  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
    raise;
end T_Utf8;

