with Ada.Wide_Text_Io;
with Basic_Proc, My_Io, Utf_16, Rnd;
procedure T_Utf16 is

  procedure Put (N : Natural) is
  begin
    My_Io.Put (N, Base => 16);
  end Put;

  Str : constant String := "aàâeéèêëiîïoôuùü";
  Ucodes : constant array (Positive range <>) of Utf_16.Unicode_Number
         := (16#61#, 16#E0#, 16#E2#,
             16#65#, 16#E9#, 16#E8#, 16#EA#, 16#EB#,
             16#69#, 16#EE#, 16#EF#,
             16#6F#, 16#F4#,
             16#75#, 16#F9#, 16#FC#);

  U1, U2 : Utf_16.Unicode_Number;
begin
  Basic_Proc.Put_Line_Output (Str);
  for I in Ucodes'Range loop
    declare
      Ustr : constant Wide_String := Utf_16.Encode(Ucodes(I));
    begin
      Ada.Wide_Text_Io.Put (Ustr);
      for J in Ustr'Range loop
        Put (Wide_Character'Pos (Ustr(J)));
      end loop;
    end;
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.New_Line_Output;

  Rnd.Randomize;
  loop
    begin
      U1 := Rnd.Int_Random (Utf_16.Unicode_Number'First,
                            Utf_16.Unicode_Number'Last);
      declare
        Str : constant Utf_16.Sequence := Utf_16.Encode (U1);
      begin
        Put (U1);
        Basic_Proc.Put_Output (" -> ");
        for I in Str'Range loop
          Put (Wide_Character'Pos (Str(I)));
        end loop;
        Basic_Proc.Put_Output (" -> ");

        U2 := Utf_16.Decode (Str);
        Put (U2);
        if U1 /= U2 then
          Basic_Proc.Put_Line_Output (" Bug");
          exit;
        else
          Basic_Proc.Put_Line_Output (" OK");
        end if;
      end;
    exception
      when Utf_16.Excluded_Non_Character =>
        null;
    end;
  end loop;

end T_Utf16;

