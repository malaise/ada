with Ada.Text_Io;
with Utf_8, Rnd;
procedure T_Utf8 is

  Str : constant String := "aàâeéèêëiîïoôuùü";
  Ucodes : constant array (Positive range <>) of Utf_8.Unicode_Number
         := (16#0061#, 16#C3A0#, 16#C3A2#, 
             16#0065#, 16#C3A9#, 16#C3A8#, 16#C3AA#, 16#C3AB#,
             16#0069#, 16#C3AE#, 16#C3AF#,
             16#006F#, 16#C3B4#,
             16#0075#, 16#C3B9#, 16#C3BC#);

  U1, U2 : Utf_8.Unicode_Number;
begin
  Ada.Text_Io.Put_Line (Str);
  for I in Ucodes'Range loop
    Ada.Text_Io.Put (Utf_8.Encode(Ucodes(I)));
  end loop;
  Ada.Text_Io.New_Line;

  loop
    U1 := Rnd.Int_Random (Utf_8.Unicode_Number'First,
                          Utf_8.Unicode_Number'Last);
    declare
      Str : constant Utf_8.Sequence := Utf_8.Encode (U1);
    begin
      U2 := Utf_8.Decode (Str);
      if U1 /= U2 then
        Ada.Text_Io.Put_Line ("Bug:" & U1'Img & " /= " & U2'Img);
        exit;
      else
        Ada.Text_Io.Put_Line ("OK:" & U1'Img);
      end if;
    end;
  end loop;

end T_Utf8;

