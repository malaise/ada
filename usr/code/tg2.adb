with Ada.Text_Io; -- For exception trace
with My_Io;
with Grid_2;

procedure Tg2 is
  Key : constant String := "C'est la cle 2";
  Str1 : constant Grid_2.Long_String := "Ah que coucou!";
  Str2 : Grid_2.Long_String (1 .. Str1'Length);
  Str3 : Grid_2.Long_String (1 .. Str1'Length);
begin
  Str2 := Grid_2.Encode (Key, Str1);
  My_Io.Put (">");
  for I in Str2'Range loop
    My_Io.Put(Str2(I));
  end loop;
  My_Io.Put_Line("<");
  Str3 := Grid_2.Decode (Key, Str2);
  My_Io.Put (">");
  for I in Str3'Range loop
    My_Io.Put(Str3(I));
  end loop;
  My_Io.Put_Line("<");
end Tg2;

