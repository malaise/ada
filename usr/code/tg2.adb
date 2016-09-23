with Basic_Proc;
with Grid_2;

procedure Tg2 is
  Key : constant String := "C'est la cle 2";
  Str1 : constant Grid_2.Long_String := "Ah que coucou!";
  Str2 : Grid_2.Long_String (1 .. Str1'Length);
  Str3 : Grid_2.Long_String (1 .. Str1'Length);
begin
  Str2 := Grid_2.Encode (Key, Str1);
  Basic_Proc.Put_Output (">");
  for C of Str2 loop
    Basic_Proc.Put_Output (C);
  end loop;
  Basic_Proc.Put_Line_Output ("<");
  Str3 := Grid_2.Decode (Key, Str2);
  Basic_Proc.Put_Output (">");
  for C of Str3 loop
    Basic_Proc.Put_Output (C);
  end loop;
  Basic_Proc.Put_Line_Output ("<");
end Tg2;

