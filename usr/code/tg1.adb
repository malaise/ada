with Ada.Text_Io; -- For exception trace
with Text_Handler, Argument, My_Io;

with Grid_1;
procedure Tg1 is
  Key : constant String := Argument.Get_Parameter(1);
  Text : Text_Handler.Text(500);
  Dummy : Text_Handler.Text(80);
  Code : Text_Handler.Text(Text.Max_Len*2);
  Rec : Grid_1.Coordinate_Rec;
begin

  Text_Handler.Set(Text, "");
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Argument.Get_Parameter(Dummy, I);
    Text_Handler.Append (Text, Dummy);
    if I /= Argument.Get_Nbre_Arg then
      Text_Handler.Append (Text, " ");
    end if;
  end loop;

  Grid_1.Initialize(Key);
  My_Io.Put_Line ("Key : " & Key);
  Grid_1.Dump;
  My_Io.New_Line;


  My_Io.Put_Line ("Text : " & Text_Handler.Value(Text));
  Text_Handler.Set (Code, "");
  for I in 1 .. Text_Handler.Length(Text) loop
    Rec := Grid_1.Encode (Text_Handler.Value(Text)(I));
    Text_Handler.Append (Code, Rec.Row);
    Text_Handler.Append (Code, Rec.Col);
  end loop;
  My_Io.Put_Line ("Code : " & Text_Handler.Value(Code));

  Text_Handler.Set (TEXT, "");
  for I in 1 .. Text_Handler.Length(Code) loop
    if I mod 2 = 1 then
      Rec.Row := Text_Handler.Value(Code)(I);
    else
      Rec.Col := Text_Handler.Value(Code)(I);
      Text_Handler.Append (Text, Grid_1.Decode(Rec));
    end if;
  end loop;
  My_Io.Put_Line ("Text : " & Text_Handler.Value(Text));


end Tg1;

