with Text_Handler, Argument, My_Io;

with Grid_1;
procedure Tg1 is
  Key : constant String := Argument.Get_Parameter(1);
  Text : Text_Handler.Text(500);
  Dummy : Text_Handler.Text(80);
  Code : Text_Handler.Text(Text.Max_Len*2);
  Rec : Grid_1.Coordinate_Rec;
begin

  Text.Empty;
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Dummy.Set (Argument.Get_Parameter(Occurence => I));
    Text.Append (Dummy);
    if I /= Argument.Get_Nbre_Arg then
      Text.Append (" ");
    end if;
  end loop;

  Grid_1.Initialize(Key);
  My_Io.Put_Line ("Key : " & Key);
  Grid_1.Dump;
  My_Io.New_Line;


  My_Io.Put_Line ("Text : " & Text.Value);
  Code.Empty;
  for I in 1 .. Text.Length loop
    Rec := Grid_1.Encode (Text.Value(I));
    Code.Append (Rec.Row);
    Code.Append (Rec.Col);
  end loop;
  My_Io.Put_Line ("Code : " & Code.Value);

  Text.Empty;
  for I in 1 .. Code.Length loop
    if I mod 2 = 1 then
      Rec.Row := Code.Value(I);
    else
      Rec.Col := Code.Value(I);
      Text.Append (Grid_1.Decode(Rec));
    end if;
  end loop;
  My_Io.Put_Line ("Text : " & Text.Value);

end Tg1;

