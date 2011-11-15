with As.B, Argument, My_Io;
with Grid_1;
procedure Tg1 is
  Key : constant String := Argument.Get_Parameter(1);
  Text : As.B.Asb_Bs(500);
  Dummy : As.B.Asb_Bs(80);
  Code : As.B.Asb_Bs(Text.Max * 2);
  Rec : Grid_1.Coordinate_Rec;
begin

  Text.Set_Null;
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


  My_Io.Put_Line ("Text : " & Text.Image);
  Code.Set_Null;
  for I in 1 .. Text.Length loop
    Rec := Grid_1.Encode (Text.Element (I));
    Code.Append (Rec.Row);
    Code.Append (Rec.Col);
  end loop;
  My_Io.Put_Line ("Code : " & Code.Image);

  Text.Set_Null;
  for I in 1 .. Code.Length loop
    if I mod 2 = 1 then
      Rec.Row := Code.Element (I);
    else
      Rec.Col := Code.Element (I);
      Text.Append (Grid_1.Decode(Rec));
    end if;
  end loop;
  My_Io.Put_Line ("Text : " & Text.Image);

end Tg1;

