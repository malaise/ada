with Argument, Parser, As.U.Utils, Basic_Proc, Split_Lines;
procedure T_Split_Lines is

  Len : Positive;
  Text : As.U.Asu_Us;
  Iter : Parser.Iterator;
  Res : As.U.Utils.Asu_Ua.Unb_Array;

begin
  -- Parse len
  begin
    Len := Positive'Value (Argument.Get_Parameter (1));
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Syntax error. Usage: "
          & Argument.Get_Program_Name & " <len> [ { <text> } ]");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  -- Parse text
  for I in 2 .. Argument.Get_Nbre_Arg loop
    if I /= 2 then
      Text.Append (" ");
    end if;
    Text.Append (Argument.Get_Parameter (Occurence => I));
  end loop;

  Basic_Proc.Put_Line_Output ("Splitting >" & Text.Image & "<");
  Iter.Set (Text.Image);
  Res := Split_Lines (Iter, Len);

  Basic_Proc.Put_Line_Output (" 0        1         2         3         4         5         6         7        ");
  Basic_Proc.Put_Line_Output (" 123456789012345678901234567890123456789012345678901234567890123456789012345678");
  for I in 1 .. Res.Length loop
    Basic_Proc.Put_Line_Output (">" & Res.Element(I).Image & "<");
  end loop;

end T_Split_Lines;

