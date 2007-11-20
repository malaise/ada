with Ada.Text_Io;
with Normal;
with String_Mng.Navigator;
procedure T_Strnav is
  Str : constant String := "ABCDEFG1234567890HIJKLMNOP";
  Nav : String_Mng.Navigator.Navigator_Type;
begin
  Ada.Text_Io.Put_Line ("Init navig to ""1234567890"", starting at 8");
  Nav.Set (Str (8 .. 17));
  Ada.Text_Io.Put_Line ("Move by 4 forward (move at 12, containing 5)");
  Nav.Move (4);
  Ada.Text_Io.Put_Line ("Set No_Char to '-'");
  Nav.Set_No_Char ('-');

  for I in -10 .. 10 loop
    Ada.Text_Io.Put_Line ("Lookup " & Normal (I, 3) & " -> " & Nav.Lookup (I));
  end loop;

  -- Rewind to last
  Ada.Text_Io.Put_Line ("Rewind to last");
  Nav.Rewind (False);
  for I in -10 .. 10 loop
    Ada.Text_Io.Put_Line ("Lookup " & Normal (I, 3) & " -> " & Nav.Lookup (I));
  end loop;

  -- Move 2 forward
  Ada.Text_Io.Put_Line ("Move 2 forward");
  Nav.Move (2);
  for I in -10 .. 10 loop
    Ada.Text_Io.Put_Line ("Lookup " & Normal (I, 3) & " -> " & Nav.Lookup (I));
  end loop;

end T_Strnav;

