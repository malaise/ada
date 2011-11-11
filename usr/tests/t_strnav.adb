with Basic_Proc, Normal;
with String_Mng.Navigator;
procedure T_Strnav is
  Str : constant String := "ABCDEFG1234567890HIJKLMNOP";
  Nav : String_Mng.Navigator.Navigator_Type;

  procedure Dump (N : in String_Mng.Navigator.Navigator_Type) is
  begin
    for I in -10 .. 10 loop
      Basic_Proc.Put_Line_Output ("Lookup " & Normal (I, 3)
                                & " -> " & N.Lookup (I));
    end loop;
  end Dump;

begin
  Basic_Proc.Put_Line_Output (
      "Init navig to ""1234567890"", (starting at 8 from init string)");
  Nav.Set (Str (8 .. 17));
  Basic_Proc.Put_Line_Output ("Move by 4 forward (move at 12, containing 5)");
  Nav.Move (4);
  Basic_Proc.Put_Line_Output ("Set No_Char to '-'");
  Nav.Set_No_Char ('-');
  Dump (Nav);

  -- Rewind to last
  Basic_Proc.Put_Line_Output ("Rewind to last");
  Nav.Rewind (False);
  Dump (Nav);

  -- Move 2 forward
  Basic_Proc.Put_Line_Output ("Move 2 forward");
  Nav.Move (2);
  Dump (Nav);

  -- Put image
  Basic_Proc.Put_Line_Output ("Image:");
  Basic_Proc.Put_Line_Output (Nav.Image);

end T_Strnav;

