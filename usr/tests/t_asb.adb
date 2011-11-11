with As.B, Integer_Image, Basic_Proc;
procedure T_Asb is

  N1, N2 : As.B.Asb_Bs(128);
  use type As.B.Asb_Bs;

begin

  Basic_Proc.Put_Line_Output ("Empty array:");
  N1.Set_Null;
  if N1 /= N2
  or else not N1.Is_Null or else not N2.Is_Null
  or else N1 /= As.B.Asb_Null or else N2 /= As.B.Asb_Null(21) then
    Basic_Proc.Put_Output ("Init is not empty");
    return;
  end if;
  Basic_Proc.Put_Line_Output ("Length " & Integer_Image(N1.Length));
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Array of 3:");
  N1.Set ('3');
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Array of 1, 3, 5:");
  N1.Set ("135");
  Basic_Proc.Put_Line_Output ("Length " & Integer_Image(N1.Length));
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Element 2: " & N1.Element (2));
  N1.Replace_Element (2, 'u');
  Basic_Proc.Put_Line_Output ("Replaced by u: " & N1.Element (2));
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Append a, b, c, d, e");
  N2.Set ("ab");
  N1.Append (N2);
  N1.Append ( "cd");
  N1.Append ('e');
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Same with concat");
  N1.Set ("1u5");
  N1.Append (N2);
  N1.Append ("cd");
  N1.Append ("e");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Same with reverse concat");
  N2.Set (As.B.Tbs ("de"));
  N1.Set ("1u5abc" & N2);
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Same with prepend");
  N1.Set ("abcde");
  N1.Prepend ("u5");
  N1.Prepend ("1");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Slice 4 .. 6");
  Basic_Proc.Put_Line_Output ("Slice string " & N1.Slice (4, 6));
  N1.Bslice (N2, 4, 6);
  Basic_Proc.Put_Line_Output ("Uslice Image " & N2.Image);
  N1.Set ("abc");
  if N1 = N2 and then N1 = "abc"  and then "abc" = N1 then
    Basic_Proc.Put_Line_Output ("Check ""="" OK");
  else
    Basic_Proc.Put_Line_Output ("Check ""="" FAILED");
    return;
  end if;
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Replace from 4 to 7 with B, C");
  N1.Set ("1u5abcde");
  N1.Replace (4, 7, "BC");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Replace from B with 45");
  N1.Overwrite (4, "45");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Delete from 4 to 5, insert a, b, c, d before 4");
  N1.Delete (4, 5);
  N1.Insert (4, "abcd");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Replace from 4 to 5 with B, C, D");
  N1.Replace (4, 5, "BCD");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.Put_Line_Output ("Delete from 4 to 6, insert A, B before 4");
  N1.Delete (4, 6);
  N1.Insert (4, "AB");
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Check Finalization");
  declare
    N3 : As.B.Asb_Bs(1);
  begin
    N3 := As.B.Tbs ("B");
    declare
      N4 : constant As.B.Asb_Bs := As.B.Tbs ('C');
      N5 : constant As.B.Asb_Bs := N3 & N4;
    begin
     Basic_Proc.Put_Line_Output ("Array of B, C");
     Basic_Proc.Put_Line_Output ("Image " & N5.Image);
    end;
  end;

  Basic_Proc.Put_Line_Output ("Done.");
end T_Asb;

