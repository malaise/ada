with As.B, Integer_Image, Basic_Proc;
procedure T_Asb is

  N1, N2 : As.B.Asb_Bs(128);
  Str : String(1 .. 500) := (others => '#');
  use type As.B.Asb_Bs;

begin
  Str(200 .. 209) := "0123456789";
  Str(301 .. 326) := "abcdefghijklmnopqrstuvwxyz";
  Str(401 .. 426) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

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

  N1.Set ("1u5abcdE");
  Basic_Proc.Put_Line_Output ("Replace from 4 to 7 with B, C");
  N1.Replace (4, 7, Str(402 .. 403)); --> 1u5BCE
  Basic_Proc.Put_Line_Output ("Replace from 1 to 3 with a, b, c, d");
  N1.Replace (1, 3, Str(301 .. 304)); --> abcdBCE
  Basic_Proc.Put_Line_Output ("Replace from 5 to 7 with e");
  N1.Replace (5, 7, Str(305 .. 305)); --> abcde
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);

  Basic_Proc.Put_Line_Output ("Overwrite from 4 with 4, 5");
  N1.Overwrite (4, Str(204 .. 205)); --> abc45e
  Basic_Proc.Put_Line_Output ("Overwrite from 6 with 6, 7");
  N1.Overwrite (6, Str(206 .. 207)); --> abc4567
  Basic_Proc.Put_Line_Output ("Overwrite from 1 with 1, 2, 3");
  N1.Overwrite (1, Str(201 .. 203)); --> 1234567
  Basic_Proc.Put_Line_Output ("Overwrite from 8 with 8");
  N1.Overwrite (8, Str(208 .. 208)); --> 12345678
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);

  Basic_Proc.Put_Line_Output ("Replace from 1 to 0 with 0");
  N1.Replace (1, 0, Str(200 .. 200)); --> 012345678
  Basic_Proc.Put_Line_Output ("Replace from 3 to 1 with a, b");
  N1.Replace (3, 1, Str(301 .. 302)); --> 01ab2345678
  Basic_Proc.Put_Line_Output ("Replace from 12 to 11 with y, z");
  N1.Replace (12, 11, Str(325 .. 326)); --> 01ab2345678yz
  Basic_Proc.Put_Line_Output ("Image " & N1.Image);

  Basic_Proc.Put_Line_Output ("Delete from 3 to 4, insert a, b, c, d before 4");
  N1.Delete (3, 4); --> 012345678yz
  N1.Insert (4, Str(301 .. 304)); --> 012abcd345678yz
  Basic_Proc.Put_Line_Output ("Replace from 4 to 5 with B, C, D");
  N1.Replace (4, 5, Str(402 .. 404)); --> 012BCDcd345678yz
  Basic_Proc.Put_Line_Output ("Delete from 4 to 8");
  N1.Delete (4, 8); --> 012345678yz
  Basic_Proc.Put_Line_Output ("Trail 2");
  N1.Trail (2); --> 012345678
  Basic_Proc.Put_Line_Output ("Insert 9 before 10");
  N1.Insert (10, Str(209 .. 209)); --> 0123456789
  Basic_Proc.Put_Line_Output ("Replace from 11 to 10 with A, B, C");
  N1.Replace (11, 10, Str(401 .. 403)); --> 0123456789ABC
  Basic_Proc.Put_Line_Output ("Overwrite from 14 with D, E, F");
  N1.Overwrite (14, Str(404 .. 406)); --> 0123456789ABCDEF
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

