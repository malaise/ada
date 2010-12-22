with Ada.Text_Io;
with As.U; use As.U;
with Int_Image;
procedure T_Asu is

  function Image is new Int_Image (Natural);

  N1, N2 : Asu_Us;

begin

  Ada.Text_Io.Put_Line ("Empty array:");
  Ada.Text_Io.Put_Line ("Length " & Image(N1.Length));
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Array of 3:");
  N1 := Tus ('3');
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Array of 1, 3, 5:");
  N1 := Tus ("135");
  Ada.Text_Io.Put_Line ("Length " & Image(N1.Length));
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Element 2: " & N1.Element (2));
  N1.Replace_Element (2, 'u');
  Ada.Text_Io.Put_Line ("Replaced by u: " & N1.Element (2));
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Append a, b, c, d, e");
  N2 := Tus ("ab");
  N1.Append (N2);
  N1.Append ( "cd");
  N1.Append ('e');
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Same with concat");
  N1 := Tus ("1u5");
  N1 := N1 & N2;
  N1 := N1 & "cd";
  N1 := N1 & "e";
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Same with reverse concat");
  N2 := "d" & Tus ("e");
  N1 := "1u5abc" & N2;
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Same with prepend");
  N1 := Tus ("abcde");
  N1.Prepend ("u5");
  N1.Prepend ("1");
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Slice 4 .. 6");
  Ada.Text_Io.Put_Line ("Slice string " & N1.Slice (4, 6));
  N2 := N1.Uslice (4, 6);
  Ada.Text_Io.Put_Line ("Uslice Image " & N2.Image);
  N1 := Tus ("abc");
  if N1 = N2 and then N1 = "abc"  and then "abc" = N1 then
    Ada.Text_Io.Put_Line ("Check ""="" OK");
  else
    Ada.Text_Io.Put_Line ("Check ""="" FAILED");
  end if;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Replace from 4 to 7 with B, C");
  N1 := Tus ("1u5abcde");
  N1.Replace (4, 7, "BC");
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Delete from 4 to 5, insert a, b, c, d before 4");
  N1.Delete (4, 5);
  N1.Insert (4, "abcd");
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Replace from 4 to 5 with B, C, D");
  N1.Replace (4, 5, "BCD");
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.Put_Line ("Delete from 4 to 6, insert A, B before 4");
  N1.Delete (4, 6);
  N1.Insert (4, "AB");
  Ada.Text_Io.Put_Line ("Image " & N1.Image);
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Check Finalization");
  declare
    N3 : Asu_Us;
  begin
    N3 := Tus ("B");
    declare
      N4 : constant Asu_Us := Tus ('C');
      N5 : Asu_Us;
    begin
     N5 := N3 & N4;
     Ada.Text_Io.Put_Line ("Array of B, C");
     Ada.Text_Io.Put_Line ("Image " & N5.Image);
    end;
  end;

  Ada.Text_Io.Put_Line ("Done.");
end T_Asu;

