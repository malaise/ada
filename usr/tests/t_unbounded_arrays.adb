with Ada.Text_Io;
with As.U; use As.U;
with Unbounded_Arrays, Int_Image;
procedure T_Unbounded_Arrays is

  type Nat_Array is array (Positive range <>) of Natural;

  package Natua is new Unbounded_Arrays (Natural, Nat_Array);
  function Image is new Int_Image (Natural);

  use Natua;

  N1, N2 : Natua.Unbounded_Array;

  function Image (A : Nat_Array) return String is
    Res : Asu_Us;
  begin
    for I in A'Range loop
      Asu.Append (Res, Image (A(I)));
      if I /= A'Last then
        Asu.Append (Res, ", ");
      end if;
    end loop;
    return Asu_Ts (Res);
  end Image;

  function Image (N : Natua.Unbounded_Array) return String is
  begin
    return ">" & Image(N.To_Array) & "<";
  end Image;

begin

  Ada.Text_Io.Put_Line ("Empty array:");
  Ada.Text_Io.Put_Line ("Length " & Image(N1.Length));
  Ada.Text_Io.Put_Line ("Content " & Image(N1.To_Array));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Array of 3:");
  N1 := Natua.To_Unbounded_Array (3);
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Array of 1, 3, 5:");
  N1 := Natua.To_Unbounded_Array ( (1, 3, 5) );
  Ada.Text_Io.Put_Line ("Length " & Image(N1.Length));
  Ada.Text_Io.Put_Line ("Content " & Image(N1.To_Array));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Element 2: " & Image(N1.Element (2)));
  N1.Replace_Element (2, 21);
  Ada.Text_Io.Put_Line ("Replaced by 21: " & Image(N1.Element (2)));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Append 30, 40, 50, 60, 70");
  N2 := Natua.To_Unbounded_Array ( (30, 40) );
  N1.Append (N2);
  N1.Append ( (50, 60) );
  N1.Append (70);
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Same with concat");
  N1 := Natua.To_Unbounded_Array ( (1, 21, 5) );
  N1 := N1 & N2;
  N1 := N1 & (50, 60);
  N1 := N1 & 70;
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Same with reverse concat");
  N2 := 60 & Natua.To_Unbounded_Array (70);
  N1 := (1, 21, 5, 30, 40, 50) & N2;
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Same with prepend");
  N1 := Natua.To_Unbounded_Array ( (30, 40, 50, 60, 70) );
  N1.Prepend ( (21, 5) );
  N1.Prepend (1);
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Slice 4 .. 6");
  Ada.Text_Io.Put_Line ("Content " & Image(N1.Slice (4, 6)));
  N2 := N1.Unbounded_Slice (4, 6);
  Ada.Text_Io.Put_Line ("Image " & Image(N2));
  N1 := Natua.To_Unbounded_Array ( (30, 40, 50) );
  if N1 = N2 and then N1 = (30, 40, 50)  and then (30, 40, 50) = N1 then
    Ada.Text_Io.Put_Line ("Check ""="" OK");
  else
    Ada.Text_Io.Put_Line ("Check ""="" FAILED");
  end if;
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Replace from 4 to 7 with 41, 51");
  N1 := Natua.To_Unbounded_Array ( (1, 21, 5, 30, 40, 50, 60, 70) );
  N1.Replace (4, 7, (41, 51));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Delete from 4 to 5, insert 30, 40, 50, 60 before 4");
  N1.Delete (4, 5);
  N1.Insert (4, (30, 40, 50, 60));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Replace from 4 to 5 with 41, 51, 61");
  N1.Replace (4, 5, (41, 51, 61));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.Put_Line ("Delete from 4 to 6, insert 30, 40 before 4");
  N1.Delete (4, 6);
  N1.Insert (4, (30, 40));
  Ada.Text_Io.Put_Line ("Image " & Image(N1));
  Ada.Text_Io.New_Line;

  Ada.Text_Io.Put_Line ("Check Finalization");
  declare
    N3 : Natua.Unbounded_Array;
  begin
    N3 := Natua.To_Unbounded_Array (21);
    declare
      N4 : constant Natua.Unbounded_Array := Natua.To_Unbounded_Array (22);
      N5 : Natua.Unbounded_Array;
    begin
     N5 := N3 & N4;
     Ada.Text_Io.Put_Line ("Array of 21, 22");
     Ada.Text_Io.Put_Line ("Image " & Image(N5));
    end;
  end;

  Ada.Text_Io.Put_Line ("Done.");
end T_Unbounded_Arrays;

