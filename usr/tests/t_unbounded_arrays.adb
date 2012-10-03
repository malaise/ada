with Basic_Proc, As.U, Unbounded_Arrays, Images;
procedure T_Unbounded_Arrays is

  type Nat_Array is array (Positive range <>) of Natural;

  package Natua is new Unbounded_Arrays (Natural, Nat_Array);

  use Natua;

  N1, N2, N3 : Natua.Unbounded_Array;

  function Image (A : Nat_Array) return String is
    Res : As.U.Asu_Us;
  begin
    for I in A'Range loop
      Res.Append (Images.Integer_Image (A(I)));
      if I /= A'Last then
        Res.Append (", ");
      end if;
    end loop;
    return Res.Image;
  end Image;

  function Image (N : Natua.Unbounded_Array) return String is
  begin
    return ">" & Image(N.To_Array) & "<";
  end Image;

begin

  Basic_Proc.Put_Line_Output ("Empty array:");
  N1.Set_Null;
  N3 := Natua.Null_Unb_Array;
  if N1 /= N2 or else N1 /= N3
  or else not N1.Is_Null or else not N2.Is_Null or else not N3.Is_Null
  or else     N1 /= Natua.Null_Unb_Array or else N2 /= Natua.Null_Unb_Array
  or else     N3 /= Natua.Null_Unb_Array then
    Basic_Proc.Put_Output ("Init is not empty");
    return;
  end if;
  Basic_Proc.Put_Line_Output ("Length " & Images.Integer_Image(N1.Length));
  Basic_Proc.Put_Line_Output ("Content " & Image(N1.To_Array));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Array of 3:");
  N1 := Natua.To_Unbounded_Array (3);
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Array of 1, 3, 5:");
  N1 := Natua.To_Unbounded_Array ( (1, 3, 5) );
  Basic_Proc.Put_Line_Output ("Length " & Images.Integer_Image(N1.Length));
  Basic_Proc.Put_Line_Output ("Content " & Image(N1.To_Array));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Element 2: "
                            & Images.Integer_Image(N1.Element (2)));
  N1.Replace_Element (2, 21);
  Basic_Proc.Put_Line_Output ("Replaced by 21: "
                            & Images.Integer_Image(N1.Element (2)));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Append 30, 40, 50, 60, 70");
  N2 := Natua.To_Unbounded_Array ( (30, 40) );
  N1.Append (N2);
  N1.Append ( (50, 60) );
  N1.Append (70);
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Same with concat");
  N1 := Natua.To_Unbounded_Array ( (1, 21, 5) );
  N1 := N1 & N2;
  N1 := N1 & (50, 60);
  N1 := N1 & 70;
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Same with reverse concat");
  N2 := 60 & Natua.To_Unbounded_Array (70);
  N1 := (1, 21, 5, 30, 40, 50) & N2;
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Same with prepend");
  N1 := Natua.To_Unbounded_Array ( (30, 40, 50, 60, 70) );
  N1.Prepend ( (21, 5) );
  N1.Prepend (1);
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Slice 4 .. 6");
  Basic_Proc.Put_Line_Output ("Content " & Image(N1.Slice (4, 6)));
  N2 := N1.Unbounded_Slice (4, 6);
  Basic_Proc.Put_Line_Output ("Image " & Image(N2));
  N1 := Natua.To_Unbounded_Array ( (30, 40, 50) );
  if N1 = N2 and then N1 = (30, 40, 50)  and then (30, 40, 50) = N1 then
    Basic_Proc.Put_Line_Output ("Check ""="" OK");
  else
    Basic_Proc.Put_Line_Output ("Check ""="" FAILED");
  end if;
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Replace from 4 to 7 with 41, 51");
  N1 := Natua.To_Unbounded_Array ( (1, 21, 5, 30, 40, 50, 60, 70) );
  N1.Replace (4, 7, (41, 51));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Delete from 4 to 5, insert 30, 40, 50, 60 before 4");
  N1.Delete (4, 5);
  N1.Insert (4, (30, 40, 50, 60));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Replace from 4 to 5 with 41, 51, 61");
  N1.Replace (4, 5, (41, 51, 61));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.Put_Line_Output ("Delete from 4 to 6, insert 30, 40 before 4");
  N1.Delete (4, 6);
  N1.Insert (4, (30, 40));
  Basic_Proc.Put_Line_Output ("Image " & Image(N1));
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Check Finalization");
  declare
    N3 : Natua.Unbounded_Array;
  begin
    N3 := Natua.To_Unbounded_Array (21);
    declare
      N4 : constant Natua.Unbounded_Array := Natua.To_Unbounded_Array (22);
      N5 : Natua.Unbounded_Array;
    begin
     N5 := N3 & N4;
     Basic_Proc.Put_Line_Output ("Array of 21, 22");
     Basic_Proc.Put_Line_Output ("Image " & Image(N5));
    end;
  end;

  Basic_Proc.Put_Line_Output ("Done.");
end T_Unbounded_Arrays;

