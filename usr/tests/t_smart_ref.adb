with Ada.Text_Io;
with T_Smart_Int;
procedure T_Smart_Ref is

  R1, R2, R3 : T_Smart_Int.Int_Ref.Handle;
  I : Integer := 0;

begin
  Ada.Text_Io.Put_Line ("Test: Begin - Initializing R1 to 21 and R3 to 23");
  R1.Set (21);
  R3.Set (23);
  Ada.Text_Io.Put_Line ("Test: R1 and R3 set - Copying R1 to R2");
  R2.Set (R1);
  Ada.Text_Io.Put_Line ("Test: R2 set - Dereferencing R3");
  R3.Get (I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R3, Got" & I'Img & " - Dereferencing R2");
  R2.Get (I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R2, Got" & I'Img & " - Dereferencing R1");
  R2.Get (I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R1, Got" & I'Img
                      & " - Releasing R2 and Dereferencing R1");
  R2.Release;
  R1.Get (I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R1. Got" & I'Img
                      & " - Releasing R3");
  R3.Release;
  Ada.Text_Io.Put_Line ("Test: The end. Garbage collecting");
end T_Smart_Ref;

