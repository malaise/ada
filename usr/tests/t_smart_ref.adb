with Ada.Text_Io;
with T_Smart_Int;
procedure T_Smart_Ref is

  R1, R2, R3 : T_Smart_Int.Int_Ref.Handle;
  I : Integer := 0;

begin
  Ada.Text_Io.Put_Line ("Test: Begin - Initializing R1 to 21 and R3 to 23");
  T_Smart_Int.Int_Ref.Set (R1, 21);
  T_Smart_Int.Int_Ref.Set (R3, 23);
  Ada.Text_Io.Put_Line ("Test: R1 and R3 set - Copying R1 to R2");
  T_Smart_Int.Int_Ref.Set(R2, R1);
  Ada.Text_Io.Put_Line ("Test: R2 set - Dereferencing R3");
  T_Smart_Int.Int_Ref.Dereference(R3, I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R3, Got" & I'Img & " - Dereferencing R2");
  T_Smart_Int.Int_Ref.Dereference(R2, I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R2, Got" & I'Img & " - Dereferencing R1");
  T_Smart_Int.Int_Ref.Dereference(R1, I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R1, Got" & I'Img
                      & " - Releasing R2 and Dereferencing R1");
  T_Smart_Int.Int_Ref.Release (R2);
  T_Smart_Int.Int_Ref.Dereference(R1, I);
  Ada.Text_Io.Put_Line ("Test: Dereferenced R1. Got" & I'Img
                     & " - Releasing R3");
  T_Smart_Int.Int_Ref.Release (R3);
  Ada.Text_Io.Put_Line ("Test: The end. Garbage collecting");
end T_Smart_Ref;

