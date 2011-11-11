with Basic_Proc;
with T_Smart_Int;
procedure T_Smart_Ref is

  R1, R2, R3 : T_Smart_Int.Int_Ref.Handle;
  I, J : Integer := 0;

begin
  Basic_Proc.Put_Line_Output (
      "Test: Begin - Initializing R1 to 21 and R3 to 23");
  R1.Init (21);
  R3.Init (23);
  Basic_Proc.Put_Line_Output ("Test: R1 and R3 set - Setting R2 to R1");
  R2 := R1;
  Basic_Proc.Put_Line_Output ("Test: Dereferencing R3");
  R3.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R3, Got" & I'Img
                      & " - Dereferencing R2");
  R2.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R2, Got" & I'Img
                      & " - Dereferencing R1");
  R2.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1, Got" & I'Img
                      & " - Releasing R2 and Dereferencing R1");
  R2.Release;
  R1.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1. Got" & I'Img
                      & " - Setting R1 to R3");
  R1 := R3;
  R1.Get (I);
  R3.Get (J);
  Basic_Proc.Put_Line_Output ("Test: Set R1 to R3. Got R1=" & I'Img
                      & " and R3=" & J'Img & " - Releasing R3");
  R3.Release;
  Basic_Proc.Put_Line_Output (
      "Test: R3 released - The end: garbage collecting");
end T_Smart_Ref;

