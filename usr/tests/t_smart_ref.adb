with Basic_Proc, Smart_Reference;
with T_Smart_Int;
procedure T_Smart_Ref is


  package Int_Ref is new Smart_Reference
      (T_Smart_Int.Lim, T_Smart_Int.Set, T_Smart_Int.Fin);

  R1, R2, R3 : Int_Ref.Handle;
  I, J :  T_Smart_Int.Lim;

begin
  T_Smart_Int.Init (I, 21);
  T_Smart_Int.Init (J, 23);
  Basic_Proc.Put_Line_Output (
      "Test: Begin - Initializing R1 to 21 and R3 to 23");
  R1.Init (I);
  R3.Init (J);
  Basic_Proc.Put_Line_Output ("Test: R1 and R3 set - Setting R2 to R1");
  R2 := R1;
  Basic_Proc.Put_Line_Output ("Test: Dereferencing R3");
  R3.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R3, Got"
                      & T_Smart_Int.Image (I)
                      & " - Dereferencing R2");
  R2.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R2, Got"
                      & T_Smart_Int.Image (I)
                      & " - Dereferencing R1");
  R1.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1, Got"
                      & T_Smart_Int.Image (I)
                      & " - Releasing R2 and Dereferencing R1");
  R2.Release;
  R1.Get (I);
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1. Got"
                      & T_Smart_Int.Image (I)
                      & " - Setting R1 to R3");
  R1 := R3;
  R1.Get (I);
  R3.Get (J);
  Basic_Proc.Put_Line_Output ("Test: Set R1 to R3. Got R1="
                      & T_Smart_Int.Image (I)
                      & " and R3=" & T_Smart_Int.Image (J)
                      & " - Releasing R3");
  R3.Release;
  Basic_Proc.Put_Line_Output (
      "Test: R3 released - The end: garbage collecting");
end T_Smart_Ref;

