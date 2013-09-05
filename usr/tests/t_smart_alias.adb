with Basic_Proc, Smart_Alias;
with T_Smart_Int;
procedure T_Smart_Alias is

  procedure Fin (Acc : access T_Smart_Int.Lim) is
  begin
    Basic_Proc.Put_Line_Output ("Finalization of "
        & T_Smart_Int.Image (Acc.all));
  end Fin;

  package Int_Alias is new Smart_Alias
      (T_Smart_Int.Lim, Fin);

  R1, R2, R3 : Int_Alias.Handle;
  I, J :  aliased T_Smart_Int.Lim;
  Ia : constant Int_Alias.Object_Access := I'Access;
  Ja : constant Int_Alias.Object_Access := J'Access;

begin
  T_Smart_Int.Init (I, 21);
  T_Smart_Int.Init (J, 23);
  Basic_Proc.Put_Line_Output (
      "Test: Begin - Initializing R1 to 21 and R3 to 23");
  R1.Init (Ia);
  R3.Init (Ja);
  Basic_Proc.Put_Line_Output ("Test: R1 and R3 set - Setting R2 to R1");
  R2 := R1;
  Basic_Proc.Put_Line_Output ("Test: Dereferencing R3");
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R3, Got"
                      & T_Smart_Int.Image (R3.Get_Access.all)
                      & " - Dereferencing R2");
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R2, Got"
                      & T_Smart_Int.Image (R2.Get_Access.all)
                      & " - Dereferencing R1");
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1, Got"
                      & T_Smart_Int.Image (R1.Get_Access.all)
                      & " - Releasing R2 and Dereferencing R1");
  R2.Release;
  Basic_Proc.Put_Line_Output ("Test: Dereferenced R1. Got"
                      & T_Smart_Int.Image (R1.Get_Access.all)
                      & " - Setting R1 to R3");
  R1 := R3;
  Basic_Proc.Put_Line_Output ("Test: Set R1 to R3. Got R1="
                      & T_Smart_Int.Image (R1.Get_Access.all)
                      & " and R3=" & T_Smart_Int.Image (R3.Get_Access.all)
                      & " - Releasing R3");
  R3.Release;
  Basic_Proc.Put_Line_Output (
      "Test: R3 released - The end: garbage collecting");
end T_Smart_Alias;

