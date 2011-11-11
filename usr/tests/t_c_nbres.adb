with Argument, C_Nbres, Basic_Proc;

procedure T_C_Nbres is
  C1, C2 : C_Nbres.Complex;
  P : C_Nbres.Polar;
  use type C_Nbres.Complex;
begin
  if Argument.Get_Nbre_Arg /= 2
  and then Argument.Get_Nbre_Arg /= 4 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <real_part> <imaginary part> [ <real_part> <imaginary part> ]");
    return;
  end if;

  C1 := C_Nbres.Get (Argument.Get_Parameter (Occurence => 1),
                    Argument.Get_Parameter (Occurence => 2));
  Basic_Proc.Put_Line_Output ("Got: " & C_Nbres.Put (C1));
  -- Do things with 1 complex
  P := C_Nbres.To_Polar (C1);
  Basic_Proc.Put_Line_Output (" in polar: " & C_Nbres.Module(P)'Img & " "
                       & C_Nbres.Angle_Radian(P)'Img & "rd "
                       & C_Nbres.Angle_Degree(P)'Img & "°");

  if Argument.Get_Nbre_Arg = 2 then
    return;
  end if;
  -- Do things with 2 complex
  C2 := C_Nbres.Get (Argument.Get_Parameter (Occurence => 3),
                    Argument.Get_Parameter (Occurence => 4));
  Basic_Proc.Put_Line_Output ("Got: " & C_Nbres.Put (C2));

  Basic_Proc.Put_Line_Output (" A+B: " & C_Nbres.Put (C1 + C2));
  Basic_Proc.Put_Line_Output (" A-B: " & C_Nbres.Put (C1 - C2));
  Basic_Proc.Put_Line_Output (" A*B: " & C_Nbres.Put (C1 * C2));
  Basic_Proc.Put_Line_Output (" A/B: " & C_Nbres.Put (C1 / C2));
  Basic_Proc.Put_Line_Output (" A**B.real: "
                      & C_Nbres.Put (C1 ** C_Nbres.Part_Real(C2)));
end T_C_Nbres;

