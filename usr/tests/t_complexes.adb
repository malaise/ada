with Argument, Complexes, Basic_Proc;

procedure T_Complexes is
  C1, C2 : Complexes.Complex;
  P : Complexes.Polar;
  use type Complexes.Complex;
begin
  if Argument.Get_Nbre_Arg /= 2
  and then Argument.Get_Nbre_Arg /= 4 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <real_part> <imaginary part> [ <real_part> <imaginary part> ]");
    return;
  end if;

  C1.Get (Argument.Get_Parameter (Occurence => 1),
          Argument.Get_Parameter (Occurence => 2));
  Basic_Proc.Put_Line_Output ("Got: " & C1.Image);
  -- Do things with 1 complex
  P := C1.To_Polar;
  Basic_Proc.Put_Line_Output (" in polar: " & Complexes.Module(P)'Img & " "
                       & Complexes.Angle_Radian(P)'Img & "rd "
                       & Complexes.Angle_Degree(P)'Img & "°"); --## rule line off Char

  if Argument.Get_Nbre_Arg = 2 then
    return;
  end if;
  -- Do things with 2 complexes
  C2.Get (Argument.Get_Parameter (Occurence => 3),
          Argument.Get_Parameter (Occurence => 4));
  Basic_Proc.Put_Line_Output ("Got: " & C2.Image);

  Basic_Proc.Put_Line_Output (" A+B: " & Complexes.Image (C1 + C2));
  Basic_Proc.Put_Line_Output (" A-B: " & Complexes.Image (C1 - C2));
  Basic_Proc.Put_Line_Output (" A*B: " & Complexes.Image (C1 * C2));
  Basic_Proc.Put_Line_Output (" A/B: " & Complexes.Image (C1 / C2));
  Basic_Proc.Put_Line_Output (" A**B.real: "
                      & Complexes.Image (C1 ** C2.Part_Real));
end T_Complexes;

