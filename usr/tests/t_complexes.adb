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

  C1 := Complexes.Get (Argument.Get_Parameter (Occurence => 1),
                    Argument.Get_Parameter (Occurence => 2));
  Basic_Proc.Put_Line_Output ("Got: " & Complexes.Put (C1));
  -- Do things with 1 complex
  P := Complexes.To_Polar (C1);
  Basic_Proc.Put_Line_Output (" in polar: " & Complexes.Module(P)'Img & " "
                       & Complexes.Angle_Radian(P)'Img & "rd "
                       & Complexes.Angle_Degree(P)'Img & "°"); --## rule line off Char

  if Argument.Get_Nbre_Arg = 2 then
    return;
  end if;
  -- Do things with 2 complexes
  C2 := Complexes.Get (Argument.Get_Parameter (Occurence => 3),
                       Argument.Get_Parameter (Occurence => 4));
  Basic_Proc.Put_Line_Output ("Got: " & Complexes.Put (C2));

  Basic_Proc.Put_Line_Output (" A+B: " & Complexes.Put (C1 + C2));
  Basic_Proc.Put_Line_Output (" A-B: " & Complexes.Put (C1 - C2));
  Basic_Proc.Put_Line_Output (" A*B: " & Complexes.Put (C1 * C2));
  Basic_Proc.Put_Line_Output (" A/B: " & Complexes.Put (C1 / C2));
  Basic_Proc.Put_Line_Output (" A**B.real: "
                      & Complexes.Put (C1 ** Complexes.Part_Real(C2)));
end T_Complexes;

