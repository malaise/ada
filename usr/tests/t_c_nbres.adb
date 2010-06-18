with Ada.Text_Io;
with Argument, C_Nbres;

procedure T_C_Nbres is
  C1, C2 : C_Nbres.Complex;
  P : C_Nbres.Polar;
  use type C_Nbres.Complex;
begin
  if Argument.Get_Nbre_Arg /= 2
  and then Argument.Get_Nbre_Arg /= 4 then
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
          & " <real_part> <imaginary part> [ <real_part> <imaginary part> ]");
    return;
  end if;

  C1 := C_Nbres.Get (Argument.Get_Parameter (Occurence => 1),
                    Argument.Get_Parameter (Occurence => 2));
  Ada.Text_Io.Put_Line ("Got: " & C_Nbres.Put (C1));
  -- Do things with 1 complex
  P := C_Nbres.To_Polar (C1);
  Ada.Text_Io.Put_Line (" in polar: " & C_Nbres.Module(P)'Img & " "
                       & C_Nbres.Angle_Radian(P)'Img & "rd "
                       & C_Nbres.Angle_Degree(P)'Img & "°");

  if Argument.Get_Nbre_Arg = 2 then
    return;
  end if;
  -- Do things with 2 complex
  C2 := C_Nbres.Get (Argument.Get_Parameter (Occurence => 3),
                    Argument.Get_Parameter (Occurence => 4));
  Ada.Text_Io.Put_Line ("Got: " & C_Nbres.Put (C2));

  Ada.Text_Io.Put_Line (" A+B: " & C_Nbres.Put (C1 + C2));
  Ada.Text_Io.Put_Line (" A-B: " & C_Nbres.Put (C1 - C2));
  Ada.Text_Io.Put_Line (" A*B: " & C_Nbres.Put (C1 * C2));
  Ada.Text_Io.Put_Line (" A/B: " & C_Nbres.Put (C1 / C2));
  Ada.Text_Io.Put_Line (" A**B.real: "
                      & C_Nbres.Put (C1 ** C_Nbres.Part_Real(C2)));
end T_C_Nbres;

