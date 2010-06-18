-- When 1 vector is unknown compute it by addition or substraction of
--  the 2 others
with C_Nbres;
separate (Nav_Data)
function Add (X, Y : Nav_Types.T_Vector) return Nav_Types.T_Vector is
  Cx, Cy : C_Nbres.Complex;
  P : C_Nbres.Polar;
begin
  Cx := C_Nbres.To_Complex (C_Nbres.Create_Polar(
                               C_Nbres.Real(X.Speed),
                               C_Nbres.Degree(To_Real(X.Angle)) ));
  Cy := C_Nbres.To_Complex (C_Nbres.Create_Polar(
                               C_Nbres.Real(Y.Speed),
                               C_Nbres.Degree(To_Real(Y.Angle)) ));
  P := C_Nbres.To_Polar (C_Nbres."+" (Cx, Cy));

  return (
   Speed => Nav_Types.T_Speed(C_Nbres.Module(P)),
   Angle => To_Angle(Real(C_Nbres.Angle_Degree(P))) );
end Add;

