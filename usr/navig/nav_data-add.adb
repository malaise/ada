-- When 1 vector is unknown compute it by addition or substraction of
--  the 2 others
with C_Nbres;
separate (Nav_Data)
function Add (X, Y : Nav_Types.T_Vector) return Nav_Types.T_Vector is
  Cx, Cy, Cr : C_Nbres.Complex;
begin
  Cx := C_Nbres.Create_Complex(C_Nbres.Real(X.Speed),
                               C_Nbres.Degree(To_Real(X.Angle)) );
  Cy := C_Nbres.Create_Complex(C_Nbres.Real(Y.Speed),
                               C_Nbres.Degree(To_Real(Y.Angle)) );
  Cr := C_Nbres."+" (Cx, Cy);

  return (
   Speed => Nav_Types.T_Speed(C_Nbres.Module(Cr)),
   Angle => To_Angle(Real(C_Nbres.Angle_Degree(Cr))) );
end Add;

