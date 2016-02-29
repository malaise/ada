-- When 1 vector is unknown compute it by addition or substraction of
--  the 2 others
with Complexes;
separate (Nav_Data)
function Add (X, Y : Nav_Types.T_Vector) return Nav_Types.T_Vector is
  Cx, Cy : Complexes.Complex;
  P : Complexes.Polar;
begin
  Cx := Complexes.To_Complex (Complexes.Create_Polar(
                               Complexes.Real(X.Speed),
                               Complexes.Degree(To_Real(X.Angle)) ));
  Cy := Complexes.To_Complex (Complexes.Create_Polar(
                               Complexes.Real(Y.Speed),
                               Complexes.Degree(To_Real(Y.Angle)) ));
  P := Complexes.To_Polar (Complexes."+" (Cx, Cy));

  return (
   Speed => Nav_Types.T_Speed(Complexes.Module(P)),
   Angle => To_Angle(Real(Complexes.Angle_Degree(P))) );
end Add;

