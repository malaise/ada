separate (Nav_Data)

-- When Plan.Angle and Traj.Speed are unknown
procedure Comp_Nav (Data : in out T_Data) is
  R : Real;
  Cw, Cp : Real;
  use Nav_Types;
begin
  -- Plan.Angle := Traj.Angle - Arc_Sin (Wind.Speed / Plan.Speed
  --  * Sin (Wind.Angle - Traj.Angle)
  R := My_Math.Sin (To_Real(Angle =>Data.Wind.Angle - Data.Traj.Angle),
   My_Math.Degree);
  R := My_Math.Arc_Sin (
        My_Math.Real(Data.Wind.Speed) / My_Math.Real(Data.Plan.Speed) * R,
        My_Math.Degree);
  Data.Plan.Angle := Data.Traj.Angle - To_Angle (R);

  -- Traj.Speed := Wind.Speed * Cos (Traj_Angle - Wind_Angle) +
  --  Plan.Speed * Cos (Plan.Angle - Traj_Angle)
  Cw := My_Math.Cos(To_Real(Angle => Data.Traj.Angle - Data.Wind.Angle),
   My_Math.Degree);
  Cp := My_Math.Cos(To_Real(Angle => Data.Plan.Angle - Data.Traj.Angle),
   My_Math.Degree);
  Data.Traj.Speed := Nav_Types.T_Speed (My_Math.Real(Data.Wind.Speed) * Cw
                                      + My_Math.Real(Data.Plan.Speed) * Cp);
exception
  when My_Math.Math_Error => raise Comp_Err;
end Comp_Nav;

