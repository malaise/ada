separate (Nav_Data)

-- When PLAN.ANGLE and TRAJ.SPEED are unknown
procedure Comp_Nav (Data : in out T_Data) is
  R : Real;
  Cw, Cp : Real;
  use Nav_Types;
begin
  -- PLAN.ANGLE := TRAJ.ANGLE - ARC_SIN (WIND.SPEED / PLAN.SPEED
  --  * SIN (WIND.ANGLE - TRAJ.ANGLE)
  R := My_Math.Sin (To_Real(Angle =>Data.Wind.Angle - Data.Traj.Angle),
   My_Math.Degree);
  R := My_Math.Arc_Sin (
        My_Math.Real(Data.Wind.Speed) / My_Math.Real(Data.Plan.Speed) * R,
        My_Math.Degree);
  Data.Plan.Angle := Data.Traj.Angle - To_Angle (R);

  -- TRAJ.SPEED := WIND.SPEED * COS(TRAJ_ANGLE - WIND_ANGLE) +
  --  PLAN.SPEED * COS(PLAN.ANGLE - TRAJ_ANGLE)
  Cw := My_Math.Cos(To_Real(Angle => Data.Traj.Angle - Data.Wind.Angle),
   My_Math.Degree);
  Cp := My_Math.Cos(To_Real(Angle => Data.Plan.Angle - Data.Traj.Angle),
   My_Math.Degree);
  Data.Traj.Speed := Nav_Types.T_Speed (My_Math.Real(Data.Wind.Speed) * Cw
                                      + My_Math.Real(Data.Plan.Speed) * Cp);
exception
  when My_Math.Math_Error => raise Comp_Err;
end Comp_Nav;
