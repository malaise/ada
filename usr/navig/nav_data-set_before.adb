with My_Math;
separate (Nav_Data)

-- if DRIFT is known, compute plan angle or traj angles before
--  global computation
procedure Set_Before (Data : in out T_Data) is
  use Nav_Types;
begin
  if Data.Set (Traj_A) then
    -- TRAJ.ANGLE is set : PLAN.ANGLE := TRAJ.ANGLE - DRIFT
    Data.Plan.Angle := Data.Traj.Angle - Data.Drift;
    Data.Set (Plan_A) := True;
  elsif Data.Set (Plan_A) then
    -- PLAN.ANGLE is set : TRAJ.ANGLE := PLAN.ANGLE + DRIFT
    Data.Traj.Angle := Data.Plan.Angle + Data.Drift;
    Data.Set (Traj_A) := True;
  else

    -- TRAJ_ANGLE and PLAN_ANGLE are not set. 3rd unknown is TRAJ_SPEED
    -- TRAJ_A = 90 + WIND_A - ARC_COS ( PLAN_S/WIND_S*COS(90+DRIFT) )
    declare
      R : Real;
    begin
      R := 90.0 + To_Real (Data.Drift);
      R := Real(Data.Plan.Speed) / Real(Data.Wind.Speed)
         * My_Math.Cos (R, My_Math.Degree);
      R := My_Math.Arc_Cos (R, My_Math.Degree);
      Data.Traj.Angle := To_Angle (90.0 - R) + Data.Wind.Angle;
      Data.Plan.Angle := Data.Traj.Angle - Data.Drift;
      Data.Set (Traj_A) := True;
      Data.Set (Plan_A) := True;
    exception
      when My_Math.Math_Error => raise Comp_Err;
    end;

  end if;
end Set_Before;
