with My_Math;
separate (Nav_Data)

-- if Drift is known, compute plan angle or traj angles before
--  global computation
procedure Set_Before (Data : in out T_Data) is
  use Nav_Types;
begin
  if Data.Set (Traj_A) then
    -- Traj.Angle is set : Plan.Angle := Traj.Angle - Drift
    Data.Plan.Angle := Data.Traj.Angle - Data.Drift;
    Data.Set (Plan_A) := True;
  elsif Data.Set (Plan_A) then
    -- Plan.Angle is set : Traj.Angle := Plan.Angle + Drift
    Data.Traj.Angle := Data.Plan.Angle + Data.Drift;
    Data.Set (Traj_A) := True;
  else

    -- Traj_Angle and Plan_Angle are not set. 3rd unknown is Traj_Speed
    -- Traj_A = 90 + Wind_A - Arc_Cos ( Plan_S/Wind_S*Cos(90+Drift) )
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

