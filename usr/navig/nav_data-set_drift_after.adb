separate (Nav_Data)

-- if DRIFT is not known, compute it with plan angle and traj angles after
--  global computation
procedure Set_Drift_After (Data : in out T_Data) is
begin
  Data.Drift := Nav_Types."-" (Data.Traj.Angle, Data.Plan.Angle);
  Data.Set(Drift) := True;
end Set_Drift_After;

