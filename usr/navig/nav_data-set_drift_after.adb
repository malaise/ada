separate (NAV_DATA)

-- if DRIFT is not known, compute it with plan angle and traj angles after
--  global computation
procedure SET_DRIFT_AFTER (DATA : in out T_DATA) is
begin
  DATA.DRIFT := NAV_TYPES."-" (DATA.TRAJ.ANGLE, DATA.PLAN.ANGLE);
  DATA.SET(DRIFT) := TRUE;
end SET_DRIFT_AFTER;

