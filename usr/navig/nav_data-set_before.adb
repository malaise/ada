with MY_MATH;
separate (NAV_DATA)

-- if DRIFT is known, compute plan angle or traj angles before
--  global computation
procedure SET_BEFORE (DATA : in out T_DATA) is
  use NAV_TYPES;
begin
  if DATA.SET (TRAJ_A) then
    -- TRAJ.ANGLE is set : PLAN.ANGLE := TRAJ.ANGLE - DRIFT
    DATA.PLAN.ANGLE := DATA.TRAJ.ANGLE - DATA.DRIFT;
    DATA.SET (PLAN_A) := TRUE;
  elsif DATA.SET (PLAN_A) then
    -- PLAN.ANGLE is set : TRAJ.ANGLE := PLAN.ANGLE + DRIFT
    DATA.TRAJ.ANGLE := DATA.PLAN.ANGLE + DATA.DRIFT;
    DATA.SET (TRAJ_A) := TRUE;
  else

    -- TRAJ_ANGLE and PLAN_ANGLE are not set. 3rd unknown is TRAJ_SPEED
    -- TRAJ_A = 90 + WIND_A - ARC_COS ( PLAN_S/WIND_S*COS(90+DRIFT) )
    declare
      R : REAL;
    begin
      R := 90.0 + TO_REAL (DATA.DRIFT);
      R := REAL(DATA.PLAN.SPEED) / REAL(DATA.WIND.SPEED)
         * MY_MATH.COS (R, MY_MATH.DEGREE);
      R := MY_MATH.ARC_COS (R, MY_MATH.DEGREE);
      DATA.TRAJ.ANGLE := TO_ANGLE (90.0 - R) + DATA.WIND.ANGLE;
      DATA.PLAN.ANGLE := DATA.TRAJ.ANGLE - DATA.DRIFT;
      DATA.SET (TRAJ_A) := TRUE;
      DATA.SET (PLAN_A) := TRUE;
    exception
      when MY_MATH.MY_MATH_ERROR => raise COMP_ERR;
    end;

  end if;
end SET_BEFORE;
