-- Check of a navigation question coherency
with MY_MATH;
use MY_MATH;
package body NAV_DATA is

  -- For math computations of angles
  subtype REAL is MY_MATH.REAL;

  -- problem in computation
  COMP_ERR : exception;

  -- Utilities to convert angles
  -- From deg & min to real
  function TO_REAL (DEG : NAV_TYPES.T_DEGREE; MIN : NAV_TYPES.T_MINUTE := 0)
   return REAL is
  begin
    return REAL (DEG) + REAL (MIN) / 60.0;
  end TO_REAL;

   -- From angle to real
  function TO_REAL (ANGLE : NAV_TYPES.T_ANGLE) return REAL is
  begin
    return REAL (ANGLE.DEGREES) + REAL (ANGLE.MINUTES) / 60.0;
  end TO_REAL;

  -- From drift to real
  function TO_REAL (DRIFT : NAV_TYPES.T_DRIFT) return REAL is
  begin
    if DRIFT.POSITIV then
      return REAL (DRIFT.DEGREES) + REAL (DRIFT.MINUTES) / 60.0;
    else
      return - (REAL (DRIFT.DEGREES) + REAL (DRIFT.MINUTES) / 60.0);
    end if;
  end TO_REAL;

  -- From real to angle
  function TO_ANGLE (R : REAL) return NAV_TYPES.T_ANGLE is
    LOC_R : REAL := R;
    DEG : NAV_TYPES.T_COMMON_DEG;
    MIN : NATURAL;
    RET_ANGLE : NAV_TYPES.T_ANGLE;
    use NAV_TYPES;
  begin

    -- reduction of R
    while LOC_R > REAL (NAV_TYPES.T_DEGREE'LAST) loop
      LOC_R := LOC_R -
       REAL (NAV_TYPES.T_COMMON_DEG(NAV_TYPES.T_DEGREE'LAST) + 1);
    end loop;
    while LOC_R < 0.0 loop
      LOC_R := LOC_R +
       REAL (NAV_TYPES.T_COMMON_DEG(NAV_TYPES.T_DEGREE'LAST) + 1);
    end loop;

    -- split of degrees and minutes
    DEG := NAV_TYPES.T_COMMON_DEG (MY_MATH.TRUNC(LOC_R) );
    MIN := NATURAL (MY_MATH.ROUND ( MY_MATH.FRAC(LOC_R) *
     ( REAL(NAV_TYPES.T_MINUTE'LAST) + 1.0) ) );

    if MIN <= NATURAL(NAV_TYPES.T_MINUTE'LAST) then
      RET_ANGLE.MINUTES := NAV_TYPES.T_MINUTE (MIN);
    else
      DEG := DEG + 1;
      MIN := MIN - NATURAL(NAV_TYPES.T_MINUTE'LAST) - 1;
    end if;
    -- Done
    return NAV_TYPES.REDUCTION (DEG, NAV_TYPES.T_MINUTE(MIN));
  end TO_ANGLE;


  function CHECK (PROBLEM : T_DATA) return T_CONSISTENCY is separate;


  -- if DRIFT is known, compute plan angle and traj angles before
  --  global computation
  procedure SET_BEFORE (DATA : in out T_DATA) is separate;

  -- if DRIFT is unknown, compute it after global computation
  procedure SET_DRIFT_AFTER (DATA : in out T_DATA) is separate;

  -- when only unknown is TRAJ_S after set_before
  procedure COMP_TRAJS (DATA : in out T_DATA) is separate;

  -- When 1 vector is unknown compute it by addition or substraction of
  --  the 2 others
  function ADD (X, Y : NAV_TYPES.T_VECTOR) return NAV_TYPES.T_VECTOR
   is separate;

  -- When PLAN.SPEED and TRAJ.SPEED are unknown
  procedure COMP_SPEED (DATA : in out T_DATA) is separate;

  -- When PLAN.ANGLE and TRAJ.SPEED are unknown
  procedure COMP_NAV (DATA : in out T_DATA) is separate;


  procedure RESOLUTION (PROBLEM : in T_DATA;
   CHECK : out T_CONSISTENCY; SOLUTION : out T_DATA) is
    CHECK_RESULT : T_CONSISTENCY;
    DATA : T_DATA;
  begin
    -- Resolution of data in out
    DATA := PROBLEM;

    -- Update wind angle
    if DATA.SET(WIND_A) then
      DATA.WIND.ANGLE := NAV_TYPES."+" (DATA.WIND.ANGLE, 180);
    end if;

    -- Verification of the consistency of the problem
    CHECK_RESULT := NAV_DATA.CHECK (DATA);
    CHECK := CHECK_RESULT;
    if CHECK_RESULT /= OK then
      return;
    end if;

    -- Drift is known : compute TRAJ angle or PLAN angle, or both
    if DATA.SET (DRIFT) then
      SET_BEFORE (DATA);
    end if;

    if DATA.SET(DRIFT) and then DATA.SET(TRAJ_A) and then DATA.SET(PLAN_A)
    and then DATA.SET(WIND_A) and then DATA.SET(PLAN_S) and then DATA.SET(WIND_S) then
      -- after set_before, last unknown is TRAJ_S
      COMP_TRAJS (DATA);
      DATA.SET (TRAJ_S) := TRUE;
    else

      -- 2 data are unknown toward the 6 data of the 3 vectors
      --  Compute the 3 vectors
      if not DATA.SET(WIND_S) and then not DATA.SET(WIND_A) then
        -- WIND := TRAJ - PLAN
        DATA.WIND := ADD (DATA.TRAJ, NAV_TYPES.T_VECTOR'(
         SPEED => DATA.PLAN.SPEED,
         ANGLE => NAV_TYPES."+" (DATA.PLAN.ANGLE, 180) ) );
        DATA.SET(WIND_A) := TRUE;
        DATA.SET(WIND_S) := TRUE;
      elsif not DATA.SET(PLAN_S) and then not DATA.SET(PLAN_A) then
        -- PLAN := TRAJ - WIND
        DATA.PLAN := ADD (DATA.TRAJ, NAV_TYPES.T_VECTOR'(
         SPEED => DATA.WIND.SPEED,
         ANGLE => NAV_TYPES."+" (DATA.WIND.ANGLE, 180) ) );
        DATA.SET(PLAN_A) := TRUE;
        DATA.SET(PLAN_S) := TRUE;
      elsif not DATA.SET(TRAJ_S) and then not DATA.SET(TRAJ_A) then
        -- TRAJ := PLAN + WIND
        DATA.TRAJ := ADD (DATA.PLAN, DATA.WIND);
        DATA.SET(TRAJ_A) := TRUE;
        DATA.SET(TRAJ_S) := TRUE;
      elsif not DATA.SET(PLAN_S) and then not DATA.SET(TRAJ_S) then
        -- PLAN.SPEED and TRAJ.SPEED unknown
        COMP_SPEED (DATA);
        DATA.SET(PLAN_S) := TRUE;
        DATA.SET(TRAJ_S) := TRUE;
      elsif not DATA.SET(PLAN_A) and then not DATA.SET(TRAJ_S) then
        -- PLAN.ANGLE and TRAJ.SPEED unknown
        COMP_NAV (DATA);
        DATA.SET(PLAN_A) := TRUE;
        DATA.SET(TRAJ_S) := TRUE;
      else
        raise RESOL_ERROR;
      end if;

      -- DRIFT is not known : compute it with TRAJ and PLAN angles
      if not DATA.SET (DRIFT) then
        SET_DRIFT_AFTER (DATA);
      end if;

    end if;

    -- Update wind angle
    DATA.WIND.ANGLE := NAV_TYPES."+" (DATA.WIND.ANGLE, 180);

    SOLUTION := DATA;
    SOLUTION.SET := PROBLEM.SET;
  exception
    when COMP_ERR =>
      CHECK := VAL_ERR;
    when others => raise;

  end RESOLUTION;

end NAV_DATA;
