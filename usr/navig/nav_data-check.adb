separate (NAV_DATA)

function CHECK (PROBLEM : T_DATA) return T_CONSISTENCY is
  SET : constant T_DATA_SET := PROBLEM.SET;
begin
  -- Check number of unkown data
  UNKNOWN_CHECK:
  declare
    N_UNKNOWN : NATURAL := 0;
  begin
    for I in T_LIST_DATA loop
      if not SET(I) then N_UNKNOWN := N_UNKNOWN + 1; end if;
    end loop;
    if N_UNKNOWN /= 3 then
      return KNOWN_ERR;
    end if;
  end UNKNOWN_CHECK;

  -- Check drift.
  if SET(DRIFT) then
    -- one in traj angle or plan angle must be unknown
    if SET(PLAN_A) and then SET(TRAJ_A) then
      return DRIFT_ERR;
    end if;
  end if;

  -- Check the 3 vector angles
  ANGLE_CHECK:
  declare
    PA, TA, WA : NAV_TYPES.T_ANGLE;
    A_SET : BOOLEAN;
    use NAV_TYPES;
  begin
    -- See if PLAN_A and TRAJ_A are set
    if SET(DRIFT) then
      A_SET := SET(PLAN_A) or else SET(TRAJ_A);
    else
      A_SET := SET(PLAN_A) and then SET(TRAJ_A);
    end if;
    A_SET := A_SET and then SET(WIND_A);

    if A_SET then
      if SET(DRIFT) then
        if SET(PLAN_A) then
          PA := PROBLEM.PLAN.ANGLE;
          TA := PA + PROBLEM.DRIFT;
        else
          TA := PROBLEM.TRAJ.ANGLE;
          PA := TA - PROBLEM.DRIFT;
        end if;
      else
        PA := PROBLEM.PLAN.ANGLE;
        TA := PROBLEM.TRAJ.ANGLE;
      end if;
      WA := PROBLEM.WIND.ANGLE;
      -- check that the 3 angles are mod 180 different
      if WA = PA or else WA = PA + 180 or else
         PA = TA or else PA = TA + 180 or else
         TA = WA or else TA = WA + 180 then
        return ANGLE_ERR;
      end if;
      -- check that WIND and PLAN are on both sides of TRAJ
      if (WA < TA or else WA > TA + 180) and then
         (PA < TA or else PA > TA + 180) then
        return ANGLE_ERR;
      elsif (WA > TA and then WA < TA + 180) and then
            (PA > TA and then PA < TA + 180) then
        return ANGLE_ERR;
      end if;

    end if;
  end ANGLE_CHECK;

  if SET(WIND_S) /= SET(WIND_A) then
    -- Check wind. Angle and speed both set or both not set.
    return WIND_ERR;
  elsif (not SET(TRAJ_A)) and then SET(TRAJ_S) then
    -- Check trajectory. If angle is unknown then speed must be unknown
    return TRAJ_ERR;
  end if;

  return OK;

end CHECK;
