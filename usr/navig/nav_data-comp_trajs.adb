separate (NAV_DATA)
-- when only unknown is TRAJ_S after set_before
procedure COMP_TRAJS (DATA : in out T_DATA) is
  -- TRAJ_S = WIND_S*COS(TRAJ_A-WIND_A) + PLAN_S*COS(TRAJ_A-PLAN_A)
  CRTW, CRTP : REAL;
  use NAV_TYPES;
begin
  CRTW := MATH.COS ( TO_REAL
   (NAV_TYPES.T_ANGLE'(DATA.TRAJ.ANGLE - DATA.WIND.ANGLE) ), MATH.DEGREE );
  CRTP := MATH.COS ( TO_REAL
   (NAV_TYPES.T_ANGLE'(DATA.TRAJ.ANGLE - DATA.PLAN.ANGLE) ), MATH.DEGREE );
  DATA.TRAJ.SPEED := NAV_TYPES.T_SPEED (MATH.REAL(DATA.WIND.SPEED) * CRTW
                                      + MATH.REAL(DATA.PLAN.SPEED) * CRTP);
exception
  when others => raise COMP_ERR;
end COMP_TRAJS;
