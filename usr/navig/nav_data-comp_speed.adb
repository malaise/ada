separate (NAV_DATA)
-- When PLAN.SPEED and TRAJ.SPEED are unknown
procedure COMP_SPEED (DATA : in out T_DATA) is
  -- angle_traj-angle_wind ...
  A_T_W, A_T_P, A_P_W, A_P_T : REAL;
  -- sin(ATW)/sin(ATP) and sin(APW)/sin(APS)
  S_WP, S_WS : REAL;
  use NAV_TYPES;
begin
  -- Compute angles substractions
  A_T_W := TO_REAL (ANGLE => DATA.TRAJ.ANGLE - DATA.WIND.ANGLE);
  A_T_P := TO_REAL (ANGLE => DATA.TRAJ.ANGLE - DATA.PLAN.ANGLE);
  A_P_W := TO_REAL (ANGLE => DATA.PLAN.ANGLE - DATA.WIND.ANGLE);
  A_P_T := TO_REAL (ANGLE => DATA.PLAN.ANGLE - DATA.TRAJ.ANGLE);

  -- Compute angles sin rapports
  S_WP := abs (MY_MATH.SIN (A_T_W, MY_MATH.DEGREE) / MY_MATH.SIN (A_T_P, MY_MATH.DEGREE));
  S_WS := abs (MY_MATH.SIN (A_P_W, MY_MATH.DEGREE) / MY_MATH.SIN (A_P_T, MY_MATH.DEGREE));

  -- PLAN.SPEED := WIND.SPEED * S_WP
  --  and TRAJ.SPEED := WIND.SPEED * S_WS
  DATA.PLAN.SPEED := DATA.WIND.SPEED * NAV_TYPES.T_SPEED(S_WP);
  DATA.TRAJ.SPEED := DATA.WIND.SPEED * NAV_TYPES.T_SPEED(S_WS);
end COMP_SPEED;

