-- Type of a navigation question, resolution  with check of data coherency

with NAV_TYPES;

package NAV_DATA is

  -- The different values which can be set or not
  type T_LIST_DATA is (WIND_S, WIND_A, PLAN_S, PLAN_A, TRAJ_S, TRAJ_A, DRIFT);
  type T_DATA_SET  is array (T_LIST_DATA) of BOOLEAN;

  -- A complete definition of a nav problem
  type T_DATA is record
   WIND : NAV_TYPES.T_VECTOR;
   PLAN : NAV_TYPES.T_VECTOR;
   TRAJ : NAV_TYPES.T_VECTOR;
   DRIFT : NAV_TYPES.T_DRIFT;
   SET  : T_DATA_SET := (others => FALSE);
  end record;

  -- The possible reports of a problem check
  type T_CONSISTENCY is (
   KNOWN_ERR, -- Must be 3 unknown data
   ANGLE_ERR, -- If all 3 angles are known, must have different directions
   WIND_ERR,  -- Wind must be completly known or unknown
   TRAJ_ERR,  -- If TRAJ_A is unknown, TRAJ_S must be unknown
   DRIFT_ERR, -- If drift is known, one in TRAJ_A or PLAN_A must be unknown
   VAL_ERR,   -- If problem of computation (incompatible values)
   OK);       -- Data are consistent

  procedure RESOLUTION (PROBLEM : in T_DATA;
   CHECK : out T_CONSISTENCY; SOLUTION : out T_DATA);

  RESOL_ERROR : exception;

end NAV_DATA;


