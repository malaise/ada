-- Type of a navigation question, resolution  with check of data coherency

with Nav_Types;

package Nav_Data is

  -- The different values which can be set or not
  type T_List_Data is (Wind_S, Wind_A, Plan_S, Plan_A, Traj_S, Traj_A, Drift);
  type T_Data_Set  is array (T_List_Data) of Boolean;

  -- A complete definition of a nav problem
  type T_Data is record
   Wind : Nav_Types.T_Vector;
   Plan : Nav_Types.T_Vector;
   Traj : Nav_Types.T_Vector;
   Drift : Nav_Types.T_Drift;
   Set  : T_Data_Set := (others => False);
  end record;

  -- The possible reports of a problem check
  type T_Consistency is (
   Known_Err, -- Must be 3 unknown data
   Angle_Err, -- If all 3 angles are known, must have different directions
   Wind_Err,  -- Wind must be completly known or unknown
   Traj_Err,  -- If Traj_A is unknown, Traj_S must be unknown
   Drift_Err, -- If Drift is known, one in Traj_A or Plan_A must be unknown
   Val_Err,   -- If problem of computation (incompatible values)
   Ok);       -- Data are consistent

  procedure Resolution (Problem : in T_Data;
   Check : out T_Consistency; Solution : out T_Data);

  Resol_Error : exception;

end Nav_Data;


