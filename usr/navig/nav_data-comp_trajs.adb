separate (Nav_Data)
-- when only unknown is TRAJ_S after set_before
procedure Comp_Trajs (Data : in out T_Data) is
  -- TRAJ_S = WIND_S*COS(TRAJ_A-WIND_A) + PLAN_S*COS(TRAJ_A-PLAN_A)
  Crtw, Crtp : Real;
  use Nav_Types;
begin
  Crtw := My_Math.Cos ( To_Real
   (Nav_Types.T_Angle'(Data.Traj.Angle - Data.Wind.Angle) ), My_Math.Degree );
  Crtp := My_Math.Cos ( To_Real
   (Nav_Types.T_Angle'(Data.Traj.Angle - Data.Plan.Angle) ), My_Math.Degree );
  Data.Traj.Speed := Nav_Types.T_Speed (My_Math.Real(Data.Wind.Speed) * Crtw
                                      + My_Math.Real(Data.Plan.Speed) * Crtp);
exception
  when others => raise Comp_Err;
end Comp_Trajs;
