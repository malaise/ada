separate (Nav_Data)
-- When Plan.Speed and Traj.Speed are unknown
procedure Comp_Speed (Data : in out T_Data) is
  -- angle_traj-angle_wind ...
  A_T_W, A_T_P, A_P_W, A_P_T : Real;
  -- sin(Atw)/sin(Atp) and sin(Apw)/sin(Aps)
  S_Wp, S_Ws : Real;
  use Nav_Types;
begin
  -- Compute angles substractions
  A_T_W := To_Real (Angle => Data.Traj.Angle - Data.Wind.Angle);
  A_T_P := To_Real (Angle => Data.Traj.Angle - Data.Plan.Angle);
  A_P_W := To_Real (Angle => Data.Plan.Angle - Data.Wind.Angle);
  A_P_T := To_Real (Angle => Data.Plan.Angle - Data.Traj.Angle);

  -- Compute angles sin rapports
  S_Wp := abs (My_Math.Sin (A_T_W, My_Math.Degree) / My_Math.Sin (A_T_P, My_Math.Degree));
  S_Ws := abs (My_Math.Sin (A_P_W, My_Math.Degree) / My_Math.Sin (A_P_T, My_Math.Degree));

  -- Plan.Speed := Wind.Speed * S_Wp
  --  and Traj.Speed := Wind.Speed * S_Ws
  Data.Plan.Speed := Data.Wind.Speed * Nav_Types.T_Speed(S_Wp);
  Data.Traj.Speed := Data.Wind.Speed * Nav_Types.T_Speed(S_Ws);
end Comp_Speed;

