-- Check of a navigation question coherency
with My_Math;
use My_Math;
package body Nav_Data is

  -- For math computations of angles
  subtype Real is My_Math.Real;

  -- problem in computation
  Comp_Err : exception;

  -- Utilities to convert angles
  -- From deg & min to real
  function To_Real (Deg : Nav_Types.T_Degree; Min : Nav_Types.T_Minute := 0)
   return Real is
  begin
    return Real (Deg) + Real (Min) / 60.0;
  end To_Real;

   -- From angle to real
  function To_Real (Angle : Nav_Types.T_Angle) return Real is
  begin
    return Real (Angle.Degrees) + Real (Angle.Minutes) / 60.0;
  end To_Real;

  -- From drift to real
  function To_Real (Drift : Nav_Types.T_Drift) return Real is
  begin
    if Drift.Positiv then
      return Real (Drift.Degrees) + Real (Drift.Minutes) / 60.0;
    else
      return - (Real (Drift.Degrees) + Real (Drift.Minutes) / 60.0);
    end if;
  end To_Real;

  -- From real to angle
  function To_Angle (R : Real) return Nav_Types.T_Angle is
    Loc_R : Real := R;
    Deg : Nav_Types.T_Common_Deg;
    Min : Natural;
    Ret_Angle : Nav_Types.T_Angle;
    use Nav_Types;
  begin

    -- reduction of R
    while Loc_R > Real (Nav_Types.T_Degree'Last) loop
      Loc_R := Loc_R -
       Real (Nav_Types.T_Common_Deg(Nav_Types.T_Degree'Last) + 1);
    end loop;
    while Loc_R < 0.0 loop
      Loc_R := Loc_R +
       Real (Nav_Types.T_Common_Deg(Nav_Types.T_Degree'Last) + 1);
    end loop;

    -- split of degrees and minutes
    Deg := Nav_Types.T_Common_Deg (My_Math.Trunc(Loc_R) );
    Min := Natural (My_Math.Round ( abs(My_Math.Frac(Loc_R)) *
     ( Real(Nav_Types.T_Minute'Last) + 1.0) ) );

    if Min <= Natural(Nav_Types.T_Minute'Last) then
      Ret_Angle.Minutes := Nav_Types.T_Minute (Min);
    else
      Deg := Deg + 1;
      Min := Min - Natural(Nav_Types.T_Minute'Last) - 1;
    end if;
    -- Done
    return Nav_Types.Reduction (Deg, Nav_Types.T_Minute(Min));
  end To_Angle;


  function Check (Problem : T_Data) return T_Consistency is separate;


  -- if Drift is known, compute plan angle and traj angles before
  --  global computation
  procedure Set_Before (Data : in out T_Data) is separate;

  -- if Drift is unknown, compute it after global computation
  procedure Set_Drift_After (Data : in out T_Data) is separate;

  -- when only unknown is Traj_S after set_before
  procedure Comp_Trajs (Data : in out T_Data) is separate;

  -- When 1 vector is unknown compute it by addition or substraction of
  --  the 2 others
  function Add (X, Y : Nav_Types.T_Vector) return Nav_Types.T_Vector
   is separate;

  -- When Plan.Speed and Traj.Speed are unknown
  procedure Comp_Speed (Data : in out T_Data) is separate;

  -- When Plan.Angle and Traj.Speed are unknown
  procedure Comp_Nav (Data : in out T_Data) is separate;


  procedure Resolution (Problem : in T_Data;
   Check : out T_Consistency; Solution : out T_Data) is
    Check_Result : T_Consistency;
    Data : T_Data;
  begin
    -- Resolution of data in out
    Data := Problem;

    -- Update wind angle
    if Data.Set(Wind_A) then
      Data.Wind.Angle := Nav_Types."+" (Data.Wind.Angle, 180);
    end if;

    -- Verification of the consistency of the problem
    Check_Result := Nav_Data.Check (Data);
    Check := Check_Result;
    if Check_Result /= Ok then
      return;
    end if;

    -- Drift is known : compute Traj angle or Plan angle, or both
    if Data.Set (Drift) then
      Set_Before (Data);
    end if;

    if Data.Set(Drift) and then Data.Set(Traj_A) and then Data.Set(Plan_A)
    and then Data.Set(Wind_A) and then Data.Set(Plan_S) and then Data.Set(Wind_S) then
      -- after set_before, last unknown is Traj_S
      Comp_Trajs (Data);
      Data.Set (Traj_S) := True;
    else

      -- 2 data are unknown toward the 6 data of the 3 vectors
      --  Compute the 3 vectors
      if not Data.Set(Wind_S) and then not Data.Set(Wind_A) then
        -- Wind := Traj - Plan
        Data.Wind := Add (Data.Traj, Nav_Types.T_Vector'(
         Speed => Data.Plan.Speed,
         Angle => Nav_Types."+" (Data.Plan.Angle, 180) ) );
        Data.Set(Wind_A) := True;
        Data.Set(Wind_S) := True;
      elsif not Data.Set(Plan_S) and then not Data.Set(Plan_A) then
        -- Plan := Traj - Wind
        Data.Plan := Add (Data.Traj, Nav_Types.T_Vector'(
         Speed => Data.Wind.Speed,
         Angle => Nav_Types."+" (Data.Wind.Angle, 180) ) );
        Data.Set(Plan_A) := True;
        Data.Set(Plan_S) := True;
      elsif not Data.Set(Traj_S) and then not Data.Set(Traj_A) then
        -- Traj := Plan + Wind
        Data.Traj := Add (Data.Plan, Data.Wind);
        Data.Set(Traj_A) := True;
        Data.Set(Traj_S) := True;
      elsif not Data.Set(Plan_S) and then not Data.Set(Traj_S) then
        -- Plan.Speed and Traj.Speed unknown
        Comp_Speed (Data);
        Data.Set(Plan_S) := True;
        Data.Set(Traj_S) := True;
      elsif not Data.Set(Plan_A) and then not Data.Set(Traj_S) then
        -- Plan.Angle and Traj.Speed unknown
        Comp_Nav (Data);
        Data.Set(Plan_A) := True;
        Data.Set(Traj_S) := True;
      else
        raise Resol_Error;
      end if;

      -- Drift is not known : compute it with Traj and Plan angles
      if not Data.Set (Drift) then
        Set_Drift_After (Data);
      end if;

    end if;

    -- Update wind angle
    Data.Wind.Angle := Nav_Types."+" (Data.Wind.Angle, 180);

    Solution := Data;
    Solution.Set := Problem.Set;
  exception
    when Comp_Err =>
      Check := Val_Err;
    when others => raise;

  end Resolution;

end Nav_Data;

