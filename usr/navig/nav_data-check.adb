separate (Nav_Data)

function Check (Problem : T_Data) return T_Consistency is
  Set : constant T_Data_Set := Problem.Set;
begin
  -- Check number of unkown data
  Unknown_Check:
  declare
    N_Unknown : Natural := 0;
  begin
    for I in T_List_Data loop
      if not Set(I) then N_Unknown := N_Unknown + 1; end if;
    end loop;
    if N_Unknown /= 3 then
      return Known_Err;
    end if;
  end Unknown_Check;

  -- Check drift.
  if Set(Drift) then
    -- one in traj angle or plan angle must be unknown
    if Set(Plan_A) and then Set(Traj_A) then
      return Drift_Err;
    end if;
  end if;

  -- Check the 3 vector angles
  Angle_Check:
  declare
    Pa, Ta, Wa : Nav_Types.T_Angle;
    A_Set : Boolean;
    use Nav_Types;
  begin
    -- See if PLAN_A and TRAJ_A are set
    if Set(Drift) then
      A_Set := Set(Plan_A) or else Set(Traj_A);
    else
      A_Set := Set(Plan_A) and then Set(Traj_A);
    end if;
    A_Set := A_Set and then Set(Wind_A);

    if A_Set then
      if Set(Drift) then
        if Set(Plan_A) then
          Pa := Problem.Plan.Angle;
          Ta := Pa + Problem.Drift;
        else
          Ta := Problem.Traj.Angle;
          Pa := Ta - Problem.Drift;
        end if;
      else
        Pa := Problem.Plan.Angle;
        Ta := Problem.Traj.Angle;
      end if;
      Wa := Problem.Wind.Angle;
      -- check that the 3 angles are mod 180 different
      if Wa = Pa or else Wa = Pa + 180 or else
         Pa = Ta or else Pa = Ta + 180 or else
         Ta = Wa or else Ta = Wa + 180 then
        return Angle_Err;
      end if;
      -- check that WIND and PLAN are on both sides of TRAJ
      if (Wa < Ta or else Wa > Ta + 180) and then
         (Pa < Ta or else Pa > Ta + 180) then
        return Angle_Err;
      elsif (Wa > Ta and then Wa < Ta + 180) and then
            (Pa > Ta and then Pa < Ta + 180) then
        return Angle_Err;
      end if;

    end if;
  end Angle_Check;

  if Set(Wind_S) /= Set(Wind_A) then
    -- Check wind. Angle and speed both set or both not set.
    return Wind_Err;
  elsif (not Set(Traj_A)) and then Set(Traj_S) then
    -- Check trajectory. If angle is unknown then speed must be unknown
    return Traj_Err;
  end if;

  return Ok;

end Check;
