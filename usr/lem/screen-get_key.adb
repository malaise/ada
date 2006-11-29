with Ada.Calendar;
with Timers;
separate (Screen)

-- Get a key
-- type Got_List is (Right_Key, Left_Key, Up_Key, Down_Key, Break, Refresh,
--                     Timeout);
function Get_Key (Wait : in Duration) return Got_List is
  Str    : String (1 .. 0);
  Last   : Natural;
  Stat   : Con_Io.Curs_Mvt;
  Pos    : Positive;
  Insert : Boolean;
  Time_Out : Con_Io.Delay_Rec;
  use type Ada.Calendar.Time;
begin
  -- Compute expiration time
  if Wait >= 0.0 then
    Time_Out := (Delay_Kind => Timers.Delay_Exp,
                 Period => Timers.No_Period,
                 Expiration_Time => Ada.Calendar.Clock + Wait);
  else
    Time_Out := (Delay_Kind => Timers.Delay_Sec,
                 Period => Timers.No_Period,
                 Delay_Seconds => Timers.Infinite_Seconds);
  end if;
  loop
    -- Get key
    Con_Io.Get (Str, Last, Stat, Pos, Insert, Time_Out => Time_Out);
    -- Map
    case Stat is
      when Con_Io.Up =>
        return Up_Key;
      when Con_Io.Down =>
        return Down_Key;
      when Con_Io.Ctrl_Up =>
        return Super_Up_Key;
      when Con_Io.Ctrl_Down =>
        return Super_Down_Key;
      when Con_Io.Left =>
        return Left_Key;
      when Con_Io.Right =>
        return Right_Key;
      when Con_Io.Ctrl_Left =>
        return Super_Left_Key;
      when Con_Io.Ctrl_Right =>
        return Super_Right_Key;
      when Con_Io.Full | Con_Io.Ret =>
        return Other_Key;
      when Con_Io.Esc | Con_Io.Break =>
        return Break;
      when Con_Io.Timeout =>
        return Timeout;
      when Con_Io.Refresh =>
        return Refresh;
      when others =>
        null;
    end case;
  end loop;
end Get_Key;

