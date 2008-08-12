with Timers;
separate (Screen)

-- Get a key
-- type Got_List is (Right_Key, Left_Key, Up_Key, Down_Key, Break, Refresh,
--                     Timeout);
function Get_Key (Wait : in Duration) return Got_List is
  Str    : Wide_String (1 .. 0);
  Last   : Natural;
  Stat   : Con_Io.Curs_Mvt;
  Pos    : Positive;
  Insert : Boolean;
  Time_Out : Con_Io.Delay_Rec;
  Mouse_Status : Con_Io.Mouse_Event_Rec;
  Prev_Action : Repeat_Action_List;
  Prev_Time : Ada.Calendar.Time;
  Now : Ada.Calendar.Time;
  Delta_Double : constant Duration := 0.5;
  use type Ada.Calendar.Time,
           Con_Io.Mouse_Button_Status_List, Con_Io.Mouse_Button_List;
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
  -- Values before loop, for check
  Prev_Action := Prev_Click_Action;
  Prev_Time := Prev_Click_Time;
  loop
    -- Get key
    Con_Io.Get (Str, Last, Stat, Pos, Insert, Time_Out => Time_Out);
    Now := Ada.Calendar.Clock;
    -- (Default) values for return
    Prev_Click_Action := None;
    Prev_Click_Time := Now;
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
        Prev_Click_Action := Prev_Action;
        Prev_Click_Time := Prev_Time;
        return Timeout;
      when Con_Io.Refresh =>
        Prev_Click_Action := Prev_Action;
        Prev_Click_Time := Prev_Time;
        return Refresh;
      when Con_Io.Mouse_Button =>
        Con_Io.Get_Mouse_Event (Mouse_Status);
        if Mouse_Status.Status = Con_Io.Pressed then
          if Mouse_Status.Button = Con_Io.Up then
            return Up_Key;
          elsif Mouse_Status.Button = Con_Io.Down then
            return Down_Key;
          elsif Mouse_Status.Button = Con_Io.Left then
            Prev_Click_Action := Left_Key;
            if Prev_Action = Left_Key
            and then Now - Prev_Time <= Delta_Double then
              return Super_Left_Key;
            else
              return Left_Key;
            end if;
          elsif Mouse_Status.Button = Con_Io.Right then
            Prev_Click_Action := Right_Key;
            if Prev_Action = Right_Key
            and then Now - Prev_Time <= Delta_Double then
              return Super_Right_Key;
            else
              return Right_Key;
            end if;
          end if;
        end if;
      when others =>
        null;
    end case;
  end loop;
end Get_Key;

