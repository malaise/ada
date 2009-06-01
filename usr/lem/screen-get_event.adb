with Timers;
separate (Screen)

-- Get an event
-- type Evt_Kind_List is (Move_Key, Move_Click, Break, Other_Key,
--                        Timeout, Refresh);
-- type Mvt_Kind_List is (Right_Key, Left_Key, Super_Right_Key, Super_Left_Key,
--                        Up_Key, Down_Key, Super_Up_Key, Super_Down_Key);
--  type Evt_Rec (Evt : Evt_Kind_List := Refresh) is record
--    case Evt is
--      when Move_Key | Move_Click =>
--        Mvt : Mvt_Kind_List;
--      when others =>
--        null;
--    end case;
--  end record;

function Get_Event (Wait : in Duration) return Evt_Rec is
  Str    : Wide_String (1 .. 1);
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
  -- Position and color for blind get
  Con_Io.Move (Get_Pos);
  Con_Io.Set_Foreground (Con_Io.Get_Background);
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
        return (Move_Key, Up_Key);
      when Con_Io.Down =>
        return (Move_Key, Down_Key);
      when Con_Io.Ctrl_Up =>
        return (Move_Key, Super_Up_Key);
      when Con_Io.Ctrl_Down =>
        return (Move_Key, Super_Down_Key);
      when Con_Io.Left =>
        return (Move_Key, Left_Key);
      when Con_Io.Right =>
        return (Move_Key, Right_Key);
      when Con_Io.Ctrl_Left =>
        return (Move_Key, Super_Left_Key);
      when Con_Io.Ctrl_Right =>
        return (Move_Key, Super_Right_Key);
      when Con_Io.Tab | Con_Io.Ret =>
        return (Evt => Next);
      when Con_Io.Full =>
        if Str(1) = 'g' then
          -- Grab / ungrab pointer
          Pointer_Grabbed := not Pointer_Grabbed;
          Con_Io.Set_Pointer_Shape (Con_Io.None, Pointer_Grabbed);
        elsif Str(1) = ' ' then
          -- Pause / resume game
          return (Evt => Pause);
        end if;
      when Con_Io.Stab =>
        return (Evt => Prev);
      when Con_Io.Break | Con_Io.Esc =>
        return (Evt => Break);
      when Con_Io.Timeout =>
        Prev_Click_Action := Prev_Action;
        Prev_Click_Time := Prev_Time;
        return (Evt => Timeout);
      when Con_Io.Refresh =>
        Prev_Click_Action := Prev_Action;
        Prev_Click_Time := Prev_Time;
        return (Evt => Refresh);
      when Con_Io.Mouse_Button =>
        Con_Io.Get_Mouse_Event (Mouse_Status);
        if Mouse_Status.Status = Con_Io.Pressed then
          if Mouse_Status.Button = Con_Io.Up then
            return (Move_Click, Up_Key);
          elsif Mouse_Status.Button = Con_Io.Down then
            return (Move_Click, Down_Key);
          elsif Mouse_Status.Button = Con_Io.Left then
            Prev_Click_Action := Left_Key;
            if Prev_Action = Left_Key
            and then Now - Prev_Time <= Delta_Double then
              return (Move_Click, Super_Left_Key);
            else
              return (Move_Click, Left_Key);
            end if;
          elsif Mouse_Status.Button = Con_Io.Right then
            Prev_Click_Action := Right_Key;
            if Prev_Action = Right_Key
            and then Now - Prev_Time <= Delta_Double then
              return (Move_Click, Super_Right_Key);
            else
              return (Move_Click, Right_Key);
            end if;
          elsif Mouse_Status.Button = Con_Io.Middle then
            return (Evt => Next);
          end if;
        end if;
      when others =>
        null;
    end case;
  end loop;
end Get_Event;

