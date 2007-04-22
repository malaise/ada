with Ada.Calendar; use Ada.Calendar;
with Con_Io, Timers;
with Sok_Display, Sok_Time;
package body Sok_Input is

  Play : Boolean := True;
  Delta_Get : constant Con_Io.Delay_Rec(Timers.Delay_Sec)
            := (Delay_Kind => Timers.Delay_Sec,
                Period => Con_Io.No_Period,
                Delay_Seconds => 1.0);

  function Get_Key return Key_List is
    Str  : Wide_String (1 .. 1);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    use Con_Io;
  begin
    loop
      if Play then
        Sok_Time.Disp_Time;
      end if;
      Con_Io.Get (Str, Last, Stat, Pos, Ins,
                  Time_Out => Delta_Get,
                  Echo     => False);
      if Play then
        case Stat is
          when Up => return Up;
          when Down => return Down;
          when Pgup | Pgdown | Ctrl_Pgup | Ctrl_Pgdown
             | Ctrl_Up | Ctrl_Down | Ctrl_Right | Ctrl_Left =>
            null;
          when Left => return Left;
          when Right => return Right;
          when Full =>
            case Con_Io.Wide_To_String(Str)(1) is
              when 'u' | 'U' =>
                return Undo;
              when 'w' | 'W' =>
                Play := not Play;
                Con_Io.Clear;
                Sok_Time.Stop_Time;
              when ' '  =>
                return Next;
              when others => null;
            end case; -- on char
          when Tab | Stab  => null;
          when Ret => return Next;
          when Esc => return Esc;
          when Break => raise Break_Requested;
          when Mouse_Button => null;
          when Timeout => null;
          when Fd_Event | Timer_Event | Signal_Event | Con_Io.Wakeup_Event =>
            null;
          when Refresh =>
            return Refresh;
        end case;
      else
        if Stat = Full
        and then (Str(1) = 'w' or else Str(1) = 'W') then
          Play := not Play;
          Sok_Time.Start_Time;
          return Refresh;
        end if;
      end if;
    end loop;
  end Get_Key;

  procedure Pause is
    Str  : Wide_String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    use Con_Io;
  begin
    loop
      Con_Io.Get (Str, Last, Stat, Pos, Ins,
                  Time_Out => Delta_Get,
                  Echo     => False);
      case Stat is
        when Up => return;
        when Down => return;
        when Pgup | Pgdown | Ctrl_Pgup | Ctrl_Pgdown
           | Ctrl_Up | Ctrl_Down | Ctrl_Right | Ctrl_Left =>
          null;
        when Left => return;
        when Right => return;
        when Full => return;
        when Tab | Stab  => null;
        when Ret => return;
        when Esc => return;
        when Break => raise Break_Requested;
        when Mouse_Button => null;
        when Timeout => null;
        when Fd_Event | Timer_Event | Signal_Event | Con_Io.Wakeup_Event =>
          null;
        when Refresh => null;
      end case;
    end loop;
  end Pause;

  procedure End_Of_Program is
  begin
    null;
  end End_Of_Program;

end Sok_Input;
