with Timers, Language, Lower_Char;
with Sok_Time, Sok_Display;
package body Sok_Input is

  Play : Boolean := True;
  Delta_Get : constant Con_Io.Delay_Rec(Timers.Delay_Sec)
            := (Delay_Kind    => Timers.Delay_Sec,
                Clock         => null,
                Period        => Con_Io.No_Period,
                Delay_Seconds => 1.0);

  Mouse_Event : Mouse_Event_Rec;
  No_Event : constant Mouse_Event_Rec := Mouse_Event;
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  function Get_Key return Key_List is
    Str  : Con_Io.Unicode_Sequence (1 .. 1);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    use Con_Io;
  begin
    if not Console.Is_Open then
      Console := Sok_Display.Get_Console;
      Screen.Set_To_Screen (Console'Access);
    end if;
    Mouse_Event := No_Event;
    loop
      if Play then
        Sok_Time.Disp_Time;
      end if;
      Screen.Get (Str, Last, Stat, Pos, Ins,
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
            declare
              Lstr : constant String := Language.Copy(Str);
              C : constant Character := Lower_Char (Lstr(1));
            begin
              case C is
                when 'u' =>
                  return Undo;
                when 'w' =>
                  Play := not Play;
                  Screen.Clear;
                  Sok_Time.Stop_Time;
                when ' '  =>
                  return Next;
                when others => null;
              end case; -- on char
            end;
          when Tab | Stab  => null;
          when Ret => return Next;
          when Esc => return Esc;
          when Break => raise Break_Requested;
          when Mouse_Button =>
            declare
              Evt : Con_Io.Mouse_Event_Rec;
            begin
              Console.Get_Mouse_Event (Evt);
              if Evt.Valid and then Evt.Button = Con_Io.Left
              and then Evt.Status /= Motion then
                Mouse_Event := (True, Evt.Status = Con_Io.Pressed,
                                Evt.Row, Evt.Col);
                return Mouse;
              end if;
            end;
          when Selection => null;
          when Timeout => null;
          when Fd_Event | Timer_Event | Signal_Event =>
            null;
          when Refresh =>
            return Refresh;
        end case;
      else
        if Stat = Full
        and then Lower_Char (Language.Unicode_To_Char (Str(1))) = 'w' then
          Play := not Play;
          Sok_Time.Start_Time;
          return Refresh;
        end if;
      end if;
    end loop;
  end Get_Key;

  function Get_Mouse return Mouse_Event_Rec is
  begin
    return Mouse_Event;
  end Get_Mouse;

  procedure Pause is
    Str  : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    use Con_Io;
  begin
    if not Console.Is_Open then
      Console := Sok_Display.Get_Console;
      Screen.Set_To_Screen (Console'Access);
    end if;
    Mouse_Event := No_Event;
    loop
      Screen.Get (Str, Last, Stat, Pos, Ins,
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
        when Break =>
          -- Pause is always called on error before exiting
          return;
        when Mouse_Button => null;
        when Timeout => null;
        when Selection => null;
        when Fd_Event | Timer_Event | Signal_Event =>
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
