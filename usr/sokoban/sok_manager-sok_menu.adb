separate (Sok_Manager)

function Sok_Menu (Allow_Write : Boolean) return Menu_Result_Rec is

  Cur_Action : Sok_Display.Menu_Action_List := Sok_Display.Reset;
  Key : Sok_Input.Key_List;

  subtype Menu_Error_List is Sok_Display.Error_List
   range Sok_Display.No_Frame .. Sok_Display.Score_Io;
  procedure Put_Menu_Error (Error : in Menu_Error_List) is
  begin
    Sok_Display.Put_Error (Error);
    declare
      Key : Sok_Input.Key_List;
    begin
      Key := Sok_Input.Get_Key;
    exception
      when Sok_Input.Break_Requested =>
        Sok_Display.Clear_Error;
        raise;
      when others =>
        Sok_Display.Clear_Error;
    end;
  end Put_Menu_Error;

  procedure Back is
  begin
    Sok_Display.Clear_Menu;
    Sok_Display.Put_Help (Sok_Display.Frame);
  end Back;

  procedure Redisplay is
  begin
    Sok_Display.Put_Frame (State.Frame);
    if not Allow_Write then
      Set_Blink (State.Frame, True);
    end if;
    Sok_Display.Put_Help (Cur_Action);
    Sok_Display.Put_Menu (Cur_Action, Allow_Write);
    Sok_Time.Disp_Time;
  end Redisplay;

  use Sok_Display;
begin
  Sok_Display.Clear_Menu;
  Sok_Display.Put_Menu (Cur_Action, Allow_Write);
  loop
    Sok_Display.Put_Help (Cur_Action);
    Key := Sok_Input.Get_Key;
    case Key is
      when Sok_Input.Right =>
        if Cur_Action /= Sok_Display.Menu_Action_List'Last then
          Cur_Action := Sok_Display.Menu_Action_List'Succ (Cur_Action);
        else
          Cur_Action := Sok_Display.Menu_Action_List'First;
        end if;
        if Cur_Action = Sok_Display.Write and not Allow_Write then
          Cur_Action := Sok_Display.Read;
        end if;
        Sok_Display.Update_Menu (Cur_Action);
      when Sok_Input.Left =>
        if Cur_Action /= Sok_Display.Menu_Action_List'First then
          Cur_Action := Sok_Display.Menu_Action_List'Pred (Cur_Action);
        else
          Cur_Action := Sok_Display.Menu_Action_List'Last;
        end if;
        if Cur_Action = Sok_Display.Write and not Allow_Write then
          Cur_Action := Sok_Display.Break;
        end if;
        Sok_Display.Update_Menu (Cur_Action);
      when Sok_Input.Up | Sok_Input.Down | Sok_Input.Undo =>
        null;
      when Sok_Input.Next =>
        -- validation
        Sok_Display.Clear_Menu;
        case Cur_Action is
          when Sok_Display.Read =>
            Sok_Display.Put_Menu (Cur_Action, Allow_Write);
            begin
              Sok_File.Restore (State);
              State.Score := Sok_File.Read_Score(State.No_Frame);
              Back;
              return (Result => Restart_Frame, Update_State => Update_Time);
            exception
              when Sok_File.Frame_File_Not_Found =>
                Put_Menu_Error (No_Frame);
                Back;
                return (Result => Go_On);
              when Sok_File.Error_Reading_Frame =>
                Put_Menu_Error (Restore);
                Back;
                return (Result => Go_On);
              when Sok_File.Score_Io_Error =>
                Put_Menu_Error (Score_Io);
                State.Score.Set := False;
                Back;
                return (Result => Go_On);
            end;
          when Sok_Display.Write =>
            Sok_Display.Put_Menu (Cur_Action, Allow_Write);
            begin
              Sok_File.Save (State);
              Back;
              return (Result => Go_On);
            exception
              when Sok_File.Error_Writing_Frame =>
                Put_Menu_Error (Save);
                Back;
                return (Result => Go_On);
            end;
          when Sok_Display.Reset =>
            Back;
            return (Result => Restart_Frame, Update_State => Reset_All);
          when Sok_Display.Get_New =>
            -- Loop while redisplay
            loop
              Sok_Display.Put_Menu (Cur_Action, Allow_Write);
              declare
                Got_No : Sok_Types.Frame_Range;
                Result : Sok_Display.Get_Result_List;
              begin
                Sok_Display.Get_No_Frame (Got_No, Result);
                case Result is
                  when Sok_Display.Set =>
                    Back;
                    State.No_Frame := Got_No;
                    return (Result => Restart_Frame, Update_State => Reset_All);
                  when Sok_Display.Refresh =>
                    Redisplay;
                  when Sok_Display.Esc =>
                    exit;
                end case;
              exception
                when Sok_Display.Format_Error =>
                  Put_Menu_Error (Format);
                  Back;
                  Sok_Display.Clear_Menu;
                  Sok_Display.Put_Menu (Cur_Action, Allow_Write);
              end;
            end loop;
          when Sok_Display.Break =>
            Back;
            raise Sok_Input.Break_Requested;
        end case;
      when Sok_Input.Esc =>
        if Allow_Write then
          Sok_Display.Put_Help (Sok_Display.Frame);
        else
          Sok_Display.Put_Help (Sok_Display.Done);
        end if;
        Sok_Display.Clear_Menu;
        return (Result => Go_On);
      when Sok_Input.Refresh =>
        Redisplay;
    end case;
  end loop;

end Sok_Menu;
