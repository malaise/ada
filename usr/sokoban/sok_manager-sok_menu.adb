separate (Sok_Manager)

function Sok_Menu (Done : Boolean) return Menu_Result_List is


  Cur_Action : Sok_Display.Menu_Action_List := Sok_Display.Reset;
  Prev_Click, Curr_Click : Sok_Display.Got_Action_List := Sok_Display.Done;
  Key : Sok_Input.Key_List;
  Mouse : Sok_Input.Mouse_Event_Rec;

  subtype Menu_Error_List is Sok_Display.Error_List
   range Sok_Display.No_Frame .. Sok_Display.Score_Io;
  procedure Put_Menu_Error (Error : in Menu_Error_List) is
  begin
    Sok_Display.Put_Error (Error);
    declare
      Key : Sok_Input.Key_List;
      pragma Unreferenced (Key);
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
    if Done then
      Sok_Display.Put_Help (Sok_Display.Done);
    else
      Sok_Display.Put_Help (Sok_Display.Frame);
    end if;
  end Back;

  procedure Redisplay is
  begin
    Sok_Display.Put_Frame (State.Frame);
    Set_Blink (State.Frame, Done);
    Sok_Display.Put_Help (Cur_Action);
    Sok_Display.Put_Menu (Cur_Action, True);
    Sok_Time.Disp_Time;
  end Redisplay;

  use Sok_Display;
  use type Sok_Input.Key_List;
begin
  Sok_Display.Clear_Menu;
  Sok_Display.Put_Menu (Cur_Action, True);
  loop
    Sok_Display.Put_Help (Cur_Action);
    Key := Sok_Input.Get_Key;

    -- Handle click / release
    -- Map valid release to Next
    if Key = Sok_Input.Mouse then
      -- Mouse click or release
      -- Get position
      Mouse := Sok_Input.Get_Mouse;
      -- Always reset previous click
      if Prev_Click /= Sok_Display.Done then
        Sok_Display.Update_Menu (Prev_Click, False);
      end if;
      -- Get menu entry corresponding to mouse position of click/release
      Curr_Click := Sok_Display.Get_Action (Mouse.Row, Mouse.Col);
      if Curr_Click /= Sok_Display.Done then
        -- Click/Release is valid
        if Mouse.Click then
          -- Valid click => show and store
          Sok_Display.Update_Menu (Curr_Click, True);
          Prev_Click := Curr_Click;
          -- Put help on this selection
          Cur_Action := Curr_Click;
        elsif Curr_Click = Prev_Click then
          -- Release in same field as click (otherwise discard) => validate
          Key := Sok_Input.Next;
        end if;
      end if;
    end if;

    case Key is
      when Sok_Input.Right =>
        if Cur_Action /= Sok_Display.Menu_Action_List'Last then
          Cur_Action := Sok_Display.Menu_Action_List'Succ (Cur_Action);
        else
          Cur_Action := Sok_Display.Menu_Action_List'First;
        end if;
        Sok_Display.Update_Menu (Cur_Action, False);
      when Sok_Input.Left =>
        if Cur_Action /= Sok_Display.Menu_Action_List'First then
          Cur_Action := Sok_Display.Menu_Action_List'Pred (Cur_Action);
        else
          Cur_Action := Sok_Display.Menu_Action_List'Last;
        end if;
        Sok_Display.Update_Menu (Cur_Action, False);
      when Sok_Input.Up | Sok_Input.Down | Sok_Input.Undo =>
        null;
      when Sok_Input.Mouse =>
        -- Intermediate or invalide mouse event
        null;
      when Sok_Input.Next =>
        -- Validation
        Sok_Display.Clear_Menu;
        case Cur_Action is
          when Sok_Display.Read =>
            Sok_Display.Put_Menu (Cur_Action, True);
            begin
              Sok_File.Restore (State);
              State.Score := Sok_File.Read_Score(State.No_Frame);
              Back;
              return Update_Time;
            exception
              when Sok_File.Frame_File_Not_Found =>
                Put_Menu_Error (No_Frame);
                Back;
                return Go_On;
              when Sok_File.Error_Reading_Frame =>
                Put_Menu_Error (Restore);
                Back;
                return Go_On;
              when Sok_File.Score_Io_Error =>
                Put_Menu_Error (Score_Io);
                State.Score.Set := False;
                Back;
                return Go_On;
            end;
          when Sok_Display.Write =>
            Sok_Display.Put_Menu (Cur_Action, True);
            begin
              Sok_File.Save (State);
              Back;
              return Go_On;
            exception
              when Sok_File.Error_Writing_Frame =>
                Put_Menu_Error (Save);
                Back;
                return Go_On;
            end;
          when Sok_Display.Reset =>
            Back;
            return Reset_All;
          when Sok_Display.Get_New =>
            -- Loop while redisplay
            loop
              Sok_Display.Put_Menu (Cur_Action, True);
              declare
                Got_No : Sok_Types.Frame_Range;
                Result : Sok_Display.Get_Result_List;
              begin
                Sok_Display.Get_No_Frame (Got_No, Result);
                case Result is
                  when Sok_Display.Set =>
                    Back;
                    State.No_Frame := Got_No;
                    return Reset_All;
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
                  Sok_Display.Put_Menu (Cur_Action, True);
              end;
            end loop;
          when Sok_Display.Break =>
            Back;
            raise Sok_Input.Break_Requested;
        end case;
      when Sok_Input.Esc =>
        Back;
        return Go_On;
      when Sok_Input.Refresh =>
        Redisplay;
    end case;
  end loop;

end Sok_Menu;
