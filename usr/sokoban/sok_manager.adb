with Calendar;    -- for time

with Dos;         -- for sound
with Con_Io;      -- for reset_term

with Sok_Display;
with Sok_Input;
with Sok_Movement;
with Sok_Save;
with Sok_Time;
package body Sok_Manager is


  -- internal state of a frame
  type State_Rec is record
    Frame        : Sok_Types.Frame_Tab;
    No_Frame     : Sok_Types.Frame_Range;
    Position     : Sok_Types.Coordinate_Rec;
    Nbre_Targets : Natural;
    Box_Ok       : Natural;
    Moves        : Natural;
    Pushes       : Natural;
    Score        : Sok_Types.Score_Rec;
  end record;
  State : State_Rec;


  -- Menu return : go on with same frame or a new frame
  --  if new frame, reset_all state (read) or update time (restored)
  type Menu_Result_List is (Go_On, Restart_Frame);
  type Update_State_List is (Reset_All, Update_Time);
  type Menu_Result_Rec (Result : Menu_Result_List := Go_On) is record
    case Result is
      when Go_On     => null;
      when Restart_Frame => Update_State : Update_State_List;
    end case;
  end record;

  -- frames reading, saving and restoring.
  package Sok_File is

    -- to read a new frame
    procedure Read (No_Frame : in  Sok_Types.Frame_Range;
                    Frame    : out Sok_Types.Frame_Tab);
    Data_File_Not_Found, Error_Reading_Data : exception;

    -- save a frame and recover a frame, with saved movements
    procedure Save (State : in State_Rec);
    Error_Writing_Frame : exception;

    procedure Restore (State : out State_Rec);
    Frame_File_Not_Found, Error_Reading_Frame : exception;

    -- Initialise scores, read/update score
    procedure Init_Scores;
    function Read_Score (No : Sok_Types.Frame_Range) return Sok_Types.Score_Rec;
    procedure Write_Score (No : in Sok_Types.Frame_Range;
                           Score : in Sok_Types.Score_Rec);
    Score_Io_Error : exception;
    

  end Sok_File;

  procedure Play_Frame (Update_State : out Update_State_List);

  Internal_Error : exception;

  package body Sok_File is separate;


  procedure Play_Game (First_Frame : in Sok_Types.Frame_Range) is
    Frame_Result : Update_State_List;
    Found : Boolean;

    procedure End_Of_Program is
    begin
      -- End of game
      Sok_Input.End_Of_Program;

      Sok_Display.End_Of_Program;
    end End_Of_Program;

    use Sok_Types;

  begin
    begin

      -- init for first frame
      begin
        Sok_Display.Init;
      exception
        when others =>
        raise Sok_Input.Break_Requested;
      end;
      State.No_Frame := First_Frame;
      Frame_Result := Reset_All;
      begin
        Sok_File.Init_Scores;
      exception
        when Sok_File.Score_Io_Error =>
          Sok_Display.Put_Error (Sok_Display.Init_Score);
          raise;
      end;

      loop
        case Frame_Result is
          when Reset_All =>
            -- Read frame
            begin
              Sok_File.Read (State.No_Frame, State.Frame);
            exception
              when Sok_File.Data_File_Not_Found =>
                Sok_Display.Put_Error (Sok_Display.No_Data);
                raise;
              when Sok_File.Error_Reading_Data =>
                Sok_Display.Put_Error (Sok_Display.Read);
                raise;
            end;

            -- Read score
            begin
              State.Score := Sok_File.Read_Score(State.No_Frame);
            exception
              when Sok_File.Score_Io_Error =>
                Sok_Display.Put_Error (Sok_Display.Score_Io);
                raise;
            end;

            -- Init state
            State.Moves        := 0;
            State.Pushes       := 0;

            -- clear saved movements
            Sok_Save.Reset;

            -- Init time and start;
            Sok_Time.Reset_Time;
            Sok_Time.Start_Time;

            -- find man starting position and complete state
            Found := False;
            State.Nbre_Targets := 0;
            State.Box_Ok       := 0;
            for I in Sok_Types.Row_Range loop
              for J in Sok_Types.Col_Range loop
                if State.Frame (I, J).Pattern /= Sok_Types.Wall and then
                   State.Frame (I, J).Content = Sok_Types.Man then
                   if Found then
                   -- man already found !!
                    raise Internal_Error;
                  else
                    Found := True;
                    State.Position := (Row => I, Col => J);
                  end if;
                end if;

                if State.Frame (I, J).Pattern = Sok_Types.Target then
                  State.Nbre_Targets := State.Nbre_Targets + 1;
                  if State.Frame(I, J).Content = Sok_Types.Box then
                    State.Box_Ok := State.Box_Ok + 1;
                  end if;
                end if;
              end loop;
            end loop;
            if not Found then
              -- No man !!
              raise Internal_Error;
            end if;

          when Update_Time =>
            -- Init of time already done in sok_file
            Sok_Time.Start_Time;
        end case;

        -- display
        Sok_Display.Put_Frame (State.Frame);
        Sok_Display.Put_Help (Sok_Display.Frame);
        Sok_Display.Put_Score (State.Score);

        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);

        Play_Frame (Frame_Result);
      end loop;

    exception
      when Internal_Error =>
        Sok_Display.Put_Error (Sok_Display.Internal);
        raise;
    end;

  exception
    when Sok_Input.Break_Requested =>
      End_Of_Program;
    when others =>
      Sok_Input.Pause;
      Sok_Display.Clear_Error;
      End_Of_Program;
  end Play_Game;

  procedure Set_Blink (
   Frame : in Sok_Types.Frame_Tab;
   Blink : Boolean) is
   use Sok_Types;
  begin
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        if Frame (I, J).Pattern = Sok_Types.Target and then
           Frame (I, J).Content = Sok_Types.Box then
          Sok_Display.Put_Square (
           Square     => Frame (I, J),
           Coordinate => (Row=>I, Col=>J),
           Blink      => Blink);
        end if;
      end loop;
    end loop;
  end Set_Blink;

  -- if return is GO_ON, nothing to do
  -- if return is RESTART_FRAME, part of state has been set
  --  if RESET_ALL, only NO_FRAME is set (read)
  --  if UPDATE_TIME, only time has to be updated (restore)
  function Sok_Menu (Allow_Write : Boolean) return Menu_Result_Rec is separate;


  procedure Play_Frame (Update_State : out Update_State_List) is

    Key : Sok_Input.Key_List;

    Save_Score : Boolean;
    Disp_Score : Sok_Types.Score_Rec;

    Result : Sok_Movement.Result_List;

    Saved_Pos : Sok_Types.Coordinate_Rec;

    Poped_Data : Sok_Movement.Saved_Data_Rec;

    Menu_Result : Menu_Result_Rec;

    use Sok_Types, Sok_Input;

  begin
    -- Score to display
    Disp_Score := State.Score;

    -- movements
    loop
      Key := Sok_Input.Get_Key;

      if Key = Sok_Input.Esc then
        -- menu
        Menu_Result := Sok_Menu (Allow_Write => True);
        case Menu_Result.Result is
          when Go_On =>
            Key := Sok_Input.Esc;
            Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                                  State.Nbre_Targets, State.No_Frame);
            Sok_Display.Put_Score (Disp_Score);
          when Restart_Frame =>
            Update_State := Menu_Result.Update_State;
            return;
        end case;
      end if;

      if Key = Sok_Input.Refresh then
        Sok_Display.Put_Frame (State.Frame);
        Sok_Display.Put_Help (Sok_Display.Frame);
        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);
        Sok_Display.Put_Score (Disp_Score);
      end if;

      if Key in Sok_Movement.Movement_List then
        -- movement
        Sok_Movement.Do_Movement (State.Frame, State.Position, Key, Result);
        case Result is
          when Sok_Movement.Refused =>
            null;
          when Sok_Movement.Done =>
            -- movement without box moving
            State.Moves := State.Moves + 1;
          when Sok_Movement.Box_Moved =>
            -- Same number of box OK
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
          when Sok_Movement.Box_Ok_More =>
            -- One more box OK
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
            State.Box_Ok := State.Box_Ok + 1;
            if State.Box_Ok = State.Nbre_Targets then
              -- frame finished
              Sok_Time.Stop_Time;
              Set_Blink(State.Frame, True);
              Sok_Display.Put_Help (Sok_Display.Done);
              Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                                    State.Nbre_Targets, State.No_Frame);

              -- Update and save score if needed
              Save_Score := False;
              if not State.Score.Set then
                State.Score.Set := True;
                Sok_Time.Get_Time (State.Score.Day, State.Score.Dur);
                State.Score.Moves := State.Moves;
                State.Score.Pushes := State.Pushes;
                Save_Score := True;
              else
                declare
                  Day : Natural;
                  Dur : Duration;
                begin
                  Sok_Time.Get_Time(Day, Dur);
                  if (Day < State.Score.Day )
                  or else (Day = State.Score.Day
                     and then Dur < State.Score.Dur) then
                    State.Score.Day := Day;
                    State.Score.Dur := Dur;
                    Save_Score := True;
                  end if;
                end;
                if State.Moves < State.Score.Moves then
                  State.Score.Moves := State.Moves;
                  Save_Score := True;
                end if;
                if State.Pushes < State.Score.Pushes then
                  State.Score.Pushes := State.Pushes;
                  Save_Score := True;
                end if;
              end if;
              if Save_Score then
                begin
                  Sok_File.Write_Score (State.No_Frame, State.Score);
                exception
                  when Sok_File.Score_Io_Error =>
                    Sok_Display.Put_Error (Sok_Display.Init_Score);
                    raise;
                end;
              end if;

              -- wait input to go on
              loop
                Key := Sok_Input.Get_Key;
                if Key = Sok_Input.Esc then
                  -- menu
                  Menu_Result := Sok_Menu (Allow_Write => False);
                  case Menu_Result.Result is
                    when Go_On =>
                      -- Esc in menu
                      Key := Sok_Input.Esc;
                      Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                                            State.Nbre_Targets, State.No_Frame);
                      Sok_Display.Put_Score (Disp_Score);
                    when Restart_Frame =>
                      Update_State := Menu_Result.Update_State;
                      return;
                  end case;
                elsif Key = Sok_Input.Undo then
                  Con_Io.Bell;
                elsif Key = Sok_Input.Refresh then
                  -- Refresh
                  Sok_Display.Put_Frame (State.Frame);
                  Set_Blink(State.Frame, True);
                  Sok_Display.Put_Help (Sok_Display.Done);
                  Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                                        State.Nbre_Targets, State.No_Frame);
                  Sok_Display.Put_Score (Disp_Score);
                end if;
                exit when Key = Sok_Input.Next;
              end loop;
              Set_Blink(State.Frame, False);
              if State.No_Frame /= Sok_Types.Frame_Range'Last then
                State.No_Frame := Sok_Types.Frame_Range'Succ(State.No_Frame);
              else
                State.No_Frame := Sok_Types.Frame_Range'First;
              end if;
              -- restart with new frame
              Update_State := Reset_All;
              return;
            end if;
          when Sok_Movement.Box_Ok_Less =>
            -- One less box OK
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
            State.Box_Ok := State.Box_Ok - 1;
        end case;

        -- save movement
        case Result is
          when Sok_Movement.Refused =>
            null;
          when Sok_Movement.Done =>
            Sok_Save.Push (
             (Pos_Orig => State.Position,
              Movement => Key,
              Result   => Sok_Movement.Done) );
          when Sok_Movement.Box_Moved | Sok_Movement.Box_Ok_More |
               Sok_Movement.Box_Ok_Less =>
            Sok_Save.Push (
             (Pos_Orig => State.Position,
              Movement => Key,
              Result   => Sok_Movement.Box_Moved) );
         end case;

      elsif Key = Sok_Input.Undo then
        begin
          -- try to pop movement
          Poped_Data := Sok_Save.Pop;
          Sok_Movement.Undo_Movement (State.Frame, Poped_Data,
                                      Result, Saved_Pos);
          case Result is
            when Sok_Movement.Refused =>
              -- impossible;
              null;
            when Sok_Movement.Done =>
              -- movement without box moving
              State.Moves := State.Moves - 1;
            when Sok_Movement.Box_Moved =>
              -- Same number of box OK
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
            when Sok_Movement.Box_Ok_More =>
              -- One more box OK
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
              State.Box_Ok := State.Box_Ok + 1;
            when Sok_Movement.Box_Ok_Less =>
              -- One less box OK
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
              State.Box_Ok := State.Box_Ok - 1;
          end case;
          State.Position := Saved_Pos;
        exception
          when Sok_Save.No_More_Saved_Movements =>
            Con_Io.Bell;
        end;

      end if; -- move or undo

      if Sok_Movement."/=" (Result, Sok_Movement.Refused) then
        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);
      end if;


    end loop;
  end Play_Frame;





end Sok_Manager;
