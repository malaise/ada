with Ada.Calendar; -- For time

with Sok_Display;
with Sok_Input;
with Sok_Movement;
with Sok_Save;
with Sok_Time;
package body Sok_Manager is

  -- Internal state of a frame
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

  -- Menu return : go on with same frame or,
  --  if new frame, reset_all state (read) or update time (restored)
  type Menu_Result_List is (Go_On, Reset_All, Update_Time);


  -- Frames reading, saving and restoring.
  package Sok_File is

    -- To read a new frame
    procedure Read (No_Frame : in  Sok_Types.Frame_Range;
                    Frame    : out Sok_Types.Frame_Tab);
    Data_File_Not_Found, Error_Reading_Data : exception;

    -- Save a frame and recover a frame, with saved movements
    procedure Save (State : in State_Rec);
    Error_Writing_Frame : exception;

    procedure Restore (State : out State_Rec);
    Frame_File_Not_Found, Error_Reading_Frame : exception;

    -- Initialise scores, read/update score
    function Read_Score (No : Sok_Types.Frame_Range) return Sok_Types.Score_Rec;
    procedure Write_Score (No : in Sok_Types.Frame_Range;
                           Score : in Sok_Types.Score_Rec);
    Score_Io_Error : exception;


  end Sok_File;
  package body Sok_File is separate;

  -- Body below
  -- Init the frame (reset, find man, count targets)
  procedure Init_Frame;
  -- Play a frame return True if end of frame (False if menu)
  function Play_Frame return Boolean;
  -- Handle end of frame, return True if next frame (False if menu)
  function End_Of_Frame return Boolean;

  procedure Set_Blink (Frame : in Sok_Types.Frame_Tab;
                       Blink : in Boolean);
  -- If return is Go_On, nothing to do
  -- If return is Restart_Frame, part of state has been set
  --  if Reset_All, only No_Frame is set (read)
  --  if Update_Time, only time has to be updated (restore)
  function Sok_Menu (Done : Boolean) return Menu_Result_List is separate;

  Internal_Error : exception;

  procedure Play_Game (First_Frame : in Sok_Types.Desired_Frame_Range) is
    -- What to do
    type Step_List is (Playframe, Endframe, Menuplay, Menuend);
    Step : Step_List;

    procedure End_Of_Program is
    begin
      -- End of game
      Sok_Input.End_Of_Program;

      Sok_Display.End_Of_Program;
    end End_Of_Program;

  begin

    -- Init for first frame
    begin
      Sok_Display.Init;
    exception
      when others =>
      raise Sok_Input.Break_Requested;
    end;
    -- See if restore or init
    if First_Frame = Sok_Types.Restore_Frame then
      -- Try to restore
      begin
        -- Check that frame file is readable
        Sok_File.Read (Sok_Types.Frame_Range'First, State.Frame);
        Sok_File.Restore (State);
        State.Score := Sok_File.Read_Score(State.No_Frame);
      exception
        when others =>
          -- Restore failed => init to first
          State.No_Frame := Sok_Types.Frame_Range'First;
          Init_Frame;
      end;
    else
      -- Init to requested frame
      State.No_Frame := First_Frame;
      Init_Frame;
    end if;


    -- Play or end of frame
    if State.Box_Ok /= State.Nbre_Targets then
      Step := Playframe;
    else
      Step := Endframe;
    end if;

    loop

      -- Redisplay
      Sok_Display.Put_Frame (State.Frame);
      Sok_Display.Put_Score (State.Score);
      Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                            State.Nbre_Targets, State.No_Frame);

      -- Play or handle end of frame
      case Step is
        when Playframe =>
          if Play_Frame then
            -- Frame finished
            Step := Endframe;
          else
            Step := Menuplay;
          end if;
        when Endframe =>
          if End_Of_Frame then
            -- Space in end of frame -> Next frame
            Init_Frame;
            Step := Playframe;
          else
            Step := Menuend;
          end if;
        when Menuplay | Menuend =>
          -- Call menu
          case Sok_Menu (Step = Menuend) is
            when Go_On =>
              -- Esc Esc, back to original mode
              if Step = Menuend then
                Step := Endframe;
              else
                Step := Playframe;
              end if;
            when Reset_All =>
              -- Reset frame, find man, count targets...
              Init_Frame;
              Step := Playframe;
            when Update_Time =>
              -- Restored: Check if end of frame
              Sok_Time.Start_Time;
              if State.Box_Ok = State.Nbre_Targets then
                Step := Endframe;
              else
                Step := Playframe;
              end if;
          end case;
      end case;

    end loop;

  exception
    when Sok_Input.Break_Requested =>
      End_Of_Program;
    when Internal_Error =>
      Sok_Display.Put_Error (Sok_Display.Internal);
      Sok_Input.Pause;
      Sok_Display.Clear_Error;
      End_Of_Program;
    when others =>
      Sok_Input.Pause;
      Sok_Display.Clear_Error;
      End_Of_Program;
  end Play_Game;

  procedure Init_Frame is
    Man_Found : Boolean;
    use Sok_Types;
  begin

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

    -- Clear saved movements
    Sok_Save.Reset;

    -- Init time and start;
    Sok_Time.Reset_Time;
    Sok_Time.Start_Time;

    -- Find man starting position and complete state
    Man_Found := False;
    State.Nbre_Targets := 0;
    State.Box_Ok       := 0;
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        if State.Frame (I, J).Pattern /= Sok_Types.Wall and then
           State.Frame (I, J).Content = Sok_Types.Man then
           if Man_Found then
           -- Man already found !!
            raise Internal_Error;
          else
            Man_Found := True;
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
    if not Man_Found then
      -- No man !!
      raise Internal_Error;
    end if;
  end Init_Frame;

  -- Play a frame
  function Play_Frame return Boolean is

    Key : Sok_Input.Key_List;

    Result : Sok_Movement.Result_List;

    Saved_Pos : Sok_Types.Coordinate_Rec;

    Poped_Data : Sok_Movement.Saved_Data_Rec;

    use type Sok_Input.Key_List;

  begin
    Sok_Display.Put_Help (Sok_Display.Frame);
    -- Movements
    loop
      Key := Sok_Input.Get_Key;

      if Key = Sok_Input.Esc then
        -- Menu
        return False;
      end if;

      if Key = Sok_Input.Refresh then
        Sok_Display.Put_Frame (State.Frame);
        Sok_Display.Put_Help (Sok_Display.Frame);
        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);
        Sok_Display.Put_Score (State.Score);
      end if;

      if Key in Sok_Movement.Movement_List then
        -- Movement
        Sok_Movement.Do_Movement (State.Frame, State.Position, Key, Result);
        case Result is
          when Sok_Movement.Refused =>
            null;
          when Sok_Movement.Done =>
            -- Movement without box moving
            State.Moves := State.Moves + 1;
          when Sok_Movement.Box_Moved =>
            -- Same number of box Ok
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
          when Sok_Movement.Box_Ok_More =>
            -- One more box Ok
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
            State.Box_Ok := State.Box_Ok + 1;
            if State.Box_Ok = State.Nbre_Targets then
              -- Frame finished
              return True;
            end if;
          when Sok_Movement.Box_Ok_Less =>
            -- One less box Ok
            State.Moves  := State.Moves  + 1;
            State.Pushes := State.Pushes + 1;
            State.Box_Ok := State.Box_Ok - 1;
        end case;

        -- Save movement
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
          -- Try to pop movement
          Poped_Data := Sok_Save.Pop;
          Sok_Movement.Undo_Movement (State.Frame, Poped_Data,
                                      Result, Saved_Pos);
          case Result is
            when Sok_Movement.Refused =>
              -- Impossible;
              null;
            when Sok_Movement.Done =>
              -- Movement without box moving
              State.Moves := State.Moves - 1;
            when Sok_Movement.Box_Moved =>
              -- Same number of box Ok
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
            when Sok_Movement.Box_Ok_More =>
              -- One more box Ok
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
              State.Box_Ok := State.Box_Ok + 1;
            when Sok_Movement.Box_Ok_Less =>
              -- One less box Ok
              State.Moves  := State.Moves  - 1;
              State.Pushes := State.Pushes - 1;
              State.Box_Ok := State.Box_Ok - 1;
          end case;
          State.Position := Saved_Pos;
        exception
          when Sok_Save.No_More_Saved_Movements =>
            Sok_Display.Bell;
        end;

      end if; -- Move or undo

      if Sok_Movement."/=" (Result, Sok_Movement.Refused) then
        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);
      end if;


    end loop;
  end Play_Frame;

  -- Handle the screen at the end of a frame
  function End_Of_Frame return Boolean is
    Save_Score : Boolean;
    Key : Sok_Input.Key_List;
    use type Sok_Input.Key_List;
  begin
    -- Freeze frame
    Sok_Time.Stop_Time;
    Set_Blink(State.Frame, True);
    Sok_Display.Put_Help (Sok_Display.Done);
    Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                          State.Nbre_Targets, State.No_Frame);

    -- Update and save score if needed
    Save_Score := False;
    if not State.Score.Set then
      -- Nothing set for this frame
      State.Score.Set := True;
      Sok_Time.Get_Time (State.Score.Day, State.Score.Dur);
      State.Score.Moves := State.Moves;
      State.Score.Pushes := State.Pushes;
      Save_Score := True;
    else
      -- Store each new record
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
    -- Save
    if Save_Score then
      begin
        Sok_File.Write_Score (State.No_Frame, State.Score);
      exception
        when Sok_File.Score_Io_Error =>
          Sok_Display.Put_Error (Sok_Display.Init_Score);
          Set_Blink(State.Frame, False);
          raise;
      end;
    end if;

    -- Wait for an input to go on
    loop
      Key := Sok_Input.Get_Key;
      if Key = Sok_Input.Esc then
        -- Menu
        return False;
      elsif Key = Sok_Input.Undo then
        Sok_Display.Bell;
      elsif Key = Sok_Input.Refresh then
        -- Refresh
        Sok_Display.Put_Frame (State.Frame);
        Set_Blink(State.Frame, True);
        Sok_Display.Put_Help (Sok_Display.Done);
        Sok_Display.Put_Line (State.Moves, State.Pushes, State.Box_Ok,
                              State.Nbre_Targets, State.No_Frame);
        Sok_Display.Put_Score (State.Score);
      end if;
      exit when Key = Sok_Input.Next;
    end loop;
    -- Return hit
    -- Restart with next frame
    if State.No_Frame /= Sok_Types.Frame_Range'Last then
      State.No_Frame := Sok_Types.Frame_Range'Succ(State.No_Frame);
    else
      State.No_Frame := Sok_Types.Frame_Range'First;
    end if;
    Set_Blink(State.Frame, False);
    return True;
  end End_Of_Frame;

  procedure Set_Blink (Frame : in Sok_Types.Frame_Tab;
                       Blink : in Boolean) is
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

end Sok_Manager;

