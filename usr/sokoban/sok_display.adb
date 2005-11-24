with Normal, Day_Mng, Con_Io, Timers;
with Sok_Input, Sok_Time;

-- displaying of sokoban
package body Sok_Display is

  Len_Moves  : constant := 5;
  Len_Pushes : constant := 5;
  Len_Days   : constant := 3;

  Title_Win : Con_Io.Window;
  Frame_Win : Con_Io.Window;
  Line_Win  : Con_Io.Window;
  Score_Win : Con_Io.Window;
  Time_Win  : Con_Io.Window;
  Help_Win  : Con_Io.Window;
  Menu_Win  : Con_Io.Window;
  Error_Win : Con_Io.Window;
  Get_Win   : Con_Io.Window;

  Current_Action : Action_List;
  Current_Allow_Write : Boolean;

  procedure Init is
    Help_Row : constant := 1;
    Help_Col : constant := 1;
  begin
    Con_Io.Init;
    Con_Io.Reset_Term;

    -- Title      : row 00 to 00 col 00 to 57 (01 row, 57 col)
    Con_Io.Open (Title_Win, (00, 00), (00, 55) );

    -- Time zone  : row 00 to 00 col 57 to 76 (01 row 20 col)
    Con_Io.Open (Time_Win,  (00, 57), (00, 76) );

    -- Frame      : row 02 to 17 col 05 to 42 (16 row 38 col) (38=19*2)
    Con_Io.Open (Frame_Win, (02, 05), (17, 42) );

    -- State line : row 20 to 20 col 10 to 75 (01 row 66 col)
    Con_Io.Open (Line_Win,  (20, 10), (20, 75) );

    -- Score line : row 21 to 20 col 10 to 75 (01 row 66 col)
    Con_Io.Open (Score_Win,  (21, 10), (21, 75) );

    -- Help beside: row 05 to 15 col 55 to 77 (left to frame)
    Con_Io.Open (Help_Win,  (05, 55), (15, 77) );
    Con_Io.Set_Foreground (Con_Io.Light_Green, Name => Help_Win);

    -- Menu       : row 19 to 21 col 02 to 79 (bottom)
    Con_Io.Open (Menu_Win,  (19, 00), (21, 79) );

    -- Error      : row 19 to 21 col 02 to 79 (bottom)
    Con_Io.Open (Error_Win, (19, 00), (21, 79) );

    -- Get        : row 22 to 25 col 19 to 59 (bottom)
    Con_Io.Open (Get_Win,   (22, 19), (24, 59) );

  end Init;

  procedure Put_Help (Help : in Action_List) is
  begin
    Con_Io.Clear (Help_Win);
    case Help is
      when Frame =>
        Con_Io.Put_Line (" Arrows", Name => Help_Win);
        Con_Io.Put ("  for movements", Name => Help_Win);
        Con_Io.New_Line (Help_Win, 2);

        Con_Io.Put_Line (" u  or  Backspace", Name => Help_Win);
        Con_Io.Put ("  for undo", Name => Help_Win);
        Con_Io.New_Line (Help_Win, 2);

        Con_Io.Put_Line (" Ctrl Break or Ctrl C", Name => Help_Win);
        Con_Io.Put ("  to quit", Name => Help_Win);
      when Done =>
        Con_Io.Put_Line ("  - FRAME completed -", Blink_Stat => Con_Io.Blink,
                         Name => Help_Win);
        Con_Io.New_Line (Help_Win, 2);

        Con_Io.Put_Line (" Space or Return", Name => Help_Win);
        Con_Io.Put ("  for next frame", Name => Help_Win);
        Con_Io.New_Line (Help_Win, 2);

        Con_Io.Put_Line (" Ctrl Break or Ctrl C", Name => Help_Win);
        Con_Io.Put ("  to quit", Name => Help_Win);
      when Write =>
        Con_Io.Move ( (02, 00), Name => Help_Win);
        Con_Io.Put_Line (" Save current", Name => Help_Win);
        Con_Io.Put_Line ("  frame and movements", Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" Only one frame saved", Name => Help_Win);
        Con_Io.Put_Line ("  at a time", Name => Help_Win);
      when Read =>
        Con_Io.Move ( (02, 00), Name => Help_Win);
        Con_Io.Put_Line (" Restore last saved", Name => Help_Win);
        Con_Io.Put_Line ("  frame and movements", Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" Only one frame saved", Name => Help_Win);
        Con_Io.Put_Line ("  at a time", Name => Help_Win);
      when Reset =>
        Con_Io.Move ( (03, 00), Name => Help_Win);
        Con_Io.Put_Line (" Restart current frame", Name => Help_Win);
        Con_Io.Put_Line ("  from the beginning", Name => Help_Win);
      when Get_New =>
        Con_Io.Move ( (03, 00), Name => Help_Win);
        Con_Io.Put_Line (" Start a new frame", Name => Help_Win);
        Con_Io.Put_Line ("  from the beginning", Name => Help_Win);
      when Break =>
        Con_Io.Move ( (05, 00), Name => Help_Win);
        Con_Io.Put_Line (" Exit SOKOBAN", Name => Help_Win);

    end case;

    case Help is
      when Frame | Done =>
        Con_Io.Move ( (09, 00) ,Help_Win);
        Con_Io.Put_Line (" Esc", Name => Help_Win);
        Con_Io.Put ("  for command menu", Name => Help_Win);
      when others =>
        Con_Io.Move ( (09, 00) ,Help_Win);
        Con_Io.Put_Line (" Esc", Name => Help_Win);
        Con_Io.Put ("  to play again", Name => Help_Win);
    end case;
  end Put_Help;

  -- puts all the frame
  procedure Put_Frame (Frame : in Sok_Types.Frame_Tab) is
  begin
    Con_Io.Reset_Term;
    Con_Io.Move ( (00, 20), Name => Title_Win);
    Con_Io.Put ("S O K O B A N", Title_Win,
     Foreground => Con_Io.White, Move => False);
    Con_Io.Move ( (0, 50), Title_Win);
    Con_Io.Put ("Time :", Title_Win, Move => False);

    Con_Io.Clear (Frame_Win);
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        Put_Square (Frame (I,J), (Row =>I, Col =>J) );
      end loop;
    end loop;
  end Put_Frame;

  -- puts a square
  procedure Put_Square (Square     : in Sok_Types.Square_Rec;
                        Coordinate : in Sok_Types.Coordinate_Rec;
                        Blink      : in Boolean := False) is
  begin

    Con_Io.Move ((Coordinate.Row - 1, (Coordinate.Col - 1) * 2),
     Name => Frame_Win);
    case Square.Pattern is
      when Sok_Types.Wall =>
        Con_Io.Put ("  ", Frame_Win, Background => Con_Io.Light_Gray,
         Move => False);
      when Sok_Types.Target =>
        case Square.Content is
          when Sok_Types.Nothing =>
            Con_Io.Put ("* ", Frame_Win, Foreground => Con_Io.Red,
             Move => False);
          when Sok_Types.Man =>
            Con_Io.Put ("!!", Frame_Win, Foreground => Con_Io.Red,
             Move => False);
          when Sok_Types.Box =>
            if Blink then
              Con_Io.Put ("[]", Frame_Win,
                                Foreground => Con_Io.Red,
                                Blink_Stat => Con_Io.Blink,
                                Move => False);
            else
              Con_Io.Put ("[]", Frame_Win,
                                Foreground => Con_Io.Red,
                                Blink_Stat => Con_Io.Not_Blink,
                                Move => False);
            end if;
        end case;
      when Sok_Types.Free =>
        case Square.Content is
          when Sok_Types.Nothing =>
            Con_Io.Put ("  ", Frame_Win, Move => False);
          when Sok_Types.Man =>
            Con_Io.Put ("!!", Frame_Win,  Foreground => Con_Io.Cyan,
                        Move => False);
          when Sok_Types.Box =>
            Con_Io.Put ("[]", Frame_Win, Move => False);
        end case;
    end case;
  end Put_Square;


  -- puts the down line
  procedure Put_Line (Moves : in Natural; Pushes : in Natural;
                      Boxes_In : in Natural; Nb_Boxes : in Positive;
                      Frame : in Sok_Types.Frame_Range) is
  begin
    Con_Io.Move (Name => Line_Win);
    Con_Io.Put ("Frame : "      & Normal (Frame, 2),          Line_Win);
    Con_Io.Put ("    Moves : "  & Normal (Moves, Len_Moves),  Line_Win);
    Con_Io.Put ("    Pushes : " & Normal (Pushes, Len_Moves), Line_Win);
    Con_Io.Put ("    Boxes : " & Normal (Boxes_In, 2)
                & '/' & Normal (Nb_Boxes, 2), Line_Win,
              Move => False);
  end Put_Line;

  function Time_Image (Day : Natural;
                       Time : Ada.Calendar.Day_Duration) return String is
    Hours    : Day_Mng.T_Hours;
    Minutes  : Day_Mng.T_Minutes;
    Seconds  : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
    Str : String (1 .. Len_Days+16);
  begin
    Day_Mng.Split (Time, Hours, Minutes, Seconds, Millisec);
    if Day = 0 then
      Str(1 .. Len_Days+5) := "   " & "     ";
    elsif Day = 1 then
      Str(1 .. Len_Days+5) := Normal (Day, Len_Days) & " day ";
    else
      Str(1 .. Len_Days+5) := Normal (Day, Len_Days) & " days";
    end if;
    Str(Len_Days+6 .. Len_Days+16) := " " &
                Normal (Hours,   2, Gap => '0') & "h" &
                Normal (Minutes, 2, Gap => '0') & "mn" &
                Normal (Seconds, 2, Gap => '0') & "s";
    return Str;
  end Time_Image;

  procedure Put_Time (Day : in Natural;
                      Time : in Ada.Calendar.Day_Duration) is

  begin
    Con_Io.Move (Name => Time_Win);
    Con_Io.Put (Time_Image(Day, Time),
                Time_Win,
                Move => False);
  end Put_Time;

  procedure Put_Score (Score : in Sok_Types.Score_Rec) is
  begin
    Con_Io.Move (Name => Score_Win);
    if Score.Set then
      Con_Io.Put ("Best results", Score_Win);
      Con_Io.Put ("  Moves : "  & Normal (Score.Moves, Len_Moves),  Score_Win);
      Con_Io.Put ("    Pushes : " & Normal (Score.Pushes, Len_Moves), Score_Win);
      Con_Io.Put ("  " & Time_Image(Score.Day, Score.Dur),
                  Score_Win,
                  Move => False);
    else
      Con_Io.Clear (Score_Win);
    end if;
  end Put_Score;

  procedure Put_Action (Action : in Menu_Action_List; Selected : in Boolean) is
    Color : Con_Io.Effective_Colors;
    Blink : Con_Io.Effective_Blink_Stats;
    Len_Field : constant := 11;
    Pad       : constant :=  4;
    Write_Col   : constant := Pad;
    Read_Col    : constant := Write_Col   + Len_Field + Pad;
    Reset_Col   : constant := Read_Col    + Len_Field + Pad;
    Get_New_Col : constant := Reset_Col   + Len_Field + Pad;
    Break_Col   : constant := Get_New_Col + Len_Field + Pad;
  begin
    if not Selected then
      Color := Con_Io.Light_Gray;
      Blink := Con_Io.Not_Blink;
    else
      Color := Con_Io.Green;
      Blink := Con_Io.Blink;
    end if;

    case Action is
      when Write =>
        if Current_Allow_Write then
          Con_Io.Move ( (0, Write_Col), Menu_Win);
          Con_Io.Put ("           ", Menu_Win, Foreground => Color,
           Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
          Con_Io.Move ( (1, Write_Col), Menu_Win);
          Con_Io.Put ("   SAVE    ", Menu_Win, Foreground => Color,
           Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
          Con_Io.Move ( (2, Write_Col), Menu_Win);
          Con_Io.Put ("           ", Menu_Win, Foreground => Color,
           Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        else
          Con_Io.Move ( (1, Write_Col), Menu_Win);
          Con_Io.Put ("   SAVE    ", Menu_Win, Move => False);
        end if;
      when Read =>
        Con_Io.Move ( (0, Read_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (1, Read_Col), Menu_Win);
        Con_Io.Put ("  RESTORE  ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (2, Read_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
      when Reset =>
        Con_Io.Move ( (0, Reset_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (1, Reset_Col), Menu_Win);
        Con_Io.Put ("   RESET   ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (2, Reset_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
      when Get_New =>
        Con_Io.Move ( (0, Get_New_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (1, Get_New_Col), Menu_Win);
        Con_Io.Put (" GOTO NEW  ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (2, Get_New_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
      when Break =>
        Con_Io.Move ( (0, Break_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (1, Break_Col), Menu_Win);
        Con_Io.Put ("    EXIT   ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
        Con_Io.Move ( (2, Break_Col), Menu_Win);
        Con_Io.Put ("           ", Menu_Win, Foreground => Color,
         Blink_Stat => Blink, Background => Con_Io.Blue, Move => False);
    end case;
  end Put_Action;

  procedure Put_Menu (
   Init_Action : in Menu_Action_List;
   Allow_Write : in Boolean) is
  begin
    if not Allow_Write and then Init_Action = Write then
      raise Constraint_Error;
    end if;
    Current_Action := Init_Action;
    Current_Allow_Write := Allow_Write;
    for I in Menu_Action_List loop
      Put_Action (I, I = Init_Action);
    end loop;
  end Put_Menu;

  procedure Update_Menu (New_Action : in Menu_Action_List) is
  begin
    Put_Action (Current_Action, False);
    Current_Action := New_Action;
    Put_Action (Current_Action, True);
  end Update_Menu;

  procedure Clear_Menu is
  begin
    Con_Io.Clear (Menu_Win);
  end Clear_Menu;

  procedure Put_Error (Error : in Error_List) is
  begin
    Con_Io.Set_Background (Con_Io.Red, Name => Error_Win);
    Con_Io.Clear (Error_Win);
    Con_Io.Move ( (1, 30) , Error_Win);
    case Error is
      when No_Data =>
        Con_Io.Put ("DATA FILE NOT FOUND", Error_Win);
      when Read =>
        Con_Io.Put ("ERROR READING DATA", Error_Win);

      when No_Frame =>
        Con_Io.Put ("NO FRAME SAVED", Error_Win);
      when Restore =>
        Con_Io.Put ("ERROR RESTORING FRAME", Error_Win);
      when Save =>
        Con_Io.Put ("ERROR SAVING FRAME", Error_Win);

      when Init_Score =>
        Con_Io.Put ("ERROR INITIALIZING SCORES", Error_Win);
      when Score_Io =>
        Con_Io.Put ("ERROR READ/WRITE SCORE", Error_Win);

      when Internal =>
        Con_Io.Put ("INTERNAL ERROR", Error_Win);

      when Format =>
        Con_Io.Put ("ERROR. NUMBER REQUIRED (1 .. 50)", Error_Win);
    end case;
    Con_Io.Move ( (2, 65), Error_Win);
    Con_Io.Put ("Hit a key", Error_Win);
  end Put_Error;

  procedure Clear_Error is
  begin
    Con_Io.Set_Background (Con_Io.Default_Background, Error_Win);
    Con_Io.Clear (Error_Win);
  end Clear_Error;

  -- get frame number
  procedure Get_No_Frame (No : out Sok_Types.Frame_Range; Result : out Get_Result_List) is

    Str : String (1..2) := (others => ' ');
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive := 1;
    Ins  : Boolean := False;
  begin
    Con_Io.Set_Background (Con_Io.Cyan, Get_Win);
    Con_Io.Set_Foreground (Con_Io.Black, Name => Get_Win);
    Con_Io.Clear (Get_Win);
    Con_Io.Move ( (01, 07), Get_Win);
    Con_Io.Put ("Enter frame no (" &
                Normal (Sok_Types.Frame_Range'First, 1) &
                " to " &
                Normal (Sok_Types.Frame_Range'Last, 2) &
                ") : ", Get_Win);
    Con_Io.Move ( (02, 02), Get_Win);
    Con_Io.Put ("Enter to validate, Escape to give_up", Get_Win);

    loop
      Con_Io.Move ( (01, 34), Get_Win);
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins, Get_Win,
       Foreground => Con_Io.Light_Gray, Background => Con_Io.Black,
       Time_Out => (Delay_Kind => Timers.Delay_Sec,
                    Period     => Con_Io.No_Period,
                    Delay_Seconds => 1.0) );

      case Stat is
        when Con_Io.Esc =>
          Result := Esc;
          exit;

        when Con_Io.Refresh =>
          Result := Refresh;
          exit;

        when Con_Io.Break =>
          raise Sok_Input.Break_Requested;

        when Con_Io.Timeout =>
          Sok_Time.Disp_Time;

        when Con_Io.Ret =>
          -- digit or space allowed
          for I in Str'Range loop
            if Str(I) not in '0' .. '9' and then Str(I) /= ' ' then
              raise Format_Error;
            end if;
          end loop;
          -- not empty
          if Last = 0 then
            Result := Esc;
            exit;
          end if;
          begin
            No := Sok_Types.Frame_Range'Value (Str (1..Last));
            Result := Set;
            exit;
          exception
            when Constraint_Error =>
              raise Format_Error;
          end;
        when others =>
          null;
        end case;
    end loop;

    Con_Io.Set_Background (Con_Io.Default_Background, Get_Win);
    Con_Io.Clear (Get_Win);

  exception
    when Format_Error =>
      Con_Io.Set_Background (Con_Io.Default_Background, Get_Win);
      Con_Io.Clear (Get_Win);
      raise;
  end Get_No_Frame;



  procedure End_Of_Program is
  begin
    Con_Io.Clear;
  end End_Of_Program;


end Sok_Display;
