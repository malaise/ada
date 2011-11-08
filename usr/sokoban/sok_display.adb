with Normal, Day_Mng, Timers, Language;
with Sok_Input, Sok_Time;

-- displaying of sokoban
package body Sok_Display is

  Len_Moves  : constant := 5;
  Len_Days   : constant := 3;

  Console : aliased Con_Io.Console;


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

  Black : Con_Io.Effective_Colors;

  procedure Init is
  begin
    Black := Con_Io.Color_Of ("Black");
    Console.Open (Def_Back => Black,
                  Def_Fore => Con_Io.Color_Of ("Light_Grey"));
    Console.Clear_Screen;

    -- Title      : row 00 to 00 col 00 to 57 (01 row, 57 col)
    Title_Win.Open (Console'Access, (00, 00), (00, 55) );

    -- Time zone  : row 00 to 00 col 57 to 76 (01 row 20 col)
    Time_Win.Open (Console'Access, (00, 57), (00, 76) );

    -- Frame      : row 02 to 17 col 05 to 42 (16 row 38 col) (38=19*2)
    Frame_Win.Open (Console'Access, (02, 05), (17, 42) );

    -- State line : row 20 to 20 col 10 to 75 (01 row 66 col)
    Line_Win.Open (Console'Access, (20, 10), (20, 75) );

    -- Score line : row 21 to 20 col 10 to 75 (01 row 66 col)
    Score_Win.Open (Console'Access, (21, 10), (21, 75) );

    -- Help beside: row 05 to 15 col 55 to 77 (left to frame)
    Help_Win.Open (Console'Access, (05, 55), (15, 77) );
    Help_Win.Set_Foreground (Con_Io.Color_Of ("Lime_Green"));

    -- Menu       : row 19 to 21 col 02 to 79 (bottom)
    Menu_Win.Open (Console'Access, (19, 00), (21, 79) );

    -- Error      : row 19 to 21 col 02 to 79 (bottom)
    Error_Win.Open (Console'Access, (19, 00), (21, 79) );

    -- Get        : row 22 to 25 col 19 to 59 (bottom)
    Get_Win.Open (Console'Access, (22, 19), (24, 59) );

  end Init;

  function Get_Console return Con_Io.Console is
  begin
    return Console;
  end Get_Console;

  procedure Put_Help (Help : in Action_List) is
  begin
    Help_Win.Clear;
    case Help is
      when Frame =>
        Help_Win.Put_Line (" Arrows");
        Help_Win.Put ("  for movements");
        Help_Win.New_Line (2);

        Help_Win.Put_Line (" u  or  Backspace");
        Help_Win.Put ("  for undo");
        Help_Win.New_Line (2);

        Help_Win.Put_Line (" Ctrl Break or Ctrl C");
        Help_Win.Put ("  to quit");
      when Done =>
        Help_Win.Put_Line ("  - FRAME completed -");
        Help_Win.New_Line (2);

        Help_Win.Put_Line (" Space or Return");
        Help_Win.Put ("  for next frame");
        Help_Win.New_Line (2);

        Help_Win.Put_Line (" Ctrl Break or Ctrl C");
        Help_Win.Put ("  to quit");
      when Write =>
        Help_Win.Move ( (02, 00));
        Help_Win.Put_Line (" Save current");
        Help_Win.Put_Line ("  frame and movements");
        Help_Win.New_Line;
        Help_Win.Put_Line (" Only one frame saved");
        Help_Win.Put_Line ("  at a time");
      when Read =>
        Help_Win.Move ( (02, 00));
        Help_Win.Put_Line (" Restore last saved");
        Help_Win.Put_Line ("  frame and movements");
        Help_Win.New_Line;
        Help_Win.Put_Line (" Only one frame saved");
        Help_Win.Put_Line ("  at a time");
      when Reset =>
        Help_Win.Move ( (03, 00));
        Help_Win.Put_Line (" Restart current frame");
        Help_Win.Put_Line ("  from the beginning");
      when Get_New =>
        Help_Win.Move ( (03, 00));
        Help_Win.Put_Line (" Start a new frame");
        Help_Win.Put_Line ("  from the beginning");
      when Break =>
        Help_Win.Move ( (05, 00));
        Help_Win.Put_Line (" Exit SOKOBAN");

    end case;

    case Help is
      when Frame | Done =>
        Help_Win.Move ( (09, 00));
        Help_Win.Put_Line (" Esc");
        Help_Win.Put ("  for command menu");
      when others =>
        Help_Win.Move ( (09, 00));
        Help_Win.Put_Line (" Esc");
        Help_Win.Put ("  to play again");
    end case;
  end Put_Help;

  -- puts all the frame
  procedure Put_Frame (Frame : in Sok_Types.Frame_Tab) is
  begin
    Console.Clear_Screen;
    Title_Win.Move ( (00, 20));
    Title_Win.Put ("S O K O B A N",
     Foreground => Con_Io.Color_Of ("White"), Move => False);
    Title_Win.Move ( (0, 50));
    Title_Win.Put ("Time :", Move => False);

    Frame_Win.Clear;
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        Put_Square (Frame (I,J), (Row =>I, Col =>J) );
      end loop;
    end loop;
  end Put_Frame;

  -- puts a square
  Wall_Color : constant Con_Io.Effective_Colors
             := Con_Io.Color_Of ("Light_Grey");
  Target_Color : constant Con_Io.Effective_Colors
             := Con_Io.Color_Of ("Red");
  Man_Color : constant Con_Io.Effective_Colors
             := Con_Io.Color_Of ("Cyan");
  Man_Target_Color : constant Con_Io.Effective_Colors
             := Con_Io.Color_Of ("Magenta");
  procedure Put_Square (Square     : in Sok_Types.Square_Rec;
                        Coordinate : in Sok_Types.Coordinate_Rec;
                        Blink      : in Boolean := False) is
  begin

    Frame_Win.Move ((Coordinate.Row - 1, (Coordinate.Col - 1) * 2));
    case Square.Pattern is
      when Sok_Types.Wall =>
        Frame_Win.Put ("  ", Background => Wall_Color, Move => False);
      when Sok_Types.Target =>
        case Square.Content is
          when Sok_Types.Nothing =>
            Frame_Win.Put ("* ", Foreground => Target_Color, Move => False);
          when Sok_Types.Man =>
            Frame_Win.Put ("!!", Man_Target_Color, Move => False);
          when Sok_Types.Box =>
            if Blink then
              Frame_Win.Put ("[]", Foreground => Target_Color,
                                   Move => False);
            else
              Frame_Win.Put ("[]", Foreground => Target_Color,
                                   Move => False);
            end if;
        end case;
      when Sok_Types.Free =>
        case Square.Content is
          when Sok_Types.Nothing =>
            Frame_Win.Put ("  ", Move => False);
          when Sok_Types.Man =>
            Frame_Win.Put ("!!", Foreground => Man_Color,
                        Move => False);
          when Sok_Types.Box =>
            Frame_Win.Put ("[]", Move => False);
        end case;
    end case;
  end Put_Square;


  -- puts the down line
  procedure Put_Line (Moves : in Natural; Pushes : in Natural;
                      Boxes_In : in Natural; Nb_Boxes : in Positive;
                      Frame : in Sok_Types.Frame_Range) is
  begin
    Line_Win.Move;
    Line_Win.Put ("Frame : "      & Normal (Frame, 2));
    Line_Win.Put ("    Moves : "  & Normal (Moves, Len_Moves));
    Line_Win.Put ("    Pushes : " & Normal (Pushes, Len_Moves));
    Line_Win.Put ("    Boxes : " & Normal (Boxes_In, 2)
                & '/' & Normal (Nb_Boxes, 2), Move => False);
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
    Time_Win.Move;
    Time_Win.Put (Time_Image(Day, Time), Move => False);
  end Put_Time;

  procedure Put_Score (Score : in Sok_Types.Score_Rec) is
  begin
    Score_Win.Move;
    if Score.Set then
      Score_Win.Put ("Best results");
      Score_Win.Put ("  Moves : "  & Normal (Score.Moves, Len_Moves));
      Score_Win.Put ("    Pushes : " & Normal (Score.Pushes, Len_Moves));
      Score_Win.Put ("  " & Time_Image(Score.Day, Score.Dur), Move => False);
    else
      Con_Io.Clear (Score_Win);
    end if;
  end Put_Score;

  Len_Field   : constant := 11;
  Pad         : constant :=  4;
  Write_Col   : constant := Pad;
  Read_Col    : constant := Write_Col   + Len_Field + Pad;
  Reset_Col   : constant := Read_Col    + Len_Field + Pad;
  Get_New_Col : constant := Reset_Col   + Len_Field + Pad;
  Break_Col   : constant := Get_New_Col + Len_Field + Pad;

  procedure Put_Action (Action : in Menu_Action_List;
                        Selected : in Boolean;
                        Clicked : in Boolean) is
    Ink, Paper : Con_Io.Effective_Colors;
  begin
    if not Selected then
      Ink := Con_Io.Color_Of ("Light_Grey");
      Paper := Con_Io.Color_Of ("Blue");
    elsif not Clicked then
      Ink := Con_Io.Color_Of ("Red");
      Paper := Con_Io.Color_Of ("Blue");
    else
      Ink := Con_Io.Color_Of ("Blue");
      Paper := Con_Io.Color_Of ("Red");
    end if;

    case Action is
      when Write =>
        if Current_Allow_Write then
          Menu_Win.Move ( (0, Write_Col));
          Menu_Win.Put ("           ", Foreground => Ink,
                      Background => Paper, Move => False);
          Menu_Win.Move ( (1, Write_Col));
          Menu_Win.Put ("   SAVE    ", Foreground => Ink,
                      Background => Paper, Move => False);
          Menu_Win.Move ( (2, Write_Col));
          Menu_Win.Put ("           ", Foreground => Ink,
                      Background => Paper, Move => False);
        else
          Menu_Win.Move ( (1, Write_Col));
          Menu_Win.Put ("   SAVE    ", Move => False);
        end if;
      when Read =>
        Menu_Win.Move ( (0, Read_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (1, Read_Col));
        Menu_Win.Put ("  RESTORE  ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (2, Read_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
      when Reset =>
        Menu_Win.Move ( (0, Reset_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (1, Reset_Col));
        Menu_Win.Put ("   RESET   ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (2, Reset_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
      when Get_New =>
        Menu_Win.Move ( (0, Get_New_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (1, Get_New_Col));
        Menu_Win.Put (" GOTO NEW  ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (2, Get_New_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
      when Break =>
        Menu_Win.Move ( (0, Break_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (1, Break_Col));
        Menu_Win.Put ("    EXIT   ", Foreground => Ink,
                    Background => Paper, Move => False);
        Menu_Win.Move ( (2, Break_Col));
        Menu_Win.Put ("           ", Foreground => Ink,
                    Background => Paper, Move => False);
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
      Put_Action (I, I = Init_Action, False);
    end loop;
  end Put_Menu;

  procedure Update_Menu (New_Action : in Menu_Action_List;
                         Clicked : in Boolean) is
  begin
    Put_Action (Current_Action, False, False);
    Current_Action := New_Action;
    Put_Action (Current_Action, True, Clicked);
  end Update_Menu;

  procedure Clear_Menu is
  begin
    Con_Io.Clear (Menu_Win);
  end Clear_Menu;

  function Get_Action (Row, Col : Natural) return Got_Action_List is
    Relative_Col : Con_Io.Col_Range;
  begin
    if not Menu_Win.In_Window ( (Row, Col)) then
      return Done;
    end if;
    Relative_Col := Menu_Win.To_Relative ( (Row, Col)).Col;
    case Relative_Col is
      when Write_Col   .. Write_Col   + Len_Field - 1 => return Write;
      when Read_Col    .. Read_Col    + Len_Field - 1 => return Read;
      when Reset_Col   .. Reset_Col   + Len_Field - 1 => return Reset;
      when Get_New_Col .. Get_New_Col + Len_Field - 1 => return Get_New;
      when Break_Col   .. Break_Col   + Len_Field - 1 => return Break;
      when others => return Done;
    end case;
  end Get_Action;

  procedure Put_Error (Error : in Error_List) is
  begin
    Error_Win.Set_Background (Con_Io.Color_Of ("Red"));
    Error_Win.Clear;
    Error_Win.Move ( (1, 30) );
    case Error is
      when No_Data =>
        Error_Win.Put ("DATA FILE NOT FOUND");
      when Read =>
        Error_Win.Put ("ERROR READING DATA");

      when No_Frame =>
        Error_Win.Put ("NO FRAME SAVED");
      when Restore =>
        Error_Win.Put ("ERROR RESTORING FRAME");
      when Save =>
        Error_Win.Put ("ERROR SAVING FRAME");

      when Init_Score =>
        Error_Win.Put ("ERROR INITIALIZING SCORES");
      when Score_Io =>
        Error_Win.Put ("ERROR READ/WRITE SCORE");

      when Internal =>
        Error_Win.Put ("INTERNAL ERROR");

      when Format =>
        Error_Win.Put ("ERROR. NUMBER REQUIRED (1 .. 50)");
    end case;
    Error_Win.Move ( (2, 65));
    Error_Win.Put ("Hit a key");
  end Put_Error;

  procedure Clear_Error is
  begin
    Error_Win.Set_Background (Console.Background);
    Error_Win.Clear;
  end Clear_Error;

  -- get frame number
  procedure Get_No_Frame (No : out Sok_Types.Frame_Range;
                          Result : out Get_Result_List) is

    Str : Con_Io.Unicode_Sequence(1..2) := (others => Con_Io.Space);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive := 1;
    Ins  : Boolean := False;
  begin
    Get_Win.Set_Background (Con_Io.Color_Of ("Cyan"));
    Get_Win.Set_Foreground (Black);
    Get_Win.Clear;
    Get_Win.Move ( (01, 07));
    Get_Win.Put ("Enter frame no (" &
                Normal (Sok_Types.Frame_Range'First, 1) &
                " to " &
                Normal (Sok_Types.Frame_Range'Last, 2) &
                ") : ");
    Get_Win.Move ( (02, 02));
    Get_Win.Put ("Enter to validate, Escape to give_up");

    loop
      Get_Win.Move ( (01, 34));
      Get_Win.Put_Then_Get (Str, Last, Stat, Pos, Ins,
       Foreground => Con_Io.Color_Of ("Light_Grey"),
       Background => Black,
       Time_Out => (Delay_Kind    => Timers.Delay_Sec,
                    Clock         => null,
                    Period        => Con_Io.No_Period,
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
          -- Digit or space allowed
          for I in Str'Range loop
            if Language.Unicode_To_Char (Str(I)) not in '0' .. '9'
            and then Language.Unicode_To_Char (Str(I)) /= ' ' then
              raise Format_Error;
            end if;
          end loop;
          -- Not empty
          if Last = 0 then
            Result := Esc;
            exit;
          end if;
          begin
            No := Sok_Types.Frame_Range'Value (
              Language.Unicode_To_String (Str(1 .. Last)));
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

    Get_Win.Set_Background (Console.Background);
    Get_Win.Clear;

  exception
    when Format_Error =>
      Get_Win.Set_Background (Console.Background);
      Get_Win.Clear;
      raise;
  end Get_No_Frame;



  procedure End_Of_Program is
  begin
    Console.Clear_Screen;
  end End_Of_Program;

  procedure Bell is
  begin
    Console.Bell;
  end Bell;

end Sok_Display;

