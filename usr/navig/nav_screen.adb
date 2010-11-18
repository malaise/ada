-- All the primitives to access the screen
with Task_Mng, Timers, Language;
package body Nav_Screen is

  -- The 8 needed windows
  W_Title, W_Mask, W_Get, W_Res, W_Act, W_Err, W_Help, W_Time : Con_Io.Window;

  -- The left column of get ang result areas
  Col_Get : constant Con_Io.Col_Range := 29;
  Col_Res : constant Con_Io.Col_Range := 70;
  -- Start column in mask and width of dots and arrows
  Col_Line : constant Con_Io.Col_Range := 37;
  Width_Line : constant Positive := 32;
  -- Columns of actions
  Comp_Wid : constant Con_Io.Col_Range :=  7;
  Quit_Wid : constant Con_Io.Col_Range :=  4;
  Help_Wid : constant Con_Io.Col_Range :=  9;
  Clea_Wid : constant Con_Io.Col_Range :=  5;
  Act_Off : constant := 5;

  -- Background color of all get fields
  Get_Back : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Blue");
  -- Foreground color of results
  Res_Fore : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Lime_Green");

  -- Alarm
  Red : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Red");

  -- Delay max of a get (data or action) in seconds.
  Delta_Get : constant Con_Io.Delay_Rec(Timers.Delay_Sec) :=
    (Delay_Kind    => Timers.Delay_Sec,
     Clock         => null,
     Period        => Con_Io.No_Period,
     Delay_Seconds => 0.5);
  -- Number of deltas before clearing err messages
  Time_Out_Get : constant := 6;

  -- Time displaying
  procedure Show_Time is separate;

  package Time_Task_Mng is new Task_Mng (Call_Back => Show_Time);

  -- Clear all the screen
  procedure Reset is
  begin
    Con_Io.Reset_Term;
  end Reset;

  -- To write the title
  procedure Title is
  begin
    Time_Task_Mng.Start;
    Con_Io.Move ((0, 30), W_Title);
    Con_Io.Put ("AERONAUTICAL NAVIGATION", W_Title,
                Con_Io.Color_Of ("Light_Blue"));

    Con_Io.Move ((2, 0), W_Title);
    Con_Io.Put ("Keys: Enter, arrows, Ins, Del, Backspace, Home, End, Page Up,"
     & " Page Down", W_Title);
    Con_Io.Move ((3, 9), W_Title);
    Con_Io.Put ("digits, '.', '?', '+', '-'", W_Title);

    Con_Io.Move ((4, 0), W_Title);
    Con_Io.Put ("Format: speeds positives in knots or km/h (0.0 .. 999.9)",
     W_Title);
    Con_Io.Move ((5, 8), W_Title);
    Con_Io.Put ("angles positives in degrees and minutes (0.00 .. 359.59)",
     W_Title);
    Con_Io.Move ((6, 8), W_Title);
    Con_Io.Put ("drift in degrees and minutes (-90.00 .. +90.00)",
     W_Title);
    Con_Io.Move ((7, 8), W_Title);
    Con_Io.Put ("? in a field to clear it", W_Title);

    Show_Time;
    Time_Task_Mng.Schedule;

  end Title;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure Put_Mask is
    Col : Con_Io.Col_Range;
  begin
    Con_Io.Move ( (1, Col_Get), W_Mask); Con_Io.Put ("Data", W_Mask);
    Con_Io.Move ( (1, Col_Res), W_Mask); Con_Io.Put ("Results", W_Mask);
    Con_Io.Move ( ( 3,  0), W_Mask); Con_Io.Put ("Wind : ", W_Mask);
    Con_Io.Move ( ( 6,  0), W_Mask); Con_Io.Put ("Plane : ", W_Mask);
    Con_Io.Move ( ( 9,  0), W_Mask); Con_Io.Put ("Trajectory : ", W_Mask);
    Con_Io.Move ( (12,  0), W_Mask); Con_Io.Put ("Drift : ", W_Mask);
    Con_Io.Move ( ( 3, 14), W_Mask); Con_Io.Put ("speed", W_Mask);
    Con_Io.Move ( ( 4, 14), W_Mask); Con_Io.Put ("from", W_Mask);
    Con_Io.Move ( ( 6, 14), W_Mask); Con_Io.Put ("air speed", W_Mask);
    Con_Io.Move ( ( 7, 14), W_Mask); Con_Io.Put ("heading", W_Mask);
    Con_Io.Move ( ( 9, 14), W_Mask); Con_Io.Put ("ground speed", W_Mask);
    Con_Io.Move ( (10, 14), W_Mask); Con_Io.Put ("route", W_Mask);

    Col := 0;

    Con_Io.Move ( (0, Col), W_Act);
    Con_Io.Put ("Compute", W_Act, Background => Con_Io.Default_Background);
    Con_Io.Move ( (0, Col + Comp_Wid + 1), W_Act); Con_Io.Put (' ', W_Act);
    Col := Col + Comp_Wid + Act_Off;

    Con_Io.Move ( (0, Col), W_Act);
    Con_Io.Put ("Quit", W_Act, Background => Con_Io.Default_Background);
    Con_Io.Move ( (0, Col + Quit_Wid + 1), W_Act); Con_Io.Put (' ', W_Act);
    Col := Col + Quit_Wid + Act_Off;

    Con_Io.Move ( (0, Col), W_Act);
    Con_Io.Put ("Show help", W_Act, Background => Con_Io.Default_Background);
    Con_Io.Move ( (0, Col + Help_Wid + 1), W_Act); Con_Io.Put (' ', W_Act);
    Col := Col + Help_Wid + Act_Off;

    Con_Io.Move ( (0, Col), W_Act);
    Con_Io.Put ("Clear", W_Act, Background => Con_Io.Default_Background);
    Con_Io.Move ( (0, Col + Clea_Wid + 1), W_Act); Con_Io.Put (' ', W_Act);

  end Put_Mask;

  -- Row of a field in get and result areas
  function Fld_Row (Field : Nav_Data.T_List_Data) return Con_Io.Row_Range is
  begin
    case Field is
      when Nav_Data.Wind_S => return 0;
      when Nav_Data.Wind_A => return 1;
      when Nav_Data.Plan_S => return 3;
      when Nav_Data.Plan_A => return 4;
      when Nav_Data.Traj_S => return 6;
      when Nav_Data.Traj_A => return 7;
      when Nav_Data.Drift  => return 9;
    end case;
  end Fld_Row;

  -- Col of a field in get and result areas
  function Fld_Col (Field : Nav_Data.T_List_Data) return Con_Io.Row_Range is
  begin
    case Field is
      when Nav_Data.Wind_S | Nav_Data.Plan_S | Nav_Data.Traj_S => return 1;
      when Nav_Data.Wind_A | Nav_Data.Plan_A | Nav_Data.Traj_A => return 1;
      when Nav_Data.Drift  => return 0;
    end case;
  end Fld_Col;

  -- Get a problem data field
  procedure Get (Field : in Nav_Data.T_List_Data;
                 Blink : in Boolean := False;
                 Str : in out String;
                 Pos : in out Positive;
                 Insert : in out Boolean;
                 Next : out Movement) is
    Lstr : Con_Io.Unicode_Sequence (1 .. Str'Length);
    Last : Natural;
    Nxt : Movement;
    use type Con_Io.Curs_Mvt;
  begin
    Lstr := Language.Copy (Str);
    for I in 1 .. Time_Out_Get loop
      Con_Io.Move ( (Fld_Row(Field), Fld_Col(Field)), W_Get);
      if Blink then
        Con_Io.Put_Then_Get (Lstr, Last, Nxt, Pos, Insert, W_Get,
         Red, Time_Out => Delta_Get);
      else
        Con_Io.Put_Then_Get (Lstr, Last, Nxt, Pos, Insert, W_Get,
         Time_Out => Delta_Get);
      end if;
      Time_Task_Mng.Schedule;
      exit when Nxt /= Con_Io.Timeout;
    end loop;
    Next := Nxt;
    Str := Language.Copy (Lstr);
  end Get;

  -- Put the formated field when successfully got
  procedure Put (Field : in Nav_Data.T_List_Data; Str : in String;
   Blink : in Boolean := False) is
  begin
    Con_Io.Move ( (Fld_Row(Field), Fld_Col(Field)), W_Get);
    if Blink then
      Con_Io.Put (Str, W_Get, Red);
    else
      Con_Io.Put (Str, W_Get);
    end if;
  end Put;

  -- Put a field of the result
  procedure Put_Result (Field : in Nav_Data.T_List_Data; Str : in String) is
  begin
    Con_Io.Move ( (Fld_Row(Field), Fld_Col(Field)), W_Res);
    Con_Io.Put (Str, W_Res);
  end Put_Result;


  -- Draw a line of dots between field in got area and it in result area
  procedure Dot (Field : in Nav_Data.T_List_Data) is
    Dots : constant String (1 .. Width_Line) := (others => '.');
  begin
    -- Move (+3 cause in w_mask and not in w_get nor w_put)
    Con_Io.Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Con_Io.Put (Dots, W_Mask, Res_Fore);
  end Dot;

  -- Draw an arrow between a clear field in got area and the result
  procedure Arrow (Field : in Nav_Data.T_List_Data) is
    Minus : constant String (1 .. Width_Line-1) := (others => '-');
  begin
    Con_Io.Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Con_Io.Put (Minus & '>', W_Mask, Res_Fore);
  end Arrow;

  -- Clears a line of dots or an arrow
  procedure Clear_Line (Field : in Nav_Data.T_List_Data) is
    Spaces : constant String (1 .. Width_Line) := (others => ' ');
  begin
    Con_Io.Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Con_Io.Put (Spaces, W_Mask);
  end Clear_Line;

  function Get_Action return Action is
    Str : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    Cur_Action : Action;
    subtype Operation is Action range Compute .. Clear;

    function Act_Col (Oper : Operation) return Con_Io.Col_Range is
    begin
      case Oper is
        when Compute =>
         return Comp_Wid + 1;
        when Quit    =>
         return Comp_Wid + Act_Off + Quit_Wid + 1;
        when Help    =>
         return Comp_Wid + Act_Off + Quit_Wid + Act_Off + Help_Wid + 1;
        when Clear   =>
         return Comp_Wid + Act_Off + Quit_Wid + Act_Off + Help_Wid
          + Act_Off + Clea_Wid + 1;
      end case;
    end Act_Col;
    use type Con_Io.Curs_Mvt;
  begin
    Cur_Action := Compute;
    Stat := Con_Io.Right;
    loop
      -- Infinite get with Get_Back on Get_Back
      if Stat /= Con_Io.Timeout then
        Con_Io.Move (0, Act_Col(Cur_Action), W_Act);
        Con_Io.Put ('X', W_Act, Background => Get_Back);
      end if;
      Con_Io.Move (0, Act_Col(Cur_Action), W_Act);
      Con_Io.Get (Str, Last, Stat, Pos, Ins, W_Act, Time_Out => Delta_Get);
      Time_Task_Mng.Schedule;
      if Stat /= Con_Io.Timeout then
        Con_Io.Put (' ', W_Act, Background => Get_Back);
      end if;
      case Stat is
        when Con_Io.Up => return Prev;
        when Con_Io.Down | Con_Io.Pgdown | Con_Io.Pgup => return Next;
        when Con_Io.Ret => return Cur_Action;
        when Con_Io.Esc | Con_Io.Timeout | Con_Io.Full | Con_Io.Mouse_Button
           | Con_Io.Break | Con_Io.Ctrl_Pgup | Con_Io.Ctrl_Pgdown
           | Con_Io.Ctrl_Up   | Con_Io.Ctrl_Down
           | Con_Io.Ctrl_Left | Con_Io.Ctrl_Right | Con_Io.Selection =>
          null;
        when Con_Io.Left | Con_Io.Stab =>
          if Cur_Action /= Operation'First then
            Cur_Action := Operation'Pred (Cur_Action);
          else
            Cur_Action := Operation'Last;
          end if;
        when Con_Io.Right | Con_Io.Tab =>
          if Cur_Action /= Operation'Last then
            Cur_Action := Operation'Succ (Cur_Action);
          else
            Cur_Action := Operation'First;
          end if;
        when Con_Io.Fd_Event | Con_Io.Timer_Event | Con_Io.Signal_Event =>
          null;
        when Con_Io.Refresh =>
          return Refresh;
      end case;
    end loop;
  end Get_Action;

  -- Displays the "wrong format" error message
  procedure Err_Format is
  begin
    Con_Io.Move ( (0, 0), W_Err);
    Con_Io.Put ("ERROR : Wrong input format or bad value.", W_Err);
  end Err_Format;

  -- Display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure Err_Check (Error : in Nav_Data.T_Consistency) is
  begin
    Con_Io.Move ( (0, 0), W_Err);
    Con_Io.Put ("ERROR : ", W_Err);
    case Error is
      when Nav_Data.Known_Err =>
        Con_Io.Put ("Only 3 fields must be unknown.", W_Err);
      when Nav_Data.Angle_Err =>
        Con_Io.Put ("The 3 known angles are not consistent.", W_Err);
      when Nav_Data.Wind_Err =>
        Con_Io.Put ("The wind must be fully known or fully unknown.", W_Err);
      when Nav_Data.Traj_Err =>
        Con_Io.Put("If the route is unknown, the ground speed must be unknown as well.",
         W_Err);
      when Nav_Data.Drift_Err =>
        Con_Io.Put ( "If the drift is known, the heading or the route must be unknown.",
         W_Err);
      when Nav_Data.Val_Err =>
        Con_Io.Put ("The values are not compatible.", W_Err);
      when Nav_Data.Ok => null;
    end case;
  end Err_Check;

  procedure Clear_Err is
  begin
    Con_Io.Clear (W_Err);
  end Clear_Err;


  -- Ask the operator wether he realy wants to quit
  function Confirm_Quit return Boolean is
    Str : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    use type Con_Io.Curs_Mvt;
  begin
    Stat := Con_Io.Refresh;
    loop
      if Stat = Con_Io.Refresh then
        Con_Io.Move (Name => W_Err);
        Con_Io.Put ("Confirm you want to quit by entering 'Return': ", W_Err);
      end if;
      Con_Io.Move ( (0, 49), W_Err);
      Con_Io.Get (Str, Last, Stat, Pos, Ins, W_Err, Time_Out => Delta_Get);
      Time_Task_Mng.Schedule;
      exit when Stat /= Con_Io.Timeout;
    end loop;
    Con_Io.Clear (W_Err);
    if Stat = Con_Io.Ret then
      Abort_Clock;
    end if;
    return Stat = Con_Io.Ret;
  end Confirm_Quit;

  -- Displays the help screen
  procedure Put_Help is
    Str : Con_Io.Unicode_Sequence (1..0);
    Lst : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;

    procedure Put is
    begin
      Title;
      Con_Io.Clear (W_Help);
      Con_Io.Move ((0, 8), W_Help);
      Con_Io.Put ("heading and trailing spaces are ignored", W_Help);
      Con_Io.Move ((1, 8), W_Help);
      Con_Io.Put ("decimal are optional for speeds and angles", W_Help);
      Con_Io.Move ((2, 8), W_Help);
      Con_Io.Put ("+ is optional for the drift", W_Help);
      Con_Io.Move ((4, 0), W_Help);
      Con_Io.Put ("Constraints: ", W_Help);
      Con_Io.Move ((4, 13), W_Help);
      Con_Io.Put ("There must be 3 unknown fields", W_Help);
      Con_Io.Move ((5, 13), W_Help);
      Con_Io.Put ("The 3 angles (wind, heading, route) must be different, and", W_Help);
      Con_Io.Move ((6, 14), W_Help);
      Con_Io.Put ("heading and wind must be on different sides of the route", W_Help);
      Con_Io.Move ((7, 13), W_Help);
      Con_Io.Put ("The wind must be fully known or fully unknown", W_Help);
      Con_Io.Move ((8, 13), W_Help);
      Con_Io.Put ("If the route is unknown, the ground speed must be unknown as well", W_Help);
      Con_Io.Move ((9, 13), W_Help);
      Con_Io.Put ("If the drift is known, the heading or the route must be unknown",
       W_Help);
      Con_Io.Move ((10, 13), W_Help);
      Con_Io.Put ("The wind and the air speed must allow to follow the route",
       W_Help);
      Con_Io.Move ((13, 0), W_Help);
      Con_Io.Put ("Enter Return to go back to the data ", W_Help);
    end Put;
    use type Con_Io.Curs_Mvt;
  begin
    Stat := Con_Io.Refresh;
    loop
      if Stat = Con_Io.Refresh then
        Put;
      end if;
      Con_Io.Move ((13, 45), W_Help);
      Con_Io.Get (Str, Lst, Stat, Pos, Ins, W_Help,
       Con_Io.Default_Background, Con_Io.Default_Background, Delta_Get);
      Time_Task_Mng.Schedule;
      exit when Stat = Con_Io.Ret;
    end loop;
    Con_Io.Clear (W_Help);
  end Put_Help;

  procedure Abort_Clock is
  begin
    Con_Io.Move ( (0, 0), W_Act);
    Time_Task_Mng.Abort_Task;
  exception
    when Time_Task_Mng.Task_Aborted =>
      null;
  end Abort_Clock;


begin -- Nav_Screen
  Con_Io.Default_Background := Con_Io.Color_Of ("Black");
  Con_Io.Open (W_Title, ( 0,  0), ( 7, 79));
  Con_Io.Open (W_Mask,  ( 8,  0), (22, 79));
  Con_Io.Open (W_Help,  ( 8,  0), (22, 79));
  Con_Io.Open (W_Get,   (11, Col_Get), (20, Col_Get + 7));
  Con_Io.Open (W_Res,   (11, Col_Res), (20, Col_Res + 7));
  Con_Io.Open (W_Act,   (22,  0), (22, 79));
  Con_Io.Open (W_Err,   (24,  2), (24, 79));
  Con_Io.Open (W_Time,  ( 0, 60), ( 0, 79));
  Con_Io.Set_Background (Get_Back, W_Get);
  Con_Io.Set_Background (Get_Back, W_Act);
  Con_Io.Set_Foreground (Res_Fore, W_Res);
  Con_Io.Set_Foreground (Red, W_Err);
  Con_Io.Set_Foreground (Con_Io.Color_Of ("Light_Blue"), Name => W_Time);
end Nav_Screen;

