-- all the primitives to access the screen
with System;
with Task_Mng, Timers;
package body Nav_Screen is
  use Con_Io;

  -- the 8 needed windows
  W_Title, W_Mask, W_Get, W_Res, W_Act, W_Err, W_Help, W_Time : Window;

  -- the left column of get ang result areas
  Col_Get : constant Col_Range := 29;
  Col_Res : constant Col_Range := 70;
  -- start column in mask and width of dots and arrows
  Col_Line : constant Col_Range := 37;
  Width_Line : constant Positive := 32;
  -- columns of actions
  Comp_Wid : constant Col_Range :=  7;
  Quit_Wid : constant Col_Range :=  4;
  Help_Wid : constant Col_Range :=  9;
  Clea_Wid : constant Col_Range :=  5;
  Act_Off : constant := 5;

  -- background color of all get fields
  Get_Back : constant Effective_Basic_Colors := Blue;
  -- foreground color of results
  Res_Fore : constant Effective_Colors := Light_Green;

  -- delay max of a get (data or action) in seconds.
  Delta_Get : constant Con_Io.Delay_Rec(Timers.Delay_Sec) := 
    (Delay_Kind => Timers.Delay_Sec,
     Period => No_Period,
     Delay_Seconds => 0.5);
  -- number of deltas before clearing err messages
  Time_Out_Get : constant := 6;

  -- Time displaying
  procedure Show_Time is separate;

  package Time_Task_Mng is new Task_Mng (Call_Back => Show_Time);

  -- Clear all the screen
  procedure Reset is
  begin
    Reset_Term;
  end Reset;

  -- To write the title
  procedure Title is
  begin
    Time_Task_Mng.Start;
    Move ((0, 30), W_Title);
    Put ("AERONAUTICAL NAVIGATION", W_Title, Light_Blue);

    Move ((2, 0), W_Title);
    Put ("Keys: Enter, arrows, Ins, Del, Backspace, Home, End, Page Up,"
     & " Page Down", W_Title);
    Move ((3, 9), W_Title);
    Put ("digits, '.', '?', '+', '-'", W_Title);

    Move ((4, 0), W_Title);
    Put ("Format: speeds positives in knots or km/h (0.0 .. 999.9)",
     W_Title);
    Move ((5, 8), W_Title);
    Put ("angles positives in degrees and minutes (0.00 .. 359.59)",
     W_Title);
    Move ((6, 8), W_Title);
    Put ("drift in degrees and minutes (-90.00 .. +90.00)",
     W_Title);
    Move ((7, 8), W_Title);
    Put ("? in a field to clear it", W_Title);

    Show_Time;
    Time_Task_Mng.Schedule;

  end Title;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure Put_Mask is
    Col : Col_Range;
  begin
    Move ( (1, Col_Get), W_Mask); Put ("Data", W_Mask);
    Move ( (1, Col_Res), W_Mask); Put ("Results", W_Mask);
    Move ( ( 3,  0), W_Mask); Put ("Wind : ", W_Mask);
    Move ( ( 6,  0), W_Mask); Put ("Plane : ", W_Mask);
    Move ( ( 9,  0), W_Mask); Put ("Trajectory : ", W_Mask);
    Move ( (12,  0), W_Mask); Put ("Drift : ", W_Mask);
    Move ( ( 3, 14), W_Mask); Put ("speed", W_Mask);
    Move ( ( 4, 14), W_Mask); Put ("from", W_Mask);
    Move ( ( 6, 14), W_Mask); Put ("air speed", W_Mask);
    Move ( ( 7, 14), W_Mask); Put ("heading", W_Mask);
    Move ( ( 9, 14), W_Mask); Put ("ground speed", W_Mask);
    Move ( (10, 14), W_Mask); Put ("route", W_Mask);

    Col := 0;

    Move ( (0, Col), W_Act);
    Put ("Compute", W_Act, Background => Default_Background);
    Move ( (0, Col + Comp_Wid + 1), W_Act); Put (' ', W_Act);
    Col := Col + Comp_Wid + Act_Off;

    Move ( (0, Col), W_Act);
    Put ("Quit", W_Act, Background => Default_Background);
    Move ( (0, Col + Quit_Wid + 1), W_Act); Put (' ', W_Act);
    Col := Col + Quit_Wid + Act_Off;

    Move ( (0, Col), W_Act);
    Put ("Show help", W_Act, Background => Default_Background);
    Move ( (0, Col + Help_Wid + 1), W_Act); Put (' ', W_Act);
    Col := Col + Help_Wid + Act_Off;

    Move ( (0, Col), W_Act);
    Put ("Clear", W_Act, Background => Default_Background);
    Move ( (0, Col + Clea_Wid + 1), W_Act); Put (' ', W_Act);

  end Put_Mask;

  -- row of a field in get and result areas
  function Fld_Row (Field : Nav_Data.T_List_Data) return Row_Range is
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

  -- COL of a field in get and result areas
  function Fld_Col (Field : Nav_Data.T_List_Data) return Row_Range is
  begin
    case Field is
      when Nav_Data.Wind_S | Nav_Data.Plan_S | Nav_Data.Traj_S => return 1;
      when Nav_Data.Wind_A | Nav_Data.Plan_A | Nav_Data.Traj_A => return 1;
      when Nav_Data.Drift  => return 0;
    end case;
  end Fld_Col;

  -- get a problem data field
  procedure Get (Field : in Nav_Data.T_List_Data; Blink : in Boolean := False;
   Str : in out String; Pos : in out Positive; Insert : in out Boolean;
   Next : out Movement) is
    Last : Natural;
    Nxt : Movement;
  begin
    for I in 1 .. Time_Out_Get loop
      Move ( (Fld_Row(Field), Fld_Col(Field)), W_Get);
      if Blink then
        Con_Io.Put_Then_Get (Str, Last, Nxt, Pos, Insert, W_Get, Red,
         Con_Io.Blink, Time_Out => Delta_Get);
      else
        Con_Io.Put_Then_Get (Str, Last, Nxt, Pos, Insert, W_Get,
         Time_Out => Delta_Get);
      end if;
      Time_Task_Mng.Schedule;
      exit when Nxt /= Timeout;
    end loop;
    Next := Nxt;
  end Get;

  -- put the formated field when successfully got
  procedure Put (Field : in Nav_Data.T_List_Data; Str : in String;
   Blink : in Boolean := False) is
  begin
    Move ( (Fld_Row(Field), Fld_Col(Field)), W_Get);
    if Blink then
      Put (Str, W_Get, Red, Con_Io.Blink);
    else
      Put (Str, W_Get);
    end if;
  end Put;

  -- put a field of the result
  procedure Put_Result (Field : in Nav_Data.T_List_Data; Str : in String) is
  begin
    Move ( (Fld_Row(Field), Fld_Col(Field)), W_Res);
    Put (Str, W_Res);
  end Put_Result;


  -- draw a line of dots between field in got area and it in result area
  procedure Dot (Field : in Nav_Data.T_List_Data) is
    Dots : constant String (1 .. Width_Line) := (others => '.');
  begin
    -- move (+3 cause in w_mask and not in w_get nor w_put)
    Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Put (Dots, W_Mask, Res_Fore);
  end Dot;

  -- draw an arrow between a clear field in got area and the result
  procedure Arrow (Field : in Nav_Data.T_List_Data) is
    Minus : constant String (1 .. Width_Line-1) := (others => '-');
  begin
    Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Put (Minus & '>', W_Mask, Res_Fore);
  end Arrow;

  -- clears a line of dots or an arrow
  procedure Clear_Line (Field : in Nav_Data.T_List_Data) is
    Spaces : constant String (1 .. Width_Line) := (others => ' ');
  begin
    Move ( (Fld_Row(Field) + 3, Col_Line), W_Mask);
    Put (Spaces, W_Mask);
  end Clear_Line;

  function Get_Action return Action is
    Str : String (1 .. 0);
    Last : Natural;
    Stat : Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    Cur_Action : Action;
    subtype Operation is Action range Compute .. Clear;

    function Act_Col (Oper : Operation) return Col_Range is
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
  begin
    Cur_Action := Compute;
    Stat := Right;
    loop
      -- infinite get with GET_BACK on GET_BACK
      if Stat /= Timeout then
        Move (0, Act_Col(Cur_Action), W_Act);
        Put ('X', W_Act, Background => Get_Back);
      end if;
      Move (0, Act_Col(Cur_Action), W_Act);
      Get (Str, Last, Stat, Pos, Ins, W_Act, Time_Out => Delta_Get);
      Time_Task_Mng.Schedule;
      if Stat /= Timeout then
        Put (' ', W_Act, Background => Get_Back);
      end if;
      case Stat is
        when Up => return Prev;
        when Down | Pgdown | Pgup => return Next;
        when Ret => return Cur_Action;
        when Esc | Timeout | Full | Mouse_Button | Break | Ctrl_Pgup | Ctrl_Pgdown => null;
        when Left | Stab =>
          if Cur_Action /= Operation'First then
            Cur_Action := Operation'Pred (Cur_Action);
          else
            Cur_Action := Operation'Last;
          end if;
        when Right | Tab =>
          if Cur_Action /= Operation'Last then
            Cur_Action := Operation'Succ (Cur_Action);
          else
            Cur_Action := Operation'First;
          end if;
        when Fd_Event | Timer_Event | Con_Io.Signal_Event => 
          null;
        when Refresh => 
          return Refresh;
      end case;
    end loop;
  end Get_Action;

  -- displays the "wrong format" error message
  procedure Err_Format is
  begin
    Move ( (0, 0), W_Err);
    Put ("ERROR : Wrong input format or bad value.", W_Err);
  end Err_Format;

  -- display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure Err_Check (Error : in Nav_Data.T_Consistency) is
  begin
    Move ( (0, 0), W_Err);
    Put ("ERROR : ", W_Err);
    case Error is
      when Nav_Data.Known_Err =>
        Put ("Only 3 fields must be unknown.", W_Err);
      when Nav_Data.Angle_Err =>
        Put ("The 3 known angles are not consistent.", W_Err);
      when Nav_Data.Wind_Err =>
        Put ("The wind must be fully known or fully unknown.", W_Err);
      when Nav_Data.Traj_Err =>
        Put("If the route is unknown, the ground speed must be unknown as well.",
         W_Err);
      when Nav_Data.Drift_Err =>
        Put ( "If the drift is known, the heading or the route must be unknown.",
         W_Err);
      when Nav_Data.Val_Err =>
        Put ("The values are not compatible.", W_Err);
      when Nav_Data.Ok => null;
    end case;
  end Err_Check;

  procedure Clear_Err is
  begin
    Con_Io.Clear (W_Err);
  end Clear_Err;


  -- Ask the operator wether he realy wants to quit
  function Confirm_Quit return Boolean is
    Str : String (1 .. 0);
    Last : Natural;
    Stat : Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
  begin
    Stat := Con_Io.Refresh;
    loop
      if Stat = Con_Io.Refresh then
        Move (Name => W_Err);
        Put ("Confirm you want to quit by entering 'Return': ", W_Err);
      end if;
      Move ( (0, 49), W_Err);
      Get (Str, Last, Stat, Pos, Ins, W_Err, Time_Out => Delta_Get);
      Time_Task_Mng.Schedule;
      exit when Stat /= Con_Io.Timeout;
    end loop;
    Clear (W_Err);
    if Stat = Ret then
      Abort_Clock;
    end if;
    return Stat = Ret;
  end Confirm_Quit;

  -- displays the help screen
  procedure Put_Help is
    Str : String (1..0);
    Lst : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;

    procedure Put is
    begin
      Title;
      Clear (W_Help);
      Move ((0, 8), W_Help);
      Put ("heading and trailing spaces are ignored", W_Help);
      Move ((1, 8), W_Help);
      Put ("decimal are optional for speeds and angles", W_Help);
      Move ((2, 8), W_Help);
      Put ("+ is optional for the drift", W_Help);
      Move ((4, 0), W_Help);
      Put ("Constraints: ", W_Help);
      Move ((4, 13), W_Help);
      Put ("There must be 3 unknown fields", W_Help);
      Move ((5, 13), W_Help);
      Put ("The 3 angles (wind, heading, route) must be different, and", W_Help);
      Move ((6, 14), W_Help);
      Put ("heading and wind must be on different sides of the route", W_Help);
      Move ((7, 13), W_Help);
      Put ("The wind must be fully known or fully unknown", W_Help);
      Move ((8, 13), W_Help);
      Put ("If the route is unknown, the ground speed must be unknown as well", W_Help);
      Move ((9, 13), W_Help);
      Put ("If the drift is known, the heading or the route must be unknown",
       W_Help);
      Move ((10, 13), W_Help);
      Put ("The wind and the air speed must allow to follow the route",
       W_Help);
      Move ((13, 0), W_Help);
      Put ("Enter Return to go back to the data ", W_Help);
    end Put;

  begin
    Stat := Con_Io.Refresh;
    loop
      if Stat = Con_Io.Refresh then
        Put;
      end if;
      Move ((13, 45), W_Help);
      Get (Str, Lst, Stat, Pos, Ins, W_Help,
       Default_Background, Default_Blink_Stat, Default_Background,
       Delta_Get);
      Time_Task_Mng.Schedule;
      exit when Stat = Con_Io.Ret;
    end loop;
    Clear (W_Help);
  end Put_Help;

  procedure Abort_Clock is
  begin
    Move ( (0, 0), W_Act);
    Time_Task_Mng.Abort_Task;
  exception
    when Time_Task_Mng.Task_Aborted =>
      null;
  end Abort_Clock;


begin -- NAV_SCREEN
  Con_Io.Open (W_Title, ( 0,  0), ( 7, 79));
  Con_Io.Open (W_Mask,  ( 8,  0), (22, 79));
  Con_Io.Open (W_Help,  ( 8,  0), (22, 79));
  Con_Io.Open (W_Get,   (11, Col_Get), (20, Col_Get + 7));
  Con_Io.Open (W_Res,   (11, Col_Res), (20, Col_Res + 7));
  Con_Io.Open (W_Act,   (22,  0), (22, 79));
  Con_Io.Open (W_Err,   (24,  2), (24, 79));
  Con_Io.Open (W_Time,  ( 0, 60), ( 0, 79));
  Set_Background (Get_Back, W_Get);
  Set_Background (Get_Back, W_Act);
  Set_Foreground (Res_Fore, Not_Blink, W_Res);
  Set_Foreground (Red, Not_Blink, W_Err);
  Set_Foreground (Light_Blue, Name => W_Time);
end Nav_Screen;

