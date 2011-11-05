-- All the primitives to access the screen
with Timers, Language;
package body Nav_Screen is

  Console : aliased Con_Io.Console;

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

  -- Clear all the screen
  procedure Reset is
  begin
    if not Console.Is_Open then
      Console.Open (Def_Back => Con_Io.Color_Of ("Black"));
      W_Title.Open (Console'Access, ( 0,  0), ( 7, 79));
      W_Mask.Open (Console'Access, ( 8,  0), (22, 79));
      W_Help.Open (Console'Access, ( 8,  0), (22, 79));
      W_Get.Open (Console'Access, (11, Col_Get), (20, Col_Get + 7));
      W_Res.Open (Console'Access, (11, Col_Res), (20, Col_Res + 7));
      W_Act.Open (Console'Access, (22,  0), (22, 79));
      W_Err.Open (Console'Access, (24,  2), (24, 79));
      W_Time.Open (Console'Access, ( 0, 60), ( 0, 79));
      W_Get.Set_Background (Get_Back);
      W_Act.Set_Background (Get_Back);
      W_Res.Set_Foreground (Res_Fore);
      W_Err.Set_Foreground (Red);
      W_Time.Set_Foreground (Con_Io.Color_Of ("Light_Blue"));
    end if;
    Console.Clear_Screen;
  end Reset;

  -- To write the title
  procedure Title is
  begin
    W_Title.Move ((0, 30));
    W_Title.Put ("AERONAUTICAL NAVIGATION", Con_Io.Color_Of ("Light_Blue"));

    W_Title.Move ((2, 0));
    W_Title.Put ("Keys: Enter, arrows, Ins, Del, Backspace, Home, End, Page Up,"
     & " Page Down");
    W_Title.Move ((3, 9));
    W_Title.Put ("digits, '.', '?', '+', '-'");

    W_Title.Move ((4, 0));
    W_Title.Put ("Format: speeds positives in knots or km/h (0.0 .. 999.9)");
    W_Title.Move ((5, 8));
    W_Title.Put ("angles positives in degrees and minutes (0.00 .. 359.59)");
    W_Title.Move ((6, 8));
    W_Title.Put ("drift in degrees and minutes (-90.00 .. +90.00)");
    W_Title.Move ((7, 8));
    W_Title.Put ("? in a field to clear it");

    Show_Time;

  end Title;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure Put_Mask is
    Col : Con_Io.Col_Range;
  begin
    W_Mask.Move ( (1, Col_Get)); W_Mask.Put ("Data");
    W_Mask.Move ( (1, Col_Res)); W_Mask.Put ("Results");
    W_Mask.Move ( ( 3,  0)); W_Mask.Put ("Wind : ");
    W_Mask.Move ( ( 6,  0)); W_Mask.Put ("Plane : ");
    W_Mask.Move ( ( 9,  0)); W_Mask.Put ("Trajectory : ");
    W_Mask.Move ( (12,  0)); W_Mask.Put ("Drift : ");
    W_Mask.Move ( ( 3, 14)); W_Mask.Put ("speed");
    W_Mask.Move ( ( 4, 14)); W_Mask.Put ("from");
    W_Mask.Move ( ( 6, 14)); W_Mask.Put ("air speed");
    W_Mask.Move ( ( 7, 14)); W_Mask.Put ("heading");
    W_Mask.Move ( ( 9, 14)); W_Mask.Put ("ground speed");
    W_Mask.Move ( (10, 14)); W_Mask.Put ("route");

    Col := 0;

    W_Act.Move ( (0, Col));
    W_Act.Put ("Compute", Background => Console.Background);
    W_Act.Move ( (0, Col + Comp_Wid + 1)); W_Act.Put (' ');
    Col := Col + Comp_Wid + Act_Off;

    W_Act.Move ( (0, Col));
    W_Act.Put ("Quit", Background => Console.Background);
    W_Act.Move ( (0, Col + Quit_Wid + 1)); W_Act.Put (' ');
    Col := Col + Quit_Wid + Act_Off;

    W_Act.Move ( (0, Col));
    W_Act.Put ("Show help", Background => Console.Background);
    W_Act.Move ( (0, Col + Help_Wid + 1)); W_Act.Put (' ');
    Col := Col + Help_Wid + Act_Off;

    W_Act.Move ( (0, Col));
    W_Act.Put ("Clear", Background => Console.Background);
    W_Act.Move ( (0, Col + Clea_Wid + 1)); W_Act.Put (' ');

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
      Show_Time;
      W_Get.Move ( (Fld_Row(Field), Fld_Col(Field)));
      if Blink then
        W_Get.Put_Then_Get (Lstr, Last, Nxt, Pos, Insert,
         Red, Time_Out => Delta_Get);
      else
        W_Get.Put_Then_Get (Lstr, Last, Nxt, Pos, Insert,
         Time_Out => Delta_Get);
      end if;
      exit when Nxt /= Con_Io.Timeout;
    end loop;
    Next := Nxt;
    Str := Language.Copy (Lstr);
  end Get;

  -- Put the formated field when successfully got
  procedure Put (Field : in Nav_Data.T_List_Data; Str : in String;
   Blink : in Boolean := False) is
  begin
    W_Get.Move ( (Fld_Row(Field), Fld_Col(Field)));
    if Blink then
      W_Get.Put (Str, Red);
    else
      W_Get.Put (Str);
    end if;
  end Put;

  -- Put a field of the result
  procedure Put_Result (Field : in Nav_Data.T_List_Data; Str : in String) is
  begin
    W_Res.Move ( (Fld_Row(Field), Fld_Col(Field)));
    W_Res.Put (Str);
  end Put_Result;


  -- Draw a line of dots between field in got area and it in result area
  procedure Dot (Field : in Nav_Data.T_List_Data) is
    Dots : constant String (1 .. Width_Line) := (others => '.');
  begin
    -- Move (+3 cause in w_mask and not in w_get nor w_put)
    W_Mask.Move ( (Fld_Row(Field) + 3, Col_Line));
    W_Mask.Put (Dots, Res_Fore);
  end Dot;

  -- Draw an arrow between a clear field in got area and the result
  procedure Arrow (Field : in Nav_Data.T_List_Data) is
    Minus : constant String (1 .. Width_Line-1) := (others => '-');
  begin
    W_Mask.Move ( (Fld_Row(Field) + 3, Col_Line));
    W_Mask.Put (Minus & '>', Res_Fore);
  end Arrow;

  -- Clears a line of dots or an arrow
  procedure Clear_Line (Field : in Nav_Data.T_List_Data) is
    Spaces : constant String (1 .. Width_Line) := (others => ' ');
  begin
    W_Mask.Move ( (Fld_Row(Field) + 3, Col_Line));
    W_Mask.Put (Spaces);
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
      Show_Time;
      if Stat /= Con_Io.Timeout then
        W_Act.Move (0, Act_Col(Cur_Action));
        W_Act.Put ('X', Background => Get_Back);
      end if;
      W_Act.Move (0, Act_Col(Cur_Action));
      W_Act.Get (Str, Last, Stat, Pos, Ins, Time_Out => Delta_Get);
      if Stat /= Con_Io.Timeout then
        W_Act.Put (' ', Background => Get_Back);
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
    W_Err.Move ( (0, 0));
    W_Err.Put ("ERROR : Wrong input format or bad value.");
  end Err_Format;

  -- Display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure Err_Check (Error : in Nav_Data.T_Consistency) is
  begin
    W_Err.Move ( (0, 0));
    W_Err.Put ("ERROR : ");
    case Error is
      when Nav_Data.Known_Err =>
        W_Err.Put ("Only 3 fields must be unknown.");
      when Nav_Data.Angle_Err =>
        W_Err.Put ("The 3 known angles are not consistent.");
      when Nav_Data.Wind_Err =>
        W_Err.Put ("The wind must be fully known or fully unknown.");
      when Nav_Data.Traj_Err =>
        W_Err.Put("If the route is unknown, the ground speed must be unknown as well.");
      when Nav_Data.Drift_Err =>
        W_Err.Put ( "If the drift is known, the heading or the route must be unknown.");
      when Nav_Data.Val_Err =>
        W_Err.Put ("The values are not compatible.");
      when Nav_Data.Ok => null;
    end case;
  end Err_Check;

  procedure Clear_Err is
  begin
    W_Err.Clear;
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
      Show_Time;
      if Stat = Con_Io.Refresh then
        W_Err.Move;
        W_Err.Put ("Confirm you want to quit by entering 'Return': ");
      end if;
      W_Err.Move ( (0, 49));
      W_Err.Get (Str, Last, Stat, Pos, Ins, Time_Out => Delta_Get);
      exit when Stat /= Con_Io.Timeout;
    end loop;
    Con_Io.Clear (W_Err);
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
      Console.Clear_Screen;
      Title;
      W_Help.Clear;
      W_Help.Move ((0, 8));
      W_Help.Put ("heading and trailing spaces are ignored");
      W_Help.Move ((1, 8));
      W_Help.Put ("decimal are optional for speeds and angles");
      W_Help.Move ((2, 8));
      W_Help.Put ("+ is optional for the drift");
      W_Help.Move ((4, 0));
      W_Help.Put ("Constraints: ");
      W_Help.Move ((4, 13));
      W_Help.Put ("There must be 3 unknown fields");
      W_Help.Move ((5, 13));
      W_Help.Put ("The 3 angles (wind, heading, route) must be different, and");
      W_Help.Move ((6, 14));
      W_Help.Put ("heading and wind must be on different sides of the route");
      W_Help.Move ((7, 13));
      W_Help.Put ("The wind must be fully known or fully unknown");
      W_Help.Move ((8, 13));
      W_Help.Put ("If the route is unknown, the ground speed must be unknown as well");
      W_Help.Move ((9, 13));
      W_Help.Put ("If the drift is known, the heading or the route must be unknown");
      W_Help.Move ((10, 13));
      W_Help.Put ("The wind and the air speed must allow to follow the route");
      W_Help.Move ((13, 0));
      W_Help.Put ("Enter Return to go back to the data ");
    end Put;
    use type Con_Io.Curs_Mvt;
  begin
    Stat := Con_Io.Refresh;
    loop
      Show_Time;
      if Stat = Con_Io.Refresh then
        Put;
      end if;
      W_Help.Move ((13, 45));
      W_Help.Get (Str, Lst, Stat, Pos, Ins,
       Con_Io.Default_Background, Con_Io.Default_Background, Delta_Get);
      exit when Stat = Con_Io.Ret;
    end loop;
    Con_Io.Clear (W_Help);
  end Put_Help;

end Nav_Screen;

