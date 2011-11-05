with Ada.Calendar;
with My_Math, Normal, Argument, Con_Io, Basic_Proc;
with Moon, Debug, Lem;
package body Screen is

  -- Lem position
  type Lem_Position (Set : Boolean := False) is record
    case Set is
      when True => Pos : Space.Position_Rec;
      when False => null;
    end case;
  end record;
  No_Pos : constant Lem_Position := (Set => False);

  -- Previous Lem position to rease on update/delete
  Prev_Pos : Lem_Position := No_Pos;
  -- Put the gauges each two displays of Lem
  Do_Put_Gauges : Boolean;

  -- Console and screen
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  -- First and Last X on screen for space
  First_X : Con_Io.X_Range;
  Last_X : Con_Io.X_Range;
  -- First and Last Y on screen for space
  First_Y : Con_Io.Y_Range;
  Last_Y : Con_Io.Y_Range;

  -- X and Y factor for space to screen conversion
  X_Factor : My_Math.Real;
  Y_Factor : My_Math.Real;

  -- Screen coordinate
  type Coordinate_Rec is record
    X : Con_Io.X_Range;
    Y : Con_Io.Y_Range;
  end record;

  -- Space to screen conversion
  function X_To_Screen (X_Pos : Space.Position_Range) return Con_Io.X_Range is
    use type My_Math.Real;
  begin
    return First_X + Integer(My_Math.Trunc(My_Math.Real(X_Pos) * X_Factor));
  end X_To_Screen;
  function Y_To_Screen (Y_Pos : Space.Position_Range) return Con_Io.Y_Range is
    use type My_Math.Real;
  begin
    return First_Y + Integer(My_Math.Trunc(My_Math.Real(Y_Pos) * Y_Factor));
  end Y_To_Screen;
  function To_Screen (Position : Space.Position_Rec)
           return Coordinate_Rec is
    use type My_Math.Real;
  begin
    return (X => X_To_Screen (Position.X_Pos),
            Y => Y_To_Screen (Position.Y_Pos) );
  end To_Screen;

  -- Screen position of gauge names (thrust, speeds and fuel)
  Thn, Vsn, Fun, Hsn : Coordinate_Rec;

  -- Screen position of Get square
  Get_Pos : Con_Io.Square;

  -- Gauges definition
  Gauge_Size : constant := 5;
  -- Thrust
  Thx : Con_Io.X_Range;
  Thymin, Thymax : Con_Io.Y_Range;
  Thfactor : My_Math.Real;
  -- Vertical speed
  use type Lem.Speed_Range;
  Max_Vert_Speed : constant Lem.Speed_Range := 5.0 * Flight.Max_Verti_Speed;
  Vsx : Con_Io.X_Range;
  Vsymin, Vsymax, Vsymid : Con_Io.Y_Range;
  Vsfactor : My_Math.Real;
  -- Fuel
  Fuxmin, Fuxmax : Con_Io.X_Range;
  Fuy : Con_Io.Y_Range;
  Fufactor : My_Math.Real;
  -- Horizontal speed
  Max_Hori_Speed : constant Lem.Speed_Range := 5.0 * Flight.Max_Horiz_Speed;
  Hsxmin, Hsxmax, Hsxmid : Con_Io.X_Range;
  Hsy : Con_Io.Y_Range;
  Hsfactor : My_Math.Real;

  -- Close (definitively)
  procedure Close is
  begin
    Console.Close;
  end Close;

  -- Pointer grabbing switch
  Pointer_Grabbed : Boolean;

  -- Reset screen. Display titles and moon ground
  procedure Init is
    use type My_Math.Real;
  begin
    -- Reset screen and hide mouse
    if not Console.Is_Open then
      Console.Open (Def_Back => Con_Io.Color_Of ("Black"));
      Screen.Set_To_Screen (Console'Access);
    end if;
    Screen.Clear;
    begin
      if Argument.Get_Parameter (1, "g") = "" then
        -- "-g": Grab
        Pointer_Grabbed := True;
      else
        -- "-grab"
        Pointer_Grabbed := True;
      end if;
    exception
      when Argument.Argument_Not_Found =>
        Pointer_Grabbed := False;
    end;
    Console.Set_Pointer_Shape (Con_Io.None, Pointer_Grabbed);
    -- Clear previous LEM position
    Prev_Pos := No_Pos;
    -- Compute space
    -- Space last X leaves "Th Vs" (5) for Thrust and V speed
    First_X := 1;
    Last_X := Console.X_Max - Console.Font_Width * 5 - 2;
    -- Space first Y leaves "Hs" for H speed and Fu for fuel
    First_Y := Console.Font_Height * 2 + 2;
    Last_Y := Console.Y_Max - 1;
    -- Compute conversion factors
    X_Factor := My_Math.Real (Last_X - First_X) / My_Math.Real (Space.X_Max);
    Y_Factor := My_Math.Real (Last_Y - First_Y) / My_Math.Real (Space.Y_Max);
    -- Compute position of gauge names
    Thn := (Last_X + 2, First_Y + 1);
    Vsn := (Thn.X + Console.Font_Width * 3 - 4, Thn.Y);
    Fun := (First_X + 1, First_Y - Console.Font_Height);
    Hsn := (Fun.X, Fun.Y - Console.Font_Height + 1);
    -- Compute position and factor of gauges
    -- Thrust
    Thx := Thn.X + 6;
    Thymin := Thn.Y +  Console.Font_Height;
    Thymax := Last_Y;
    Thfactor := My_Math.Real(Thymax - Thymin) / My_Math.Real(Lem.Max_Y_Thrust);
    -- Vert speed
    Vsx := Vsn.X + 6;
    Vsymin := Thymin;
    Vsymax := Thymax;
    Vsymid := (Vsymin + Vsymax) / 2;
    Vsfactor := My_Math.Real(Vsymax - Vsymid) / My_Math.Real(Max_Vert_Speed);
    -- Fuel
    Fuxmin := Fun.X + Console.Font_Width * 4;
    Fuxmax := Last_X;
    Fuy := Fun.Y + 1;
    Fufactor := My_Math.Real(Fuxmax - Fuxmin) / My_Math.Real(Lem.Max_Fuel);
    -- Horizontal speed
    Hsxmin := Hsn.X + Console.Font_Width * 4;
    Hsxmax := Last_X;
    Hsxmid := (Hsxmin + Hsxmax) / 2;
    Hsy := Hsn.Y + 1;
    Hsfactor := My_Math.Real(Hsxmax - Hsxmid) / My_Math.Real(Max_Hori_Speed);
    -- Put constant info
    Get_Pos := (23, 3);
    Refresh;
    Do_Put_Gauges := True;
  end Init;

  -- Put "constant" info
  procedure Refresh is
    Ground : constant Moon.Ground_Array := Moon.Get_Ground;
    J : Positive;
    Screen_Point : Coordinate_Rec;
    Screen_Ground : Con_Io.Natural_Array (1 .. (Ground'Length + 2) * 2);
    use type My_Math.Real;
  begin
    Screen.Clear;
    -- Workaround to bug with ATI 3D driver: Ground is not properly
    --  drawn on refresh (during game or pause)
    --  without a flush and a small delay
    Console.Flush;
    delay 0.05;
    -- Draw ground
    J := 1;
    -- 1, 1
    Screen_Ground(J) := First_X;
    J := J + 1;
    Screen_Ground(J) := First_Y;
    J := J + 1;
    for I in Ground'Range loop
      -- Convert this ground point to screen
      Screen_Point := To_Screen (Ground (I));
      -- Store X then Y
      Screen_Ground(J) := Screen_Point.X;
      J := J + 1;
      Screen_Ground(J) := Screen_Point.Y;
      J := J + 1;
    end loop;
    -- Max, 1
    Screen_Ground(J) := Last_X;
    J := J + 1;
    Screen_Ground(J) := First_Y;
    Screen.Set_Foreground (Con_Io.Color_Of ("Light_Grey"));
    Console.Fill_Area (Screen_Ground);
    -- Frame
    Screen.Set_Foreground (Con_Io.Color_Of ("Blue"));
    Console.Draw_Rectangle (First_X - 1, First_Y - 1,
                                    Last_X + 1, Last_Y + 1);
    -- Gauge Letters
    Console.Put ("Th", Thn.X, Thn.Y);
    Console.Put ("Vs", Vsn.X, Vsn.Y);
    Console.Put ("Fu:", Fun.X, Fun.Y);
    Console.Put ("Hs:", Hsn.X, Hsn.Y);
    -- Done
    Console.Flush;
  end Refresh;

  -- Draw/hide the LEM
  procedure Draw_Lem (Pos : Space.Position_Rec) is
    -- Lower left and upper right of LEM
    -- Bottom and top of left and right feet
    Ll, Ur, Blf, Tlf, Brf, Trf : Coordinate_Rec;
    --   +--------+
    --   |        |
    --   +---+----+
    --     /    \
    --    /      \
    use type My_Math.Real;
    Height : constant Con_Io.Y_Range
           := Integer(My_Math.Trunc(My_Math.Real(Lem.Width) / 2.0 * Y_Factor));
    use type Space.Position_Range;
  begin
    -- Compute Lower left and Upper right corners (Pos is LEM center)
    Ll := To_Screen ( (Pos.X_Pos - Lem.Width / 2.0,
                       Pos.Y_Pos) );
    Ur := (X => X_To_Screen (Pos.X_Pos + Lem.Width / 2.0),
           Y => Ll.Y + Height);
    -- Compute left and right legs bottom and top
    Blf := (X => Ll.X,
            Y => Ll.Y - Height);
    Brf := (X => Ur.X,
            Y => Blf.Y);
    Tlf := (X => X_To_Screen (Pos.X_Pos - Lem.Width / 4.0),
            Y => Ll.Y);
    Trf := (X => X_To_Screen (Pos.X_Pos + Lem.Width / 4.0),
            Y => Ll.Y);
    -- Fill body
    Console.Fill_Rectangle (Ll.X, Ll.Y, Ur.X, Ur.Y);
    -- Draw legs
    Console.Draw_Line (Tlf.X, Tlf.Y, Blf.X, Blf.Y);
    Console.Draw_Line (Tlf.X + 1, Tlf.Y, Blf.X + 1, Blf.Y);
    Console.Draw_Line (Trf.X, Trf.Y, Brf.X, Brf.Y);
    Console.Draw_Line (Trf.X - 1, Trf.Y, Brf.X - 1, Brf.Y);
  end Draw_Lem;

  -- Update (hide then draw) the gauges
  procedure Put_Gauges (Flight_Status : in Flight.Status_Rec;
                        Elapsed_Time  : in Chronos.Date_Rec) is
    -- Extra info to get directly from the Lem
    Y_Thrust : constant Lem.Y_Thrust_Range := Lem.Get_Y_Thrust;
    Fuel     : constant Lem.Fuel_Range := Lem.Get_Fuel;
    -- Sizes on screen and temp variables
    Thrust_Size : Natural;
    Vspeed : Lem.Speed_Range;
    Vspeed_Size : Integer;
    Fuel_Size : Natural;
    Hspeed : Lem.Speed_Range;
    Hspeed_Size : Integer;
    Blue : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Blue");
    Red : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Red");
    Yellow : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Yellow");
    Magenta : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Magenta");
    Lime_Green : constant Con_Io.Effective_Colors
               := Con_Io.Color_Of ("Lime_Green");
    use type My_Math.Real, Lem.Mass_Range, Flight.Status_List;
  begin
    -- Thrust gauge
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Fill_Rectangle (Thx, Thymin, Thx + Gauge_Size, Thymax);
    Thrust_Size := Natural (My_Math.Trunc(
                   My_Math.Real(Y_Thrust) * Thfactor));
    Screen.Set_Foreground (Blue);
    Console.Fill_Rectangle (Thx, Thymin, Thx + Gauge_Size,
                                    Thymin + Thrust_Size);
    -- Vertical speed
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Fill_Rectangle (Vsx, Vsymin, Vsx + Gauge_Size, Vsymax);
    Vspeed := Flight_Status.Speed.Y_Speed;
    if Vspeed > Max_Vert_Speed then
      Vspeed := Max_Vert_Speed;
    elsif Vspeed < -Max_Vert_Speed then
      Vspeed := -Max_Vert_Speed;
    end if;
    Screen.Set_Foreground (Blue);
    Console.Draw_Line (Vsx - 3, Vsymid, Vsx + Gauge_Size + 3, Vsymid);
    Vspeed_Size := Integer (My_Math.Trunc(
                   My_Math.Real(Vspeed) * Vsfactor));
    if Vspeed < -Flight.Max_Verti_Speed then
      Screen.Set_Foreground (Red);
    end if;
    -- Small adjustment
    Vspeed_Size := Vsymid + Vspeed_Size;
    if Vspeed_Size < Vsymin then
      Vspeed_Size := Vsymin;
    elsif Vspeed_Size > Vsymax then
      Vspeed_Size := Vsymax;
    end if;
    Console.Fill_Rectangle (Vsx, Vsymid, Vsx + Gauge_Size,
                                    Vspeed_Size);
    -- Fuel
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Fill_Rectangle (Fuxmin, Fuy, Fuxmax, Fuy + Gauge_Size);
    Fuel_Size := Natural (My_Math.Trunc(
                   My_Math.Real(Fuel) * Fufactor));
    if Fuel >= Lem.Max_Fuel / 5.0 then
      Screen.Set_Foreground (Blue);
    elsif Fuel >= Lem.Max_Fuel / 10.0 then
      Screen.Set_Foreground (Yellow);
    else
      Screen.Set_Foreground (Red);
    end if;
    Console.Fill_Rectangle (Fuxmin, Fuy,
                                    Fuxmin + Fuel_Size, Fuy + Gauge_Size);
    -- Horizontal speed
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Fill_Rectangle (Hsxmin, Hsy, Hsxmax, Hsy + Gauge_Size);
    Hspeed := Flight_Status.Speed.X_Speed;
    if Hspeed > Max_Hori_Speed then
      Hspeed := Max_Hori_Speed;
    elsif Hspeed < -Max_Hori_Speed then
      Hspeed := -Max_Hori_Speed;
    end if;
    Screen.Set_Foreground (Blue);
    Console.Draw_Line (Hsxmid, Hsy - 3, Hsxmid, Hsy + Gauge_Size + 3);
    Hspeed_Size := Integer (My_Math.Trunc(
                   My_Math.Real(Hspeed) * Hsfactor));
    if abs Hspeed > Flight.Max_Horiz_Speed then
      Screen.Set_Foreground (Red);
    end if;
    Console.Fill_Rectangle (Hsxmid, Hsy, Hsxmid + Hspeed_Size,
                                    Hsy + Gauge_Size);
    -- Approach/Landed indicator
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Put ("     ", Thn.X, Fun.Y);
    if Flight_Status.Status = Flight.Approaching then
      Screen.Set_Foreground (Lime_Green);
      Console.Put ("APPR ", Thn.X, Fun.Y);
    elsif Flight_Status.Status = Flight.Close then
      Screen.Set_Foreground (Yellow);
      Console.Put ("CLOSE", Thn.X, Fun.Y);
    elsif Flight_Status.Status = Flight.Landed
    or else Flight_Status.Status = Flight.Safe_Landed then
      Screen.Set_Foreground (Magenta);
      Console.Put ("LAND ", Thn.X, Fun.Y);
    end if;
    -- Elapsed time "mm.ss"
    Screen.Set_Foreground (Screen.Get_Background);
    Console.Put ("    ", Thn.X, Hsn.Y);
    Screen.Set_Foreground (Blue);
    Console.Put (
             Normal (Elapsed_Time.Minutes, 2, True, '0') & "."
           & Normal (Elapsed_Time.Seconds, 2, True, '0'),
             Thn.X, Hsn.Y);
  end Put_Gauges;

  -- Update lem and show the gauges
  procedure Update (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Date_Rec;
                    Update_Gauges : in Boolean) is

  begin
    if Prev_Pos.Set then
      -- Hide prev pos
      Screen.Set_Foreground (Screen.Get_Background);
      Draw_Lem (Prev_Pos.Pos);
    end if;
    -- Show new pos
    Screen.Set_Foreground (Con_Io.Color_Of ("Cyan"));
    Draw_Lem (Flight_Status.Pos);
    -- Save pos
    Prev_Pos := (True, Flight_Status.Pos);
    if Update_Gauges then
      Do_Put_Gauges := True;
    end if;
    -- Show Y thrust, speeds and fuel each 2 times lem is shown
    if Do_Put_Gauges then
      Put_Gauges (Flight_Status, Elapsed_Time);
    end if;
    Do_Put_Gauges := not Do_Put_Gauges;
    Console.Flush;
  end Update;

  -- Delete lem and show the gauges
  procedure Delete (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Date_Rec) is
  begin
    if Prev_Pos.Set then
      -- Hide prev pos
      Screen.Set_Foreground (Screen.Get_Background);
      Draw_Lem (Prev_Pos.Pos);
    end if;
    Prev_Pos := No_Pos;
    -- Show Y thrust, speeds and fuel
    Put_Gauges (Flight_Status, Elapsed_Time);
    Console.Flush;
  end Delete;


  -- Put text in the center text (within X range)
  procedure Center (Str : in String;
                    Y : Con_Io.Y_Range) is
    X : Con_Io.X_Range;
  begin
    -- Middle
    X := First_X + (First_X + Last_X) / 2;
    -- Start of text
    X := X -(Str'Length * Console.Font_Width) / 2;
    -- Put
    Console.Put (Str, X, Y);
  end Center;

  -- Put game end
  -- subtype End_Reason_List is Flight.Status_List
  --                            range Flight.Landed .. Flight.Lost;
  function Y_Text return Con_Io.Y_Range is
  begin
    return 20 * Console.Font_Height;
  end Y_Text;
  function Y_Offset return Con_Io.Y_Range is
  begin
    return 3 * Console.Font_Height / 2;
  end Y_Offset;

  procedure Put_End (Reason : in End_Reason_List) is
    use type Flight.Status_List;
    Factor : Natural;
  begin
    case Reason is
      when Flight.Landed =>
        Screen.Set_Foreground (Con_Io.Color_Of ("Lime_Green"));
        Center ("You landed the LEM", Y_Text);
      when Flight.Safe_Landed =>
        Screen.Set_Foreground (Con_Io.Color_Of ("Lime_Green"));
        Center ("You landed the LEM safely", Y_Text);
      when Flight.Lost =>
        Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));
        Center ("You lost the LEM", Y_Text);
      when Flight.Crashed =>
        Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));
        Center ("You crashed the LEM", Y_Text);
    end case;
    Screen.Set_Foreground (Con_Io.Color_Of ("Light_Grey"));
    case Reason is
      when Flight.Landed | Flight.Safe_Landed =>
        Center ("Hit Return or click middle button for a new game",
                Y_Text - Y_Offset * 2);
        Center ("or hit Shit-Tab to redo this game", Y_Text - Y_Offset * 3);
        Factor := 4;
      when Flight.Lost | Flight.Crashed =>
        Center ("Hit Return or click middle button to retry",
                Y_Text - Y_Offset * 2);
        Factor := 3;
    end case;
    Center ("or hit Escape to quit", Y_Text - Y_Offset * Factor);
    Console.Flush;
  end Put_End;

  procedure Put_Pause is
  begin
    Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));
    Screen.Set_Xor_Mode (Con_Io.Xor_On);
    Center ("Game Paused", Y_Text);
    Center ("Hit Space to resume", Y_Text - Y_Offset);
    Screen.Set_Xor_Mode (Con_Io.Xor_Off);
  end Put_Pause;

  -- Memory of prev event to handle double click
  type Repeat_Action_List is (Right_Key, Left_Key, None);
  Prev_Click_Action : Repeat_Action_List := None;
  Prev_Click_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
  -- Get an event
  function Get_Event (Wait : in Duration) return Evt_Rec is separate;

  -- Check if two heights are the same on screen
  --  (to be used as a "flat" ground criteria)
  function Same_Height (A, B : Space.Position_Range) return Boolean is
  begin
    if Debug.Set_Flight then
      Basic_Proc.Put_Line_Error ("SCREEN same height " &
        Con_Io.Y_Range'Image (Y_To_Screen (A)) & " and " &
        Con_Io.Y_Range'Image (Y_To_Screen (B)));
    end if;
    return abs (Y_To_Screen (A) - Y_To_Screen (B)) <= 1;
  end Same_Height;

end Screen;

