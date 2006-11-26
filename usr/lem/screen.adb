with Ada.Text_Io;
with My_Math, Normal;
with Moon, Debug;
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

  -- First and Last X on screen for space
  First_X : Con_Io.Graphics.X_Range;
  Last_X : Con_Io.Graphics.X_Range;
  -- First and Last Y on screen for space
  First_Y : Con_Io.Graphics.Y_Range;
  Last_Y : Con_Io.Graphics.Y_Range;

  -- X and Y factor for space to screen conversion
  X_Factor : My_Math.Real;
  Y_Factor : My_Math.Real;

  -- Screen coordinate
  type Coordinate_Rec is record
    X : Con_Io.Graphics.X_Range;
    Y : Con_Io.Graphics.Y_Range;
  end record;

  -- Space to screen conversion
  function X_To_Screen (X_Pos : Space.Position_Range) return Con_Io.Graphics.X_Range is
    use type My_Math.Real;
  begin
    return First_X + Integer(My_Math.Trunc(My_Math.Real(X_Pos) * X_Factor));
  end X_To_Screen;
  function Y_To_Screen (Y_Pos : Space.Position_Range) return Con_Io.Graphics.Y_Range is
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

  -- Gauges definition
  Gauge_Size : constant := 5;
  -- Thrust
  Thx : Con_Io.Graphics.X_Range;
  Thymin, Thymax : Con_Io.Graphics.Y_Range;
  Thfactor : My_Math.Real;
  -- Vertical speed
  use type Lem.Speed_Range;
  Max_Vert_Speed : constant Lem.Speed_Range := 5.0 * Flight.Max_Verti_Speed;
  Vsx : Con_Io.Graphics.X_Range;
  Vsymin, Vsymax, Vsymid : Con_Io.Graphics.Y_Range;
  Vsfactor : My_Math.Real;
  -- Fuel
  Fuxmin, Fuxmax : Con_Io.Graphics.X_Range;
  Fuy : Con_Io.Graphics.Y_Range;
  Fufactor : My_Math.Real;
  -- Horizontal speed
  Max_Hori_Speed : constant Lem.Speed_Range := 5.0 * Flight.Max_Horiz_Speed;
  Hsxmin, Hsxmax, Hsxmid : Con_Io.Graphics.X_Range;
  Hsy : Con_Io.Graphics.Y_Range;
  Hsfactor : My_Math.Real;

  -- Close (definitively)
  procedure Close is
  begin
    Con_Io.Destroy;
  end Close;

  -- Reset screen. Display titles and moon ground
  procedure Init is
    use Con_Io.Graphics;
    use type My_Math.Real;
  begin
    -- Reset screen
    Con_Io.Init;
    Con_Io.Reset_Term;
    -- Clear previous LEM position
    Prev_Pos := No_Pos;
    -- Compute space
    -- Space last X leaves "TT VV" (5) for Thrust and V speed
    First_X := 1;
    Last_X := X_Max - Font_Width * 5 - 2;
    -- Space first Y leaves "H" for H speed and F for fuel
    First_Y := Font_Height * 2 + 2;
    Last_Y := Y_Max - 1;
    -- Compute conversion factors
    X_Factor := My_Math.Real (Last_X - First_X) / My_Math.Real (Space.X_Max);
    Y_Factor := My_Math.Real (Last_Y - First_Y) / My_Math.Real (Space.Y_Max);
    -- Compute position of gauge names
    Thn := (Last_X + 2, First_Y + 1);
    Vsn := (Thn.X + Con_Io.Graphics.Font_Width * 3 - 4, Thn.Y);
    Fun := (First_X + 1, First_Y - Con_Io.Graphics.Font_Height);
    Hsn := (Fun.X, Fun.Y - Con_Io.Graphics.Font_Height + 3);
    -- Compute position and factor of gauges
    -- Thrust
    Thx := Thn.X + 6;
    Thymin := Thn.Y +  Con_Io.Graphics.Font_Height;
    Thymax := Last_Y;
    Thfactor := My_Math.Real(Thymax - Thymin) / My_Math.Real(Lem.Max_Y_Thrust);
    -- Vert speed
    Vsx := Vsn.X + 6;
    Vsymin := Thymin;
    Vsymax := Thymax;
    Vsymid := (Vsymin + Vsymax) / 2;
    Vsfactor := My_Math.Real(Vsymax - Vsymid) / My_Math.Real(Max_Vert_Speed);
    -- Fuel
    Fuxmin := Fun.X + Con_Io.Graphics.Font_Width * 4;
    Fuxmax := Last_X;
    Fuy := Fun.Y + 1;
    Fufactor := My_Math.Real(Fuxmax - Fuxmin) / My_Math.Real(Lem.Max_Fuel);
    -- Horizontal speed
    Hsxmin := Hsn.X + Con_Io.Graphics.Font_Width * 4;
    Hsxmax := Last_X;
    Hsxmid := (Hsxmin + Hsxmax) / 2;
    Hsy := Hsn.Y + 1;
    Hsfactor := My_Math.Real(Hsxmax - Hsxmid) / My_Math.Real(Max_Hori_Speed);
    -- Put constant info
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
    J := J + 1;
    Con_Io.Set_Foreground (Con_Io.Light_Gray);
    Con_Io.Graphics.Fill_Area (Screen_Ground);
    -- Frame
    Con_Io.Set_Foreground (Con_Io.Blue);
    Con_Io.Graphics.Draw_Rectangle (First_X - 1, First_Y - 1,
                                    Last_X + 1, Last_Y + 1);
    -- Gauge Letters
    Con_Io.Graphics.Put ("Th", Thn.X, Thn.Y);
    Con_Io.Graphics.Put ("Vs", Vsn.X, Vsn.Y);
    Con_Io.Graphics.Put ("Fu:", Fun.X, Fun.Y);
    Con_Io.Graphics.Put ("Hs:", Hsn.X, Hsn.Y);
    -- Done
    Con_Io.Flush;
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
    Height : constant Con_Io.Graphics.Y_Range
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
    Con_Io.Graphics.Fill_Rectangle (Ll.X, Ll.Y, Ur.X, Ur.Y);
    -- Draw legs
    Con_Io.Graphics.Draw_Line (Tlf.X, Tlf.Y, Blf.X, Blf.Y);
    Con_Io.Graphics.Draw_Line (Tlf.X + 1, Tlf.Y, Blf.X + 1, Blf.Y);
    Con_Io.Graphics.Draw_Line (Trf.X, Trf.Y, Brf.X, Brf.Y);
    Con_Io.Graphics.Draw_Line (Trf.X - 1, Trf.Y, Brf.X - 1, Brf.Y);
  end Draw_Lem;

  -- Update (hide then draw) the gauges
  procedure Put_Gauges (Flight_Status : in Flight.Status_Rec;
                        Elapsed_Time  : in Chronos.Time_Rec) is
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
    use type My_Math.Real, Lem.Speed_Range, Lem.Mass_Range, Flight.Status_List;
  begin
    -- Thrust gauge
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Fill_Rectangle (Thx, Thymin, Thx + Gauge_Size, Thymax);
    Thrust_Size := Natural (My_Math.Trunc(
                   My_Math.Real(Y_Thrust) * Thfactor));
    Con_Io.Set_Foreground (Con_Io.Blue);
    Con_Io.Graphics.Fill_Rectangle (Thx, Thymin, Thx + Gauge_Size,
                                    Thymin + Thrust_Size);
    -- Vertical speed
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Fill_Rectangle (Vsx, Vsymin, Vsx + Gauge_Size, Vsymax);
    Vspeed := Flight_Status.Speed.Y_Speed;
    if Vspeed > Max_Vert_Speed then
      Vspeed := Max_Vert_Speed;
    elsif Vspeed < -Max_Vert_Speed then
      Vspeed := -Max_Vert_Speed;
    end if;
    Con_Io.Set_Foreground (Con_Io.Blue);
    Con_Io.Graphics.Draw_Line (Vsx - 3, Vsymid, Vsx + Gauge_Size + 3, Vsymid);
    Vspeed_Size := Integer (My_Math.Trunc(
                   My_Math.Real(Vspeed) * Vsfactor));
    if Vspeed < -Flight.Max_Verti_Speed then
      Con_Io.Set_Foreground (Con_Io.Red);
    end if;
    -- Small adjustment
    Vspeed_Size := Vsymid + Vspeed_Size;
    if Vspeed_Size < Vsymin then
      Vspeed_Size := Vsymin;
    elsif Vspeed_Size > Vsymax then
      Vspeed_Size := Vsymax;
    end if;
    Con_Io.Graphics.Fill_Rectangle (Vsx, Vsymid, Vsx + Gauge_Size,
                                    Vspeed_Size);
    -- Fuel
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Fill_Rectangle (Fuxmin, Fuy, Fuxmax, Fuy + Gauge_Size);
    Fuel_Size := Natural (My_Math.Trunc(
                   My_Math.Real(Fuel) * Fufactor));
    if Fuel < Lem.Max_Fuel / 5.0 then
      Con_Io.Set_Foreground (Con_Io.Red);
    else
      Con_Io.Set_Foreground (Con_Io.Blue);
    end if;
    Con_Io.Graphics.Fill_Rectangle (Fuxmin, Fuy,
                                    Fuxmin + Fuel_Size, Fuy + Gauge_Size);
    -- Horizontal speed
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Fill_Rectangle (Hsxmin, Hsy, Hsxmax, Hsy + Gauge_Size);
    Hspeed := Flight_Status.Speed.X_Speed;
    if Hspeed > Max_Hori_Speed then
      Hspeed := Max_Hori_Speed;
    elsif Hspeed < -Max_Hori_Speed then
      Hspeed := -Max_Hori_Speed;
    end if;
    Con_Io.Set_Foreground (Con_Io.Blue);
    Con_Io.Graphics.Draw_Line (Hsxmid, Hsy - 3, Hsxmid, Hsy + Gauge_Size + 3);
    Hspeed_Size := Integer (My_Math.Trunc(
                   My_Math.Real(Hspeed) * Hsfactor));
    if abs Hspeed > Flight.Max_Horiz_Speed then
      Con_Io.Set_Foreground (Con_Io.Red);
    end if;
    Con_Io.Graphics.Fill_Rectangle (Hsxmid, Hsy, Hsxmid + Hspeed_Size,
                                    Hsy + Gauge_Size);
    -- Approach/Landed indicator
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Put ("    ", Thn.X, Fun.Y);
    if Flight_Status.Status = Flight.Approaching then
      Con_Io.Set_Foreground (Con_Io.Light_Green);
      Con_Io.Graphics.Put ("APP ", Thn.X, Fun.Y);
    elsif Flight_Status.Status = Flight.Landed
    or else Flight_Status.Status = Flight.Safe_Landed then
      Con_Io.Set_Foreground (Con_Io.Magenta);
      Con_Io.Graphics.Put ("LAND", Thn.X, Fun.Y);
    end if;
    -- Elapsed time "mm.ss"
    Con_Io.Set_Foreground (Con_Io.Get_Background);
    Con_Io.Graphics.Put ("    ", Thn.X, Hsn.Y);
    Con_Io.Set_Foreground (Con_Io.Blue);
    Con_Io.Graphics.Put (
             Normal (Elapsed_Time.Minutes, 2, True, '0') & "."
           & Normal (Elapsed_Time.Seconds, 2, True, '0'),
             Thn.X, Hsn.Y);
  end Put_Gauges;

  -- Update lem and show the gauges
  procedure Update (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Time_Rec;
                    Update_Gauges : in Boolean) is

  begin
    if Prev_Pos.Set then
      -- Hide prev pos
      Con_Io.Set_Foreground (Con_Io.Get_Background);
      Draw_Lem (Prev_Pos.Pos);
    end if;
    -- Show new pos
    Con_Io.Set_Foreground (Con_Io.Cyan);
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
    Con_Io.Flush;
  end Update;

  -- Delete lem and show the gauges
  procedure Delete (Flight_Status : in Flight.Status_Rec;
                    Elapsed_Time  : in Chronos.Time_Rec) is
  begin
    if Prev_Pos.Set then
      -- Hide prev pos
      Con_Io.Set_Foreground (Con_Io.Get_Background);
      Draw_Lem (Prev_Pos.Pos);
    end if;
    Prev_Pos := No_Pos;
    -- Show Y thrust, speeds and fuel
    Put_Gauges (Flight_Status, Elapsed_Time);
    Con_Io.Flush;
  end Delete;


  -- Put text in the center text (within X range)
  procedure Center (Str : in String;
                    Y : Con_Io.Graphics.Y_Range) is
    X : Con_Io.Graphics.X_Range;
  begin
    -- Middle
    X := First_X + (First_X + Last_X) / 2;
    -- Start of text
    X := X -(Str'Length * Con_Io.Graphics.Font_Width) / 2;
    -- Put
    Con_Io.Graphics.Put (Str, X, Y);
  end Center;

  -- Put game end
  -- subtype End_Reason_List is Flight.Status_List
  --                            range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List) is
    use type Flight.Status_List;
    Y_Text : constant Con_Io.Graphics.Y_Range := 250;
    Y_Offset : constant Con_Io.Graphics.Y_Range
             := 2 * Con_Io.Graphics.Font_Height;
  begin
    case Reason is
      when Flight.Landed =>
        Con_Io.Set_Foreground (Con_Io.Light_Green);
        Center ("You landed the LEM", Y_Text);
      when Flight.Safe_Landed =>
        Con_Io.Set_Foreground (Con_Io.Light_Green);
        Center ("You landed the LEM safely", Y_Text);
      when Flight.Lost =>
        Con_Io.Set_Foreground (Con_Io.Magenta);
        Center ("You lost the LEM", Y_Text);
      when Flight.Crashed =>
        Con_Io.Set_Foreground (Con_Io.Magenta);
        Center ("You crashed the LEM", Y_Text);
    end case;
    Con_Io.Set_Foreground (Con_Io.Light_Gray);
    case Reason is
      when Flight.Landed | Flight.Safe_Landed =>
        Center ("Hit any key for a new game", Y_Text - Y_Offset);
      when Flight.Lost | Flight.Crashed =>
        Center ("Hit any key to retry", Y_Text - Y_Offset);
    end case;
    Center ("or Escape to quit",
            Y_Text - Y_Offset - Con_Io.Graphics.Font_Height);
    Con_Io.Flush;
  end Put_End;

  -- Get a key
  function Get_Key (Wait : in Duration) return Got_List is separate;

  -- Check if two heights are the same on screen
  --  (to be used as a "flat" ground criteria)
  function Same_Height (A, B : Space.Position_Range) return Boolean is
  begin
    if Debug.Set_Flight then
      Ada.Text_Io.Put_Line ("SCREEN same height " &
        Con_Io.Graphics.Y_Range'Image (Y_To_Screen (A)) & " and " &
        Con_Io.Graphics.Y_Range'Image (Y_To_Screen (B)));
    end if;
    return abs (Y_To_Screen (A) - Y_To_Screen (B)) <= 1;
  end Same_Height;

end Screen;

