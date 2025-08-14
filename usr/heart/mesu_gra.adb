with Ada.Exceptions, Ada.Calendar;
with Afpx.Utils, Con_Io, Normal, My_Math, As.B, Language, Basic_Proc,
     Upper_Char, Environ, Str_Util, Images;
with Mesu_Def, Str_Mng, Mesu_Nam, Pers_Mng, Pers_Def, Mesu_Fil;
package body Mesu_Gra is
  use type My_Math.Real;

  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  -- X and Y first and last, in screen and reality
  X_First : constant Natural := 0;
  -- To be computed: 4 * font_width
  Xs_First : Con_Io.X_Range := 43;
  -- To be computed: X_Max
  Xs_Last  : Con_Io.X_Range;
  -- To be computed: last sample in time
  X_Last  : Natural;

  Y_First  : Pers_Def.Bpm_Range;
  Y_Last   : Pers_Def.Bpm_Range;
  Y_Step   : constant Pers_Def.Bpm_Range := 25;
  -- To be computed: 2 * font height
  Ys_First : Con_Io.Y_Range;
  -- To be computed: Y_Max - Max_Nb_Mesure * font height
  Ys_Last  : Con_Io.Y_Range;

  -- Scale factors from reality to screen
  -- To be computed
  X_Factor : Float;
  Y_Factor : Float;

  -- Font offset: how much to lower Y to get char "centered" to Y axis
  Font_Offset_Height : Con_Io.Y_Range;

  -- What to know about a record
  type Mesure_Cell is record
    Person : Pers_Def.Person_Rec;
    Mesure : Mesu_Def.Mesure_Rec;
    Drown  : Boolean;
  end record;

  -- The info stored about records
  subtype Mesure_Range is Natural range 0 .. Max_Nb_Mesure;
  Mesure_Array : array (1 .. Max_Nb_Mesure) of Mesure_Cell;
  Nb_Mesure : Mesure_Range;

  use type Pers_Def.Bpm_Range;

  -- From reality to screen
  function X_To_Screen (X : in Natural) return Con_Io.X_Range is
    (Con_Io.X_Range(Float(X - X_First) * X_Factor) + Xs_First);

  function Y_To_Screen (Bpm : in Pers_Def.Bpm_Range) return Con_Io.Y_Range is
    (Con_Io.Y_Range(Float(Bpm - Y_First) * Y_Factor) + Ys_First);

  -- From reality to screen
  function X_To_Screen_Secure (X : in Natural) return Integer is
    (Integer(Float(X - X_First) * X_Factor) + Xs_First);

  function Y_To_Screen_Secure (Bpm : in Pers_Def.Bpm_Range) return Integer is
    (Integer(Float(Bpm - Y_First) * Y_Factor) + Ys_First);

  -- From screen to reality
  function Screen_To_Reality (X : Con_Io.X_Range) return Natural is
  ( if X < Xs_First then
      X_First
    elsif X > Xs_Last then
      X_Last
    else
      Natural (Float (X - Xs_First) / X_Factor) + X_First
  );

  function Screen_To_Reality (Y : Con_Io.Y_Range) return Pers_Def.Bpm_Range is
  ( if Y < Ys_First then
      Y_First
    elsif Y > Ys_Last then
      Y_Last
    else
      Pers_Def.Bpm_Range (Float (Y - Ys_First) / Y_Factor) + Y_First
  );


  procedure Pixel (X : in Integer; Y : in Integer; In_Graphic : in Boolean) is
  begin
    if In_Graphic and then
    (       X < Xs_First or else X > Xs_Last
    or else Y < Ys_First or else Y > Ys_Last) then
      return;
    end if;
    Console.Draw_Point (X, Y);
  exception
    when others =>
      null;
  end Pixel;


  procedure Draw_Line (Xa : in Integer; Ya : in Integer;
                       Xb : in Integer; Yb : in Integer;
                       In_Graphic       : in Boolean := False;
                       Draw_First_Point : in Boolean := True) is
    -- Y := A * X + B;
    A : My_Math.Real;
    B : My_Math.Real;
    X, X1, X2 : Integer;
    Y, Y1, Y2 : Integer;
  begin
    if Xa = Xb then
      -- Vertical line. Must have Ya <= Yb
      if Ya <= Yb then
        for Y in Ya .. Yb loop
          Pixel (Xa, Y, In_Graphic);
        end loop;
      else
        for Y in reverse Ya .. Yb loop
          Pixel (Xa, Y, In_Graphic);
        end loop;
      end if;
    elsif Ya = Yb then
      -- Horizontal line
      if Xa <= Xb then
        for X in Xa .. Xb loop
          Pixel (X, Ya, In_Graphic);
        end loop;
      else
        for X in reverse Xa .. Xb loop
          Pixel (X, Ya, In_Graphic);
        end loop;
      end if;
    else
      -- Other lines
      if abs (Float(Xb - Xa) / Float (Console.X_Max -
                                      Con_Io.X_Range'First))
      >  abs (Float(Yb - Ya) / Float (Console.Y_Max -
                                      Con_Io.Y_Range'First) ) then
        if Xa < Xb then
          X1 := Xa;
          Y1 := Ya;
          X2 := Xb;
          Y2 := Yb;
        else
          X1 := Xb;
          Y1 := Yb;
          X2 := Xa;
          Y2 := Ya;
        end if;
        -- Y := A * X + B
        A := My_Math.Real (Y2 - Y1) / My_Math.Real (X2 - X1);
        B := My_Math.Real(Y1) - A * My_Math.Real(X1);
        for X in X1 .. X2 loop
          Y := Con_Io.Y_Range(My_Math.Round (A * My_Math.Real(X) + B));
          Pixel (X, Y, In_Graphic);
        end loop;
      else
        if Ya < Yb then
          X1 := Xa;
          Y1 := Ya;
          X2 := Xb;
          Y2 := Yb;
        else
          X1 := Xb;
          Y1 := Yb;
          X2 := Xa;
          Y2 := Ya;
        end if;
        -- X := A * Y + B
        A := My_Math.Real (X2 - X1) / My_Math.Real (Y2 - Y1);
        B := My_Math.Real(X1) - A * My_Math.Real(Y1);
        for Y in Y1 .. Y2 loop
          X := Con_Io.X_Range(My_Math.Round (A * My_Math.Real(Y) + B));
          Pixel (X, Y, In_Graphic);
        end loop;
      end if;
    end if;

    if not Draw_First_Point then
      Pixel (Xa, Ya, In_Graphic);
    end if;
  exception
    when others =>
      null;
  end Draw_Line;

  -- General refresh
  procedure Refresh (Tz : in Boolean);

  -- Graphic layout (help, scales, Tz)
  procedure Draw_Layout is
    Help_Color  : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
    Scale_Color : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
    -- Scale step on X in seconds
    Secs : Natural;
    -- Horizontal scales, depending on longest record
    subtype Unit_Str is String (1 .. 2);
    type Scale_Rec is record
       -- If Duration < Less_Than in hours
       Less_Than : Natural;
       -- Unit is
       Unit : Unit_Str;
       -- Unit factor in minutes
       Factor : Positive;
       -- Draw a line each in minutes
       Lines : Positive;
       -- Put value each Vals line drawn
       Vals : Positive;
     end record;
     type Scale_Array is array (Integer range <>) of Scale_Rec;
     Scales : constant Scale_Array := (
         -- Less that 3h => 10min & 30min
         (      3, "mn",       1,      10,  3),
         -- Less that 10h => 30min & 1h
         (     10, "h ",      60,      30,  2),
         -- Less that 1d => 1h & 6h
         (     24, "h ",      60,      60,  6),
         -- Less that 3d => 1/2d & 1d
         ( 3 * 24, "d ", 24 * 60, 12 * 60,  2),
         -- Less that 10d => 1d & 1d
         (10 * 24, "d ", 24 * 60, 24 * 60,  1),
         -- More that 10d => 1d & 10d
         (      0, "d ", 24 * 60, 24 * 60, 10));
    Scale : Scale_Rec;
    -- Scale step on Y in Bpm
    Bpm : Pers_Def.Bpm_Range;
    X : Con_Io.X_Range;
    Y : Con_Io.Y_Range;

  begin
    -- Help
    Screen.Move (Screen.Row_Range_Last, 5);
    Screen.Set_Foreground (Help_Color);
    Screen.Put ("'1' - '" & Normal(Nb_Mesure,1) & "', 'S', 'H': Show/hide  "
              & "/  T: Training zones  /  Esc: Quit");
    -- Axes of scale
    Screen.Set_Foreground (Scale_Color);
    Console.Draw_Line (Xs_First, Ys_First, Xs_Last, Ys_First);
    Console.Draw_Line (Xs_First, Ys_First, Xs_First, Ys_Last);
    -- Horizontal scale, find appropriate scale for X legend
    for S of Scales loop
       if S.Less_Than = 0
       or else X_Last < S.Less_Than * 60 * 60 then
         -- Set Scale Factor and Lines in seconds
         Scale := S;
         Scale.Factor := Scale.Factor * 60;
         Scale.Lines := Scale.Lines * 60;
         exit;
      end if;
    end loop;
    -- Horizontal scale : one + each 10 mn (600 seconds)
    --                    Time in mn each 3 +
    for I in 0 .. X_Last / Scale.Lines loop
      Secs := I * Scale.Lines;
      X := X_To_Screen (Secs);
      Console.Draw_Line (X, Ys_First - 2, X, Ys_First + 2);
      if I rem Scale.Vals = 0 or else I = X_Last / Scale.Lines then
        if X / Console.Font_Width + 2 <= Console.Col_Range_Last then
          Screen.Move (Console.Row_Range_Last - 1,
                       X / Console.Font_Width - 1);
        else
          Screen.Move (Console.Row_Range_Last - 1, Console.Col_Range_Last - 2);
        end if;
        if I = 0 then
          Screen.Put ("0" & Scale.Unit);
        else
          Screen.Put (Normal (Secs / Scale.Factor, 3));
        end if;
      end if;
    end loop;
    -- Vertical scale : one + each 25 Bpm
    --                  Bpm for each +
    for I in Y_First / Y_Step .. Y_Last / Y_Step loop
      Bpm := I * Y_Step;
      Y := Y_To_Screen (Bpm);
      Console.Draw_Line (Xs_First - 2, Y, Xs_First + 2, Y);
      Console.Put (Normal (Integer(Bpm), 3),
                           1,
                           Y + Font_Offset_Height);
    end loop;

  end Draw_Layout;

  procedure Draw_Tz (Show : in Boolean) is
    Tz_Color    : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
    Y : Con_Io.Y_Range;
    Mesure_Index : Mesure_Range;
  begin
    if not Show then
      Screen.Clear;
      Refresh (False);
      return;
    end if;
    Screen.Set_Foreground (Tz_Color);
    -- First drawn mesure if any (else first mesure)
    Mesure_Index := 1;
    for Mesu in 1 .. Nb_Mesure loop
      if Mesure_Array(Mesu).Drown then
        Mesure_Index := Mesu;
        exit;
      end if;
    end loop;

    -- Then drawn Tz
    for Bpm of Mesure_Array(Mesure_Index).Mesure.Tz loop
      if Bpm >= Y_First then
        Y := Y_To_Screen(Bpm);
        Draw_Line (Xs_First, Y, Xs_Last - 4 * Console.Font_Width, Y);
        Console.Put (Normal(Integer(Bpm), 3),
                     Console.X_Max - 3 * Console.Font_Width,
                     Y + Font_Offset_Height);
      end if;
    end loop;
  end Draw_Tz;

  -- Draw one record
  procedure Draw_Mesure (No : in Mesure_Range) is
    -- These are the colors in use for the samples names and graphs
    Colors : constant array (1 .. Max_Nb_Mesure) of Con_Io.Effective_Colors
           := (1 => Con_Io.Color_Of ("Blue"),
               2 => Con_Io.Color_Of ("Cyan"),
               3 => Con_Io.Color_Of ("White"),
               4 => Con_Io.Color_Of ("Brown"),
               5 => Con_Io.Color_Of ("Tomato"),
               6 => Con_Io.Color_Of ("Red"),
               7 => Con_Io.Color_Of ("Magenta"),
               8 => Con_Io.Color_Of ("Dark_Green"),
               9 => Con_Io.Color_Of ("Medium_Spring_Green"));
    Sec1, Sec2 : Natural;
    Bpm1, Bpm2 : Pers_Def.Bpm_Range;
    Mesure : Mesu_Def.Mesure_Rec renames Mesure_Array(No).Mesure;
    Title_Txt : As.B.Asb_Bs(Con_Io.Col_Range'Last);
  begin
    Screen.Set_Foreground (Colors(No));
    -- Person and date
    Screen.Move (No-1, 10);
    if Mesure.Samples.Length < 2 then
      -- 0 or only 1 sample. Cannot draw this one
      Title_Txt.Set ("(*)");
    else
      Title_Txt.Set ("   ");
    end if;
    Title_Txt.Append (
           Normal(No, 1) & ":"
         & Mesure_Array(No).Person.Name & " "
         & Mesure_Array(No).Person.Activity & " "
         & Str_Mng.To_Printed_Date_Str(Mesure.Date) & " "
         & Str_Mng.To_Printed_Time_Str(Mesure.Time) & " "
         & Mesure.Comment);
    Screen.Put (Title_Txt.Image);

    if Mesure.Samples.Length < 2 then
      return;
    end if;

    Sec1 := 0;
    Bpm1 := Mesure.Samples.Element (1);

    Pixel (X_To_Screen_Secure (Sec1), Y_To_Screen_Secure (Bpm1), True);
    for I in 2 .. Mesure.Samples.Length loop
      Sec2 := Sec1 + Integer(Mesure.Sampling_Delta);
      Bpm2 := Mesure.Samples.Element (I);
      -- Check in screen
      Draw_Line (X_To_Screen_Secure (Sec1), Y_To_Screen_Secure (Bpm1),
                 X_To_Screen_Secure (Sec2), Y_To_Screen_Secure (Bpm2),
                 In_Graphic => True, Draw_First_Point => False);
      Sec1 := Sec2;
      Bpm1 := Bpm2;
    end loop;

  end Draw_Mesure;

  procedure Draw_Position (X : in Con_Io.X_Range;
                           Y : in Con_Io.Y_Range;
                           Show : in Boolean := True) is
    -- "DDDd hh:mm:ss - xxxBpm" 22 characters
    Slot : String (1 .. 22) := (others => ' ');
    Pos_Row : constant Con_Io.Row_Range := Console.Row_Range_Last - 3;
    Pos_Col : constant Con_Io.Col_Range
            := Console.Col_Range_Last - Slot'Length;
    Pos_Color : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
    Time, Days : Natural;
    Dur : Ada.Calendar.Day_Duration;
    Bpm : Pers_Def.Bpm_Range;
  begin
    Screen.Move (Pos_Row, Pos_Col);
    if Show then
      Screen.Set_Foreground (Pos_Color);
      Bpm := Screen_To_Reality (Y);
      Time := Screen_To_Reality (X);
      Days := Time / (24 * 3600);
      Dur := Ada.Calendar.Day_Duration (Time - Days * 24 * 3600);
      if Days /= 0 then
        Slot (1 .. 4) := Normal (Days, 3) & "d";
      end if;
      Slot ( 6 .. 13) := Images.Dur_Image (Dur)(1 .. 8);
      Slot (14 .. 22) := " - " & Normal (Integer (Bpm), 3) & "Bpm";
    end if;
    Screen.Put (Slot);
  end Draw_Position;

  procedure Refresh (Tz : in Boolean) is
  begin
    Screen.Clear;
    Draw_Layout;
    Draw_Position (0, 0, False);
    -- Redraw mesures
    for I in 1 .. Nb_Mesure loop
      if Mesure_Array(I).Drown then
        Draw_Mesure (I);
      end if;
    end loop;
    -- Tz_Drown is already up to date
    if Tz then
      Draw_Tz(True);
    end if;
  end Refresh;

  -- The main
  procedure Graphic is
    Bkp_Ctx   : Afpx.Utils.Backup_Context;
    Line      : Afpx.Line_Rec;
    Screen_Id : Con_Io.Screen_Id_Range;
    Font_No : constant Con_Io.Font_No_Range := 1;
    Screen_Width  : Con_Io.X_Range;
    Screen_Height : Con_Io.Y_Range;
    Env_Geo_Name : constant String := "HEART_GRAPH";
    Font_Width  : Natural;
    Font_Height : Natural;
    Font_Offset : Natural;
    Width_Margin : constant Con_Io.X_Range := 5;
    Height_Margin : constant Con_Io.Y_Range := 10;
    Rows : Con_Io.Row_Range;
    Cols : Con_Io.Col_Range;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S    : Mesu_Nam.File_Date_Str;
    Time_S    : Mesu_Nam.File_Time_Str;
    Pid_S     : Mesu_Nam.File_Pid_Str;
    Pos_Pers  : Natural;
    Person    : Pers_Def.Person_Rec;
    Same_Tz   : Boolean;
    Tz_Drown  : Boolean;
    Get_Res   : Con_Io.Get_Result;
    Mouse_Evt : Con_Io.Mouse_Event_Rec(Con_Io.X_Y);
    Char      : Character;
    No_Mesure : Mesure_Range;
    Nb_Drown  : Mesure_Range;

    -- Check if same Tz
    procedure Check_Same_Tz is
      First_Drown_Mesure : Mesure_Range := 0;
    begin
      Same_Tz := True;
      for Mesu in 1 .. Nb_Mesure loop
        if Mesure_Array(Mesu).Drown then
          if First_Drown_Mesure = 0 then
            -- This one is the first drown mesure
            First_Drown_Mesure := Mesu;
          else
            -- Compare this Tz to the ones of first drown mesure
            for I in Pers_Def.Person_Tz_Array'Range loop
              if Mesure_Array(Mesu).Mesure.Tz(I) /=
                 Mesure_Array(First_Drown_Mesure).Mesure.Tz(I) then
                Same_Tz := False;
                return;
              end if;
            end loop;
          end if;
        end if;
      end loop;
      if First_Drown_Mesure = 0 then
        -- No mesure drown
        Same_Tz := False;
      end if;
    end Check_Same_Tz;


    procedure Close is
    begin
      -- Back to Afpx
      if Console.Is_Open then
        Console.Close;
      end if;
      if Afpx.Is_Suspended then
        Afpx.Resume;
      end if;
      Afpx.Redisplay;
    end Close;

    use type Con_Io.Curs_Mvt;
  begin
    -- Compute console size
    -- Get screen geometry
    Con_Io.Get_Screen_Geometry (Screen_Width, Screen_Height);
    -- Get ENV console screen and size
    -- <id>-<width>x<height>
    declare
      Env_Geo : constant String := Environ.Getenv (Env_Geo_Name);
      Env_Width  : Con_Io.X_Range;
      Env_Height : Con_Io.Y_Range;
      Sep1, Sep2 : Natural;
    begin
      Sep1 := Str_Util.Locate (Env_Geo, "-");
      Sep2 := Str_Util.Locate (Env_Geo, "x");
      if Sep1 = 0 then
        if Sep2 = 0 then
          -- Only the screen Id
          Screen_Id :=
              Con_Io.Screen_Id_Range'Value (Env_Geo(1 .. Env_Geo'Last));
        else
          -- Only the size
          Screen_Id := Con_Io.Default_Screen_Id;
        end if;
      else
        -- Both screen Id and size
        Screen_Id := Con_Io.Screen_Id_Range'Value (Env_Geo(1..Sep1 - 1));
      end if;
      -- Sep1 is 0 or index of '-'
      Env_Width  := Con_Io.X_Range'Value (Env_Geo (Sep1 + 1 .. Sep2 - 1));
      Env_Height := Con_Io.X_Range'Value (Env_Geo (Sep2 + 1 .. Env_Geo'Last));
      -- Everything went well
      Screen_Width  := Env_Width;
      Screen_Height := Env_Height;
    exception
      when others =>
        -- Default if we cannot get screen and geometry from env
        Screen_Id := Con_Io.Default_Screen_Id;
        -- Apply margin in percent and remain within Con_Io Row and Col ranges
        Screen_Width  := Screen_Width  - Screen_Width  * Width_Margin  / 100;
        Screen_Height := Screen_Height - Screen_Height * Height_Margin / 100;
    end;
    -- Convert size into row/col
    Con_Io.Get_Font_Geometry (Font_No, Font_Width, Font_Height, Font_Offset);
    if Screen_Height / Font_Height < Con_Io.Row_Range'Last then
      Rows := Screen_Height / Font_Height;
    else
      Rows := Con_Io.Row_Range'Last;
    end if;
    if Screen_Width / Font_Width < Con_Io.Col_Range'Last then
      Cols := Screen_Width / Font_Width;
    else
      Cols := Con_Io.Col_Range'Last;
    end if;

    -- Create a new console
    Afpx.Suspend;
    Console.Open (Screen_Id => Screen_Id,
                  Font_No  => Font_No,
                  Row_Last => Rows - 1,
                  Col_Last => Cols - 1,
                  Def_Back => Afpx.Get_Descriptor_Background);
    Console.Set_Name ("Heart draw");
    Screen.Set_To_Screen (Console'Access);
    Console.Set_Y_Mode (Con_Io.Con_Io_Mode);
    Screen.Set_Background (Con_Io.Color_Of ("Dark_Grey"));

    -- Screen scale
    Xs_First := 4 * Font_Width;
    Xs_Last  := Console.X_Max;
    Ys_First := Console.Font_Height * 2;
    Ys_Last := Console.Y_Max
             - Max_Nb_Mesure * Console.Font_Height;

    -- Init array of mesures
    -- List is not empty
    Bkp_Ctx.Backup;
    Nb_Mesure := 0;
    -- For each in list : store in array
    Afpx.Line_List.Rewind;
    loop
      -- Get line, file_name, split
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Mesu_Nam.Split_File_Name (File_Name, Date_S, Time_S, Pid_S);
      -- Get person
      Pers_Mng.Search (Pers_Def.The_Persons, Pers_Def.Pid_Range'Value(Pid_S),
                       Pos_Pers);
      Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
      -- Get mesure
      Nb_Mesure := Nb_Mesure + 1;
      Mesure_Array(Nb_Mesure).Person := Person;
      Mesure_Array(Nb_Mesure).Mesure := Mesu_Fil.Load (File_Name);
      Mesure_Array(Nb_Mesure).Drown  := False;

      -- Next line except if list empty or end of list
      exit when Afpx.Line_List.Is_Empty
      or else not Afpx.Line_List.Check_Move;

      Afpx.Line_List.Move_To;
    end loop;
    Bkp_Ctx.Restore (False);

    -- Find Y min and max
    Y_First := Pers_Def.Bpm_Range'Last;
    Y_Last := Pers_Def.Bpm_Range'First;
    for I in 1 .. Nb_Mesure loop
      -- Scan samples
      for J in 1 .. Mesure_Array(I).Mesure.Samples.Length loop
        if Y_First > Mesure_Array(I).Mesure.Samples.Element (J) then
          -- Smallest Y so far
          Y_First := Mesure_Array(I).Mesure.Samples.Element (J);
        end if;
        if Y_Last < Mesure_Array(I).Mesure.Samples.Element (J) then
          -- Biggest Y so far
          Y_Last := Mesure_Array(I).Mesure.Samples.Element (J);
        end if;
      end loop;
      -- Scan Tz
      for Tz of Mesure_Array(I).Mesure.Tz loop
        if Tz /= Pers_Def.Bpm_Range'First then
          if Y_First > Tz then
            Y_First := Tz;
          end if;
          if Y_Last < Tz then
            Y_Last := Tz;
          end if;
        end if;
      end loop;
    end loop;

    -- Y_First + Y_Step must be < Y_Last
    if Y_First + Y_Step < Y_Last then
      -- Yes, round to Y_Step
      Y_First := (Y_First / Y_Step) * Y_Step;
      Y_Last  := (Y_Last  / Y_Step + 1) * Y_Step;
    else
      -- Defaults
      Y_First := 50;
      Y_Last := 200;
    end if;

    -- Compute Y factor
    Y_Factor := Float(Ys_First - Ys_Last) / Float(Y_First - Y_Last);
    Font_Offset_Height := Console.Font_Height / 2;

    -- Compute last X
    X_Last := 0;
    for I in 1 .. Nb_Mesure loop
      if Mesure_Array(I).Mesure.Samples.Length
       * Integer(Mesure_Array(I).Mesure.Sampling_Delta) > X_Last then
          -- Greatest X so far
          X_Last := Mesure_Array(I).Mesure.Samples.Length
                  * Integer(Mesure_Array(I).Mesure.Sampling_Delta);
      end if;
    end loop;

    -- Compute X Factor
    X_Factor := Float(Xs_First - Xs_Last) / Float(X_First - X_Last);

    -- Graphic mode for current screen
    Screen.Clear;

    Draw_Layout;
    Tz_Drown := False;

    -- Draw all mesures
    for I in 1 .. Nb_Mesure loop
      Mesure_Array(I).Drown := True;
      Draw_Mesure (I);
    end loop;
    Nb_Drown := Nb_Mesure;

    -- Enable mouse motion events
    Console.Enable_Motion_Events (True);
    Console.Set_Pointer_Shape (Con_Io.Cross);

    Main_Loop:
    loop
      -- Get key
      Get_Res := Screen.Get;
      if Get_Res.Mvt = Con_Io.Full then
        Char := Upper_Char (Language.Unicode_To_Char (Get_Res.Char));
      else
        Char := '#';
      end if;

      if Get_Res.Mvt = Con_Io.Esc then
        -- Exit when Escape
        exit Main_Loop;
      elsif Get_Res.Mvt = Con_Io.Break then
        -- Break
        raise Pers_Def.Exit_Requested;
      elsif Char = 'T' then
        if Tz_Drown then
          -- Hide Tzs
          Draw_Tz(False);
          Tz_Drown := False;
        else
          Check_Same_Tz;
          if Same_Tz then
            -- Draw Tzs
            Draw_Tz(True);
            Tz_Drown := True;
          end if;
        end if;
      elsif Char >= '1' and then Char <= '9' then
        -- Draw if key in 1 .. 9 then
        No_Mesure := Character'Pos(Char) - Character'Pos('1') + 1;
        if No_Mesure <= Nb_Mesure then
          if not Mesure_Array(No_Mesure).Drown then
            Mesure_Array(No_Mesure).Drown := True;
            -- Drawing a new record : check if Tz to be hidden
            if Tz_Drown then
              Check_Same_Tz;
              if not Same_Tz then
                -- This mesure has a Tz incompatible with the drown Tzs
                -- Hide Tz
                Draw_Tz(False);
                Tz_Drown := False;
              end if;
            end if;
            Nb_Drown := Nb_Drown + 1;
            -- Draw this mesure
            Draw_Mesure (No_Mesure);
          else
            -- Hidding a record
            Mesure_Array(No_Mesure).Drown := False;
            Nb_Drown := Nb_Drown - 1;
          end if;
          if Nb_Drown = 0 and then Tz_Drown then
            -- Hide Tz if no mesure
            Draw_Tz (False);
            Tz_Drown := False;
          elsif not Mesure_Array(No_Mesure).Drown then
            -- Hide this mesure
            Refresh (Tz_Drown);
          end if;
        end if;
      elsif Char = 'S' or else Char = 'H' then
        -- Show or hide all mesures
        for I in 1 .. Nb_Mesure loop
          Mesure_Array(I).Drown := Char = 'S';
        end loop;
        Refresh (Tz_Drown);
      elsif Get_Res.Mvt = Con_Io.Mouse_Button then
        Console.Get_Mouse_Event (Mouse_Evt, Con_Io.X_Y);
        Draw_Position (Mouse_Evt.X, Mouse_Evt.Y);
      elsif Get_Res.Mvt = Con_Io.Refresh then
        -- Refresh
        Refresh (Tz_Drown);
      end if;
    end loop Main_Loop;

    -- Back to Afpx
    Close;
  exception
    when Pers_Def.Exit_Requested =>
      -- Close this console
      Close;
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
       & Ada.Exceptions.Exception_Name (Error) & " raised.");
      Close;
  end Graphic;

end Mesu_Gra;

