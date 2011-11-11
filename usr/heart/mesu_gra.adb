with Ada.Exceptions;
with Afpx, Con_Io, Normal, My_Math, As.B, Language, Basic_Proc;
with Mesu_Def, Str_Mng, Mesu_Nam, Pers_Mng, Pers_Def, Mesu_Fil;
use Pers_Def;
package body Mesu_Gra is
  use My_Math;

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
  -- No of mesure of last Tz drawn
  Prev_Tz : Mesure_Range;


  -- From reality to screen
  function X_To_Screen (X : in Natural) return Con_Io.X_Range is
  begin
    return Con_Io.X_Range(Float(X - X_First) * X_Factor) + Xs_First;
  end X_To_Screen;

  function Y_To_Screen (Bpm : in Pers_Def.Bpm_Range) return Con_Io.Y_Range is
  begin
    return Con_Io.Y_Range(Float(Bpm - Y_First) * Y_Factor) + Ys_First;
  end Y_To_Screen;

  -- From reality to screen
  function X_To_Screen_Secure (X : in Natural) return Integer is
  begin
    return Integer(Float(X - X_First) * X_Factor) + Xs_First;
  end X_To_Screen_Secure;

  function Y_To_Screen_Secure (Bpm : in Pers_Def.Bpm_Range) return Integer is
  begin
    return Integer(Float(Bpm - Y_First) * Y_Factor) + Ys_First;
  end Y_To_Screen_Secure;

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
        B := My_Math.Real(Y1) - (A * My_Math.Real(X1));
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
        B := My_Math.Real(X1) - (A * My_Math.Real(Y1));
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

  -- Graphic layout (help, scales, Tz)
  procedure Draw_Layout is
    Help_Color  : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Brown");
    Scale_Color : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Blue");
    -- Scale step on X in seconds
    Secs_Scale_Step : constant := 600;
    Secs : Natural;
    -- Scale step on Y in Bpm
    Bpm : Pers_Def.Bpm_Range;
    X : Con_Io.X_Range;
    Y : Con_Io.Y_Range;

  begin
    -- Help
    Screen.Move (Screen.Row_Range_Last, 5);
    Screen.Set_Foreground (Help_Color);
    Screen.Put ("Escape to quit, '1' to '" & Normal(Nb_Mesure,1)
       & "' to draw/hide a record, T for Training Zones.");
    -- Axes of scale
    Screen.Set_Foreground (Scale_Color);
    Console.Draw_Line (Xs_First, Ys_First, Xs_Last, Ys_First);
    Console.Draw_Line (Xs_First, Ys_First, Xs_First, Ys_Last);
    -- Horizontal scale : one + each 10 mn (600 seconds)
    --                    Time in mn each 3 +
    for I in 0 .. X_Last / Secs_Scale_Step loop
      Secs := I * Secs_Scale_Step;
      X := X_To_Screen (Secs);
      Console.Draw_Line (X, Ys_First - 2, X, Ys_First + 2);
      if I rem 3 = 0 or else I = X_Last / Secs_Scale_Step then
        if X / Console.Font_Width - 1 <= Con_Io.Col_Range'Last - 3 then
          Screen.Move (Console.Row_Range_Last - 1,
                       X / Console.Font_Width - 1);
        else
          Screen.Move (Console.Row_Range_Last - 1, Console.Col_Range_Last - 3);
        end if;
        Screen.Put (Normal (Secs / 60, 3));
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
                           Y - Font_Offset_Height);
    end loop;

  end Draw_Layout;

  procedure Draw_Tz (Show : in Boolean) is
    Tz_Color    : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Red");
    Bpm : Pers_Def.Bpm_Range;
    Y : Con_Io.Y_Range;
    Mesure_Index : Mesure_Range;
  begin
    Screen.Set_Foreground (Tz_Color);
    if not Show then
      Mesure_Index := Prev_Tz;
    else
      -- First drawn mesure if any (else first mesure)
      Mesure_Index := 1;
      for Mesu in 1 .. Nb_Mesure loop
        if Mesure_Array(Mesu).Drown then
          Mesure_Index := Mesu;
          exit;
        end if;
      end loop;
      Prev_Tz := Mesure_Index;
    end if;

    for I in Pers_Def.Person_Tz_Array'Range loop
      Bpm := Mesure_Array(Mesure_Index).Mesure.Tz(I);
      if Bpm >= Y_First then
        Y := Y_To_Screen(Bpm);
        Draw_Line (Xs_First, Y, Xs_Last - 4 * Console.Font_Width, Y);
        Console.Put (
                    Normal(Integer(Bpm), 3),
                    Console.X_Max - (3 * Console.Font_Width),
                    Y - Font_Offset_Height);
      end if;
    end loop;
  end Draw_Tz;

  -- Draw one record
  procedure Draw_Mesure (No : in Mesure_Range) is
    Colors : constant array (1 .. Max_Nb_Mesure) of Con_Io.Effective_Colors
           := (1 => Con_Io.Color_Of ("Light_Grey"),
               2 => Con_Io.Color_Of ("Cyan"),
               3 => Con_Io.Color_Of ("Light_Blue"),
               4 => Con_Io.Color_Of ("Lime_Green"),
               5 => Con_Io.Color_Of ("Orange"),
               6 => Con_Io.Color_Of ("Blue"),
               7 => Con_Io.Color_Of ("Magenta"),
               8 => Con_Io.Color_Of ("Yellow"),
               9 => Con_Io.Color_Of ("White"));
    Sec1, Sec2 : Natural;
    Bpm1, Bpm2 : Pers_Def.Bpm_Range;
    Mesure : Mesu_Def.Mesure_Rec renames Mesure_Array(No).Mesure;
    Title_Txt : As.B.Asb_Bs(Con_Io.Col_Range'Last);
  begin
    Screen.Set_Foreground (Colors(No));
    -- Person and date
    Screen.Move (No-1, 10);
    if Mesure.Samples(1) = Pers_Def.Bpm_Range'First or else
       Mesure.Samples(2) = Pers_Def.Bpm_Range'First then
      -- 0 or only 1 sample. Cannot draw this one
      Title_Txt.Set ("(*)");
    else
      Title_Txt.Set ("   ");
    end if;
    Title_Txt.Append (
           Normal(No, 1) & ":"
         & Mesure_Array(No).Person.Name & " "
         & Mesure_Array(No).Person.Activity & " "
         & Str_Mng.To_Printed_Str(Mesure.Date) & " "
         & Mesure.Comment);
    Screen.Put (Title_Txt.Image);

    if Mesure.Samples(2) = Pers_Def.Bpm_Range'First then
      return;
    end if;


    Sec1 := 0;
    Bpm1 := Mesure.Samples(1);

    Pixel (X_To_Screen_Secure (Sec1), Y_To_Screen_Secure (Bpm1), True);
    for I in Mesu_Def.Sample_Nb_Range
    range 2 .. Mesu_Def.Sample_Nb_Range'Last loop
      exit when Mesure.Samples(I) = Pers_Def.Bpm_Range'First;
      Sec2 := Sec1 + Integer(Mesure.Sampling_Delta);
      Bpm2 := Mesure.Samples(I);
      -- Check in screen
      Draw_Line (X_To_Screen_Secure (Sec1), Y_To_Screen_Secure (Bpm1),
                 X_To_Screen_Secure (Sec2), Y_To_Screen_Secure (Bpm2),
                 In_Graphic => True, Draw_First_Point => False);
      Sec1 := Sec2;
      Bpm1 := Bpm2;
    end loop;

  end Draw_Mesure;

  -- The main
  procedure Graphic (Exit_Program : out Boolean) is
    Saved_Pos : Natural;
    Line      : Afpx.Line_Rec;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S    : Mesu_Nam.File_Date_Str;
    No_S      : Mesu_Nam.File_No_Str;
    Pid_S     : Mesu_Nam.File_Pid_Str;
    Pos_Pers  : Natural;
    Person    : Pers_Def.Person_Rec;
    Same_Tz   : Boolean;
    Tz_Drown  : Boolean;
    Get_Res   : Con_Io.Get_Result;
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


    use type Con_Io.Curs_Mvt;
  begin
    -- Re-use Afpx console
    Console := Afpx.Get_Console;
    Screen.Set_To_Screen (Console'Access);

    -- Screen scale
    Xs_First := 4 * Console.Font_Width;
    Xs_Last  := Console.X_Max;
    Ys_First := Console.Font_Height * 2;
    Ys_Last := Console.Y_Max
             - Max_Nb_Mesure * Console.Font_Height;

    -- Init array of mesures
    -- List is not empty
    Saved_Pos := Afpx.Line_List.Get_Position;
    Nb_Mesure := 0;
    -- for each in list : store in array
    Afpx.Line_List.Rewind;
    loop
      -- Get line, file_name, split
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
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
    Afpx.Line_List.Move_At (Saved_Pos);

    -- Find Y min and max
    Y_First := Pers_Def.Bpm_Range'Last;
    Y_Last := Pers_Def.Bpm_Range'First;
    for I in 1 .. Nb_Mesure loop
      -- Scan samples
      for J in Mesu_Def.Sample_Nb_Range loop
        if Mesure_Array(I).Mesure.Samples(J) = Pers_Def.Bpm_Range'First then
          -- No more sample for this record
          exit;
        end if;
        if Y_First > Mesure_Array(I).Mesure.Samples(J) then
          -- Smallest Y so far
          Y_First := Mesure_Array(I).Mesure.Samples(J);
        end if;
        if Y_Last < Mesure_Array(I).Mesure.Samples(J) then
          -- Biggest Y so far
          Y_Last := Mesure_Array(I).Mesure.Samples(J);
        end if;
      end loop;
      -- Scan Tz
      for J in Pers_Def.Person_Tz_Array'Range loop
        if Mesure_Array(I).Mesure.Tz(J) /= Pers_Def.Bpm_Range'First then
          if Y_First > Mesure_Array(I).Mesure.Tz(J) then
            Y_First := Mesure_Array(I).Mesure.Tz(J);
          end if;
          if Y_Last < Mesure_Array(I).Mesure.Tz(J) then
            Y_Last := Mesure_Array(I).Mesure.Tz(J);
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
    Font_Offset_Height := Console.Font_Height / 3;

    -- Compute last X
    X_Last := 0;
    The_Records:
    for I in 1 .. Nb_Mesure loop
      This_Record:
      for J in Mesu_Def.Sample_Nb_Range loop
        if Mesure_Array(I).Mesure.Samples(J) = Pers_Def.Bpm_Range'First then
          -- No more sample for this record
          exit This_Record;
        elsif J * Integer(Mesure_Array(I).Mesure.Sampling_Delta) > X_Last then
          -- Greatest X so far
          X_Last := J * Integer(Mesure_Array(I).Mesure.Sampling_Delta);
        end if;
      end loop This_Record;
    end loop The_Records;

    -- Compute X Factor
    X_Factor := Float(Xs_First - Xs_Last) / Float(X_First - X_Last);

    -- Graphic mode for current screen
    Screen.Set_Xor_Mode (Con_Io.Xor_On);
    Screen.Clear;

    Draw_Layout;
    Tz_Drown := False;

    -- Draw all mesures
    for I in 1 .. Nb_Mesure loop
      Mesure_Array(I).Drown := True;
      Draw_Mesure (I);
    end loop;
    Nb_Drown := Nb_Mesure;

    Main_Loop:
    loop
      -- Get key
      Get_Res := Screen.Get;
      if Get_Res.Mvt = Con_Io.Full then
        Char := Language.Unicode_To_Char (Get_Res.Char);
      else
        Char := '#';
      end if;

      -- Exit when Escape
      if Get_Res.Mvt = Con_Io.Esc then
        Exit_Program := False;
        exit Main_Loop;
      elsif Char = 'T' or else Char = 't' then
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
          else
            -- Hidding a record
            Mesure_Array(No_Mesure).Drown := False;
            Nb_Drown := Nb_Drown - 1;
          end if;
          -- Draw/hide this mesure
          Draw_Mesure (No_Mesure);
          -- Hide Tz if no mesure
          if Nb_Drown = 0 and then Tz_Drown then
            Draw_Tz (False);
            Tz_Drown := False;
          end if;
        end if;
      elsif Get_Res.Mvt = Con_Io.Refresh then
        -- Refresh
        Screen.Clear;
        Draw_Layout;
        -- Redraw mesures
        for I in 1 .. Nb_Mesure loop
          if Mesure_Array(I).Drown then
            Draw_Mesure (I);
          end if;
        end loop;
        -- Tz_Drown is already up to date
        if Tz_Drown then
          Draw_Tz(True);
        end if;
      end if;
    end loop Main_Loop;

    -- Back to Afpx
    Screen.Set_Xor_Mode (Con_Io.Xor_Off);
    Screen.Clear;
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
       & Ada.Exceptions.Exception_Name (Error) & " raised.");
      Screen.Set_Xor_Mode (Con_Io.Xor_Off);
      Screen.Clear;
  end Graphic;

end Mesu_Gra;

