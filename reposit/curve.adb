with Ada.Text_Io;
with Unicode, Language, Con_Io, Normal, Upper_Char;
package body Curve is
  use My_Math;

  -- Small font and look square
  Cur_Con_Io : aliased Con_Io.Console;
  Screen : Con_Io.Window;
  package P_Io is new Ada.Text_Io.Float_Io (T_Coordinate);

  -- Init Cur_Con_Io and Screen if necessary
  procedure Init is
  begin
    if Cur_Con_Io.Is_Init then
      return;
    end if;
    Cur_Con_Io := Con_Io.Create (Font_No => 1,
                                 Row_Last => 45,
                                 Col_Last => 76,
                                 Def_Back => Con_Io.Color_Of ("Black"),
                                 Def_Xor => Con_Io.Xor_On);
    Screen := Cur_Con_Io.Screen.all;
  end Init;

  -- Find lowest and greatest X of points
  procedure X_Boundaries (
   Points       : in T_The_Points;
   X_Min, X_Max : out T_Coordinate) is
    Loc_X_Min, Loc_X_Max : T_Coordinate;
  begin
    if Points'Length = 0 then
      Loc_X_Min := T_Coordinate'First;
      Loc_X_Max := T_Coordinate'Last;
    else
      Loc_X_Min := T_Coordinate'Last;
      Loc_X_Max := T_Coordinate'First;

      for I in Points'Range loop
        if Points(I).X < Loc_X_Min then
          Loc_X_Min := Points(I).X;
        end if;
        if Points(I).X > Loc_X_Max then
          Loc_X_Max := Points(I).X;
        end if;
      end loop;
    end if;

    X_Min := Loc_X_Min;
    X_Max := Loc_X_Max;
  end X_Boundaries;

  -- Coordinate to string
  function Coo_To_Str (C : T_Coordinate) return String is
    Str : String (1..13);
  begin
    P_Io.Put (Str, C, 5, 3);
    return Str;
  end Coo_To_Str;

  -- Draw and redraw curves until escape
  procedure Draw (Boundaries : in T_Boundaries;
                  Points : in T_The_Points) is

    -- Miscellaneous drawings on screen : help, axes, scales, points
    type T_Misc_List is (M_Help, M_Grab, M_Axes, M_Scale, M_Points, M_Curve);
    type T_Misc is array (T_Misc_List) of Boolean;
    Misc : T_Misc := (others => False);

    -- Possible Zoom_No values
    subtype Zoom_No_Range is Natural range 0 .. 9;

    -- Current and last stored Zoom window
    Curr_Zoom_No, Last_Zoom_No : Zoom_No_Range;
    Zoom_Array : array (Zoom_No_Range) of T_Boundaries;


    -- Result of Draw_One
    Draw_Result : Boolean;

    -- Compute real position (x, y) to screen position (x, y)
    package Convert is

      -- Screen coordinates of the frame
      type T_Screen_Boundaries is record
        X_Min, X_Max : Con_Io.X_Range;
        Y_Min, Y_Max : Con_Io.Y_Range;
      end record;

      -- Current limits (in pixels) of drawing
      function Get_Screen_Boundaries return T_Screen_Boundaries;

      -- Is point (pixels) inside the frame (raise Out_Of_Frame if not)
      procedure In_Frame (X_S, Y_S : in Integer);

      -- From real to screen and reverse
      function X_Real_Screen (X_Real : T_Coordinate)
                             return Con_Io.X_Range;
      function Y_Real_Screen (Y_Real : T_Coordinate)
                             return Con_Io.Y_Range;
      function X_Screen_Real (X_Screen : Con_Io.X_Range)
               return T_Coordinate;
      function Y_Screen_Real (Y_Screen : Con_Io.Y_Range)
               return T_Coordinate;

      Epsilon : constant T_Coordinate := T_Coordinate'Epsilon;

      -- Computes every conversion according to new boundaries
      procedure Maj (Bounds : in T_Boundaries);

      -- Raised by MAJ if Xmax-Xmin <= Epsilon
      --            or    Ymax-Ymin <= Epsilon
      Maj_Error : exception;

    end Convert;


    package body Convert is

      -- Screen and real coordinates of frame
      Real_Boundaries : T_Boundaries;
      Screen_Boundaries : T_Screen_Boundaries;

      -- Conversion from real to screen :  screen = real * factor + offset
      type T_Conversion is record
        Offset_X, Offset_Y : My_Math.Real;
        Factor_X, Factor_Y : My_Math.Real;
      end record;
      Conversion : T_Conversion;

      -- Raised if point is not in frame
      Out_Of_Frame : exception;

      -- Display / hide wait message
      procedure Wait_Message is
        Msg : constant String := "COMPUTING. Please wait ...";
      begin
        Screen.Set_Foreground (Con_Io.Color_Of ("Light_Blue"));
        Screen.Move (Screen.Row_Range_Last,
                     Screen.Col_Range_Last - Msg'Length);
        Screen.Put (Msg);
        Cur_Con_Io.Flush;
      end Wait_Message;

      -- Screen coordinates of the frame
      function Get_Screen_Boundaries return T_Screen_Boundaries is
      begin
        return Screen_Boundaries;
      end Get_Screen_Boundaries;

      -- Is point (pixels) inside the frame (raise Out_Of_Frame if not)
      procedure In_Frame (X_S, Y_S : in Integer) is
      begin
        if      X_S < Screen_Boundaries.X_Min
        or else X_S > Screen_Boundaries.X_Max then
          raise Out_Of_Frame;
        end if;
        if      Y_S < Screen_Boundaries.Y_Min
        or else Y_S > Screen_Boundaries.Y_Max then
          raise Out_Of_Frame;
        end if;
      end In_Frame;

      -- screen <-> real conversions
      function X_Real_Screen (X_Real : T_Coordinate)
                             return Con_Io.X_Range is
        X_Int : Integer;
        X_Scr : Con_Io.X_Range;
      begin
        X_Int := Integer (Conversion.Offset_X + X_Real * Conversion.Factor_X);
        X_Scr := X_Int;
        return X_Scr;
      exception
        when others => raise Out_Of_Frame;
      end X_Real_Screen;

      function Y_Real_Screen (Y_Real : T_Coordinate)
                             return Con_Io.Y_Range is
        Y_Int : Integer;
        Y_Scr : Con_Io.Y_Range;
      begin
        Y_Int := Integer (Conversion.Offset_Y + Y_Real * Conversion.Factor_Y);
        Y_Scr := Y_Int;
        return Y_Scr;
      exception
        when others => raise Out_Of_Frame;
      end Y_Real_Screen;

      function X_Screen_Real (X_Screen : Con_Io.X_Range)
                             return T_Coordinate is
      begin
        return (T_Coordinate(X_Screen)-Conversion.Offset_X)
             / Conversion.Factor_X;
      end X_Screen_Real;

      function Y_Screen_Real (Y_Screen : Con_Io.Y_Range)
                             return T_Coordinate is
      begin
        return (T_Coordinate(Y_Screen)-Conversion.Offset_Y)
             / Conversion.Factor_Y;
      end Y_Screen_Real;

      -- Compute real_boundaries, screen_boundaries and conversion
      -- from the points and new boundaries
      procedure Maj (Bounds : in T_Boundaries) is
        X_Real, Y_Real : T_Coordinate;
      begin
        -- Display wait message
        Wait_Message;

        if      Bounds.Scale = Free_Screen
        or else Bounds.Scale = Free_Normed then
          -- Real boundaries are provided by the caller
          Real_Boundaries := Bounds;
        else
          -- Real y boundaries must be computed
          -- (x provided, y of points and curve for y)
          Real_Boundaries := (Scale => Free_Screen,
           X_Min => Bounds.X_Min,
           Y_Min => My_Math.Real'Last,
           X_Max => Bounds.X_Max,
           Y_Max => My_Math.Real'First);

          -- Find lowest and greatest Y of points in X_min .. X_max
          for I in Points'Range loop
            if Points(I).X
               in Real_Boundaries.X_Min .. Real_Boundaries.X_Max then
              if Points(I).Y < Real_Boundaries.Y_Min then
                Real_Boundaries.Y_Min := Points(I).Y;
              end if;
              if Points(I).Y > Real_Boundaries.Y_Max then
                Real_Boundaries.Y_Max := Points(I).Y;
              end if;
            end if;
          end loop;
        end if;

        -- Compute X conversion
        if Real_Boundaries.X_Max - Real_Boundaries.X_Min <= Epsilon then
          raise Maj_Error;
        end if;
        Conversion.Factor_X :=
          My_Math.Real (Cur_Con_Io.X_Max - Con_Io.X_Range'First)
          / (Real_Boundaries.X_Max - Real_Boundaries.X_Min);
        Conversion.Offset_X  := My_Math.Real (Con_Io.X_Range'First)
         - Real_Boundaries.X_Min * Conversion.Factor_X;

        -- Now X scale is computed, we can compute curve and update Ys
        if Bounds.Scale /= Free_Screen and then
           Bounds.Scale /= Free_Normed then
          -- Find lowest and greatest y of curve
          for X in Con_Io.X_Range'First .. Cur_Con_Io.X_Max loop
            X_Real := X_Screen_Real (X);
            Y_Real := F (X_Real);
            if Y_Real < Real_Boundaries.Y_Min then
              Real_Boundaries.Y_Min := Y_Real;
            end if;
            if Y_Real > Real_Boundaries.Y_Max then
              Real_Boundaries.Y_Max := Y_Real;
            end if;
          end loop;
        end if;

        -- Compute Y conversion
        if Real_Boundaries.Y_Max - Real_Boundaries.Y_Min <= Epsilon then
          raise Maj_Error;
        end if;
        Conversion.Factor_Y :=
          My_Math.Real (Cur_Con_Io.Y_Max - Con_Io.Y_Range'First)
          / (Real_Boundaries.Y_Max - Real_Boundaries.Y_Min);
        Conversion.Offset_Y  := My_Math.Real (Con_Io.Y_Range'First)
        - Real_Boundaries.Y_Min * Conversion.Factor_Y;

        -- If Scale is normed, factors must be the same on X and Y
        -- (the lowest)
        if      Bounds.Scale = Free_Normed
        or else Bounds.Scale = Curve_Normed then
          if Conversion.Factor_X < Conversion.Factor_Y then
            Conversion.Factor_Y := Conversion.Factor_X;
          else
            Conversion.Factor_X := Conversion.Factor_Y;
          end if;
          -- Update conversion
          Conversion.Offset_X  := My_Math.Real (Con_Io.X_Range'First)
          - Real_Boundaries.X_Min * Conversion.Factor_X;
          Conversion.Offset_Y  := My_Math.Real (Con_Io.Y_Range'First)
          - Real_Boundaries.Y_Min * Conversion.Factor_Y;
        end if;

        -- Compute screen boundaries
        Screen_Boundaries.X_Min := X_Real_Screen (Real_Boundaries.X_Min);
        Screen_Boundaries.X_Max := X_Real_Screen (Real_Boundaries.X_Max);
        Screen_Boundaries.Y_Min := Y_Real_Screen (Real_Boundaries.Y_Min);
        Screen_Boundaries.Y_Max := Y_Real_Screen (Real_Boundaries.Y_Max);

        -- Hide wait message
        Wait_Message;

      exception
        when others =>
          -- Hide wait message
          Wait_Message;
          raise;
      end Maj;

    end Convert;

    -- Draw a curve and miscellaneous drawings on request
    -- -> when zoom defines new scale, new bounds are stored
    --     and True is retuned
    -- -> when new Zoom number is selected, then True is returned
    -- -> when Escape, False is returned
    function Draw_One return Boolean is

      use Convert;

      -- Draw first, show/hide or update, for scales or help
      type Draw_Action is (Init, Toggle, Update);

      -- Nothing to draw, new zoom mode, or update for zoom frame
      type Draw_Frame_Action is (None, Toggle, Update, Redraw);
      Zoom_Frame_Action : Draw_Frame_Action;

      -- Previous values of scale/frame (for update)
      -- Updated by drawing functions
      Prev_Scale_Bounds : T_Screen_Boundaries;
      Prev_Frame_Bounds : T_Screen_Boundaries;

      -- Zoom modes: init, drag, done
      -- To be set : Curr before calls to Draw_Help and Draw_Z_Frame (Toggle)
      --             Prev after  calls to Draw_Help and Draw_Z_Frame (Toggle)
      -- Prev_Zoom_Mode is set to Cur_Zoom_Mode by Draw_Z_Frame (Toggle)
      type Zoom_Mode_List is (Init, Drag, Done);
      Curr_Zoom_Mode, Prev_Zoom_Mode : Zoom_Mode_List;

      -- Current screen boundaries
      Screen_Boundaries : constant T_Screen_Boundaries
                        := Get_Screen_Boundaries;
      -- Current mouse pos/boundaries
      Mouse_Bounds : T_Screen_Boundaries;

      -- For waiting for an event
      Str  : Unicode.Unicode_Sequence (1 .. 1);
      Last : Natural;
      Stat : Con_Io.Curs_Mvt;
      Pos  : Positive;
      Ins  : Boolean;
      -- Input command
      Char : Character;
      -- Mouse event
      Mouse_Event : Con_Io.Mouse_Event_Rec(Con_Io.X_Y);
      -- Status (pos) at start / end of drag
      Clicked_Status : Con_Io.Mouse_Event_Rec(Con_Io.X_Y);

      Mvalid : Boolean;
      Mx : Con_Io.X_Range;
      My : Con_Io.Y_Range;

      -- Set mouse position within screen boundaries
      -- Invert Y
      procedure Set_Mouse_In_Frame(X : in out Con_Io.X_Range;
                                   Y : in out Con_Io.Y_Range) is
      begin
        if X < Screen_Boundaries.X_Min then
          X := Screen_Boundaries.X_Min;
        elsif X > Screen_Boundaries.X_Max then
          X := Screen_Boundaries.X_Max;
        end if;
        if Y < Screen_Boundaries.Y_Min then
          Y := Screen_Boundaries.Y_Min;
        elsif Y > Screen_Boundaries.Y_Max then
          Y := Screen_Boundaries.Y_Max;
        end if;
      end Set_Mouse_In_Frame;

      procedure Toggle_Help_Misc (Misc_Index : in T_Misc_List) is
      begin
        Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));
        case Misc_Index is
          when M_Help =>
            null;
          when M_Grab =>
            Screen.Move (Screen.Row_Range_Last - 6,
                         Screen.Col_Range_Last - 8);
            Screen.Put ("*");
          when M_Axes =>
            Screen.Move (Screen.Row_Range_Last - 5,
                         Screen.Col_Range_Last - 8);
            Screen.Put ("*");
          when M_Points =>
            Screen.Move (Screen.Row_Range_Last - 4,
                         Screen.Col_Range_Last - 8);
            Screen.Put ("*");
          when M_Curve =>
            Screen.Move (Screen.Row_Range_Last - 3,
                         Screen.Col_Range_Last - 8);
            Screen.Put ("*");
          when M_Scale =>
            Screen.Move (Screen.Row_Range_Last - 2,
                         Screen.Col_Range_Last - 8);
            Screen.Put ("*");
        end case;
      end Toggle_Help_Misc;

      -- Draw help message
      procedure Draw_Help (Action : in Draw_Action) is

        -- Dedicated message according to zoom mode (hide/show)
        procedure Put_Mode (Mode : in Zoom_Mode_List) is
        begin
          Screen.Move (Screen.Row_Range_Last - 11,
                       Screen.Col_Range_Last - 18);
          case Mode is
            when Init =>
              Screen.Put ("Point & click L");
            when Drag =>
              Screen.Put ("Drag L & release");
            when Done =>
              Screen.Put ("L or R click");
          end case;
        end Put_Mode;

      begin
        -- Optimization : most frequent case (update and same mode or no help)
        if Action = Update and then
           (Curr_Zoom_Mode = Prev_Zoom_Mode or else not Misc(M_Help)) then
          return;
        end if;

        -- Help on zoom only if mouse
        Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));

        -- Previous mode to hide : Something drawn and new thing different
        if Misc(M_Help) and then
             ((Action = Update and then
               Curr_Zoom_Mode /= Prev_Zoom_Mode) or else
              Action = Toggle)                   then
          Put_Mode (Prev_Zoom_Mode);
        end if;

        -- New mode to draw : toggle to new or update
        if (Misc(M_Help) and then
            Action = Update and then
            Curr_Zoom_Mode /= Prev_Zoom_Mode)          or else
           (not Misc(M_Help) and then Action = Toggle) or else
           (Action = Init)                             then
          Put_Mode (Curr_Zoom_Mode);
        end if;

        -- Global help
        if Action = Toggle or else Action = Init then
          Screen.Move (Screen.Row_Range_Last - 10,
                       Screen.Col_Range_Last - 18);
          Screen.Put ("Current ZOOM: " & Normal(Curr_Zoom_No, 1) );
          Screen.Move (Screen.Row_Range_Last - 9,
                       Screen.Col_Range_Last - 18);
          Screen.Put ("0.." & Normal(Last_Zoom_No, 1) & ": other ZOOM");

          -- if mouse not installed : color is set here
          Screen.Set_Foreground (Con_Io.Color_Of ("Magenta"));

          Screen.Move (Screen.Row_Range_Last - 8,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("SWITCHES:");
          Screen.Move (Screen.Row_Range_Last - 7,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("H * Help");
          Screen.Move (Screen.Row_Range_Last - 6,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("G   Grab");
          Screen.Move (Screen.Row_Range_Last - 5,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("A   Axes");
          Screen.Move (Screen.Row_Range_Last - 4,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("P   Points");
          Screen.Move (Screen.Row_Range_Last - 3,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("C   Curve");
          Screen.Move (Screen.Row_Range_Last - 2,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("S   Scales");
          Screen.Move (Screen.Row_Range_Last - 1,
                       Screen.Col_Range_Last - 10);
          Screen.Put ("Esc Exit");
        end if;

        -- New help mode
        if Action = Toggle then
          Misc(M_Help) := not Misc(M_Help);
          for I in T_Misc_List loop
            if Misc (I) then
              Toggle_Help_Misc (I);
            end if;
          end loop;
        end if;
      end Draw_Help;

      -- Draw all points
      procedure Draw_Points is

        -- Draw a point knowing its real coordinates
        procedure Draw_Point (X, Y : in T_Coordinate) is
          X_S : Con_Io.X_Range;
          Y_S : Con_Io.Y_Range;
          type Pix is record
            X, Y: Integer;
          end record;
          type T_Point_Pixels is array (Positive range <>) of Pix;
          Point_Pixels : constant T_Point_Pixels (1..15) :=
               ( (-2, -2), (-2,  2), ( 2, -2), (  2,  2), (-1, -1),
                 (-1,  0), (-1,  1), ( 1, -1), (  1,  0), ( 1,  1),
                 ( 0, -2), ( 0, -1), ( 0,  0), (  0,  1), ( 0,  2) );
        begin
          X_S := X_Real_Screen (X);
          Y_S := Y_Real_Screen (Y);
          for I in Point_Pixels'Range loop
            begin
              In_Frame (X_S + Point_Pixels(I).X, Y_S + Point_Pixels(I).Y);
              Cur_Con_Io.Draw_Point (X_S + Point_Pixels(I).X,
                                     Y_S + Point_Pixels(I).Y);
            exception
              when others => null;
            end;
          end loop;
        exception
          when others => null;
        end Draw_Point;

      begin
        Screen.Set_Foreground (Con_Io.Color_Of ("Red"));
        for I in Points'Range loop
          Draw_Point (Points(I).X, Points(I).Y);
        end loop;
        if Misc (M_Help) then
          Toggle_Help_Misc (M_Points);
        end if;
      end Draw_Points;

      -- Draw an horizontal line (for frames and axes)
      procedure Draw_X (X_Min, X_Max : in Natural; Y : in Natural) is
      begin
        Cur_Con_Io.Draw_Line (X_Min, Y, X_Max, Y);
      end Draw_X;

      -- Draw a vertical line (for frames and axes)
      procedure Draw_Y (X : in Natural; Y_Min, Y_Max : in Natural) is
      begin
        Cur_Con_Io.Draw_Line (X, Y_Min, X, Y_Max);
      end Draw_Y;

      -- Draw axes of the curve
      procedure Draw_Axes is
        X_0, Y_0 : Natural;
        Intersec : Boolean := True;
      begin
        Screen.Set_Foreground (Con_Io.Color_Of ("Light_Blue"));
        -- Horizontal
        begin
          Y_0 := Y_Real_Screen (0.0);
          In_Frame (0, Y_0);
          Draw_X(Screen_Boundaries.X_Min+1, Screen_Boundaries.X_Max-1, Y_0);
        exception
          when others => Intersec := False;
        end;
        -- Vertical
        begin
          X_0 := X_Real_Screen (0.0);
          In_Frame (X_0, 0);
          Draw_Y (X_0, Screen_Boundaries.Y_Min+1, Screen_Boundaries.Y_Max-1);
        exception
          when others => Intersec := False;
        end;
        -- Re draw intersection of axes
        if Intersec then
          begin
            In_Frame (X_0, Y_0);
            Cur_Con_Io.Draw_Point (X_0, Y_0);
          exception
            when others => null;
          end;
        end if;
        if Misc (M_Help) then
          Toggle_Help_Misc (M_Axes);
        end if;

      end Draw_Axes;

      -- Draw the curve
      procedure Draw_Curve is
        Y_S : Integer;
        X_R, Y_R : My_Math.Real;

        -- Draw the frame around the curve
        procedure Draw_Frame is
        begin
          Screen.Set_Xor_Mode (Con_Io.Xor_Off);
          Screen.Set_Foreground (Con_Io.Color_Of ("White"));
          Draw_X (Screen_Boundaries.X_Min, Screen_Boundaries.X_Max,
                  Screen_Boundaries.Y_Min);
          Draw_Y (Screen_Boundaries.X_Max, Screen_Boundaries.Y_Min,
                  Screen_Boundaries.Y_Max);
          Draw_X (Screen_Boundaries.X_Min, Screen_Boundaries.X_Max,
                  Screen_Boundaries.Y_Max);
          Draw_Y (Screen_Boundaries.X_Min, Screen_Boundaries.Y_Min,
                  Screen_Boundaries.Y_Max);
          Screen.Set_Xor_Mode (Con_Io.Xor_On);
        end Draw_Frame;

      begin
        -- Draw frame
        Draw_Frame;
        Screen.Set_Foreground (Con_Io.Color_Of ("Lime_Green"));
        -- Draw pixel for each possible X screen
        for X_S in Con_Io.X_Range
                 range Screen_Boundaries.X_Min .. Screen_Boundaries.X_Max loop
          begin
            -- Xscreen -> Xreal -> Yreal -> Yscreen
            X_R := X_Screen_Real (X_S);
            Y_R := F (X_R);
            Y_S := Y_Real_Screen (Y_R);
            In_Frame (X_S, Y_S);
            Cur_Con_Io.Draw_Point (X_S, Y_S);
          exception
            when others => null;
          end;
        end loop;
        if Misc (M_Help) then
          Toggle_Help_Misc (M_Curve);
        end if;
        -- Cur_Con_Io.Bell(1);
      end Draw_Curve;


      -- Draw scales. Only external if no mouse
      --  Also mouse position / drag current limits if mouse
      procedure Draw_Scale (Action : in Draw_Action;
                            Zoom_Bounds : in T_Screen_Boundaries) is
        type Scale_Pos is (X_Min, X_Max, Y_Min, Y_Max);

        -- Draw/hide one Zoom scale value
        procedure Put_Scale (Scale : in T_Coordinate; Pos : in Scale_Pos) is
        begin
          case Pos is
            when X_Min =>
              Screen.Move (13, 1);
            when X_Max =>
              Screen.Move (13, Screen.Col_Range_Last - 13);
            when Y_Min =>
              Screen.Move (Screen.Row_Range_Last - 2, 30);
            when Y_Max =>
              Screen.Move (2, 30);
          end case;
          Screen.Put (Coo_To_Str(Scale));
        end Put_Scale;
      begin
        -- Optimization : most frequent case
        -- update and same value or no scale
        if Action = Update and then
           (Zoom_Bounds = Prev_Scale_Bounds or else not Misc(M_Scale)) then
          return;
        end if;

        Screen.Set_Foreground (Con_Io.Color_Of ("Cyan"));

        -- Previous scale to hide : Something drawn and new values different
        if Misc(M_Scale) then
          if (Action = Update and then Zoom_Bounds /= Prev_Scale_Bounds)
          or else Action = Toggle then
            Put_Scale (X_Screen_Real(Prev_Scale_Bounds.X_Min), X_Min);
            Put_Scale (X_Screen_Real(Prev_Scale_Bounds.X_Max), X_Max);
            Put_Scale (Y_Screen_Real(Prev_Scale_Bounds.Y_Min), Y_Min);
            Put_Scale (Y_Screen_Real(Prev_Scale_Bounds.Y_Max), Y_Max);
          end if;
        end if;

        -- New scale to draw : new values update or toggle to new
        if (Misc(M_Scale) and then Action = Update and then
                              Zoom_Bounds /= Prev_Scale_Bounds)
           or else (Action = Toggle and then not Misc(M_Scale))
           or else (Action = Init   and then     Misc(M_Scale)) then
          Put_Scale (X_Screen_Real(Zoom_Bounds.X_Min), X_Min);
          Put_Scale (X_Screen_Real(Zoom_Bounds.X_Max), X_Max);
          Put_Scale (Y_Screen_Real(Zoom_Bounds.Y_Min), Y_Min);
          Put_Scale (Y_Screen_Real(Zoom_Bounds.Y_Max), Y_Max);
          Prev_Scale_Bounds := Zoom_Bounds;
        end if;

        if      Action = Toggle
        or else (Action = Init and then Misc (M_Scale)) then
          -- External scales
          Screen.Set_Foreground (Con_Io.Color_Of ("Light_Grey"));
          Screen.Move (1, 30);
          Screen.Put (Coo_To_Str(Y_Screen_Real(Screen_Boundaries.Y_Max)));
          Screen.Move (Screen.Row_Range_Last - 1, 30);
          Screen.Put (Coo_To_Str(Y_Screen_Real(Screen_Boundaries.Y_Min)));
          Screen.Move (12, 1);
          Screen.Put (Coo_To_Str(X_Screen_Real(Screen_Boundaries.X_Min)));
          Screen.Move (12, Screen.Col_Range_Last - 13);
          Screen.Put (Coo_To_Str(X_Screen_Real(Screen_Boundaries.X_Max)));
        end if;

        -- Toggle mode
        if Action = Toggle then
           Misc(M_Scale) := not Misc(M_Scale);
        end if;

        -- Update help
        if Action = Init or else Action = Toggle then
          if Misc (M_Help) then
            Toggle_Help_Misc (M_Scale);
          end if;
        end if;
      end Draw_Scale;

      -- Draw Z frame (when in drag)
      procedure Draw_Z_Frame (Action : in Draw_Frame_Action := Update;
                              Zoom_Bounds : in T_Screen_Boundaries) is

        -- Draw/hide a zoom frame
        procedure Put_Frame (Bounds : in T_Screen_Boundaries) is
        begin
          Cur_Con_Io.Draw_Rectangle (
             Bounds.X_Min,
             Bounds.Y_Min,
             Bounds.X_Max,
             Bounds.Y_Max);
        end Put_Frame;

      begin
        -- Redraw (refresh when done)
        if Action = Redraw then
          Screen.Set_Foreground (Con_Io.Color_Of ("Cyan"));
          Put_Frame(Prev_Frame_Bounds);
          return;
        end if;

        -- Optimization : most frequent case
        -- Update and same frame, or update and not in drag
        if Action = Update
        and then (Curr_Zoom_Mode /= Drag
                      or else Zoom_Bounds = Prev_Frame_Bounds) then
          return;
        end if;
        if Action = None then
          return;
        end if;

        -- If action = update, then cur mode is drag and bounds are new
        Screen.Set_Foreground (Con_Io.Color_Of ("Cyan"));

        -- Previous frame to hide : new drag or drag -> done or done -> init
        if Action = Update
           or else Curr_Zoom_Mode /= Drag then
          Put_Frame (Prev_Frame_Bounds);
        end if;

        -- New frame to draw : new drag or init -> drag or drag -> done
        if Action = Update
        or else Curr_Zoom_Mode /= Init then
          Put_Frame(Zoom_Bounds);
          Prev_Frame_Bounds := Zoom_Bounds;
        end if;
        -- Toggle zoom mode
        if Action = Toggle then
          Prev_Zoom_Mode := Curr_Zoom_Mode;
        end if;
      end Draw_Z_Frame;

      -- Exchange x_min and x_max if necessary. Same on Y.
      -- (For mouse scales when in drag)
      procedure Sort_Bounds (Bounds : in out T_Screen_Boundaries) is
        Tmp : Natural;
      begin
        if Bounds.X_Min > Bounds.X_Max then
          Tmp := Bounds.X_Min;
          Bounds.X_Min := Bounds.X_Max;
          Bounds.X_Max := Tmp;
        end if;
        if Bounds.Y_Min > Bounds.Y_Max then
          Tmp := Bounds.Y_Min;
          Bounds.Y_Min := Bounds.Y_Max;
          Bounds.Y_Max := Tmp;
        end if;
      end Sort_Bounds;

      procedure Cancel_Zoom is
      begin
        Curr_Zoom_Mode := Init;
        -- Reset zoom and hide zoom frame.
        Draw_Help (Update);
        Zoom_Frame_Action := Toggle;
        Cur_Con_Io.Enable_Motion_Events (Misc(M_Scale));
      end Cancel_Zoom;

    use type Con_Io.Curs_Mvt, Con_Io.Mouse_Button_List,
             Con_Io.Mouse_Button_Status_List;
    begin -- Draw_One

      -- Init context
      Prev_Zoom_Mode := Init;
      Curr_Zoom_Mode := Init;
      Stat := Con_Io.Refresh;
      Cur_Con_Io.Enable_Motion_Events (Misc(M_Scale));
      Misc(M_Curve) := True;

      Cur_Con_Io.Get_Current_Pointer_Pos (Mvalid, Mx, My);
      if Mvalid then
        Set_Mouse_In_Frame(Mx, My);
        Mouse_Bounds.X_Min := Mx;
        Mouse_Bounds.X_Max := Mx;
        Mouse_Bounds.Y_Min := My;
        Mouse_Bounds.Y_Max := My;
        Clicked_Status.X := Mx;
        Clicked_Status.Y := My;
        Prev_Frame_Bounds := Mouse_Bounds;
        Prev_Scale_Bounds := Mouse_Bounds;
      end if;

      loop -- Main loop of mouse and keys actions

        if Stat = Con_Io.Refresh and then Curr_Zoom_Mode /= Drag then
          -- Discard refresh when in drag
          -- Frozen mouse when done
          if Curr_Zoom_Mode /= Done then
            Cur_Con_Io.Get_Current_Pointer_Pos (Mvalid, Mx, My);
            if Mvalid then
              Set_Mouse_In_Frame(Mx, My);
              Mouse_Bounds.X_Min := Mx;
              Mouse_Bounds.X_Max := Mx;
              Mouse_Bounds.Y_Min := My;
              Mouse_Bounds.Y_Max := My;
            end if;
          end if;
          -- Draw what has to be for initial/refresh
          Screen.Clear;
          if Misc(M_Grab)   then
            if Misc(M_Help) then
              Toggle_Help_Misc (M_Grab);
            end if;
          end if;
          if Misc(M_Curve)  then Draw_Curve; end if;
          if Misc(M_Axes)   then Draw_Axes; end if;
          if Misc(M_Points) then Draw_Points; end if;
          if Misc(M_Help)   then Draw_Help(Init); end if;
          if Misc(M_Scale)  then Draw_Scale(Init, Mouse_Bounds); end if;
          if Curr_Zoom_Mode = Done then
            -- Mouse_Bounds not used
            Draw_Z_Frame (Redraw, Mouse_Bounds);
          end if;
        end if;

        -- Infinite wait
        Screen.Get (Str, Last, Stat, Pos, Ins, Echo => False);

        case Stat is
          when Con_Io.Break =>
            -- End of curve
            return False;
          when Con_Io.Refresh =>
            -- Redraw at next loop;
            null;
          when Con_Io.Fd_Event =>
            -- Call any fd callback
            Fd_Callback;
          when Con_Io.Timer_Event =>
            -- Call any timer callback
            Timer_Callback;
          when Con_Io.Signal_Event =>
            -- Call any signal callback
            Signal_Callback;
          when Con_Io.Timeout =>
            -- Should not occure: Get(Infinite_Time)
            null;
          when Con_Io.Esc =>
            if Curr_Zoom_Mode = Init then
              -- Initial zoom mode. Exit drawings.
              return False;
            elsif Curr_Zoom_Mode = Done then
              -- End of zoom mode. Cancel zoom.
              Cancel_Zoom;
              Draw_Z_Frame (Zoom_Frame_Action, Mouse_Bounds);
            end if;
          when Con_Io.Full =>
            -- Key pressed,
            -- Con_Io default char (Wide_Def_Char = '#') is rejected here,
            --  so we can use Con_Io "weak" conversion
            Char := Language.Unicode_To_Char (Str(1));
            if Upper_Char(Char) = 'A' then
              -- Toggle axes
              Draw_Axes;
              Misc(M_Axes) := not Misc(M_Axes);
            elsif Upper_Char(Char) = 'S' then
              -- Toggle scales
              Draw_Scale (Toggle, Mouse_Bounds);
              Cur_Con_Io.Enable_Motion_Events (
                 Misc(M_Scale) or else Curr_Zoom_Mode = Drag);
            elsif Upper_Char(Char) = 'P' then
              -- Toggle points
              Draw_Points;
              Misc(M_Points) := not Misc(M_Points);
            elsif Upper_Char(Char) = 'H' then
              -- Toggle help
              Draw_Help(Toggle);
            elsif Upper_Char(Char) = 'C' then
              -- Toggle curve
              Misc(M_Curve) := not Misc(M_Curve);
              Draw_Curve;
            elsif Upper_Char(Char) = 'G' then
              -- Grab / ungrab pointer
              Misc(M_Grab) := not Misc(M_Grab);
              Cur_Con_Io.Set_Pointer_Shape(Con_Io.Cross, Misc(M_Grab));
              if Misc(M_Help) then
                Toggle_Help_Misc (M_Grab);
              end if;
            elsif Char >= '0' and then
                  Char <= Character'Val(Last_Zoom_No + Character'Pos('0')) then
              -- New zoom level selection
              Curr_Zoom_No := Character'Pos(Char) - Character'Pos('0');
              -- Compute new conversions
              Maj (Zoom_Array(Curr_Zoom_No));
              return True;
            elsif Char = '+' then
              if Curr_Zoom_No /= Last_Zoom_No then
                Curr_Zoom_No := Curr_Zoom_No + 1;
                -- Compute new conversions
                Maj (Zoom_Array(Curr_Zoom_No));
                return True;
              else
                Cur_Con_Io.Bell(3);
              end if;
            elsif Char = '-' then
              if Curr_Zoom_No /= Zoom_No_Range'First then
                Curr_Zoom_No := Curr_Zoom_No - 1;
                -- Compute new conversions
                Maj (Zoom_Array(Curr_Zoom_No));
                return True;
              else
                Cur_Con_Io.Bell(3);
              end if;
            else
              -- Invalid key
              Cur_Con_Io.Bell(3);
            end if;

          when Con_Io.Mouse_Button =>
            -- New button status
            Cur_Con_Io.Get_Mouse_Event (Mouse_Event, Con_Io.X_Y);
            Set_Mouse_In_Frame(Mouse_Event.X, Mouse_Event.Y);
            -- Update scales and frame according to zoom mode
            if Curr_Zoom_Mode = Init then
              -- Before Drag : follow cur pos
              Mouse_Bounds.X_Min := Mouse_Event.X;
              Mouse_Bounds.X_Max := Mouse_Event.X;
              Mouse_Bounds.Y_Min := Mouse_Event.Y;
              Mouse_Bounds.Y_Max := Mouse_Event.Y;
            elsif Curr_Zoom_Mode = Drag then
              -- Drag : clicked_pos and cur pos
              Mouse_Bounds.X_Min := Mouse_Event.X;
              Mouse_Bounds.X_Max := Clicked_Status.X;
              Mouse_Bounds.Y_Min := Mouse_Event.Y;
              Mouse_Bounds.Y_Max := Clicked_Status.Y;
              Sort_Bounds (Mouse_Bounds);
              Zoom_Frame_Action := Update;
            end if;

            case Curr_Zoom_Mode is
              when Init =>
                if Mouse_Event.Valid
                and then Mouse_Event.Button = Con_Io.Left
                and then Mouse_Event.Status = Con_Io.Pressed then

                  Curr_Zoom_Mode := Drag;
                  Draw_Help (Update);
                  -- Store what has to be done with zoom frame
                  Clicked_Status := Mouse_Event;
                  Zoom_Frame_Action := Toggle;
                  Cur_Con_Io.Enable_Motion_Events (True);
                elsif Mouse_Event.Valid
                and then Mouse_Event.Status = Con_Io.Released then
                  if Mouse_Event.Button = Con_Io.Up
                  and then Curr_Zoom_No /= Last_Zoom_No then
                    -- Next zoom
                    Curr_Zoom_No := Curr_Zoom_No + 1;
                    -- Compute new conversions
                    Maj (Zoom_Array(Curr_Zoom_No));
                    return True;
                  elsif Mouse_Event.Button = Con_Io.Down
                  and then Curr_Zoom_No /= Zoom_No_Range'First then
                    -- Prev zoom
                    Curr_Zoom_No := Curr_Zoom_No - 1;
                    -- Compute new conversions
                    Maj (Zoom_Array(Curr_Zoom_No));
                    return True;
                  else
                    -- No change
                    Zoom_Frame_Action := None;
                  end if;
                else
                  -- No change
                  Zoom_Frame_Action := None;
                end if;
              when Drag =>
                if       Mouse_Event.Button = Con_Io.Left
                and then Mouse_Event.Status = Con_Io.Released then
                  -- release
                  if      Mouse_Event.X = Clicked_Status.X
                  or else Mouse_Event.Y = Clicked_Status.Y then
                    -- No zoom possible : Cancel
                    Cancel_Zoom;
                  else
                    -- Drag done
                    Cur_Con_Io.Enable_Motion_Events (False);
                    Curr_Zoom_Mode := Done;
                    Draw_Help (Update);
                    -- Store what has to be done with zoom frame
                    Zoom_Frame_Action := Toggle;
                  end if;
                end if;
              when Done =>
                if Mouse_Event.Valid
                and then Mouse_Event.Button = Con_Io.Left
                and then Mouse_Event.Status = Con_Io.Pressed then
                  -- Click left : Validate
                  -- Zoom status is Done. Validate new scales in Curr_Zoom_No+1
                  declare
                    Root_Bounds : T_Boundaries
                                  renames Zoom_Array(Zoom_No_Range'First);
                    New_Bounds :  T_Boundaries;
                    New_Zoom_No : Zoom_No_Range;
                  begin

                    if Curr_Zoom_No /= Zoom_No_Range'Last then
                      New_Zoom_No := Curr_Zoom_No + 1;
                    else
                      New_Zoom_No := Curr_Zoom_No;
                    end if;
                    -- New_Bounds.Scale mode is Free_Screen or Free_Normed
                    --  according to initial scale type
                    if Root_Bounds.Scale = Curve_Screen or else
                       Root_Bounds.Scale = Free_Screen then
                      New_Bounds := (Scale => Free_Screen,
                         X_Min => X_Screen_Real(Mouse_Bounds.X_Min),
                         X_Max => X_Screen_Real(Mouse_Bounds.X_Max),
                         Y_Min => Y_Screen_Real(Mouse_Bounds.Y_Min),
                         Y_Max => Y_Screen_Real(Mouse_Bounds.Y_Max) );
                    else  -- Normed
                      New_Bounds := (Scale => Free_Normed,
                         X_Min => X_Screen_Real(Mouse_Bounds.X_Min),
                         X_Max => X_Screen_Real(Mouse_Bounds.X_Max),
                         Y_Min => Y_Screen_Real(Mouse_Bounds.Y_Min),
                         Y_Max => Y_Screen_Real(Mouse_Bounds.Y_Max) );
                    end if;
                    -- Compute new conversions
                    begin
                      Maj (New_Bounds);
                      -- No exception, store it
                      Curr_Zoom_No := New_Zoom_No;
                      Last_Zoom_No := Curr_Zoom_No;
                      Zoom_Array(Curr_Zoom_No) := New_Bounds;
                      Cur_Con_Io.Enable_Motion_Events (False);
                      return True;
                    exception
                      when others =>
                        Cancel_Zoom;
                        Cur_Con_Io.Bell(3);
                    end;
                  end;

                elsif Mouse_Event.Valid
                and then Mouse_Event.Button = Con_Io.Right
                and then Mouse_Event.Status = Con_Io.Pressed then
                  Cancel_Zoom;
                else
                  -- no change in init or done
                  Zoom_Frame_Action := None;
                end if;

            end case; -- Current zoom mode

            -- perform zoom frame drawing
            if Mouse_Event.Valid
            or else Mouse_Event.Status /= Con_Io.Pressed then
              Draw_Z_Frame (Zoom_Frame_Action, Mouse_Bounds);
            end if;
          when others =>
            -- Ret, arrows, Pg*...
            -- Invalid key
            Cur_Con_Io.Bell(3);

        end case; -- event

        -- perform scales drawing
        Draw_Scale (Update, Mouse_Bounds);

      end loop;

    end Draw_One;

  begin -- Draw
    -- Initialise graphics
    Init;

    Cur_Con_Io.Set_Pointer_Shape(Con_Io.Cross, Misc(M_Grab));

    -- Initialise zooms storing
    Zoom_Array(Zoom_No_Range'First) := Boundaries;
    Curr_Zoom_No := Zoom_No_Range'First;
    Last_Zoom_No := Zoom_No_Range'First;
    -- Compute first conversions
    Convert.Maj(Zoom_Array(Zoom_No_Range'First));

    loop
      begin
        Draw_Result := Draw_One;
      exception
        when others =>
          if Curr_Zoom_No = Zoom_No_Range'First then
            -- Exception in first draw is fatal
            raise;
          else
            -- try to go back to previous (which should be ok).
            -- At least, we raise after 9 attempts!
            Curr_Zoom_No := Curr_Zoom_No - 1;
            Convert.Maj(Zoom_Array(Curr_Zoom_No));
            Draw_Result := True;
            Cur_Con_Io.Bell(3);
          end if;
      end;

      if not Draw_Result then
        -- Exit drawings
        Cur_Con_Io.Destroy;
        exit;
      else
        -- New drawing : clear graphic
        Cur_Con_Io.Reset_Term;
      end if;

    end loop;


  exception
    when others =>
      begin
        Cur_Con_Io.Destroy;
      exception
        when others => null;
      end;
      raise;
  end Draw;

  function Init return Boolean is
  begin
    Init;
    return True;
  exception
    when Con_Io.Init_Failure =>
      return False;
  end Init;

  procedure Destroy is
  begin
    Cur_Con_Io.Destroy;
  exception
    when others =>
      null;
  end Destroy;
end Curve;

