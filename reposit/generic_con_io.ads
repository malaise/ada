with Ada.Strings.Unbounded;
with X_Mng, Timers;
package Generic_Con_Io is
  -- Asu = Ada.Strings.Unbounded
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Ada.Strings.Unbounded.Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Asu_Str : Asu_Us) return String
                   renames Asu.To_String;

  -- The Font
  subtype Font_No_Range is Natural range 0 .. 3;

  -- Possible screen size
  Row_Range_First : constant Natural := 0;
  Row_Range_Last : constant := 255;
  Col_Range_First : constant Natural := 0;
  Col_Range_Last : constant := 255;
  subtype Row_Range is Natural range Row_Range_First .. Row_Range_Last;
  subtype Col_Range is Natural range Col_Range_First .. Col_Range_Last;

  -- Default screen size
  Def_Row_Last : constant Row_Range := 24;
  Def_Col_Last : constant Row_Range := 79;

  -- A position in screen
  type Full_Square is record
    Row : Row_Range;
    Col : Col_Range;
  end record;

  -- Possible colors
  type Colors is (Current,
      Color01, Color02, Color03, Color04, Color05, Color06, Color07,
      Color08, Color09, Color10, Color11, Color12, Color13, Color14);

  subtype Effective_Colors is Colors range Color01 .. Colors'Last;

  -- Default colors
  type Colors_Definition is array (Effective_Colors) of Asu_Us;
  Default_Colors : constant Colors_Definition
                 := (Color01 => Asu_Tus ("Black"),
                     Color02 => Asu_Tus ("Blue"),
                     Color03 => Asu_Tus ("Dark_Green"),
                     Color04 => Asu_Tus ("Cyan"),
                     Color05 => Asu_Tus ("Red"),
                     Color06 => Asu_Tus ("Magenta"),
                     Color07 => Asu_Tus ("Brown"),
                     Color08 => Asu_Tus ("Light_Grey"),
                     Color09 => Asu_Tus ("Grey"),
                     Color10 => Asu_Tus ("Light_Blue"),
                     Color11 => Asu_Tus ("Lime_Green"),
                     Color12 => Asu_Tus ("Orange"),
                     Color13 => Asu_Tus ("Yellow"),
                     Color14 => Asu_Tus ("White") );

  -- Set_Colors raises Already_Init if called after Initialise
  Already_Init : exception;
  procedure Set_Colors (Color_Names : in Colors_Definition);
  -- Color_Of raises Unknown_Color ti this color is not found
  Unknown_Color : exception;
  function Color_Of (Name : String) return Effective_Colors;
  function Color_Name_Of (Color : Effective_Colors) return String;

  -- Can be called to initialise con_ios
  -- If not called, this init will be called together with first con_io
  --  initialisation and with default colors
  procedure Initialise;

  generic
    Font_No : Font_No_Range;
    Row_Last : Row_Range := Def_Row_Last;
    Col_Last : Col_Range := Def_Col_Last;
  package One_Con_Io is

    Row_Range_First : constant Natural := 0;
    Row_Range_Last  : constant Natural := Row_Last;
    Col_Range_First : constant Natural := 0;
    Col_Range_Last  : constant Natural := Col_Last;

    -- Text column and row
    subtype Row_Range is Natural range Row_Range_First .. Row_Range_Last;
    subtype Col_Range is Natural range Col_Range_First .. Col_Range_Last;

    -- A square on the screen
    type Square is record
      Row : Row_Range;
      Col : Col_Range;
    end record;

    -- Redefinition of Generic_Con_Io types
    Full_Row_Range_First : constant := Generic_Con_Io.Row_Range_First;
    Full_Row_Range_Last  : constant := Generic_Con_Io.Row_Range_Last;
    Full_Col_Range_First : constant := Generic_Con_Io.Col_Range_First;
    Full_Col_Range_Last  : constant := Generic_Con_Io.Col_Range_Last;
    -- Default screen size
    Full_Def_Row_Last : constant Generic_Con_Io.Row_Range
                      := Generic_Con_Io.Def_Row_Last;
    Full_Def_Col_Last : constant Generic_Con_Io.Col_Range
                      := Generic_Con_Io.Def_Col_Last;
    subtype Full_Row_Range is Generic_Con_Io.Row_Range;
    subtype Full_Col_Range is Generic_Con_Io.Col_Range;

    subtype Full_Square is Generic_Con_Io.Full_Square;
    function Full2Con (Position : Full_Square) return Square;
    function Con2Full (Position : Square) return Full_Square;

    -- Upper left square
    Home : constant Square := (Row => Row_Range'First, Col => Col_Range'First);

    -- List of possible colors
    type Colors is new Generic_Con_Io.Colors;

    -- List of colors for outputs
    subtype Effective_Colors is Colors range Color01 .. Colors'Last;

    -- List of possible Xor_Mode for graphics
    type Xor_Modes is (Current, Xor_On, Xor_Off);
    subtype Effective_Xor_Modes is Xor_Modes range Xor_On .. Xor_Off;


    -- Standard attributes when reset
    Default_Foreground : constant Effective_Colors := Effective_Colors'Last;
    Default_Background : constant Effective_Colors := Effective_Colors'First;
    Default_Xor_Mode   : constant Effective_Xor_Modes := Xor_Off;

    type Window is limited private;

    subtype Byte_Array is X_Mng.Byte_Array;
    subtype Natural_Array is X_Mng.Natural_Array;

    -- Both can be called as soon as Generic_Con_Io is Initialise
    function Color_Of (Name : String) return Effective_Colors;
    function Color_Name_Of (Color : Effective_Colors) return String;

    -- Has to be called to initialize con_io.
    -- Should be called prior any con_io action
    -- May be called several times (no effect)
    procedure Init;

    -- To be called to close the con_io
    procedure Destroy;

    -- Suspend and resume a con_io
    -- If a program wants to open several con_io, there are two options:
    -- - One task per con_io, each task calls *Get and receives its
    --   events
    -- - One taks (or main) opens several con_io but only one is active at a
    --   time. In this case the program must suspend and not use the previous
    --   con_io, then open and use the new con_io, then close the new con_io
    --   then resume and use the first con_io.
    procedure Suspend;
    procedure Resume;

    -- The window which is screen
    function Screen return Window;

    -- Clear screen, and reset keyboard
    procedure Reset_Term;

    -- Flushes data to X
    procedure Flush;

    -- Set / get colors, xor
    procedure Set_Foreground (Foreground : in Colors := Current;
                              Name       : in Window := Screen);

    procedure Set_Background (Background : in Colors := Current;
                              Name       : in Window := Screen);

    function Get_Foreground (Name : Window := Screen) return Effective_Colors;
    function Get_Background (Name : Window := Screen) return Effective_Colors;

    procedure Set_Xor_Mode(Xor_Mode : in Xor_Modes := Current;
                           Name : in Window := Screen);
    function Get_Xor_Mode(Name : Window := Screen) return Effective_Xor_Modes;

    -- Get Upper_Left / Lower_Right absolute coordinates of a window
    function Get_Absolute_Upper_Left  (Name : Window) return Square;
    function Get_Absolute_Lower_Right (Name : Window) return Square;

    -- Get Lower_Right relative coordinates of a window (Upper_Left is (0, 0)).
    function Get_Relative_Lower_Right (Name : Window) return Square;


    -- Open a window (screen is already open)
    procedure Open (Name                    : in out Window;
                    Upper_Left, Lower_Right : in Square);
    function Is_Open (Name : Window) return Boolean;

    -- True if the absolute square (relative to screen) is in the window.
    -- False otherwise
    function In_Window (Absolute_Square : Square;
                        Name            : Window) return Boolean;

    -- Returns the relative square (relative to window), being the same
    --  physical position as the absolute square (relative to screen).
    -- May raise Invalid_Square if the absolute position is not in window.
    function To_Relative (Absolute_Square : Square;
                          Name            : Window) return Square;

    -- Returns the absolute square (in screen) corresponding to the relative
    --  square in the window
    -- May raise Invalid_Square if the relative square is not in window
    function To_Absolute (Relative_Square : Square;
                          Name            : Window) return Square;

    -- Clear window and move to home
    procedure Clear (Name : in Window := Screen);

    -- Make window re-usable (have to re_open it)
    -- screen cannot be closed
    procedure Close (Name : in out Window);

    -- Move cursor for use with put or get. Position is relative to window.
    procedure Move (Position : in Square := Home;
                    Name     : in Window := Screen);
    procedure Move (Row  : in Row_Range;
                    Col  : in Col_Range;
                    Name : in Window := Screen);
    function Position (Name : Window := Screen) return Square;

    -- Rings a bell
    procedure Bell (Repeat : in Positive := 1);

    -- Wide character tool for basic compatibility
    -- See also Language for international support
    -- These operations set '#' when a Wide_Character is not a Character
    Wide_Def_Char : constant Character := '#';
    function Wide_To_Char (W : Wide_Character) return Character;
    function Wide_To_String (Str : Wide_String) return String;
    function "&" (Left : String; Right : Wide_String) return String;
    function "&" (Left : Wide_String; Right : String) return String;


    -- Writes a character at the current cursor position and with the
    --  curent attributes. Position can be set by using move.
    -- Lf is the only special Ascii character which is interpreted.
    -- If not Move, the cursor position is not updated
    --  (Lf would be ignored then)
    procedure Put (C          : in Character;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Background : in Colors := Current;
                   Move       : in Boolean := True);

    -- Idem with a string
    -- If not Move, the cursor position is not updated
    --  (last Lf would be ignored then)
    procedure Put (S          : in String;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Background : in Colors := Current;
                   Move       : in Boolean := True);

    -- Idem but appends a Lf
    procedure Put_Line (S          : in String;
                        Name       : in Window := Screen;
                        Foreground : in Colors := Current;
                        Background : in Colors := Current);

    -- Idem with a wide character
    procedure Putw (W          : in Wide_Character;
                    Name       : in Window := Screen;
                    Foreground : in Colors := Current;
                    Background : in Colors := Current;
                    Move       : in Boolean := True);

    -- Idem with a wide string
    procedure Putw(S          : in Wide_String;
                    Name       : in Window := Screen;
                    Foreground : in Colors := Current;
                    Background : in Colors := Current;
                    Move       : in Boolean := True);

    -- Idem but appends a Lf
    procedure Putw_Line (S          : in Wide_String;
                         Name       : in Window := Screen;
                         Foreground : in Colors := Current;
                         Background : in Colors := Current);

    -- Puts Lf
    procedure New_Line (Name   : in Window := Screen;
                        Number : in Positive := 1);


    -- Take first character of keyboard buffer (no echo) or refresh event
    procedure Pause;


    -- Selection (in/out) management
    -- Set the selection to be transfered to other applications
    -- Resets if empty string
    procedure Set_Selection (Selection : in String);

    -- Request selection from other applications. An event (Curs_Mvt) of
    --  kind Selection will be received, then Get_Selection shall be called
    procedure Request_Selection;

    -- Get the requested selection
    function Get_Selection (Max_Len : Natural) return String;


    -- How to specify a delay, wait some seconds or until a specific time
    -- Period is not significant
    subtype Delay_List is Timers.Delay_List;
    Infinite_Seconds : constant Duration := Timers.Infinite_Seconds;
    subtype Period_Range is Timers.Period_Range;
    No_Period : constant Period_Range := Timers.No_Period;
    subtype Delay_Rec is Timers.Delay_Rec;
    Infinite_Delay : constant Delay_Rec := Timers.Infinite_Delay;


    -- Gets a string of at most width put characters. Width is deduced from
    --  the size to put initial string.
    -- The string must be short enought to be put in 1 line at current position
    --  in the window.
    -- The current cursor position is updated by the call
    -- The Left and Right arrows, Insert, Del, Backspace, Home, End,
    --  are managed internaly.
    --  Ctrl Suppr clears the string.
    -- The get ends either:
    --  if an Up/Down arrow, (Ctrl) Page Up/Down is pressed,
    --  if the cursor leaves the field (Left/Right arrow or character input),
    --  if Tab, Ctrl Tab, Return(Lf), Escape is pressed
    --  on CtrlC/CtrlBreak or X event Exit_Request (from window manager)
    --  on mouse click, release (or motion if enabled)
    --  on time_out expiration
    --  if a callback has been activated on a fd (see x_mng)
    --  if a callback has been activated on a timer expiration (see timer)
    --  if a refresh is needed on the screen (see x_mng)
    -- Mouse_button event can only be generated if the mouse cursor is shown

    -- The returned string ends at last significant digit (gaps with spaces),
    --  tailling spaces are parsed out and last is the index in Str of
    --  the last non-space character
    -- The in and out positions mean 1 for 1st character ..., not indexes
    --  of Str
    -- If Str'length is 0 then last=0 and stat is significant (full if normal
    --  character), but pos_out is not significant.
    -- Note that if Str'Lenght is 0, the cursor is hidden
    type Curs_Mvt is (Up, Down, Ctrl_Up, Ctrl_Down,
                      Pgup, Pgdown, Ctrl_Pgup, Ctrl_Pgdown,
                      Left, Right, Ctrl_Left, Ctrl_Right,
                      Full, Tab, Stab, Ret, Esc, Break,
                      Mouse_Button, Selection, Timeout, Fd_Event, Timer_Event,
                      Signal_Event, Refresh);
    procedure Get (Str        : out Wide_String;
                   Last       : out Natural;
                   Stat       : out Curs_Mvt;
                   Pos        : out Positive;
                   Insert     : out Boolean;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Background : in Colors := Current;
                   Time_Out   : in Delay_Rec := Infinite_Delay;
                   Echo       : in Boolean := True);

    -- Idem but the get is initialised with the initial content of the string
    --  and cursor's initial location can be set
    procedure Put_Then_Get (Str        : in out Wide_String;
                            Last       : out Natural;
                            Stat       : out Curs_Mvt;
                            Pos        : in out Positive;
                            Insert     : in out Boolean;
                            Name       : in Window := Screen;
                            Foreground : in Colors := Current;
                            Background : in Colors := Current;
                            Time_Out   : in Delay_Rec :=  Infinite_Delay;
                            Echo       : in Boolean := True);

    -- Get an event (key or other), no echo
    type Get_Result (Mvt : Curs_Mvt := Full) is record
      case Mvt is
        when Full =>
          Char : Wide_Character;
        when others =>
          null;
      end case;
    end record;
    function Get (Name     : in Window := Screen;
                  Time_Out : in Delay_Rec := Infinite_Delay) return Get_Result;


    -- Enable cursor motion events
    procedure Enable_Motion_Events (Motion_Enabled : in Boolean);

    -- Failure when initialising the screen
    Init_Failure : exception;
    -- Failure when allocating data for window
    Open_Failure        : exception;
    -- Position out of screen (or out of window)
    Invalid_Square      : exception;
    -- Window close to screen limit
    Frame_Impossible    : exception;
    -- Self explanatory
    Window_Not_Open     : exception;
    Window_Already_Open : exception;
    -- String lenght incompatible with current position and window width
    --  for get and put_then get
    String_Too_Long     : exception;
    -- For non window oriented calls (Get_Key, Graphics, Mouse)
    Not_Init : exception;
    -- For Color_Of if this color name is unkown
    Unknown_Color : exception renames Generic_Con_Io.Unknown_Color;

    -- Graphic operations on Screen window
    package Graphics is

      -- Size of the line in pixels
      -- These is the static size when line was created
      subtype X_Range is Natural;
      subtype Y_Range is Natural;

      -- The screen must be open (con_io.init)
      function X_Max return X_Range;
      function Y_Max return Y_Range;

      -- Font characteristics
      function Font_Width  return Natural;
      function Font_Height return Natural;
      function Font_Offset return Natural;

      -- Put a char with screen foreground and current Xor mode
      -- on screen background
      -- No window is affected
      procedure Put (C : in Character;
                     X : in X_Range;
                     Y : in Y_Range);

      -- Put a string with screen foreground and current Xor mode
      -- on screen background
      -- No window is affected
      procedure Put (S : in String;
                     X : in X_Range;
                     Y : in Y_Range);

      -- Draw a point with screen foreground and current Xor mode
      -- on screen background
      -- No window is affected
      procedure Draw_Point (X : in X_Range;
                            Y : in Y_Range);


      -- Draw a line between 2 points, with screen foreground
      --  and current Xor mode on screen background
      -- No window is affected
      procedure Draw_Line (X1 : in X_Range;
                           Y1 : in Y_Range;
                           X2 : in X_Range;
                           Y2 : in Y_Range);

      -- Draw a rectangle (only the border) with screen foreground and current
      --  Xor mode
      -- on screen background (only the border)
      -- No window is affected
      procedure Draw_Rectangle (X1 : in X_Range;
                                Y1 : in Y_Range;
                                X2 : in X_Range;
                                Y2 : in Y_Range);

      -- Draw a filled rectangle with screen foreground and current Xor mode
      -- on screen background
      -- No window is affected
      procedure Fill_Rectangle (X1 : in X_Range;
                                Y1 : in Y_Range;
                                X2 : in X_Range;
                                Y2 : in Y_Range);

      -- Draw points in a rectangle, starting at X1, Y1 and of width * height
      --  pixels
      -- The points array has to be width * height and contains a list of
      --  Zero (no put) or not Zero (put)
      procedure Draw_Points(X, Y          : in Natural;
                            Width, Height : in Natural;
                            Points        : in Byte_Array);

      -- Fill an area defined by several points (X, Y)
      -- The area MUST be convex otherwise the graphic result is undefined
      procedure Fill_Area (Xys : in Natural_Array);

      -- Get dynmically the current position of pointer
      -- If valid is False, it means that the pointer
      -- is currently out of the screen, then X and Y are not significant
      procedure Get_Current_Pointer_Pos (Valid : out Boolean;
                                         X     : out X_Range;
                                         Y     : out Y_Range);

    end Graphics;

    -- Set mouse pointer shape or hide mouse
    --  Arrow by default
    type Pointer_Shape_List is (Arrow, Cross, None);
    procedure Set_Pointer_Shape (Pointer_Shape : in Pointer_Shape_List;
                                 Grab : in Boolean);


    -- We want mouse position in row_col or x_y
    type Coordinate_Mode_List is (Row_Col, X_Y);

    -- Button status: when Motion, Button is Motion
    type Mouse_Button_Status_List is (Pressed, Released, Motion);
    -- List of button
    type Mouse_Button_List is (Left, Middle, Right, Motion, Up, Down,
                               Shift_Up, Shift_Down, Ctrl_Up, Ctrl_Down);
    -- Mouse status
    -- Invalid press should be discarded
    -- Invalid release/motion are out of screen
    type Mouse_Event_Rec (Coordinate_Mode : Coordinate_Mode_List := Row_Col)
                         is record
      Valid : Boolean;
      Button : Mouse_Button_List;
      Status : Mouse_Button_Status_List;
      case Coordinate_Mode is
        when Row_Col =>
          Row : Row_Range;
          Col : Col_Range;
        when X_Y =>
          X : Graphics.X_Range;
          Y : Graphics.Y_Range;
      end case;
    end record;

    -- Get a mouse event. If valid is False, it means that a release
    -- has occured outside the screen, then only Button and Status
    -- are significant
    procedure Get_Mouse_Event (
      Mouse_Event : out Mouse_Event_Rec;
      Coordinate_Mode : in Coordinate_Mode_List := Row_Col);

  private

    type Window_Data is
      record
        Upper_Left         : Square;
        Lower_Right        : Square;
        Current_Pos        : Square := Home;
        Current_Foreground : Effective_Colors;
        Current_Background : Effective_Colors;
        Current_Xor_Mode   : Effective_Xor_Modes;
      end record;

    type Window is access Window_Data;

    Screen_Data   : constant Window_Data := (
      Upper_Left         => (Row => Row_Range_First, Col => Col_Range_First),
      Lower_Right        => (Row => Row_Range_Last,  Col => Col_Range_Last),
      Current_Pos        => Home,
      Current_Foreground => Default_Foreground,
      Current_Background => Default_Background,
      Current_Xor_Mode   => Default_Xor_Mode);

    Screen_Window : constant Window := new Window_Data'(Screen_Data);


  end One_Con_Io;

end Generic_Con_Io;

