-- Inputs / outputs on console (X11 window)
private with Ada.Calendar;
private with Smart_Reference, Dynamic_List, Perpet;
with As.U, X_Mng, Timers, Unicode, Aski.Unicode;
package Con_Io is

  -----------------
  -- Definitions --
  -----------------
  -- Propagation of Unicode definitions
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  Space : Unicode_Number renames Aski.Unicode.Spc_U;
  Htab  : Unicode_Number renames Aski.Unicode.Ht_U;

  -- A physical screen ID
  subtype Screen_Id_Range is Integer range -1 .. 254;
  Default_Screen_Id : constant Screen_Id_Range := -1;

  -- The Font identifier
  subtype Font_No_Range is Natural range 0 .. 3;

  -- Possible console size
  -- The xxx_range_last are Console dependent (functions are defined below)
  Row_Range_First : constant Natural := 0;
  Last_Row : constant := 255;
  Col_Range_First : constant Natural := 0;
  Last_Col : constant := 255;
  subtype Row_Range is Natural range Row_Range_First .. Last_Row;
  subtype Col_Range is Natural range Col_Range_First .. Last_Col;

  -- Default console size (80 x 25)
  Def_Row_Last : constant Row_Range := 24;
  Def_Col_Last : constant Row_Range := 79;

  -- A position, in a window or console
  type Square is record
    Row : Row_Range;
    Col : Col_Range;
  end record;
  -- Upper left square
  Home : constant Square := (Row => Row_Range'First, Col => Col_Range'First);

  -- Possible colors
  type Colors is (Current,
      Color01, Color02, Color03, Color04, Color05, Color06, Color07,
      Color08, Color09, Color10, Color11, Color12, Color13, Color14);

  subtype Effective_Colors is Colors range Color01 .. Colors'Last;

  -- Default colors
  type Colors_Definition is array (Effective_Colors) of As.U.Asu_Us;
  Default_Colors : constant Colors_Definition
                 := (Color01 => As.U.Tus ("Light_Grey"),
                     Color02 => As.U.Tus ("Blue"),
                     Color03 => As.U.Tus ("Dark_Green"),
                     Color04 => As.U.Tus ("Cyan"),
                     Color05 => As.U.Tus ("Red"),
                     Color06 => As.U.Tus ("Magenta"),
                     Color07 => As.U.Tus ("Brown"),
                     Color08 => As.U.Tus ("White"),
                     Color09 => As.U.Tus ("Dark_Grey"),
                     Color10 => As.U.Tus ("Light_Blue"),
                     Color11 => As.U.Tus ("Lime_Green"),
                     Color12 => As.U.Tus ("Orange"),
                     Color13 => As.U.Tus ("Yellow"),
                     Color14 => As.U.Tus ("Black") );

  --------------------
  -- Initialisation --
  --------------------
  -- Set_Colors raises Already_Init if called after Initialise
  -- Console.Init raises Already_Init if called several times
  Already_Init : exception;
  -- Color_Of raises Unknown_Color if this color is not found
  Unknown_Color : exception;

  -- Set colors to specific names, before initialisation
  procedure Set_Colors (Color_Names : in Colors_Definition);

  -- Color <-> Name
  function Color_Of (Name : String) return Effective_Colors;
  function Color_Name_Of (Color : Effective_Colors) return String;

  -- List of possible Xor_Mode for graphics
  type Xor_Modes is (Current, Xor_On, Xor_Off);
  subtype Effective_Xor_Modes is Xor_Modes range Xor_On .. Xor_Off;

  -- Default attributes when creating a Console
  Default_Foreground : constant Effective_Colors := Effective_Colors'Last;
  Default_Background : constant Effective_Colors := Effective_Colors'First;
  Default_Xor_Mode   : constant Effective_Xor_Modes := Xor_Off;

  -- Can be called to initialise the consoles manager (for example, if all
  --  the consoles are created by threads, this ensures that the initialisation
  --  is done with the main stack)
  -- It is called automatically when opening the first console
  procedure Initialise;

  -- Get the geometry of a font
  procedure Get_Font_Geometry (Font_No     : in Font_No_Range;
                               Font_Width  : out Natural;
                               Font_Height : out Natural;
                               Font_Offset : out Natural);

  -------------
  -- Console --
  -------------
  -- One console
  type Console is tagged private;
  type Console_Access is access all Console;

  -- Open a console, which appears on screen
  -- Initialise Con_Io if necessary
  -- Shall be called prior any action on the console
  -- A Screen window is open with the attributes of the Console and cleared
  procedure Open (Con : in out Console;
                  Screen_Id : in Screen_Id_Range := Default_Screen_Id;
                  Font_No   : in Font_No_Range := 1;
                  Row_Last  : in Row_Range := Def_Row_Last;
                  Col_Last  : in Col_Range := Def_Col_Last;
                  Def_Fore  : in Effective_Colors := Default_Foreground;
                  Def_Back  : in Effective_Colors := Default_Background;
                  Def_Xor   : in Effective_Xor_Modes := Default_Xor_Mode);

  -- To be called to close the console
  procedure Close (Con : in out Console);

  -- Is a console open
  function Is_Open (Con : Console) return Boolean;

  -- Set console name (X11 window title)
  procedure Set_Name (Con : in Console; Name : in String);

  -- Set console icon
  subtype Icon_Pixmap is X_Mng.Icon_Pixmap;
  procedure Set_Icon (Con : in Console; Icon : in Icon_Pixmap);

  -- Suspend and resume a console
  -- If a program wants to open several consoles, there are two options:
  -- - One task per console, each task calls *Get and receives its
  --   events
  -- - One taks (or main) opens several consoles but only one is active at a
  --   time. In this case the program must suspend and not use the previous
  --   console, then open and use the new console, then close the new console
  --   then resume and use the previous console.
  procedure Suspend (Con : in Console);
  procedure Resume (Con : in Console);
  function Is_Suspended (Con : Console) return Boolean;

  -- Get the X_Mng Line associated to a console
  function Get_Line (Con : Console) return X_Mng.Line;

  -- Get colors of Console
  function Foreground (Con : Console) return Effective_Colors;
  function Background (Con : Console) return Effective_Colors;
  function Xor_Mode   (Con : Console) return Effective_Xor_Modes;

  -- Get geometry of Console
  function Row_Range_Last  (Con : Console) return Row_Range;
  function Col_Range_Last  (Con : Console) return Col_Range;


  -- Flush data to the console
  procedure Flush (Con : in Console);

  -- Ring a bell
  procedure Bell (Con : in Console; Repeat : in Positive := 1);

  -- Reset screen attributes to Console's one and clear it
  procedure Reset_Screen (Con : in Console);

  -- Clear screen (as Screen.Clear), optionally after setting a new
  -- background color (as Screen.Set_Background)
  procedure Clear_Screen (Con        : in Console;
                          Background : in Colors := Current);

  ------------
  -- Window --
  ------------
  -- Operations on window
  type Window is tagged private;
  type Window_Access is access all Window;

  subtype Byte_Array is X_Mng.Byte_Array;
  subtype Natural_Array is X_Mng.Natural_Array;

  -- The window which is screen (always open)
  function Get_Screen (Con : Console_Access) return Window;
  procedure Set_To_Screen (Name : in out Window; Con : in Console_Access);

  -- Open a window (screen is always open) with Console attributes
  procedure Open (Name : in out Window; Con : in Console_Access;
                  Upper_Left, Lower_Right : in Square);

  -- Make window re-usable (have to re_open it)
  -- Screen cannot be closed (raises Closing_Screen)
  procedure Close (Name : in out Window);

  -- Is a window open
  function Is_Open (Name : Window) return Boolean;

  -- Get the Console of a window
  function Get_Console (Name : Window_Access) return Console;

  -- Clear window and move to home
  procedure Clear (Name : in Window);

  -- Set / get colors, xor
  procedure Set_Foreground (Name       : in Window;
                            Foreground : in Colors := Current);
  function Get_Foreground (Name : Window) return Effective_Colors;

  procedure Set_Background (Name       : in Window;
                            Background : in Colors := Current);
  function Get_Background (Name : Window) return Effective_Colors;

  procedure Set_Xor_Mode (Name : in Window;
                          Xor_Mode : in Xor_Modes := Current);
  function Get_Xor_Mode (Name : Window) return Effective_Xor_Modes;

  -- Get Upper_Left / Lower_Right absolute coordinates of a window
  function Get_Absolute_Upper_Left  (Name : Window) return Square;
  function Get_Absolute_Lower_Right (Name : Window) return Square;

  -- Get Lower_Right relative coordinates of a window (Upper_Left is (0, 0)).
  function Get_Relative_Lower_Right (Name : Window) return Square;

  -- Get geometry of Window
  function Row_Range_Last  (Name : Window) return Row_Range;
  function Col_Range_Last  (Name : Window) return Col_Range;

  -- Return True if the absolute square (relative to screen) is in the window.
  -- False otherwise
  function In_Window (Name            : Window;
                      Absolute_Square : Square) return Boolean;

  -- Return the relative square (relative to window), being the same
  --  physical position as the absolute square (relative to screen).
  -- May raise Invalid_Square if the absolute position is not in window.
  function To_Relative (Name            : Window;
                        Absolute_Square : Square) return Square;

  -- Return the absolute square (in screen) corresponding to the relative
  --  square in the window
  -- May raise Invalid_Square if the relative square is not in window
  function To_Absolute (Name            : Window;
                        Relative_Square : Square) return Square;


  -- Move cursor for use with put or get. Position is relative to window.
  procedure Move (Name     : in Window;
                  Position : in Square := Home);
  procedure Move (Name     : in Window;
                  Row  : in Row_Range;
                  Col  : in Col_Range);
  function Position (Name : Window) return Square;


  ---------
  -- Put --
  ---------
  -- Write a character at the current cursor position and with the
  --  curent attributes. Position can be set by using move.
  -- Lf is the only special Ascii character which is interpreted.
  -- If not Move, the cursor position is not updated
  --  (Lf would be ignored then)
  -- The characters can be shifted by half a row down and half a column right
  --  from the cursor position (the cursor itself is not shifted)
  procedure Put (Name       : in Window;
                 C          : in Character;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Move       : in Boolean := True;
                 Row_Off    : in Boolean := False;
                 Col_Off    : in Boolean := False);

  -- Idem with a string
  -- If not Move, the cursor position is not updated
  --  (last Lf would be ignored then)
  procedure Put (Name       : in Window;
                 S          : in String;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Move       : in Boolean := True;
                 Row_Off    : in Boolean := False;
                 Col_Off    : in Boolean := False);

  -- Idem and append a Lf
  procedure Put_Line (Name       : in Window;
                      S          : in String;
                      Foreground : in Colors := Current;
                      Background : in Colors := Current;
                      Row_Off    : in Boolean := False;
                      Col_Off    : in Boolean := False);

  -- Idem with a wide character
  procedure Putw (Name       : in Window;
                  W          : in Wide_Character;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True;
                  Row_Off    : in Boolean := False;
                  Col_Off    : in Boolean := False);

  -- Idem with a wide string
  procedure Putw (Name       : in Window;
                  S          : in Wide_String;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True;
                  Row_Off    : in Boolean := False;
                  Col_Off    : in Boolean := False);

  -- Idem and append a Lf
  procedure Putw_Line (Name       : in Window;
                       S          : in Wide_String;
                       Foreground : in Colors := Current;
                       Background : in Colors := Current;
                       Row_Off    : in Boolean := False;
                       Col_Off    : in Boolean := False);

  -- Idem with a unicode number
  procedure Putu (Name       : in Window;
                  U          : in Unicode_Number;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True;
                  Row_Off    : in Boolean := False;
                  Col_Off    : in Boolean := False);

  -- Idem with a unicode sequence
  procedure Putu (Name       : in Window;
                  S          : in Unicode_Sequence;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True;
                  Row_Off    : in Boolean := False;
                  Col_Off    : in Boolean := False);

  -- Idem and append a Lf
  procedure Putu_Line (Name       : in Window;
                       S          : in Unicode_Sequence;
                       Foreground : in Colors := Current;
                       Background : in Colors := Current;
                       Row_Off    : in Boolean := False;
                       Col_Off    : in Boolean := False);
  -- Put Lf
  procedure New_Line (Name   : in Window;
                      Number : in Positive := 1);

  ------------------
  -- Put Then Get --
  ------------------
  -- Selection (in/out) management
  -- Set the selection to be transfered to other applications
  -- Resets selection if empty string
  procedure Set_Selection (Con : in Console; Selection : in String);

  -- Request selection from other applications. An event (Curs_Mvt) of
  --  kind Selection will be received, then Get_Selection shall be called
  procedure Request_Selection (Con : in Console);

  -- Get the requested selection
  function Get_Selection (Con : Console; Max_Len : Natural) return String;

  -- How to specify a delay, wait some seconds or until a specific time
  -- Period is not significant
  subtype Delay_List is Timers.Delay_List;
  Infinite_Seconds : constant Duration := Timers.Infinite_Seconds;
  subtype Period_Range is Timers.Period_Range;
  No_Period : Period_Range renames Timers.No_Period;
  subtype Delay_Rec is Timers.Delay_Rec;
  Infinite_Delay : Delay_Rec renames Timers.Infinite_Delay;


  -- Get a string of at most width put characters. Width is deduced from
  --  the size to put initial string.
  -- The string must be short enough to be put in 1 line at current position
  --  in the window.
  -- The current cursor position is updated by the call
  -- The Left and Right arrows, Insert, Suppr, Backspace, Home, End, Ctrl Suppr
  --  (clear the string), Shift Suppr (clear to end of string) are managed
  --  internaly.
  -- The get ends either:
  --  if (Ctrl/Shift) Up/Down arrow, (Ctrl/Shift) Page Up/Down, Ctrl/Shift
  --   Left/Right arrow is pressed
  --  if the cursor leaves the field on Left/Right arrow or on character
  --   input (Full),
  --  if Tab, Shift Tab, Return(Lf), Escape is pressed
  --  on CtrlC/CtrlBreak or X event Exit_Request (from window manager)
  --  on mouse click, release (or motion if enabled)
  --  on reception of a selection
  --  on time_out expiration
  --  if a callback has been activated on a fd (see event_mng)
  --  if a callback has been activated on a timer expiration (see timer)
  --  if a callback has been activated on a signal reception (see event_mng)
  --  if a refresh is needed on the screen (see x_mng)
  -- Mouse_button event can only be generated if the mouse cursor is shown
  -- Note: In some environments the Next/Prev Page mouse buttons are mapped to
  --  the Page Up/Down keyboard events

  -- The returned string ends at last significant digit (padded with spaces),
  --  tailling spaces are parsed out and last is the index in Str of
  --  the last non-space character
  -- The in and out positions mean 1 for 1st character ..., not indexes
  --  of Str
  -- If Str'length is 0 then last=0 and stat is significant (full if normal
  --  character), but pos_out is not significant.
  -- Note that if Str'Lenght is 0, the cursor is hidden
  type Extra_Mvt is (Up, Down, Shift_Up, Shift_Down, Ctrl_Up, Ctrl_Down,
                     Pgup, Pgdown, Shift_Pgup, Shift_Pgdown,
                     Ctrl_Pgup, Ctrl_Pgdown,
                     Left, Right, Ctrl_Left, Ctrl_Right,
                     Shift_Left, Shift_Right,
                     Full, Tab, Stab, Ret, Esc, Break,
                     Mouse_Button, Selection, Timeout, Fd_Event, Timer_Event,
                     Signal_Event, Refresh,
  -- Extra cursor movements when preventing change of length
                    Backspace, Suppr, Ctrl_Suppr, Shift_Suppr, Insert);
  subtype Curs_Mvt is Extra_Mvt range Up .. Refresh;
  procedure Get (Name       : in Window;
                 Str        : out Unicode_Sequence;
                 Last       : out Natural;
                 Stat       : out Curs_Mvt;
                 Pos        : out Positive;
                 Insert     : out Boolean;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Time_Out   : in Delay_Rec := Infinite_Delay;
                 Echo       : in Boolean := True);

  -- Idem but the get is initialised with the initial content of the string
  --  and cursor's initial location can be set
  procedure Put_Then_Get (Name       : in Window;
                          Str        : in out Unicode_Sequence;
                          Last       : out Natural;
                          Stat       : out Curs_Mvt;
                          Pos        : in out Positive;
                          Insert     : in out Boolean;
                          Foreground : in Colors := Current;
                          Background : in Colors := Current;
                          Time_Out   : in Delay_Rec :=  Infinite_Delay;
                          Echo       : in Boolean := True);

  -- Idem but with a Wide_String
  procedure Put_Then_Get (Name       : in Window;
                          Str        : in out Wide_String;
                          Last       : out Natural;
                          Stat       : out Curs_Mvt;
                          Pos        : in out Positive;
                          Insert     : in out Boolean;
                          Foreground : in Colors := Current;
                          Background : in Colors := Current;
                          Time_Out   : in Delay_Rec :=  Infinite_Delay;
                          Echo       : in Boolean := True);

  -- Idem but, if Extra, then any change of the string length (Suppr, Insert...)
  --   ends Ptg before modifying Str
  -- On Insert, sets Stat to Insert and returns the Char to insert (at Pos)
  -- otherwise (backspace, Suppr...) sets Stat to the extra cursor movement and
  --  returns a space
  function Put_Then_Get (Name       : in Window;
                         Extra      : in Boolean;
                         Str        : in out Unicode_Sequence;
                         Last       : out Natural;
                         Stat       : out Extra_Mvt;
                         Pos        : in out Positive;
                         Insert     : in out Boolean;
                         Foreground : in Colors := Current;
                         Background : in Colors := Current;
                         Time_Out   : in Delay_Rec :=  Infinite_Delay;
                         Echo       : in Boolean := True) return Unicode_Number;

  -- Get an event (key or other), no echo
  type Get_Result (Mvt : Curs_Mvt := Full) is record
    case Mvt is
      when Full =>
        Char : Unicode_Number;
      when others =>
        null;
    end case;
  end record;
  function Get (Name     : Window;
                Time_Out : Delay_Rec := Infinite_Delay) return Get_Result;

  --------------
  -- Graphics --
  --------------
  -- Graphic operations on Screen window (with Screen attributes)

  -- By default, the origin in Con_Io is at bottom left, which is easier
  --  for graphics. But this is different from X_Mng and X11 orgin, which is
  --  top left and easier for square to Xy conversions.
  -- This operation toggles the origin of Y scale for display and events
  type Y_Modes is (Con_Io_Mode, X_Mng_Mode);
  procedure Set_Y_Mode (Con : in Console; Y_Mode : in Y_Modes);

  -- Size of the line in pixels
  -- These is the static size when line was created
  subtype X_Range is Natural;
  subtype Y_Range is Natural;

  -- Angles in minutes of degrees
  subtype Minutes is Integer range -360 * 60 .. 360 * 60;

  -- Size of the screen
  -- Initialise Con_Io if necessary
  -- Can be called before opening consoles
  procedure Get_Screen_Geometry (
      X : out X_Range;
      Y : out Y_Range;
      Screen_Id : in Screen_Id_Range := Default_Screen_Id);

  -- From here, the Console  must be open

  -- Console geometry
  function X_Max (Con : Console) return X_Range;
  function Y_Max (Con : Console) return Y_Range;

  -- Font characteristics
  function Font_Name   (Con : Console) return String;
  function Font_Width  (Con : Console) return Natural;
  function Font_Height (Con : Console) return Natural;
  function Font_Offset (Con : Console) return Natural;

  -- Convert X, Y to a square (the one containing the pixel)
  --  and reverse (the upper left pixel of the square)
  function To_Square (Con : Console;
                      X : X_Range; Y : Y_Range) return Square;
  procedure To_Xy (Con : in Console; Position : in Square;
                   X : out X_Range; Y : out Y_Range);

  -- Put a char with screen foreground and current Xor mode
  -- on screen background
  -- X and Y denote the upper left pixel for the char
  -- No window is affected
  procedure Put (Con : in Console;
                 C   : in Character;
                 X   : in X_Range;
                 Y   : in Y_Range);

  -- Put a string with screen foreground and current Xor mode
  -- on screen background
  -- X and Y denote the upper left pixel for the first char
  -- No window is affected
  procedure Put (Con : in Console;
                 S   : in String;
                 X   : in X_Range;
                 Y   : in Y_Range);

  -- Draw a point with screen foreground and current Xor mode
  -- on screen background
  -- No window is affected
  procedure Draw_Point (Con : in Console;
                        X   : in X_Range;
                        Y   : in Y_Range);

  -- Draw a line between 2 points, with screen foreground
  --  and current Xor mode on screen background
  -- No window is affected
  procedure Draw_Line (Con : in Console;
                       X1  : in X_Range;
                       Y1  : in Y_Range;
                       X2  : in X_Range;
                       Y2  : in Y_Range);

  -- Draw a rectangle (only the border) with screen foreground and current
  --  Xor mode
  -- on screen background (only the border)
  -- No window is affected
  procedure Draw_Rectangle (Con : in Console;
                            X1  : in X_Range;
                            Y1  : in Y_Range;
                            X2  : in X_Range;
                            Y2  : in Y_Range);

  -- Draw a filled rectangle with screen foreground and current Xor mode
  -- on screen background
  -- No window is affected
  procedure Fill_Rectangle (Con : in Console;
                            X1  : in X_Range;
                            Y1  : in Y_Range;
                            X2  : in X_Range;
                            Y2  : in Y_Range);

  -- Draw an arc (only the border) with screen foreground and current Xor mode
  -- on screen background (only the border)
  -- Within a rectangle, between two angles in minutes of degrees starting from
  -- 3 o'clock position and in trigo wose (counter clockwise)
  -- No window is affected
  procedure Draw_Arc (Con : in Console;
                      X1  : in X_Range;
                      Y1  : in Y_Range;
                      X2  : in X_Range;
                      Y2  : in Y_Range;
                      A1, A2 : in Minutes);

  -- Fill an arc with screen foreground and current Xor mode
  -- on screen background
  -- Within a rectangle, between two angles in minutes of degrees starting from
  -- 3 o'clock position and in trigo wose (counter clockwise)
  -- No window is affected
  procedure Fill_Arc (Con : in Console;
                      X1  : in X_Range;
                      Y1  : in Y_Range;
                      X2  : in X_Range;
                      Y2  : in Y_Range;
                      A1, A2 : in Minutes);

  -- Draw points in a rectangle, starting at X1, Y1 and of width * height
  --  pixels
  -- The points array has to be width * height and contains a list of
  --  Zero (no put) or not Zero (put)
  procedure Draw_Points(Con           : in Console;
                        X, Y          : in Natural;
                        Width, Height : in Natural;
                        Points        : in Byte_Array);

  -- Fill an area defined by several points (X, Y)
  -- The area MUST be convex otherwise the graphic result is undefined
  procedure Fill_Area (Con : in Console; Xys : in Natural_Array);

  -----------
  -- Mouse --
  -----------
  -- Get dynmically the current position of mouse
  -- If valid is False, it means that the pointer
  -- is currently out of the screen, then X and Y are not significant
  procedure Get_Current_Pointer_Pos (Con   : in Console;
                                     Valid : out Boolean;
                                     X     : out X_Range;
                                     Y     : out Y_Range);

  -- Enable mouse motion events
  procedure Enable_Motion_Events (Con : in Console;
                                  Motion_Enabled : in Boolean);

  -- Set mouse pointer shape or hide mouse pointer
  --  Arrow is the default
  type Pointer_Shape_List is (None, Arrow, Cross, Hand, Target);
  procedure Set_Pointer_Shape (Con           : in Console;
                               Pointer_Shape : in Pointer_Shape_List);
  -- Try to restore pointer shape to Arrow, catching any exception
  procedure Reset_Pointer_Shape (Con : in Console);

  -- (Un)grab the mouse pointer
  procedure Grab_Pointer (Con  : in Console;
                          Grab : in Boolean);

  -- Set delay criteria for double-click of the Left, Middle or Right button
  subtype Double_Click_Delay_Range is Duration range 0.100 .. 0.900;
  Default_Double_Click_Delay : constant Double_Click_Delay_Range := 0.500;
  procedure Set_Double_Click_Delay (
      Con                : in Console;
      Double_Click_Delay : in Double_Click_Delay_Range);

  -- Get current delay criteria for double-click
  function Get_Double_Click_Delay (Con : in Console)
                                   return Double_Click_Delay_Range;

  -- After the validation of a simple click it may be wise sometimes to prevent
  --  the detection of the associated double-click
  procedure Cancel_Double_Click (Con  : in Console);

  -- We want mouse position in row_col or x_y
  type Coordinate_Mode_List is (Row_Col, X_Y);

  -- Button status: when Motion, Button is Motion
  type Mouse_Button_Status_List is (Pressed, Released, Enter, Leave, Motion);
  -- List of buttons
  type Mouse_Button_List is (Left, Middle, Right, Motion, Up, Down,
                             Shift_Up, Shift_Down, Ctrl_Up, Ctrl_Down,
                             Pgup, Pgdown, Shift_Pgup, Shift_Pgdown,
                             Ctrl_Pgup, Ctrl_Pgdown);
  -- Pointing buttons
  subtype Mouse_Pointing_List is Mouse_Button_List range Left .. Right;
  -- Mouse status
  -- Invalid press should be discarded
  -- Invalid release/motion are out of screen
  type Mouse_Event_Rec (Coordinate_Mode : Coordinate_Mode_List := Row_Col)
                       is record
    Valid : Boolean;
    Button : Mouse_Button_List;
    Status : Mouse_Button_Status_List;
    Double_Click : Boolean;
    Xref : X_Mng.External_Reference;
    case Coordinate_Mode is
      when Row_Col =>
        Row : Row_Range;
        Col : Col_Range;
        Sub_Row : Row_Range;
        Sub_Col : Col_Range;
      when X_Y =>
        X : X_Range;
        Y : Y_Range;
        Sub_X : X_Range;
        Sub_Y : Y_Range;
    end case;
  end record;

  -- Get a mouse event. If valid is False, it means that a release
  -- has occured outside the screen, then only Button and Status
  -- are significant
  procedure Get_Mouse_Event (
      Con             : in Console;
      Mouse_Event     : out Mouse_Event_Rec;
      Coordinate_Mode : in Coordinate_Mode_List := Row_Col);
  function Get_Mouse_Event (
      Con             : Console;
      Coordinate_Mode : Coordinate_Mode_List := Row_Col) return Mouse_Event_Rec;

  -- Convert an event in X_Y into the same in Row_Col
  subtype X_Y_Event is Mouse_Event_Rec (X_Y);
  subtype Row_Col_Event is Mouse_Event_Rec (Row_Col);
  function To_Row_Col (Con   : in Console;
                       Event : X_Y_Event) return Row_Col_Event;

  ----------------
  -- Exceptions --
  ----------------
  -- Failure when initialising the Con_Io or openging a Console
  Init_Failure : exception;
  -- Failure when opening a window
  Open_Failure        : exception;
  -- Operation (except Resume and Is_Suspended) on a suspended Console
  -- Resume a non suspended console
  Suspended : exception;
  -- Position out of screen (or out of window)
  Invalid_Square      : exception;
  -- Window close to screen limit
  Frame_Impossible    : exception;
  -- Window not open (or not open in this console)
  Window_Not_Open     : exception;
  -- Attempt to close Screen window
  Closing_Screen : exception;
  -- Self explanatory
  Window_Already_Open : exception;
  -- String lenght incompatible with current position and window width
  --  for get and put_then get
  String_Too_Long     : exception;
  -- Any operation on a not initialized console
  Not_Init : exception;

private

  type Console_Data is record
    -- Console characteristics
    Initialised : Boolean := False;
    Id : X_Mng.Line;
    Y_Mode : Y_Modes := Con_Io_Mode;
    Mouse_Status : X_Mng.Event_Kind := X_Mng.Timeout;
    Mouse_Xref : X_Mng.External_Reference := X_Mng.Null_Reference;
    Motion_Enabling : Boolean := False;
    Def_Foreground : Effective_Colors := Default_Foreground;
    Def_Background : Effective_Colors := Default_Background;
    Def_Xor_Mode   : Effective_Xor_Modes := Default_Xor_Mode;
    Font_No : Font_No_Range := Font_No_Range'First;
    Row_Range_Last : Row_Range := Row_Range'First;
    Col_Range_Last : Col_Range := Col_Range'First;
    X_Max : X_Range := X_Range'First;
    Y_Max : Y_Range := Y_Range'First;
    Font_Width  : Natural := 0;
    Font_Height : Natural := 0;
    Font_Offset : Natural := 0;
    Screen_Window : Window_Access;
    -- Cache of current attributes
    Line_Foreground : Effective_Colors := Default_Foreground;
    Line_Background : Effective_Colors := Default_Background;
    Line_Xor_Mode   : Effective_Xor_Modes := Default_Xor_Mode;
    -- Management of for double click
    Double_Click_Delay : Double_Click_Delay_Range
                       := Default_Double_Click_Delay;
    Last_Click_Time : Ada.Calendar.Time := Perpet.Origin;
    Last_Click_Button : Mouse_Button_List := Motion;
  end record;
  procedure Set (Dest : out Console_Data; Val : in Console_Data);
  procedure Finalize (Con : in out Console_Data);
  package Console_Ref_Mng is new Smart_Reference (Console_Data, Set, Finalize);
  type Console is new Console_Ref_Mng.Handle with null record;
  Null_Console : constant Console
               := (Console_Ref_Mng.Null_Handle with null record);


  -- Windows are stored in a global list
  type Window_Data is record
    Con                : Console := Null_Console;
    Open               : Boolean := False;
    Upper_Left         : Square  := (Row => Row_Range_First,
                                     Col => Col_Range_First);
    Lower_Right        : Square  := (Row => Last_Row,  Col => Last_Col);
    Current_Pos        : Square  := Home;
    Current_Foreground : Effective_Colors    := Default_Foreground;
    Current_Background : Effective_Colors    := Default_Background;
    Current_Xor_Mode   : Effective_Xor_Modes := Default_Xor_Mode;
  end record;
  procedure Set (Dest : out Window_Data; Val : in Window_Data);
  procedure Finalize (Win : in out Window_Data);
  package Window_Ref_Mng is new Smart_Reference (Window_Data, Set, Finalize);
  type Window is new Window_Ref_Mng.Handle with null record;
  Null_Window : constant Window
               := (Window_Ref_Mng.Null_Handle with null record);

  Screen_Data : constant Window_Data := (others => <>);

  -- List of open windows
  -- Defined in spec in order to avoid that the finalization of the body
  --  (which may occur before the finalization of user data) deletes the list
  --  too early
  package Window_Dyn_List_Mng is new Dynamic_List (Window);
  package Window_List_Mng renames Window_Dyn_List_Mng.Dyn_List;
  Windows : Window_List_Mng.List_Type;

end Con_Io;

