with Calendar;
with X_Mng, Timers;
package Generic_Con_Io is
  subtype Font_No_Range is Natural range 0 .. 3;

  generic
    Font_No : Font_No_Range;
  package One_Con_Io is

    Row_Range_First : constant Natural := 0;
    Row_Range_Last  : constant Natural := 24;
    Col_Range_First : constant Natural := 0;
    Col_Range_Last  : constant Natural := 79;

    -- Text column and row
    subtype Row_Range is Natural range Row_Range_First .. Row_Range_Last;
    subtype Col_Range is Natural range Col_Range_First .. Col_Range_Last;

    -- A square on the screen
    type Square is record
        Row : Row_Range;
        Col : Col_Range;
      end record;

    -- Upper left square
    Home : constant Square := (Row => Row_Range'First, Col => Col_Range'First);

    -- List of possible colors
    type Colors is (Current, Black, Blue, Green, Cyan, Red, Magenta, Brown,
      Light_Gray, Dark_Gray, Light_Blue, Light_Green, Orange,
      Yellow, White);
    subtype Basic_Colors is Colors range Current .. Light_Gray;

    -- List of colors for outputs
    subtype Effective_Colors is Colors range Black .. Colors'Last;
    subtype Effective_Basic_Colors is Basic_Colors range Black .. Basic_Colors
      'Last;

    -- List of possible blink states of foreground
    type Blink_Stats is (Current, Blink, Not_Blink);
    subtype Effective_Blink_Stats is Blink_Stats range Blink .. Not_Blink;

    -- List of possible XOR_MODE for graphics
    type Xor_Modes is (Current, Xor_On, Xor_Off);
    subtype Effective_Xor_Modes is Xor_Modes range Xor_On .. Xor_Off;


    -- Standard attributes when reset
    Default_Foreground : constant Effective_Colors := Light_Gray;
    Default_Background : constant Effective_Basic_Colors := Black;
    Default_Blink_Stat : constant Effective_Blink_Stats := Not_Blink;
    Default_Xor_Mode   : constant Effective_Xor_Modes := Xor_Off;

    type Window is limited private;

    subtype Byte_Array is X_Mng.Byte_Array;

    -- Has to be called to initialize con_io.
    -- Should be called prior to any con_io action
    -- May be called several times (no effect)
    procedure Init;

    -- To be called to close the con_io
    procedure Destroy;

    -- The window which is screen
    function Screen return Window;

    -- Clear screen, and reset keyboard
    procedure Reset_Term; 

    -- Flushes data to X
    procedure Flush;

    -- Set / get colors, blink, xor
    procedure Set_Foreground (Foreground : in Colors := Current;
                              Blink_Stat : in Blink_Stats := Current;
                              Name       : in Window := Screen);

    procedure Set_Background (Background : in Basic_Colors := Current;
                              Name       : in Window := Screen);

    function Get_Foreground (Name : Window := Screen) return Effective_Colors;
    function Get_Background (Name : Window := Screen) return
      Effective_Basic_Colors;
    function Get_Blink_Stat(Name : Window := Screen) return
      Effective_Blink_Stats;

    procedure Set_Xor_Mode(Xor_Mode : in Xor_Modes := Current;
                           Name : in Window := Screen);
    function Get_Xor_Mode(Name : Window := Screen) return Effective_Xor_Modes;

    -- Get UPPER_LEFT / LOWER_RIGHT absolute coordinates of a window
    function Get_Absolute_Upper_Left  (Name : Window) return Square;
    function Get_Absolute_Lower_Right (Name : Window) return Square;

    -- Get LOWER_RIGHT relative coordinates of a window (UPPER_LEFT is (0, 0)).
    function Get_Relative_Lower_Right (Name : Window) return Square;


    -- Open a window (screen is already open)
    procedure Open (Name                    : in out Window;
                    Upper_Left, Lower_Right : in Square);
    function Is_Open (Name : Window) return Boolean;

    -- TRUE if the absolute square (relative to screen) is in the window.
    -- FALSE otherwise
    function In_Window (Absolute_Square : Square;
                        Name            : Window) return Boolean;

    -- Returns the relative square (relative to window), being the same
    --  physical position as the absolute square (relative to screen).
    -- May raise INVALID_SQUARE if the absolute position is not in window.
    function To_Relative (Absolute_Square : Square;
                          Name            : Window) return Square;

    -- Returns the absolute square (in screen) corresponding to the relative
    --  square in the window
    -- May raise INVALID_SQUARE if the relative square is not in window
    function To_Absolute (Relative_Square : Square;
                          Name            : Window) return Square;

    -- Draw a frame around a window (must be open)
    -- the frame is OUTSIDE the window (so no frame for screen)
    -- FRAME_IMPOSSIBLE if part of the frame is not in the screen
    procedure Frame (Blink : in Boolean := False;
                     Name : in Window);
    procedure Clear_Frame (Name : in Window);

    -- Clear window and move to home
    procedure Clear (Name : in Window := Screen);

    -- Make window re-usable (have to re_open it)
    -- screen cannot be closed
    procedure Close (Name : in out Window);

    -- Move cursor for use with put or get. Position is relativ to window.
    procedure Move (Position : in Square := Home;
                    Name     : in Window := Screen);
    procedure Move (Row  : in Row_Range;
                    Col  : in Col_Range;
                    Name : in Window := Screen);
    function Position (Name : Window := Screen) return Square;

    -- Rings a bell
    procedure Bell (Repeat : in Positive := 1);

    -- Writes a character at the current cursor position and with the
    --  curent attributes. Position can be set by using move.
    -- CR is the only special ASCII character which is interpreted.
    -- If not MOVE, the cursor position is not updated (CR would be ignored then)
    procedure Put (C          : in Character;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Move       : in Boolean := True);

    -- Idem with a string
    -- If not MOVE, the cursor position is not updated
    --  (last CR would be ignored then)
    procedure Put (S          : in String;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Move       : in Boolean := True);

    -- Idem but appends a CR
    procedure Put_Line (S          : in String;
                        Name       : in Window := Screen;
                        Foreground : in Colors := Current;
                        Blink_Stat : in Blink_Stats := Current;
                        Background : in Basic_Colors := Current);

    -- Same than PUT(CHAR) not MOVE, but allows semi graphic characters
    subtype Int_Char is Natural range 0 .. 255;
    procedure Put_Int (Int        : in Int_Char;
                       Name       : in Window := Screen;
                       Foreground : in Colors := Current;
                       Blink_Stat : in Blink_Stats := Current;
                       Background : in Basic_Colors := Current);

    -- Puts CR
    procedure New_Line (Name   : in Window := Screen;
                        Number : in Positive := 1);


    -- Take first character of keyboard buffer (no echo) or refresh event
    procedure Pause;

    -- Gets first character (echo or not)
    -- No echo for RET,      ESC, BREAK, FD_EVENT, TIMER_EVENT, SIGNAL_EVENT
    -- and REFRESH where
    --             ASCII.CR, ESC, EOT,   STX,      SYN,         SI          
    -- and NUL are returned respectively

    -- Cursor movements (UP to RIGHT, TAB and STAB) and mouse events are
    --  discarded (get does not return).
    function Get (Name : Window := Screen; Echo : in Boolean := True)
                 return Character;

    -- How to specify a delay, wait some seconds or until a specific time
    -- PERIOD is not significant
    subtype Delay_List is Timers.Delay_List;
    Infinite_Seconds : constant Duration := Timers.Infinite_Seconds;
    subtype Period_Range is Timers.Period_Range;
    No_Period : constant Period_Range := Timers.No_Period;
    subtype Delay_Rec is Timers.Delay_Rec;
    Infinite_Delay : constant Delay_Rec := Timers.Infinite_Delay;
   

    -- Gets a string of at most width characters
    -- The string must be short enought to be put in 1 line at current position
    --  in the window.
    -- The current cursor position is updated by the call
    -- The arrows, Insert, suppr, backspace, Home, End, PageUp and PageDown
    --  Tab and Ctrl Tab, are managed. Ctrl Suppr clears the string.!
    -- The get ends either:
    --  if an Up/Down arrow, (ctrl) Page Up/Down is pressed,
    --  if the cursor leaves the field (Left/Right arrow or character input),
    --  if Tab, Ctrl Tab, Return(CR), Escape is pressed
    --  on CtrlC/CtrlBreak
    --  on mouse click, release (or motion if enabled)
    --  on time_out expiration
    --  if a callback has been activated on a fd (see x_mng)
    --  if a callback has been activated on a timer expiration (see timer)
    --  if a refresh is needed on the screen (see x_mng)
    -- Mouse_button event can only be generated if the mouse cursor is shown

    -- The returned string ends at last significant digit (gaps with spaces),
    --  tailling spaces are parsed out and last is the index in STR of
    --  the last non-space character
    -- The in and out positions mean 1 for 1st character ..., not indexes
    --  of STR
    -- If STR'length is 0 then last=0 and stat is significant (full if normal
    --  character), but pos_out is not significant.
    -- Note that is STR'LENGHT is 0, the cursor is hidden
    type Curs_Mvt is (Up, Down, Pgup, Pgdown, Ctrl_Pgup, Ctrl_Pgdown,
                      Left, Right, Full, Tab, Stab, Ret, Esc, Break,
                      Mouse_Button, Timeout, Fd_Event, Timer_Event, 
                      Signal_Event, Refresh);
    procedure Get (Str        : out String;
                   Last       : out Natural;
                   Stat       : out Curs_Mvt;
                   Pos        : out Positive;
                   Insert     : out Boolean;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Time_Out   : in Delay_Rec := Infinite_Delay;
                   Echo       : in Boolean := True);

    -- Idem but the get is initialised with the initial content of the string
    --  and cursor's initial location can be set
    procedure Put_Then_Get (Str        : in out String;
                            Last       : out Natural;
                            Stat       : out Curs_Mvt;
                            Pos        : in out Positive;
                            Insert     : in out Boolean;
                            Name       : in Window := Screen;
                            Foreground : in Colors := Current;
                            Blink_Stat : in Blink_Stats := Current;
                            Background : in Basic_Colors := Current;
                            Time_Out   : in Delay_Rec :=  Infinite_Delay;
                            Echo       : in Boolean := True);

    -- Avoid GET_KEY_TIME and GET_KEY functions which may depend on keyboard.

    -- Get_key_time can return if key pressed (ESC event),
    -- mouse action, refresh or timeout
    subtype Event_List is Curs_Mvt range Esc .. Refresh;

    -- Check if a key is available, or another event, until a certain time. 
    -- ESC means any key. No echo.
    procedure Get_Key_Time (Check_Break : in Boolean;
                            Event       : out Event_List;
                            Key         : out Natural;
                            Is_Char     : out Boolean;
                            Ctrl        : out Boolean;
                            Shift       : out Boolean;
                            Time_Out    : in Delay_Rec := Infinite_Delay);

    -- Gives first key code of keyboard buffer, (waits if it is empty) no echo
    -- if not is_char, key is the key code. If is_char, key is the ascii code.
    -- CARE : is_char can be set and key not compatible with ADA characters.
    -- KEY = 0 and IS_CHAR and other flags FALSE: refresh has to be done
    -- KEY = 1 and IS_CHAR and other flags FALSE: fd event has occured
    -- KEY = 2 and IS_CHAR and other flags FALSE: timer event has occured
    -- KEY = 3 and IS_CHAR and other flags FALSE: signal event has occured
    procedure Get_Key (Key     : out Natural;
                       Is_Char : out Boolean;
                       Ctrl    : out Boolean;
                       Shift   : out Boolean);


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
    -- For non window oriented calls (GET_KEY, GRAPHICS, MOUSE)
    Not_Init : exception;

    -- Graphic operations on SCREEN window
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
      -- on screen background, no blink
      -- No window if affected
      procedure Put (C : in Character;
                     X : in X_Range;
                     Y : in Y_Range);

      -- Put a string with screen foreground and current Xor mode
      -- on screen background, no blink
      -- No window if affected
      procedure Put (S : in String;
                     X : in X_Range;
                     Y : in Y_Range);

      -- Draw a point with screen foreground and current Xor mode
      -- on screen background, no blink
      -- No window if affected
      procedure Draw_Point (X : in X_Range;
                            Y : in Y_Range);


      -- Draw a line between 2 points, with screen foreground
      --  and current Xor mode on screen background, no blink
      -- No window if affected
      procedure Draw_Line (X1 : in X_Range;
                           Y1 : in Y_Range;
                           X2 : in X_Range;
                           Y2 : in Y_Range);

      -- Draw a rectangle (only the border) with screen foreground and current
      --  Xor mode
      -- on screen background, no blink (only the border)
      -- No window if affected
      procedure Draw_Rectangle (X1 : in X_Range;
                                Y1 : in Y_Range;
                                X2 : in X_Range;
                                Y2 : in Y_Range);

      -- Draw a filled rectangle with screen foreground and current Xor mode
      -- on screen background, no blink
      -- No window if affected
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

      -- Get dynmically the current position of pointer
      -- If valid is FALSE, it means that the pointer
      -- is currently out of the screen, then X and Y are not significant
      procedure Get_Current_Pointer_Pos (Valid : out Boolean;
                                         X     : out X_Range;
                                         Y     : out Y_Range);

    end Graphics;

    -- Set mouse pointer shape
    --  ARROW by default
    type Pointer_Shape_List is (Arrow, Cross);
    procedure Set_Pointer_Shape (Pointer_Shape : in Pointer_Shape_List);
    

    -- We want mouse position in row_col or x_y
    type Coordinate_Mode_List is (Row_Col, X_Y);

    -- Button status: when MOTION, BUTTON is MOTION
    type Mouse_Button_Status_List is (Pressed, Released, Motion);
    -- List of button
    type Mouse_Button_List is (Left, Middle, Right, Motion);
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

    -- Get a mouse event. If valid is FALSE, it means that a release
    -- has occured outside the screen, then only BUTTON and STATUS
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
        Current_Background : Effective_Basic_Colors;
        Current_Blink_Stat : Effective_Blink_Stats;
        Current_Xor_Mode   : Effective_Xor_Modes;
      end record;

    type Window is access Window_Data;

    Screen_Data   : constant Window_Data := (
      Upper_Left         => (Row => Row_Range_First, Col => Col_Range_First),
      Lower_Right        => (Row => Row_Range_Last,  Col => Col_Range_Last),
      Current_Pos        => Home,
      Current_Foreground => Default_Foreground,
      Current_Background => Default_Background,
      Current_Blink_Stat => Default_Blink_Stat,
      Current_Xor_Mode   => Default_Xor_Mode);

    Screen_Window : constant Window := new Window_Data'(Screen_Data);


  end One_Con_Io;

end Generic_Con_Io;

