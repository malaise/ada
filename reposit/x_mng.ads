-- Low level access to X11 operations (put text / draw / get events)
with System;
with C_Types, As.U, Timers;
package X_Mng is

  ----- TYPES -----
  -- The line identifies the X window
  Max_Line_Number : constant := 5;
  type Line is private;

  subtype Byte is C_Types.Byte;

  -- Colors and fonts
  subtype Color       is Natural range 0 .. 13;
  type Color_Definition is array (Color) of As.U.Asu_Us;
  subtype Font        is Natural range 0 .. 3;
  subtype Bell_Repeat is Positive range 1 .. 5;

  -- Definition of a line (see X_Open_Line)
  type Line_Definition_Rec is record
    Screen_Id               : Integer;
    Row, Column             : Natural;
    Height, Width           : Natural;
    Background, Border      : Color;
    No_Font                 : Font;
  end record;

  -- Keyboard codes for 1 key
  Kbd_Max_Code : constant := 4;
  subtype Kbd_Index_Code is Integer range 1 .. Kbd_Max_Code;
  type Kbd_Array is array (Kbd_Index_Code) of Byte;

  type Kbd_Tab_Code is record
    Nbre : Kbd_Index_Code;
    Tab : Kbd_Array;
  end record;

  -- For X_Draw_Points
  type Byte_Array is array (Positive range <>) of Byte;

  -- For X_Fill_Area
  type Natural_Array is array (Positive range <>) of Natural;

  -- Mouse buttons
  type Button_List is (None, Left, Middle, Right, Up, Down,
                       Shift_Up, Shift_Down, Ctrl_Up, Ctrl_Down,
                       Pgup, Pgdown, Shift_Pgup, Shift_Pgdown,
                       Ctrl_Pgup, Ctrl_Pgdown);

  -- Result of waiting (see Event_Mng for Events).
  -- Tid is mouse (was touch input device)
  -- CARE:
  -- Calling X_Mng in a Timer, Fd or Signal callback will result in a
  --  deadlock
  type Event_Kind is (
     Keyboard, Tid_Release, Tid_Press, Tid_Motion, Tid_Enter, Tid_Leave,
     Refresh, Selection, Exit_Request,
     Timer_Event, Fd_Event, Signal_Event, Timeout);

  -- The external reference (window) of an event
  subtype External_Reference is System.Address;
  Null_Reference : constant External_Reference := System.Null_Address;

  -- Icon for a line
  type Icon_Pixmap is array (Positive range <>) of As.U.Asu_Us;

  ----- EXCEPTIONS -----

  X_Failure : exception;

  ----- LINE MANAGEMENT -----

  -- Initialise the connection to X server on a host and define colors
  -- This call shall be done only once, and before any other call
  procedure X_Initialise (Server_Name    : in String);
  procedure X_Initialise (Server_Name    : in String;
                          Colors         : in Color_Definition);

  -- Get the geometry of a given font in the server
  procedure X_Get_Font_Geometry (No_Font     : in Font;
                                 Font_Width  : out Natural;
                                 Font_Height : out Natural;
                                 Font_Offset : out Natural;
                                 Bold        : in Boolean := False);

  -- Open a line on the host
  -- screen_id is integer, (a negative value for the default screen)
  -- row and column are the coordinates of the upper-left corner of the
  --  window in the screen (in characters)
  -- heigh and width are the dimention of the window in characters
  -- background and border are colors for the window
  -- line_id is the returned value (token for every further operation)
  procedure X_Open_Line(Line_Definition : in Line_Definition_Rec;
                        Line_Id         : in out Line);

  -- Close a line
  procedure X_Close_Line(Line_Id : in out Line);

  -- Suspend and resume a line
  -- If a program wants to open several lines, there are two options:
  -- - One task per line, each task calls X_Wait_Event and receives its
  --   events
  -- - One task (or main) opens several lines but only one can be active at a
  --   time. In this case the program must suspend and not use the previous
  --   line, then open and use the new line, then close or suspend the new line
  --   then resume and use a previous line.
  procedure X_Suspend (Line_Id : in out Line);
  procedure X_Resume  (Line_Id : in out Line);
  function  X_Is_Suspended  (Line_Id : Line) return Boolean;

  -- Set the name of a line
  -- This name will be displayed by the window manager if possible
  procedure X_Set_Name (Line_Id : in Line;
                        Name : in String);

  -- Set the icon of the line (at xpm format)
  procedure X_Set_Icon (Line_Id : in Line;
                        Icon : in Icon_Pixmap);

  -- Flush all the outputs of all the lines on the Display
  procedure X_Flush(Line_Id : in Line);

  -- Clear a line
  -- The character attributes are lost
  -- A flush is done
  procedure X_Clear_Line(Line_Id : in Line);

  ----- PUT and ATTRIBUTE MANAGEMENT -----

  -- Set the attributes for a further put in the same line
  -- The paper and ink are color numbers (from 0 to 13)
  -- The attributes are True or False
  procedure X_Set_Attributes(Line_Id     : in Line;
                             Paper, Ink  : in Color;
                             Bold        : in Boolean := False;
                             Underline   : in Boolean := False;
                             Inverse     : in Boolean := False);

  -- Set the xor mode or a further put in the same line
  -- If Xor_More is set, all further puts and drawings will be in xor
  procedure X_Set_Xor_Mode(Line_Id     : in Line;
                           Xor_Mode    : in Boolean);


  -- Write a char with the attributes previously set
  -- The character is the one to be written
  procedure X_Put_Char(Line_Id : in Line;
                       Car : in Character;
                       Row, Column : in Natural);

  -- Write a char with the attributes previously set
  -- The character is the one to be written
  procedure X_Put_Char(Line_Id : in Line;
                       Car : in Byte;
                       Row, Column : in Natural);

  -- Write a char in xor mode and with the other attributes previously set
  -- The character is the one to be written
  procedure X_Overwrite_Char(Line_Id : in Line;
                             Car : in Byte;
                             Row, Column : in Natural);

  -- Write a string with the attributes previously set
  --  at a specified position
  -- The string is the one to be written
  -- The output is flushed
  procedure X_Put_String(Line_Id     : in Line;
                         Str         : in String;
                         Row, Column : in Natural);

  -- Write a char on a line with specified characteristics
  -- The row and column are in characters, relative to the window
  -- The character is the one to be written
  -- The paper and ink are color numbers (from 0 to 13)
  -- The attributes are True or False
  procedure X_Put_Char_Attributes(Line_Id     : in Line;
                                  Car         : in Character;
                                  Row, Column : in Natural;
                                  Paper, Ink  : in Color;
                                  Bold        : in Boolean := False;
                                  Underline   : in Boolean := False;
                                  Inverse     : in Boolean := False);

  -- Draw a rectangle (width * height) at position with current foreground color
  -- New position is updated to lower-right square of rectangle.
  procedure X_Draw_Area(Line_Id : in Line;
                        Width, Height : in Positive;
                        Row, Column : in Natural);

  ----- GRAPHIC MANAGEMENT -----

  -- Write a char on a line with current characteristics
  --  attributes and xor mode
  -- The current row and column are not affected
  -- The character is the one to be written
  -- The X and Y are position of the character
  -- X is the number of pixels of vertical position (top down) for the
  --  reference line for the font, so you need to add the offset to it if
  --  you want to specify the upperleft pixel for the char
  -- Y is the number of pixels of horizontal position (left right)
  procedure X_Put_Char_Pixels(Line_Id     : in Line;
                              Car         : in Byte;
                              X, Y        : in Natural);

  -- Write a string on a line with current characteristics
  --  attributes and xor mode
  -- The current row and column are not affected
  -- The character is the one to be written
  -- The X and Y are position of the first character
  -- X is the number of pixels of vertical position (top down) for the
  --  reference line for the font, so you need to add the offset to it if
  --  you want to specify the upperleft pixel for the char
  -- Y is the number of pixels of horizontal position (left right)
  procedure X_Put_String_Pixels(Line_Id     : in Line;
                                Str         : in String;
                                X, Y        : in Natural);

  -- Get graphic info of window
  -- Window size in pixels
  -- Font size in pixels
  -- Height offset from top of font
  procedure X_Get_Graphic_Characteristics(Line_Id       : in Line;
                                          Window_Width  : out Natural;
                                          Window_Height : out Natural;
                                          Font_Width    : out Natural;
                                          Font_Height   : out Natural;
                                          Font_Offset   : out Natural);

  -- Get the font name of a window (the normal font name, not the bold)
  function X_Get_Font_Name (Line_Id : Line) return String;

  -- Draw a point with current characteristics
  --  attributes and xor mode
  -- The X and Y are position of the point
  procedure X_Draw_Point(Line_Id       : in Line;
                         X, Y          : in Natural);

  -- Draw a line with current characteristics
  --  attributes and xor mode
  -- The X and Y are coordinates of the 2 points
  procedure X_Draw_Line(Line_Id        : in Line;
                        X1, Y1, X2, Y2 : in Natural);

  -- Draw a rectangle (border) with current characteristics
  --  attributes and xor mode
  -- The X and Y are coordinates of the 4 corners
  procedure X_Draw_Rectangle(Line_Id        : in Line;
                             X1, Y1, X2, Y2 : in Natural);

  -- Fill a rectangle with current characteristics
  --  attributes and xor mode
  -- The X and Y are coordinates of the 4 corners
  procedure X_Fill_Rectangle(Line_Id        : in Line;
                             X1, Y1, X2, Y2 : in Natural);

  -- Draw an arc (border) with current characteristics
  --  attributes and xor mode
  -- The X and Y are coordinates of the 4 corners
  -- The angles are in minutes of degrees, trigo
  procedure X_Draw_Arc(Line_Id        : in Line;
                       X1, Y1, X2, Y2 : in Natural;
                       A1, A2         : in Integer);

  -- Fill aan arc with current characteristics
  --  attributes and xor mode
  -- The X and Y are coordinates of the 4 corners
  -- The angles are in minutes of degrees, trigo
  procedure X_Fill_Arc(Line_Id        : in Line;
                       X1, Y1, X2, Y2 : in Natural;
                       A1, A2         : in Integer);

  -- Get current position of pointer (independant from events)
  -- The X and Y are coordinates of the pointer
  procedure X_Get_Current_Pointer_Position(Line_Id : in Line;
                                           X, Y    : out Integer);

  -- Draw points in a rectangle, starting at X1, Y1 and of width * height pixels
  -- The points array has to be width * height and contains a list of
  --  Zero (no put) or not Zero (put)
  procedure X_Draw_Points(Line_Id       : in Line;
                          X, Y          : in Natural;
                          Width, Height : in Natural;
                          Points        : in Byte_Array);
  -- Fill an area defined by several points (X, Y)
  -- The Xys is the (even) list of X, Y...
  -- The area MUST be convex otherwise the graphic result is undefined
  procedure X_Fill_Area (Line_Id : in Line; Xys : in Natural_Array);

  -- Set mouse pointer shape to arrow, '+' cross or hand, or hide it
  type Pointer_Shapes is (None, Arrow, Cross, Hand, Target);
  procedure X_Set_Pointer(Line_Id : in Line;
                          Shape : in Pointer_Shapes);

  -- Grab/Ungrab mouse pointer
  procedure X_Grab_Pointer(Line_Id : in Line; Grab : in Boolean);

  ----- EVENT MANAGEMENT -----
  -- Wait until an event is availble
  -- If Timeout is a real delay (neither infinite nor expiration time) then
  --  it is updated with the time remaining
  -- Kind is Keyboard or Tid (mouse Press or Release or Motion) for this line,
  --  or Refresh or Exit_Request or Fd_Event or Timer_Event or Signal_Event...
  -- Timeout must be in real time (Clock = null) or Invalid_Timeout is raised
  -- CARE: Timer, Fd or Signal callbacks are invoked internally and calling
  --  X_Mng in these callbacks will lead to a deadlock
  Invalid_Timeout : exception;
  procedure X_Wait_Event(Line_Id : in Line;
                         Timeout : in out Timers.Delay_Rec;
                         Kind : out Event_Kind);
  -- This variant allows the retrieval of the external reference associated to
  --  the sub-window of the event
  procedure X_Wait_Event(Line_Id : in Line;
                         Timeout : in out Timers.Delay_Rec;
                         Kind : out Event_Kind;
                         Ref : out External_Reference);

  -- Read the position on Tid (mouse) in Row/Col or X/Y
  -- The line_id must be the one given to X_Wait_Event
  -- Button can be left, middle or right
  -- Row and Column are the position of the "finger" on the Tid
  --  in row/col or X/Y(pixels)
  -- Sub_Row and Sub_Column are the positions relative to the sub-window, if any
  procedure X_Read_Tid(Line_Id : in Line;
                       Row_Col : in Boolean;
                       Button  : out Button_List;
                       Row, Column : out Integer;
                       Sub_Row, Sub_Column : out Integer);

  -- Read a keyboard key
  -- The line_id must be the one given to X_Wait_Event
  -- Control if control key was on
  -- Shift if Code and shift key was on
  -- Code if function key or no translation, then Key is two bytes
  --  else key is the translated sequence of bytes
  procedure X_Read_Key(Line_Id : in Line;
                       Control : out Boolean;
                       Shift : out Boolean;
                       Code : out Boolean;
                       Key : out Kbd_Tab_Code);

  -- Enable disable mouse motion events
  procedure X_Enable_Motion_Events (Line_Id : in Line;
                                    Motion_Enable : in Boolean);

  ----- SELECTION MANAGEMENT -----
  -- Set/reset the selection to be transfered to other applications
  procedure X_Set_Selection (Line_Id : in Line; Selection : in String);
  procedure X_Reset_Selection (Line_Id : in Line);

  -- Request selection from other applications. An event of kind Selection
  --  will be received, then X_Get_Selection shall be called
  procedure X_Request_Selection (Line_Id : in Line);

  -- Get the requested selection
  -- Raises X_Failure if no selection available
  function X_Get_Selection (Line_Id : Line; Max_Len : Natural) return String;

  ----- BELL -----
  -- Ring a bell at 400Hz for 100ms and repeats it the number specified.
  procedure X_Bell (Line_Id : in Line; Repeat : in Bell_Repeat);


private

  subtype Line_Range is Natural range 0 .. Max_Line_Number;
  subtype Client_Range is Positive range 1 .. Max_Line_Number;
  No_Client_No : constant Line_Range := 0;

  -- Line access for X
  subtype Line_For_C is System.Address;
  No_Line_For_C : constant Line_For_C := System.Null_Address;


  type Line is record
    -- No_Client
    No : Line_Range := No_Client_No;
    -- Line for C saved while suspended
    Suspended_Line_For_C : Line_For_C := No_Line_For_C;
  end record;

  No_Client : constant Line := (
        No => No_Client_No,
        Suspended_Line_For_C => No_Line_For_C);

  -- For children
  procedure Call_On  (Line_Id : in Line;
                      Line_For_C_Id : out Line_For_C);
  procedure Call_Off (Line_Id : in Line;
                      New_Line_For_C_Id : in Line_For_C);
  procedure Modified;

end X_Mng;

