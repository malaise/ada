with System;
with Sys_Calls;
package X_Mng is
 
  ----- TYPES -----
  Max_Line_Number : constant := 5;
  type Line is private;
 
  type Byte is new Natural range 0 .. 255;
  for Byte'Size use System.Storage_Unit;

  subtype Color       is Natural range 0 .. 14;
  subtype Font        is Natural range 0 .. 3;
  subtype Bell_Repeat is Positive range 1 .. 5;

  type Line_Definition_Rec is record
    Screen_Id               : Integer;
    Row, Column             : Natural;
    Height, Width           : Natural;
    Background, Border      : Color;
    No_Font                 : Font;
  end record;

  -- keyboard codes for 1 key 
  Kbd_Max_Code : constant := 6;
  subtype Kbd_Index_Code is Integer range 1 .. Kbd_Max_Code;
  type Kbd_Array is array (Kbd_Index_Code) of Byte;

  type Kbd_Tab_Code is record
    Tab : Kbd_Array;
    Nbre : Kbd_Index_Code;
  end record;

  -- For X_DRAW_POINTS
  type Byte_Array is array (Positive range <>) of Byte;

  -- Mouse buttons
  type Button_List is (None, Left, Middle, Right, Up, Down);

  -- This is passed to C and used internally
  type Event_Kind is (Discard, Tid_Release, Tid_Press, Keyboard,
                      Refresh, Tid_Motion, Fd_Event, Timer_Event,
                      Signal_Event);

  ----- EXCEPTIONS -----

  X_Failure : exception;
 
  ----- LINE MANAGEMENT -----

  -- Initialise connection to X server on a host
  --  this call should be done only once, and before any other call
  procedure X_Initialise (Server_Name    : in String);

  -- Opens a line on the host
  -- screen_id is integer, (a negative value for the default screen)
  -- row and column are the coordonates of the upper-left corner of the
  --  window in the screen (in characters)
  -- heigh and width are the dimention of the window in characters
  -- no_font can be 0 for (8x13) character, or 1 for (9x15)
  -- background and border are colors for the window
  -- line_id is the returned value (token for every further operation)
  procedure X_Open_Line(Line_Definition : in Line_Definition_Rec;
                        Line_Id         : in out Line);
  
  -- Closes a line
  -- The line_id is the token, previously given by open_line
  procedure X_Close_Line(Line_Id : in out Line);

  -- Set the name of a line
  -- This name will be displayed by the window manager if possible
  -- The line_id is the token, previously given by open_line
  procedure X_Set_Line_Name (Line_Id : in Line;
                             Line_Name : in String);

  -- Flushes all the outputs of all the lines on the host
  procedure X_Flush(Line_Id : in Line);

  -- Clears a line
  -- The line_id is the token, previously given by open_line
  -- The character attributes are lost.
  -- A flush is done
  procedure X_Clear_Line(Line_Id : in Line);

  ----- PUT and ATTRIBUTE MANAGEMENT -----

  -- Sets the attributes for a further put in the same window
  -- The line_id is the token, previously given by open_line
  -- The paper and ink are color numbers (from 0 to 7)
  -- The attributes are True or False
  procedure X_Set_Attributes(Line_Id     : in Line;
                             Paper, Ink  : in Color;
                             Superbright : in Boolean := False;
                             Underline   : in Boolean := False;
                             Blink       : in Boolean := False;
                             Inverse     : in Boolean := False);

  -- Sets the xor mode or a further put in the same window
  -- The line_id is the token, previously given by open_line
  -- if Xor_More is set, all further puts and drawings will be in xor
  procedure X_Set_Xor_Mode(Line_Id     : in Line;
                           Xor_Mode    : in Boolean);

 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_Put_Char(Line_Id : in Line;
                       Car : in Character;
                       Row, Column : in Natural);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_Put_Char(Line_Id : in Line;
                       Car : in Byte;
                       Row, Column : in Natural);
 
  -- Writes a char with the attributes previously set
  -- The line_id is the token, previously given by open_line
  -- The character is the one to be written
  procedure X_Overwrite_Char(Line_Id : in Line;
                             Car : in Byte;
                             Row, Column : in Natural);
 
  -- Writes a string with the attributes previously set
  --  at a specified position
  -- The line_id is the token, previously given by open_line
  -- The string is the one to be written
  -- The output is flushed
  procedure X_Put_String(Line_Id     : in Line;
                         Str         : in String;
                         Row, Column : in Natural);

  -- Writes a char on a line with specified characteristics
  -- The line_id is the token, previously given by open_line
  -- The row and column are in characters, relative to the window
  -- The character is the one to be written
  -- The paper and ink are color numbers (from 0 to 7)
  -- The attributes are True or False
  procedure X_Put_Char_Attributes(Line_Id     : in Line;
                                  Car         : in Character;
                                  Row, Column : in Natural;
                                  Paper, Ink  : in Color;
                                  Superbright : in Boolean := False;
                                  Underline   : in Boolean := False;
                                  Blink       : in Boolean := False;
                                  Inverse     : in Boolean := False);

  -- Draws a rectangle (width * height) at position 
  --  with current foreground color.
  -- New position is updated to lower-right square of rectangle.
  procedure X_Draw_Area(Line_Id : in Line;
                        Width, Height : in Positive;
                        Row, Column : in Natural);

  ----- GRAPHIC MANAGEMENT -----

  -- Writes a char on a line with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The current row and column are not affected
  -- The character is the one to be written
  -- The X and Y are position of the character
  procedure X_Put_Char_Pixels(Line_Id     : in Line;
                              Car         : in Byte;
                              X, Y        : in Natural);

  -- Get graphic info of window
  -- The line_id is the token, previously given by open_line
  -- Window size in pixels
  -- Font size in pixels
  -- Height offset from top of font
  procedure X_Get_Graphic_Characteristics(Line_Id       : in Line;
                                          Window_Width  : out Natural;
                                          Window_Height : out Natural;
                                          Font_Width    : out Natural;
                                          Font_Height   : out Natural;
                                          Font_Offset   : out Natural);

  -- Draw a point with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are position of the point
  procedure X_Draw_Point(Line_Id       : in Line;
                         X, Y          : in Natural);

  -- Draw a line with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 2 points
  procedure X_Draw_Line(Line_Id        : in Line;
                        X1, Y1, X2, Y2 : in Natural);

  -- Draw a rectangle (border) with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 4 corners
  procedure X_Draw_Rectangle(Line_Id        : in Line;
                             X1, Y1, X2, Y2 : in Natural);

  -- Fill a rectangle with current characteristics
  --  attributes and xor mode
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the 4 corners
  procedure X_Fill_Rectangle(Line_Id        : in Line;
                             X1, Y1, X2, Y2 : in Natural);

  -- Get current position of pointer (independant from events)
  -- The line_id is the token, previously given by open_line
  -- The X and Y are coordinates of the pointer
  procedure X_Get_Current_Pointer_Position(Line_Id : in Line;
                                           X, Y    : out Integer);

  -- Draw points in a rectangle, starting at X1, Y1 and of width * height pixels
  -- The points array has to be width * height and contains a list of Zero (no put)
  --  or not Zero (put)
  procedure X_Draw_Points(Line_Id       : in Line;
                          X, Y          : in Natural;
                          Width, Height : in Natural; 
                          Points        : in Byte_Array);


  -- Set mouse cursor to cross (graphic) or arrow
  procedure X_Set_Graphic_Pointer(Line_Id : in Line;
                                  Graphic : in Boolean);

  ----- EVENT MANAGEMENT -----
  -- Wait for some ms or until a X event is availble
  -- If timeout is < 0, infinite wait
  -- The remaining time is set
  procedure X_Select (Line_Id : in Line;
                      Timeout_Ms : in out Integer; X_Event : out Boolean);

  -- Processes a X Event (Tid or Keyboard or other)
  -- kind is Keyboard or Tid (Press or Release or Motion), or Discard
  --  or Refresh or Fd_Event or Timer_Event or Signal_Event
  -- Next indicates if there is another event pendinig in X'queue
  procedure X_Process_Event(Line_Id : in Line; 
                            Kind : out Event_Kind;
                            Next : out Boolean);
 
  -- Reads the position on Tid in Row/Col or X/Y
  -- The line_id must be the one given by wait_event
  -- Button can be left, middle or right
  -- row and column are the position of the "finger" on the Tid
  --  in row/col or X/Y(pixels)
  procedure X_Read_Tid(Line_Id : in Line; Row_Col : in Boolean;
                       Button : out Button_List;
                       Row, Column : out Integer);
 
  -- Reads a key of a sequence
  -- The line_id must be the one given by wait_event
  -- key is the byte read
  procedure X_Read_Key(Line_Id : in Line; Key : out Kbd_Tab_Code);

  -- Enable disable cursor motion events
  -- The line_id must be the one given by wait_event
  procedure X_Enable_Motion_Events (Line_Id : in Line; Motion_Enable : in Boolean);

  ----- BLINK MANAGEMENT -----

  -- This procedure hides the the text which has blink attribute
  --  (gives the same ink as paper) or restores it, alternatively.
  -- It has no effect UNLESS the internal task has been
  --  stoped with X_Stop_Blinking_Task. 
  -- In this case, it has to be called twice a second to provide
  --  blinking effect.
  procedure X_Blink_Alternate(Line_Id : in Line);

  -- This procedure stops the task which, internaly to x_vdu_mng,
  --  manages the blinking of text.
  -- If a process calls this procedure, no blinking of text will
  --  be impliciptly assumed any more, and the process must
  --  call X_Blink_Alternate regulary;
  procedure X_Stop_Blinking_Task(Line_Id : in Line);

  -- This procedure restarts  the task which, internaly to x_vdu_mng,
  --  manages the blinking of text.
  -- The task should be stopped when this call is done
  procedure X_Start_Blinking_Task(Line_Id : in Line);

  -- This procedures rings a bell at 400Hz for 100ms and repeats it the number
  -- specified. 
  procedure X_Bell (Line_Id : in Line; Repeat : in Bell_Repeat);

 
private
 
  -- This is passed to C and used internally
  type Private_Event_Kind is (Discard, Tid_Release, Tid_Press, Keyboard,
                              Refresh, Tid_Motion, Fd_Event, Timer_Event,
                              Signal_Event, Exception_Event);

  -- Returned events (see Event_Mng and Timers)
  for Private_Event_Kind'Size use 32;
  for Private_Event_Kind use (
    Discard      => 0,      -- }
    Tid_Release  => 1,      -- } These are passed to C
    Tid_Press    => 2,      -- }  and returned to client
    Keyboard     => 3,      -- }  (see Event_Kind)
    Refresh      => 4,      -- } 
    Tid_Motion   => 5,      -- }

    Fd_Event     => 10,      -- } 
    Timer_Event  => 11,     -- } These are returned to client
    Signal_Event => 12,     -- }

    Exception_Event => 20); -- } These are purely internal
   
 
  subtype Line_Range is Natural range 0 .. Max_Line_Number;
  subtype Client_Range is Positive range 1 .. Max_Line_Number;
  No_Client_No : constant Line_Range := 0;

  type Line is record
    -- No_Client
    No : Line_Range := No_Client_No;
  end record;

  No_Client : constant Line := (No => No_Client_No);

end X_Mng;

