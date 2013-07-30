with Ada.Calendar, Ada.Characters.Latin_1;
with C_Types, Basic_Proc, Address_Ops, Environ, Perpet, Event_Mng, Virtual_Time;
package body X_Mng is

  -- Maximum successive X events
  Max_Successive_X : constant Positive := 21;

  Infinite_Timeout : constant Duration := Timers.Infinite_Seconds;

  -- Debug
  Debug_Var_Name : constant String := "X_MNG_DEBUG";
  Debug : Boolean := False;

  -- Result of a call to C
  subtype Result is C_Types.Int;
  Ok : constant Result := 0;

  -- True if the connection to X has been initialised
  Initialised : Boolean := False;

  -- Boolean on 32 bits for C
  type Bool_For_C is new C_Types.Bool;
  for Bool_For_C'Size use 32;

  function For_C(Ada_Boolean : in Boolean) return Bool_For_C is
  begin
    return Bool_For_C'Val(Boolean'Pos(Ada_Boolean));
  end For_C;

  ------------------------------------------------------------------
  -------------------- T H E   I N T E R F A C E -------------------
  ------------------------------------------------------------------
  -- Initialise X for one host
  -- int x_initialise (const char *server_name, const char *color_names[]);
  ------------------------------------------------------------------
  function X_Initialise (Server_Name : System.Address;
                         Color_Names : System.Address) return Result;
  pragma Import(C, X_Initialise, "x_initialise");

  ------------------------------------------------------------------
  -- Suspend processing of X events (as long as no Line)
  -- int x_suspend (void)
  ------------------------------------------------------------------
  function X_Suspend return Result;
  pragma Import(C, X_Suspend, "x_suspend");

  ------------------------------------------------------------------
  -- Resume processing of X events
  -- int x_resume (void)
  ------------------------------------------------------------------
  function X_Resume return Result;
  pragma Import(C, X_Resume, "x_resume");

  ------------------------------------------------------------------
  -- Opens a line
  -- int x_open_line (int screen_id, int row, int column,
  --                  int height, int width,
  --                  int background, int border, int no_font,
  --                  void **p_line_id);
  ------------------------------------------------------------------
  function X_Open_Line (Screen_Id          : C_Types.Int;
                        Row, Column        : C_Types.Int;
                        Height, Width      : C_Types.Int;
                        Background, Border : C_Types.Int;
                        No_Font            : C_Types.Int;
                        P_Line_Id          : System.Address) return Result;
  pragma Import(C, X_Open_Line, "x_open_line");

  ------------------------------------------------------------------
  -- Closes a line
  -- int x_close_line (void *line_id);
  ------------------------------------------------------------------
  function X_Close_Line(Line_Id : Line_For_C) return Result;
  pragma Import(C, X_Close_Line, "x_close_line");

  ------------------------------------------------------------------
  -- Set the name of a line
  -- int x_set_line_name (void *line_id, const char *line_name);
  ------------------------------------------------------------------
  function X_Set_Line_Name (Line_Id   : Line_For_C;
                            Line_Name : System.Address) return Result;
  pragma Import(C, X_Set_Line_Name, "x_set_line_name");

  ------------------------------------------------------------------
  -- Flushes all the lines of the host (really display them)
  -- int x_flush (void)
  ------------------------------------------------------------------
  function X_Flush return Result;
  pragma Import(C, X_Flush, "x_flush");

  ------------------------------------------------------------------
  -- Clears a line
  -- int x_clear_line (void *line_id);
  ------------------------------------------------------------------
  function X_Clear_Line(Line_Id : Line_For_C) return Result;
  pragma Import(C, X_Clear_Line, "x_clear_line");

  ------------------------------------------------------------------
  -- Sets the attributes for a further put in the same window
  -- int x_set_attributes (void *line_id;
  --                       int paper, int ink,
  --                       boolean superbright, boolean underline,
  --                       boolean reverse);
  ------------------------------------------------------------------
  function X_Set_Attributes(Line_Id     : Line_For_C;
                            Paper, Ink  : C_Types.Int;
                            Superbright : Bool_For_C;
                            Underline   : Bool_For_C;
                            Inverse     : Bool_For_C) return Result;
  pragma Import(C, X_Set_Attributes, "x_set_attributes");

  ------------------------------------------------------------------
  -- Set Xor mode for further outputs
  -- int x_set_xor_mode (void *line_id, boolean xor_mode);
  ------------------------------------------------------------------
  function X_Set_Xor_Mode(Line_Id : Line_For_C;
                          Xor_Mode  : Bool_For_C) return Result;
  pragma Import(C, X_Set_Xor_Mode, "x_set_xor_mode");

  ------------------------------------------------------------------
  -- Writes a char whith the attributes previously set
  -- int x_put_char (void *line_id, int  car, int row, int column);
  ------------------------------------------------------------------
  function X_Put_Char(Line_Id     : Line_For_C;
                      Car         : C_Types.Int;
                      Row, Column : in C_Types.Int) return Result;
  pragma Import(C, X_Put_Char, "x_put_char");

  ------------------------------------------------------------------
  -- Writes a char whith the attributes previously set
  -- Does not erase character at current position
  -- int x_overwrite_char (void *line_id, int  car,
  --                       int row, int column);
  ------------------------------------------------------------------
  function X_Overwrite_Char(Line_Id     : Line_For_C;
                            Car         : C_Types.Int;
                            Row, Column : in C_Types.Int) return Result;
  pragma Import(C, X_Overwrite_Char, "x_overwrite_char");

  ------------------------------------------------------------------
  -- Writes a string at location with the attributes previously set
  -- int x_put_stringt (void *line_id, const char *p_char, int number,
  --                    int row, int column);
  ------------------------------------------------------------------
  function X_Put_String(Line_Id     : Line_For_C;
                        Str_Addr    : System.Address;
                        Length      : C_Types.Int;
                        Row, Column : C_Types.Int) return Result;
  pragma Import(C, X_Put_String, "x_put_string");

  ------------------------------------------------------------------
  -- Writes a char on a line with specified characteristics
  -- int x_put_char_attributes (void *line_id; int car,
  --                            int row, int column, int paper, int ink,
  --                            boolean superbright, boolean underline,
  --                            boolean reverse);
  ------------------------------------------------------------------
  function X_Put_Char_Attributes(Line_Id     : Line_For_C;
                                 Car         : C_Types.Int;
                                 Row, Column : C_Types.Int;
                                 Paper, Ink  : C_Types.Int;
                                 Superbright : Bool_For_C;
                                 Underline   : Bool_For_C;
                                 Inverse     : Bool_For_C)
   return Result;
  pragma Import(C, X_Put_Char_Attributes, "x_put_char_attributes");

  ------------------------------------------------------------------
  -- Draws a rectangle (width * height) from position
  --  with current background color.
  --  New position is updated to lower-left square of rectangle.
  -- int x_draw_area (void *line_id, int width, int height);
  ------------------------------------------------------------------
  function X_Draw_Area(Line_Id       : Line_For_C;
                       Width, Height : C_Types.Int;
                       Row, Column   : C_Types.Int) return Result;
  pragma Import(C, X_Draw_Area, "x_draw_area");

  ------------------------------------------------------------------
  -- Puts a char with current characteristics
  --  at specified position in pixels
  -- int x_put_char_pixels (void *line_id, int car, int x, int y);
  ------------------------------------------------------------------
  function X_Put_Char_Pixels(Line_Id : Line_For_C;
                             Car     : C_Types.Int;
                             X, Y    : C_Types.Int) return Result;
  pragma Import(C, X_Put_Char_Pixels, "x_put_char_pixels");

  ------------------------------------------------------------------
  -- Gets the graphic characteristics of a line when it was created
  -- int x_get_graph_charact (void *line_id, int *p_w_width, int *p_w_height,
  --                  int *p_f_width, int *p_f_height, int *p_f_offset);
  ------------------------------------------------------------------
  function X_Get_Graphic_Characteristics(Line_Id : Line_For_C;
                                         Window_Width  : System.Address;
                                         Window_Height : System.Address;
                                         Font_Width    : System.Address;
                                         Font_Height   : System.Address;
                                         Font_Offset   : System.Address)
           return Result;
  pragma Import(C, X_Get_Graphic_Characteristics, "x_get_graph_charact");

  ------------------------------------------------------------------
  -- Draw a point with current characteristics
  -- int x_draw_point (void *line_id, int x, int y);
  ------------------------------------------------------------------
  function X_Draw_Point(Line_Id : Line_For_C;
                        X, Y    : C_Types.Int) return Result;
  pragma Import(C, X_Draw_Point, "x_draw_point");

  ------------------------------------------------------------------
  -- Draw a line with current characteristics
  -- int x_draw_line (void *line_id, int x1, int y1, int x2, int y2);
  ------------------------------------------------------------------
  function X_Draw_Line(Line_Id : Line_For_C;
                       X1, Y1, X2, Y2 : Natural) return Result;
  pragma Import(C, X_Draw_Line, "x_draw_line");

  ------------------------------------------------------------------
  -- Draw a rectangle with current characteristics
  -- int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2);
  ------------------------------------------------------------------
  function X_Draw_Rectangle(Line_Id : Line_For_C;
                            X1, Y1, X2, Y2 : Natural) return Result;
  pragma Import(C, X_Draw_Rectangle, "x_draw_rectangle");

  ------------------------------------------------------------------
  -- Fill a rectangle with current characteristics
  -- int x_draw_rectangle (void *line_id, int x1, int y1, int x2, int y2);
  ------------------------------------------------------------------
  function X_Fill_Rectangle(Line_Id : Line_For_C;
                            X1, Y1, X2, Y2 : Natural) return Result;
  pragma Import(C, X_Fill_Rectangle, "x_fill_rectangle");

  ------------------------------------------------------------------
  -- Draw points in a rectangle, starting at x1, y1 and of width * height pixels
  -- int x_draw_points (void *line_id, int x1, int y1, int width, int height,
  --                      unsigned char points[]);
  ------------------------------------------------------------------
  function X_Draw_Points(Line_Id : Line_For_C;
                         X1, Y1 : Natural;
                         Width, Height : Natural;
                         Points : System.Address) return Result;
  pragma Import(C, X_Draw_Points, "x_draw_points");

  ------------------------------------------------------------------
  -- Fill an area defined by several points (X, Y)
  -- The area MUST be convex otherwise the graphic result is undefined
  -- int x_fill_area (void *line_id, int xys[], int nb_points);
  ------------------------------------------------------------------
  function X_Fill_Area (Line_Id : Line_For_C;
                        Xys : System.Address;
                        Nb_Points : C_Types.Int) return Result;
  pragma Import(C, X_Fill_Area, "x_fill_area");

  ------------------------------------------------------------------
  -- Get current position in pixels, independently from events
  -- int x_get_pointer_pos (void *line_id, int *p_x, int *p_y);
  ------------------------------------------------------------------
  function X_Get_Current_Pointer_Position(Line_Id : Line_For_C;
                                          X, Y : System.Address)
           return Result;
  pragma Import(C, X_Get_Current_Pointer_Position, "x_get_pointer_pos");

  ------------------------------------------------------------------
  -- Set mouse pointer in graphic (cross) or standard (arrow)
  -- int x_set_graphic_pointer (void *line_id, boolean graphic);
  ------------------------------------------------------------------
  function X_Set_Graphic_Pointer(Line_Id : Line_For_C;
                                 Graphic : Bool_For_C;
                                 Grab : Bool_For_C) return Result;
  pragma Import(C, X_Set_Graphic_Pointer, "x_set_graphic_pointer");

  ------------------------------------------------------------------
  -- Hide mouse pointer
  -- int x_hide_graphic_pointer (void *line_id);
  ------------------------------------------------------------------
  function X_Hide_Graphic_Pointer(Line_Id : Line_For_C;
                                  Grab : Bool_For_C) return Result;
  pragma Import(C, X_Hide_Graphic_Pointer, "x_hide_graphic_pointer");

  ------------------------------------------------------------------
  -- Reads the position on Tid
  -- int x_read_tid (void *line_id, boolean row_col,
  --                 int *p_button, int *p_row, int *p_column);
  ------------------------------------------------------------------
  function X_Read_Tid(Line_Id         : Line_For_C;
                      Row_Col         : Bool_For_C;
                      P_Button        : System.Address;
                      P_Row, P_Column : System.Address) return Result;
  pragma Import(C, X_Read_Tid, "x_read_tid");

  ------------------------------------------------------------------
  -- Reads a key of a sequence
  -- int x_read_key (void *line_id, int *p_key, int *p_nbre);
  ------------------------------------------------------------------
  function X_Read_Key(Line_Id   : Line_For_C;
                      P_Control : System.Address;
                      P_Shift   : System.Address;
                      P_Code    : System.Address;
                      P_Keys    : System.Address;
                      P_Nbre    : System.Address) return Result;
  pragma Import(C, X_Read_Key, "x_read_key");

  ------------------------------------------------------------------
  -- Propose selection to others
  -- extern int x_set_selection (void *line_id, const char *selection);

  ------------------------------------------------------------------
  function X_Set_Selection (Line_Id : Line_For_C;
                            Selection : System.Address) return Result;
  pragma Import(C, X_Set_Selection, "x_set_selection");

  ------------------------------------------------------------------
  -- Request selection from others
  -- extern int x_request_selection (void *line_id);
  ------------------------------------------------------------------
  function X_Request_Selection (Line_Id : Line_For_C) return Result;
  pragma Import(C, X_Request_Selection, "x_request_selection");

  ------------------------------------------------------------------
  -- Get requested selection
  -- extern int x_get_selection (void *line_id, char *p_selection, int len);
  ------------------------------------------------------------------
  function X_Get_Selection (Line_Id : Line_For_C;
                             Selection : System.Address;
                             Len : C_Types.Int) return Result;
  pragma Import(C, X_Get_Selection, "x_get_selection");

  ------------------------------------------------------------------
  -- Enable / disable cursor motion events
  -- extern int x_enable_motion_events (void *line_id, boolean enable_motion);
  ------------------------------------------------------------------
  function X_Enable_Motion_Events (Line_Id : Line_For_C;
                                   Motion_Enable : Bool_For_C) return Result;
  pragma Import(C, X_Enable_Motion_Events, "x_enable_motion_events");

  ------------------------------------------------------------------
  -- Rings a bell several times
  -- int x_bell (int nbre_bell;
  ------------------------------------------------------------------
  function X_Bell (Repeat : C_Types.Int) return Result;
  pragma Import(C, X_Bell, "x_bell");


  ------------------------------------------------------------------
  --------------- T H E   D I S P A T C H E R   S P E C ------------
  ------------------------------------------------------------------

  -- Dispatcher of X calls and X events
  package Dispatch is

    -- The dispatcher ensures that each task but one is waiting for X event
    --  on an entry (Wait_Event) dedicated to it. One task processes other
    --  events (fd, timers...) or its own X events, then blocks on C select

    -- Each client must call Prepare, where it signals it is ready to wait
    --  and in which it might block on C select
    -- Then it must call Wait_Event on which it blocks until events are for it

    -- During registration (Pre_Register, then Register, up to the first
    -- Prepare), the task can do X calls and there cannot be other registration

    -- Event kind exchanged internally with dispatcher
    -- Either a Wakeup event (to mask) or the need to re-dispatch (Refresh_All)
    type Internal_Event_Kind is (Wakeup_Event, Dispatch_Event);
    type Event_Rec (Internal : Boolean := False) is record
      case Internal is
        when True =>
          Internal_Kind : Internal_Event_Kind := Dispatch_Event;
        when False =>
          Kind : Event_Kind := Timeout;
      end case;
    end record;

    -- Client definition, used internally in dispatcher
    type Client_Rec is record
      -- Client status
      Used : Boolean := False;       -- Slot available
      Running : Boolean;             -- Running or Waiting
      Refreshing : Boolean;          -- First to refresh
      -- Registration date
      Birth : Ada.Calendar.Time;
      -- Will be Line_For_C
      Line_For_C_Id : Line_For_C := No_Line_For_C;
      -- The one provided to Wait
      Wait_Exp : Timers.Expiration_Rec;
    end record;
    type Client_List is array (Positive range <>) of Client_Rec;

    ----------------
    -- OPERATIONS --
    ----------------

    -- Wake up the dispatcher before Register (then try to register)
    procedure Pre_Register;

    protected Dispatcher is

      -- Iniitalize (only once)
      procedure Initialize;

      -- Register / Unregister. Count clients and refresh all on Unregister
      entry Register (Client : out Line_Range);
      entry Unregister (Client : in out Line_Range);

      -- Two calls to protect a call to X
      entry Call_On  (Client : in Line_Range;
                      Line_For_C_Id : out Line_For_C);
      procedure Call_Off (Client : in Client_Range;
                      New_Line_For_C_Id : in Line_For_C);

      -- Ready to wait for Exp
      entry Prepare (Client : in Client_Range;
                     Exp    : in Timers.Expiration_Rec);

      -- Wait for an event
      entry Wait_Event(Client_Range) (New_Event : out Event_Rec);

    private
      -- Initialized
      Initialized : Boolean := False;

      -- Number of registered clients
      Nb_Clients : Line_Range := 0;
      -- Number of clients which have called Wait and are waiting in Wait_Event
      --  does not count the one in C select
      Nb_Waiting : Line_Range := 0;
      -- Client selected by Wait that is (to be) released from Wait_Event,
      --  and the event for it
      Selected : Line_Range := No_Client_No;
      Event : Event_Rec := (False, Timeout);
      Next_Event : Boolean := False;
      -- Number of successive X events
      Nb_X_Events : Natural := 0;
      -- Between Call_On and Call_Off
      In_X : Boolean := False;
      -- After un Unregister, all other clients deserve a Refresh event
      Refresh_All : Boolean := False;
      -- While a client is registering, its index
      Registered : Line_Range := No_Client_No;
      -- The clients
      Clients : Client_List(Client_Range);
    end Dispatcher;

    -- Raised on inconsitency
    Dispatch_Error : exception;
  end Dispatch;

  use Dispatch;

  ------------------------------------------------------------------
  ------------------------ T H E   C A L L S -----------------------
  ------------------------------------------------------------------

  -- Internal check that a X_Mng is initialised, that the Line is open and not
  -- suspended
  procedure Check (Line_Id : in Line) is
    use type System.Address;
  begin
    if not Initialised or else Line_Id = No_Client
    or else Line_Id.Suspended_Line_For_C /= No_Line_For_C then
      raise X_Failure;
    end if;
  end Check;

  procedure X_Initialise (Server_Name : in String;
                          Colors      : in Color_Definition;
                          Colors_Set  : in Boolean) is


    Serv_Name_For_C : constant String(1 .. Server_Name'Length+1)
                    := Server_Name & Ada.Characters.Latin_1.Nul;
    Max_Len : Positive;
    Len : Natural;
    Res : Boolean;
  begin
    if Initialised then
      return;
    end if;
    -- Get longest color name
    if Colors_Set then
      Max_Len := 1;
      for I in Colors'Range loop
        Len :=Colors(I).Length;
        if Len = 0 then
          if Debug then
            Basic_Proc.Put_Line_Output ("X_Initialise, incorrect empty color at " & I'Img);
          end if;
          raise X_Failure;
        elsif Len > Max_Len then
          Max_Len := Len;
        end if;
      end loop;
      -- Store color names for 4 and their addresses
      declare
        subtype Color_Name is String (1 .. Max_Len + 1);
        Color_Names : array (Colors'Range) of Color_Name;
        Colors4C : array (Colors'Range) of System.Address;
      begin
        for I in Colors'Range loop
          Color_Names(I) (1 .. Colors(I).Length + 1) :=
             Colors(I).Image & Ada.Characters.Latin_1.Nul;
          Colors4C(I) := Color_Names(I)(Color_Names(I)'First)'Address;
        end loop;
        Res := X_Initialise (Serv_Name_For_C(Serv_Name_For_C'First)'Address,
                       Colors4C(Colors4C'First)'Address) = Ok;
      end;
    else
        Res := X_Initialise (Serv_Name_For_C(Serv_Name_For_C'First)'Address,
                             System.Null_Address) = Ok;
    end if;
    if not Res then
      raise X_Failure;
    end if;
    Dispatcher.Initialize;
    Debug := Environ.Is_Yes (Debug_Var_Name);
    Initialised := True;
  end X_Initialise;
  procedure X_Initialise (Server_Name : in String;
                          Colors      : in Color_Definition) is
  begin
    X_Initialise (Server_Name, Colors, True);
  end X_Initialise;

  procedure X_Initialise (Server_Name : in String) is
    Colors : Color_Definition;
  begin
    X_Initialise (Server_Name, Colors, False);
  end X_Initialise;

  ------------------------------------------------------------------
  procedure X_Open_Line(Line_Definition : in Line_Definition_Rec;
                        Line_Id         : in out Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id /= No_Client then
      raise X_Failure;
    end if;
    -- Register
    -- If several simultaneous Pre_Register, then only one Wake_Up
    --   so be ready to try several times
    loop
      -- Force wake up of dispatcher
      Dispatch.Pre_Register;
      select
        Dispatcher.Register(Line_Id.No);
        exit;
      or
        delay 0.1;
      end select;
    end loop;
    if Line_Id = No_Client then
      -- Too many clients
      raise X_Failure;
    end if;

    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    -- Open window
    Res := X_Open_Line (Line_Definition.Screen_Id,
                        Line_Definition.Row,
                        Line_Definition.Column,
                        Line_Definition.Height,
                        Line_Definition.Width,
                        Line_Definition.Background,
                        Line_Definition.Border,
                        Line_Definition.No_Font,
                        Line_For_C_Id'Address) = Ok;
    if Debug then
      Basic_Proc.Put_Line_Output ("X_Open_Line " & Line_Id.No'Img
                    & " -> " & Address_Ops.Image(Line_For_C_Id));
    end if;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);

    if not Res then
      Dispatcher.Unregister(Line_Id.No);
      raise X_Failure;
    end if;

  end X_Open_Line;

  ------------------------------------------------------------------
  procedure X_Close_Line(Line_Id : in out Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Close_Line(Line_For_C_Id) = Ok;
    Res := Res and then X_Flush = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    -- Unregister
    Dispatcher.Unregister(Line_Id.No);
    if not Res then
      raise X_Failure;
    end if;
  end X_Close_Line;

  ------------------------------------------------------------------
  procedure X_Suspend (Line_Id : in out Line) is
  begin
    Check (Line_Id);
    -- Call_On to get Line_For_C and save it
    Dispatcher.Call_On (Line_Id.No, Line_Id.Suspended_Line_For_C);
    Dispatcher.Call_Off (Line_Id.No, Line_Id.Suspended_Line_For_C);
    -- Unregister: Line_No is reset (and Line_For_C would be lost)
    Dispatcher.Unregister(Line_Id.No);
  end X_Suspend;

  ------------------------------------------------------------------
  procedure X_Resume  (Line_Id : in out Line) is
    Dummy_Line_For_C : Line_For_C;
    use type System.Address;
  begin
    -- No must not be set but Suspended_Line must be
    if not Initialised
    or else Line_Id.No /= No_Client_No
    or else Line_Id.Suspended_Line_For_C = No_Line_For_C then
      raise X_Failure;
    end if;
    -- Register, force wake up of waiting task
    loop
      Dispatch.Pre_Register;
      select
        Dispatcher.Register(Line_Id.No);
        exit;
      or
        delay 0.1;
      end select;
    end loop;
    if Line_Id = No_Client then
      -- Too many clients
      raise X_Failure;
    end if;

    -- Call_Off to set saved Line_For_C
    Dispatcher.Call_On (Line_Id.No, Dummy_Line_For_C);
    Dispatcher.Call_Off (Line_Id.No, Line_Id.Suspended_Line_For_C);
    Line_Id.Suspended_Line_For_C := No_Line_For_C;
  end X_Resume;

  ------------------------------------------------------------------
  function  X_Is_Suspended  (Line_Id : Line) return Boolean is
    use type System.Address;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    return Line_Id.Suspended_Line_For_C /= No_Line_For_C;
  end X_Is_Suspended;

  ------------------------------------------------------------------
  procedure X_Set_Line_Name (Line_Id : in Line;
                             Line_Name : in String) is
    Line_Name_For_C : constant String(1 .. Line_Name'Length+1)
                    := Line_Name & Ada.Characters.Latin_1.Nul;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Line_Name(Line_For_C_Id,
                       Line_Name_For_C(Line_Name_For_C'First)'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Line_Name;

  ------------------------------------------------------------------
  procedure X_Flush (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Flush = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Flush;

  ------------------------------------------------------------------
  procedure X_Clear_Line(Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Clear_Line(Line_For_C_Id) = Ok;
    Res := Res and then X_Flush = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Clear_Line;


  ------------------------------------------------------------------
  procedure X_Set_Attributes(Line_Id     : in Line;
                             Paper, Ink  : in Color;
                             Superbright : in Boolean := False;
                             Underline   : in Boolean := False;
                             Inverse     : in Boolean:= False) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Attributes(Line_For_C_Id,
                            C_Types.Int(Paper), C_Types.Int(Ink),
                            For_C(Superbright), For_C(Underline),
                            For_C(Inverse)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Attributes;

  ------------------------------------------------------------------
  procedure X_Set_Xor_Mode(Line_Id     : in Line;
                           Xor_Mode    : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Xor_Mode(Line_For_C_Id, For_C(Xor_Mode)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Xor_Mode;

  ------------------------------------------------------------------
  procedure X_Put_Char(Line_Id : in Line; Car : in Character;
                       Row, Column : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char (Line_For_C_Id,
                       C_Types.Int(Character'Pos(Car)),
                       C_Types.Int(Row), C_Types.Int(Column)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Put_Char;

  ------------------------------------------------------------------
  procedure X_Put_Char(Line_Id : in Line; Car : in Byte;
                       Row, Column : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char (Line_For_C_Id, C_Types.Int(Car),
                       C_Types.Int(Row), C_Types.Int(Column)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Put_Char;

  ------------------------------------------------------------------
  procedure X_Overwrite_Char(Line_Id : in Line; Car : in Byte;
                             Row, Column : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Overwrite_Char (Line_For_C_Id, C_Types.Int(Car),
                             C_Types.Int(Row), C_Types.Int(Column)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Overwrite_Char;

  ------------------------------------------------------------------
  procedure X_Put_String(Line_Id     : in Line;
                         Str         : in String;
                         Row, Column : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_String (Line_For_C_Id,
                         Str (Str'First)'Address, Str'Length,
                         C_Types.Int(Row), C_Types.Int(Column)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Put_String;

  ------------------------------------------------------------------
  procedure X_Put_Char_Attributes(Line_Id     : in Line;
                                  Car         : in Character;
                                  Row, Column : in Natural;
                                  Paper, Ink  : in Color;
                                  Superbright : in Boolean := False;
                                  Underline   : in Boolean := False;
                                  Inverse     : in Boolean := False) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char_Attributes (
                              Line_For_C_Id,
                              C_Types.Int(Character'Pos(Car)),
                              C_Types.Int(Row),
                              C_Types.Int(Column),
                              C_Types.Int(Paper),
                              C_Types.Int(Ink),
                              For_C(Superbright),
                              For_C(Underline),
                              For_C(Inverse)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Put_Char_Attributes;

  ------------------------------------------------------------------
  procedure  X_Draw_Area(Line_Id : in Line;
                         Width, Height : in Positive;
                         Row, Column : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Area (Line_For_C_Id,
                        C_Types.Int(Width), C_Types.Int(Height),
                        C_Types.Int(Row), C_Types.Int(Column)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Draw_Area;


  ------------------------------------------------------------------
  procedure X_Put_Char_Pixels(Line_Id : in Line; Car : in Byte;
                              X, Y    : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
       Res := X_Put_Char_Pixels (Line_For_C_Id, C_Types.Int(Car), X, Y) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Put_Char_Pixels;

  ------------------------------------------------------------------
  procedure X_Get_Graphic_Characteristics(Line_Id       : in Line;
                                          Window_Width  : out Natural;
                                          Window_Height : out Natural;
                                          Font_Width    : out Natural;
                                          Font_Height   : out Natural;
                                          Font_Offset   : out Natural) is
    Line_For_C_Id : Line_For_C;
    Window_Width_For_C  : C_Types.Int;
    Window_Height_For_C : C_Types.Int;
    Font_Width_For_C    : C_Types.Int;
    Font_Height_For_C   : C_Types.Int;
    Font_Offset_For_C   : C_Types.Int;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Get_Graphic_Characteristics(Line_For_C_Id,
        Window_Width_For_C'Address, Window_Height_For_C'Address,
        Font_Width_For_C'Address, Font_Height_For_C'Address,
        Font_Offset_For_C'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    Window_Width  := Natural(Window_Width_For_C);
    Window_Height := Natural(Window_Height_For_C);
    Font_Width    := Natural(Font_Width_For_C);
    Font_Height   := Natural(Font_Height_For_C);
    Font_Offset   := Natural(Font_Offset_For_C);
  end X_Get_Graphic_Characteristics;

  ------------------------------------------------------------------
  procedure X_Draw_Point(Line_Id       : in Line;
                         X, Y          : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Point(Line_For_C_Id, X, Y) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Draw_Point;

  ------------------------------------------------------------------
  procedure X_Draw_Line(Line_Id       : in Line;
                        X1, Y1, X2, Y2 : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Line(Line_For_C_Id, X1, Y1, X2, Y2) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Draw_Line;

  ------------------------------------------------------------------
  procedure X_Draw_Rectangle(Line_Id       : in Line;
                             X1, Y1, X2, Y2 : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Rectangle(Line_For_C_Id, X1, Y1, X2, Y2) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Draw_Rectangle;

  ------------------------------------------------------------------
  procedure X_Fill_Rectangle(Line_Id       : in Line;
                             X1, Y1, X2, Y2 : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Fill_Rectangle(Line_For_C_Id, X1, Y1, X2, Y2) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Fill_Rectangle;

  ------------------------------------------------------------------
  procedure X_Draw_Points(Line_Id       : in Line;
                          X, Y          : in Natural;
                          Width, Height : in Natural;
                          Points        : in Byte_Array) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    if Points'Length /= Width * Height then
       raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Points(Line_For_C_Id, X, Y, Width, Height,
              Points(Points'First)'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Draw_Points;

  ------------------------------------------------------------------
  procedure X_Fill_Area(Line_Id : in Line;
                        Xys     : in Natural_Array) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    if Xys'Length rem 2 /= 0 or else Xys'Length < 6 then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Fill_Area(Line_For_C_Id, Xys'Address, Xys'Length/2) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Fill_Area;

  ------------------------------------------------------------------
  procedure X_Get_Current_Pointer_Position(Line_Id : in Line;
                                           X, Y    : out Integer) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
    X_For_C, Y_For_C : C_Types.Int;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Get_Current_Pointer_Position (Line_For_C_Id,
                   X_For_C'Address, Y_For_C'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    X := Integer (X_For_C);
    Y := Integer (Y_For_C);
  end X_Get_Current_Pointer_Position;

  ------------------------------------------------------------------
  procedure X_Set_Graphic_Pointer(Line_Id : in Line;
                                  Graphic : in Boolean;
                                  Grab : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Graphic_Pointer(Line_For_C_Id,
                            For_C(Graphic), For_C(Grab)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Graphic_Pointer;

  ------------------------------------------------------------------
  procedure X_Hide_Graphic_Pointer(Line_Id : in Line;
                                   Grab : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Hide_Graphic_Pointer(Line_For_C_Id, For_C(Grab)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Hide_Graphic_Pointer;

  ------------------------------------------------------------------
  function Translate_Events (Mng_Event : Event_Mng.Out_Event_List)
           return Event_Kind is
  begin
    case Mng_Event is
      when Event_Mng.Timer_Event  => return Timer_Event;
      when Event_Mng.Fd_Event     => return Fd_Event;
      when Event_Mng.Signal_Event => return Signal_Event;
      when Event_Mng.Timeout      => return Timeout;
    end case;
  end Translate_Events;
  procedure X_Wait_Event(Line_Id : in Line;
                         Timeout : in out Timers.Delay_Rec;
                         Kind : out Event_Kind) is
    Internal_Event : Event_Rec;
    Final_Exp : Timers.Expiration_Rec;
    use type Ada.Calendar.Time, Timers.Delay_List, Perpet.Delta_Rec,
             Event_Mng.Out_Event_List, Virtual_Time.Clock_Access;
  begin
    Check (Line_Id);
    if Timeout.Clock /= null then
      -- Virtual time is not supported
      raise Invalid_Timeout;
    end if;

    -- Compute final expiration
    Final_Exp :=
        (case Timeout.Delay_Kind is
           when Timers.Delay_Exp => (Infinite => False,
                                     Time => Timeout.Expiration_Time),
           when Timers.Delay_Sec =>
             (if Timeout.Delay_Seconds /= Infinite_Timeout then
                (Infinite => False,
                 Time => Ada.Calendar.Clock + Timeout.Delay_Seconds)
              else
                (Infinite => True)),
            when Timers.Delay_Del => (Infinite => False,
                                      Time => Ada.Calendar.Clock
                                            + Timeout.Delay_Delta));

    -- Loop while wake-up events
    loop
      -- Ready to wait
      Dispatcher.Prepare(Line_Id.No, Final_Exp);

      -- Get an event
      if Debug then
        Basic_Proc.Put_Line_Output ("X_Wait_Event: " & Line_Id.No'Img & " waiting");
      end if;
      Dispatcher.Wait_Event(Line_Id.No) (Internal_Event);
      if Debug then
        Basic_Proc.Put_Line_Output ("X_Wait_Event: " & Line_Id.No'Img & " released");
      end if;
      -- An event to report?
      if not Internal_Event.Internal then
        Kind := Internal_Event.Kind;
        exit;
      end if;

      -- Dispatch non X events if any, and report if needed
      Kind := Translate_Events (Event_Mng.Wait (0));
      exit when Kind /= X_Mng.Timeout;
    end loop;

    if Debug then
      Basic_Proc.Put_Line_Output ("X_Wait_Event: " & Line_Id.No'Img & " got " & Kind'Img);
    end if;

    -- Compute remaining time
    case Timeout.Delay_Kind is
      when Timers.Delay_Exp =>
        null;
      when Timers.Delay_Sec =>
        if Timeout.Delay_Seconds /= Infinite_Timeout then
          Timeout.Delay_Seconds := Ada.Calendar.Clock - Final_Exp.Time;
          if Timeout.Delay_Seconds < 0.0 then
            Timeout.Delay_Seconds := 0.0;
          end if;
        end if;
      when Timers.Delay_Del =>
        if Ada.Calendar.Clock > Final_Exp.Time then
          Timeout.Delay_Delta := Ada.Calendar.Clock - Final_Exp.Time;
        else
          Timeout.Delay_Delta := (0, 0.0);
        end if;
    end case;

  end X_Wait_Event;

  ------------------------------------------------------------------
  procedure X_Read_Tid(Line_Id : in Line;
                       Row_Col : in Boolean;
                       Button : out Button_List;
                       Row, Column : out Integer) is
    Loc_Button : C_Types.Int;
    Row_For_C, Col_For_C : C_Types.Int;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Read_Tid (Line_For_C_Id, For_C(Row_Col),
                       Loc_Button'Address,
                       Row_For_C'Address,
                       Col_For_C'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    -- check returned coordinates
    Button := (case Loc_Button is
                 when 1      => Left,
                 when 2      => Middle,
                 when 3      => Right,
                 when 4      => Up,
                 when 5      => Down,
                 when 6      => Shift_Up,
                 when 7      => Shift_Down,
                 when 8      => Ctrl_Up,
                 when 9      => Ctrl_Down,
                 when others => None);
    Row := Row_For_C;
    Column := Col_For_C;
  end X_Read_Tid;

  ------------------------------------------------------------------
  procedure X_Read_Key(Line_Id : in Line;
                       Control : out Boolean;
                       Shift : out Boolean;
                       Code : out Boolean;
                       Key : out Kbd_Tab_Code) is
    Control4C, Shift4C, Code4C : Bool_For_C;
    Loc_Nbre : C_Types.Int;
    Loc_Tab : array (Natural range 1..Kbd_Max_Code) of C_Types.Int;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Read_Key (Line_For_C_Id, Control4C'Address, Shift4C'Address,
             Code4C'Address, Loc_Tab'Address, Loc_Nbre'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    -- Fill booleans and table
    Control := Boolean (Control4C);
    Shift := Boolean (Shift4C);
    Code := Boolean (Code4C);
    Key.Nbre := Natural (Loc_Nbre);
    for I in Kbd_Index_Code range Kbd_Index_Code'First..Natural(Loc_Nbre) loop
        Key.Tab(I) := Byte(Loc_Tab(I));
    end loop;
  end X_Read_Key;

  ------------------------------------------------------------------
  procedure X_Enable_Motion_Events (Line_Id : in Line; Motion_Enable : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Enable_Motion_Events (Line_For_C_Id, For_C(Motion_Enable)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Enable_Motion_Events;


  ------------------------------------------------------------------
  procedure X_Set_Selection (Line_Id : in Line; Selection : in String) is
    Line_For_C_Id : Line_For_C;
    Selection_For_C : constant String(1 .. Selection'Length+1)
                    := Selection & Ada.Characters.Latin_1.Nul;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Selection (Line_For_C_Id, Selection_For_C'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Selection;

  ------------------------------------------------------------------
  procedure X_Reset_Selection (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Selection (Line_For_C_Id, System.Null_Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Reset_Selection;

  ------------------------------------------------------------------
  procedure X_Request_Selection (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Request_Selection (Line_For_C_Id) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Request_Selection;

  ------------------------------------------------------------------
  function X_Get_Selection (Line_Id : Line;
                            Max_Len : Natural) return String is
    Line_For_C_Id : Line_For_C;
    Buffer  : String (1 .. Max_Len + 1);
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Get_Selection (Line_For_C_Id, Buffer'Address, Buffer'Length) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    for I in Buffer'Range loop
      if Buffer(I) = Ada.Characters.Latin_1.Nul then
        return Buffer (1 .. I - 1);
      end if;
    end loop;
    return Buffer;
  end X_Get_Selection;


  ------------------------------------------------------------------
  procedure X_Bell (Line_Id : in Line; Repeat : in Bell_Repeat) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    Check (Line_Id);
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Bell (Integer(Repeat)) = Ok;
    Res := Res and then X_Flush = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Bell;

  ------------------------------------------------------------------
  package body Dispatch is separate;

end X_Mng;

