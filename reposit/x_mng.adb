with Calendar, System;
with Dynamic_List, My_Io, Timers;
package body X_Mng is

  -- Duration
  Infinite_Timeout : constant Duration := Timers.Infinite_Seconds;

  Debug_Var_Name : constant String := "X_MNG_DEBUG";
  Debug : Boolean := False;

  -- Result of a call to C
  subtype Result is Integer;
  Ok : constant Result := 0;

  -- Line access for X
  subtype Line_For_C  is System.Address;
  No_Line_For_C : constant Line_For_C := System.Null_Address;

  -- True if the connection to X has been initialised
  Initialised : Boolean := False; 
 
  -- Boolean on 32 bits for C
  type Bool_For_C is new Boolean;
  for Bool_For_C'Size use 32;
  for Bool_For_C use (False => 0, True => 1);

  function For_C(Ada_Boolean : in Boolean) return Bool_For_C is
  begin
    return Bool_For_C'Val(Boolean'Pos(Ada_Boolean));
  end For_C;

  function For_Ada(C_Boolean : in Bool_For_C) return Boolean is
  begin
    return Boolean'Val(Bool_For_C'Pos(C_Boolean));
  end For_Ada;

  -- Callback list
  type Cb_Rec is record
    Fd : File_Desc;
    Read : Boolean;
    Cb : Fd_Callback;
  end record;
  package Cb_Mng is new Dynamic_List(Cb_Rec);
  Cb_List : Cb_Mng.List_Type;

  -- Same FD
  function Same_Fd (Cb1, Cb2 : Cb_Rec) return Boolean is
    use type Sys_Calls.File_Desc;
  begin
    return Cb1.Read = Cb2.Read and then Cb1.Fd = Cb2.Fd;
  end Same_Fd;
  procedure Cb_Search is new Cb_Mng.Search(Same_Fd);

  -- Signal callback
  Cb_Sig : Signal_Callback := null;

  ------------------------------------------------------------------
  -------------------- T H E   I N T E R F A C E -------------------
  ------------------------------------------------------------------
  -- Initialise X for one host
  -- int x_initialise (char *server_name);
  ------------------------------------------------------------------
  function X_Initialise (Server_Name : System.Address) return Result;
  pragma Import(C, X_Initialise, "x_initialise");

  ------------------------------------------------------------------
  -- Opens a line
  -- int x_open_line (int screen_id, int row, int column,
  --                  int height, int width,
  --                  int background, int border, int no_font,
  --                  void **p_line_id);
  ------------------------------------------------------------------
  function X_Open_Line (Screen_Id          : Integer;
                        Row, Column        : Integer;
                        Height, Width      : Integer;
                        Background, Border : Integer;
                        No_Font            : Integer;
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
  -- int x_set_line_name (void *line_id, char *line_name);
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
  --                       boolean blink, boolean reverse);
  ------------------------------------------------------------------
  function X_Set_Attributes(Line_Id     : Line_For_C;
                            Paper, Ink  : Integer;
                            Superbright : Bool_For_C;
                            Underline   : Bool_For_C;
                            Blink       : Bool_For_C;
                            Inverse     : Bool_For_C) return Result;
  pragma Import(C, X_Set_Attributes, "x_set_attributes");
 
  ------------------------------------------------------------------
  -- Set XOR mode for further outputs
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
                      Car         : Integer;
                      Row, Column : in Integer) return Result;
  pragma Import(C, X_Put_Char, "x_put_char");

  ------------------------------------------------------------------
  -- Writes a char whith the attributes previously set
  -- Does not erase character at current position
  -- int x_overwrite_char (void *line_id, int  car,
  --                       int row, int column);
  ------------------------------------------------------------------
  function X_Overwrite_Char(Line_Id     : Line_For_C;
                            Car         : Integer;
                            Row, Column : in Integer) return Result;
  pragma Import(C, X_Overwrite_Char, "x_overwrite_char");

  ------------------------------------------------------------------
  -- Writes a string at location with the attributes previously set
  -- int x_put_stringt (void *line_id, char *p_char, int number,
  --                    int row, int column);
  ------------------------------------------------------------------
  function X_Put_String(Line_Id     : Line_For_C;
                        Str_Addr    : System.Address;
                        Length      : Integer;
                        Row, Column : Integer) return Result;
  pragma Import(C, X_Put_String, "x_put_string");

  ------------------------------------------------------------------
  -- Writes a char on a line with specified characteristics
  -- int x_put_char_attributes (void *line_id; int car,
  --                            int row, int column, int paper, int ink,
  --                            boolean superbright, boolean underline,
  --                            boolean blink, boolean reverse);
  ------------------------------------------------------------------
  function X_Put_Char_Attributes(Line_Id     : Line_For_C;
                                 Car         : Integer;
                                 Row, Column : Integer;
                                 Paper, Ink  : Integer;
                                 Superbright : Bool_For_C;
                                 Underline   : Bool_For_C;
                                 Blink       : Bool_For_C;
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
                       Width, Height : Integer;
                       Row, Column   : Integer) return Result;
  pragma Import(C, X_Draw_Area, "x_draw_area");
 
  ------------------------------------------------------------------
  -- Puts a char with current characteristics
  --  at specified position in pixels
  -- int x_put_char_pixels (void *line_id, int car, int x, int y);
  ------------------------------------------------------------------
  function X_Put_Char_Pixels(Line_Id : Line_For_C;
                             Car     : Integer;
                             X, Y    : Integer) return Result;
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
                        X, Y    : Integer) return Result;
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
                         Width, Height : in Natural; 
                         Points : System.Address) return Result;
  pragma Import(C, X_Draw_Points, "x_draw_points");

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
                                 Graphic : Bool_For_C) return Result;
  pragma Import(C, X_Set_Graphic_Pointer, "x_set_graphic_pointer");

  ------------------------------------------------------------------
  -- Add a fd for select
  -- int x_add_fd (int fd, boolean read);
  ------------------------------------------------------------------
  function X_Add_Fd (Fd : Integer; Read : Bool_For_C) return Result;
  pragma Import(C, X_Add_Fd, "x_add_fd");

  ------------------------------------------------------------------
  -- Del a fd from select
  -- int x_del_fd (int fd, boolean read);
  ------------------------------------------------------------------
  function X_Del_Fd (Fd : Integer; Read : Bool_For_C) return Result;
  pragma Import(C, X_Del_Fd, "x_del_fd");

  ------------------------------------------------------------------
  -- Is a fd set for select
  -- boolean x_fd_set (int fd, boolean read);
  ------------------------------------------------------------------
  function X_Fd_Set (Fd : Integer; Read : Bool_For_C) return Bool_For_C;
  pragma Import(C, X_Fd_Set, "x_fd_set");

  ------------------------------------------------------------------
  -- Wake-up the select
  ------------------------------------------------------------------
  procedure C_X_Wake_Up;
  pragma Import(C, C_X_Wake_Up, "x_wake_up");

  ------------------------------------------------------------------
  -- Wait for some events
  -- int x_select (int *p_fd, int *timeout_ms);
  ------------------------------------------------------------------
  C_Select_Sig_Event : constant Integer := -3;
  C_Select_No_Event  : constant Integer := -2;
  C_Select_X_Event   : constant Integer := -1;
  function X_Select (P_Fd : System.Address;
                     P_Read : System.Address;
                     Timeout_Ms : System.Address) return Result;
  pragma Import(C, X_Select, "x_select");

  ------------------------------------------------------------------
  -- Process a X event (TID or Keyboard or other) 
  -- int x_process_event (void **p_line_id, int *p_kind, boolean *p_next);
  ------------------------------------------------------------------
  function X_Process_Event(P_Line_Id : System.Address;
                           P_Keyb    : System.Address;
                           P_Next    : System.Address) return Result;
  pragma Import(C, X_Process_Event, "x_process_event");
 
  ------------------------------------------------------------------
  -- Reads the position on TID
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
  function X_Read_Key(Line_Id : Line_For_C;
                      P_Keys  : System.Address;
                      P_Nbre  : System.Address) return Result;
  pragma Import(C, X_Read_Key, "x_read_key");

  ------------------------------------------------------------------
  -- Enable / disable cursor motion events
  -- extern int x_enable_motion_events (void *line_id, boolean enable_motion);
  ------------------------------------------------------------------
  function X_Enable_Motion_Events (Line_Id : Line_For_C;
                                   Motion_Enable : Bool_For_C) return Result;
  pragma Import(C, X_Enable_Motion_Events, "x_enable_motion_events");
 
  ------------------------------------------------------------------
  -- Assumes blinking of X
  -- int x_blink(void)
  ------------------------------------------------------------------
  function X_Blink return Result;
  pragma Import(C, X_Blink, "x_blink");

  ------------------------------------------------------------------
  -- Stops the blinking task
  -- int x_stop_blinking(void) 
  ------------------------------------------------------------------
  function X_Stop_Blinking return Result;
  pragma Import(C, X_Stop_Blinking, "x_stop_blinking");

  ------------------------------------------------------------------
  -- Start the blinking task
  -- int x_start_blinking(void) 
  ------------------------------------------------------------------
  function X_Start_Blinking return Result;
  pragma Import(C, X_Start_Blinking, "x_start_blinking");

  ------------------------------------------------------------------
  -- Rings a bell several times
  -- int x_bell (int nbre_bell;
  ------------------------------------------------------------------
  function X_Bell (Repeat : Integer) return Result;
  pragma Import(C, X_Bell, "x_bell");


  ------------------------------------------------------------------
  --------------- T H E   D I S P A T C H E R   S P E C ------------
  ------------------------------------------------------------------
  -- Dispatcher of X calls and X events
  task Dispatcher is
    -- Start it
    entry Start;

    -- Register / Unregister
    entry Register (Client : out Line_Range);
    entry Unregister (Client : in out Line_Range);

    -- Two calls to encapsulate a call to X
    entry Call_On  (Client : in Client_Range;
                    Line_For_C_Id : out Line_For_C);
    entry Call_Off (Client : in Client_Range;
                    New_Line_For_C_Id : in Line_For_C);

    -- Ready to wait
    entry Wait (Client : in Client_Range; Timeout : in Duration);
    -- Relay wait
    entry Some_Event(Client_Range) (Some : out Boolean);
    entry Get_Event (Client : in Client_Range; Kind : out Event_Kind;
                                               Some : out Boolean);
  end Dispatcher;


  procedure Set_Debug is
    Set : Boolean;
    Tru : Boolean;
    Val : String (1 .. 1);
    Len : Natural;
  begin
    Sys_Calls.Getenv (Debug_Var_Name, Set, Tru, Val, Len);
    if Set and then (Val(1) = 'y' or else Val(1) = 'Y') then
      Debug := True;
    end if;
  exception
    when others =>
      null;
  end Set_Debug;

  ------------------------------------------------------------------
  ------------------------ T H E   C A L L S -----------------------
  ------------------------------------------------------------------

  procedure X_Initialise (Server_Name : in String) is

    Serv_Name_For_C : constant String(1 .. Server_Name'Length+1)
                    := Server_Name & Ascii.Nul;
  begin
    if not Initialised then
      if X_Initialise (Serv_Name_For_C(Serv_Name_For_C'First)'Address)
                      /= Ok then
        raise X_Failure;
      end if;

      Set_Debug;
      Dispatcher.Start;
      Initialised := True;
    end if;
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
    Dispatcher.Register(Line_Id.No);
    if Line_Id = No_Client then
      -- Too many clients
      raise X_Failure;
    end if;
    -- open window
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Open_Line (Line_Definition.Screen_Id,
                        Line_Definition.Row,
                        Line_Definition.Column, 
                        Line_Definition.Height,
                        Line_Definition.Width, 
                        Line_Definition.Background,
                        Line_Definition.Border, 
                        Line_Definition.No_Font,
                        Line_For_C_Id'Address) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
  procedure X_Set_Line_Name (Line_Id : in Line;
                             Line_Name : in String) is
    Line_Name_For_C : constant String(1 .. Line_Name'Length+1)
                    := Line_Name & Ascii.Nul;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
                             Blink       : in Boolean := False;
                             Inverse     : in Boolean:= False) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Attributes(Line_For_C_Id, 
                            Integer(Paper), Integer(Ink), 
                            For_C(Superbright), For_C(Underline),
                            For_C(Blink), For_C(Inverse)) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char (Line_For_C_Id,
                       Integer(Character'Pos(Car)),
                       Integer(Row), Integer(Column)) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char (Line_For_C_Id, Integer(Car),
                       Integer(Row), Integer(Column)) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Overwrite_Char (Line_For_C_Id, Integer(Car),
                             Integer(Row), Integer(Column)) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_String (Line_For_C_Id,
                         Str (Str'First)'Address, Str'Length,
                         Integer(Row), Integer(Column)) = Ok;
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
                                  Blink       : in Boolean := False;
                                  Inverse     : in Boolean := False) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Put_Char_Attributes (
                              Line_For_C_Id, 
                              Integer(Character'Pos(Car)),
                              Integer(Row), 
                              Integer(Column),
                              Integer(Paper),
                              Integer(Ink), 
                              For_C(Superbright),
                              For_C(Underline),
                              For_C(Blink),
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Draw_Area (Line_For_C_Id,
                        Integer(Width), Integer(Height),
                        Integer(Row), Integer(Column)) = Ok;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
       Res := X_Put_Char_Pixels (Line_For_C_Id, Integer(Car), X, Y) = Ok;
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
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Get_Graphic_Characteristics(Line_For_C_Id,
        Window_Width'Address, Window_Height'Address,
        Font_Width'Address, Font_Height'Address, Font_Offset'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Get_Graphic_Characteristics;

  ------------------------------------------------------------------
  procedure X_Draw_Point(Line_Id       : in Line;
                         X, Y          : in Natural) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
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
  procedure X_Get_Current_Pointer_Position(Line_Id : in Line;
                                           X, Y    : out Integer) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Get_Current_Pointer_Position (Line_For_C_Id,
                   X'Address, Y'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Get_Current_Pointer_Position;

  ------------------------------------------------------------------
  procedure X_Set_Graphic_Pointer(Line_Id : in Line;
                                  Graphic : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Set_Graphic_Pointer(Line_For_C_Id,
                            For_C(Graphic)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Set_Graphic_Pointer;

  ------------------------------------------------------------------
  procedure X_Add_Callback (Fd : in File_Desc; Read : in Boolean;
                            Callback : in Fd_Callback) is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Check no cb for this fd yet
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    begin
      Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev, From_Current => False);
      raise X_Failure;
    exception
      when Cb_Mng.Not_In_List =>
        null;
    end;
    -- Append
    if not Cb_Mng.Is_Empty (Cb_List) then
      Cb_Mng.Move_To (Cb_List, Cb_Mng.Prev, 0, False);
    end if;
    Cb_Mng.Insert (Cb_List, (Fd, Read, Callback));
    -- Add fd to select
    Res := X_Add_Fd (Integer(Fd), Bool_For_C(Read)) = Ok;
    if not Res then
      raise X_Failure;
    end if;
  exception
    when others =>
      raise X_Failure;
  end X_Add_Callback;

  ------------------------------------------------------------------
  procedure X_Del_Callback (Fd : in File_Desc; Read : in Boolean) is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Del fd from select
    Res := X_Del_Fd (Integer(Fd), Bool_For_C(Read)) = Ok;
    -- del from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev, From_Current => False);
    if Cb_Mng.Get_Position (Cb_List) /=  Cb_Mng.List_Length(Cb_List) then
      Cb_Mng.Delete (Cb_List, Cb_Mng.Next);
    else
      Cb_Mng.Delete (Cb_List, Cb_Mng.Prev);
    end if;
    if not Res then
      raise X_Failure;
    end if;
  exception
    when others =>
      raise X_Failure;
  end X_Del_Callback;
  
  ------------------------------------------------------------------
  function X_Callback_Set (Fd : in File_Desc; Read : in Boolean)
  return Boolean is
    Res : Bool_For_C;
  begin
    Res := X_Fd_Set (Integer(Fd), Bool_For_C(Read));
    return Boolean(Res);
  end X_Callback_Set;

  ------------------------------------------------------------------
  -- Register a callback on terminations signals
  -- Call it with null to disable
  procedure X_Set_Signal (Callback : in Signal_Callback) is
  begin
    Cb_Sig := Callback;
  end X_Set_Signal;

  ------------------------------------------------------------------
  -- Is a callback set on signals
  function X_Signal_Set return Boolean is
  begin
    return Cb_Sig /= null;
  end X_Signal_Set;

  ------------------------------------------------------------------
  procedure X_Select (Line_Id : in Line; Timeout_Ms : in out Integer;
                      X_Event : out Boolean) is
    Exp : Calendar.Time;
    use Calendar;
    Timeout : Duration;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    -- Compute expiration and set timeout in duration
    Exp := Calendar.Clock;
    if Timeout_Ms > 0 then
      Timeout := Duration (Timeout_Ms) / 1_000.0;
      Exp := Exp + Timeout;
    else
      Timeout := Infinite_Timeout;
    end if;
    -- Ready to wait
    Dispatcher.Wait(Line_Id.No, Timeout);
    -- Here we wait
    Dispatcher.Some_Event(Line_Id.No)(X_Event);
    -- Compute remaining time
    if Timeout_Ms > 0 then
      Timeout_Ms := Integer ( (Exp - Calendar.Clock) * 1_000.0);
      if Timeout_Ms < 0 then
        Timeout_Ms := 0;
      end if;
    end if;
  end X_Select;

  ------------------------------------------------------------------
  procedure X_Process_Event(Line_Id : in Line; 
                            Kind    : out Event_Kind;
                            Next    : out Boolean) is
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Get_Event(Line_Id.No, Kind, Next);
  end X_Process_Event;

  ------------------------------------------------------------------
  procedure X_Read_Tid(Line_Id : in Line;
                       Row_Col : in Boolean;
                       Button : out Button_List;
                       Row, Column : out Integer) is
    Loc_Button : Integer;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Read_Tid (Line_For_C_Id, For_C(Row_Col),
                       Loc_Button'Address,
                       Row'Address, 
                       Column'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    -- check returned coordinates
    if Loc_Button = 0 then
      Button := None;
    elsif Loc_Button = 1 then
      Button := Left;
    elsif Loc_Button = 2 then
      Button := Middle;
    elsif Loc_Button = 3 then
      Button := Right;
    end if;
  end X_Read_Tid;

  ------------------------------------------------------------------
  procedure X_Read_Key(Line_Id : in Line; Key : out Kbd_Tab_Code) is
      Loc_Tab : array (Natural range 1..Kbd_Max_Code) of Integer;
      Loc_Nbre : Integer;
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Read_Key (Line_For_C_Id, Loc_Tab'Address, Loc_Nbre'Address) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
    -- Fill table
    for I in Kbd_Index_Code range Kbd_Index_Code'First..Natural(Loc_Nbre) loop
        Key.Tab(I) := Byte(Loc_Tab(I));
    end loop;
    Key.Nbre := Natural (Loc_Nbre);
  end X_Read_Key;

  ------------------------------------------------------------------
  procedure X_Enable_Motion_Events (Line_Id : in Line; Motion_Enable : in Boolean) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Enable_Motion_Events (Line_For_C_Id, For_C(Motion_Enable)) = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Enable_Motion_Events;


  ------------------------------------------------------------------
  procedure X_Blink_Alternate (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Blink = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
  end X_Blink_Alternate;

  ------------------------------------------------------------------
  procedure X_Stop_Blinking_Task (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Stop_Blinking = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
  end X_Stop_Blinking_Task;  

  ------------------------------------------------------------------
  procedure X_Start_Blinking_Task (Line_Id : in Line) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Start_Blinking = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
  end X_Start_Blinking_Task;  

  ------------------------------------------------------------------
  procedure X_Bell (Line_Id : in Line; Repeat : in Bell_Repeat) is
    Line_For_C_Id : Line_For_C;
    Res : Boolean;
  begin
    if not Initialised or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    Dispatcher.Call_On (Line_Id.No, Line_For_C_Id);
    Res := X_Bell (Integer(Repeat)) = Ok;
    Res := Res and then X_Flush = Ok;
    Dispatcher.Call_Off(Line_Id.No, Line_For_C_Id);
    if not Res then
      raise X_Failure;
    end if;
  end X_Bell;


  ------------------------------------------------------------------
  --------------- T H E   D I S P A T C H E R   B O D Y ------------
  ------------------------------------------------------------------

  type Xx_Select_Result_List is (Select_X_Event, Select_Fd, Select_Timer,
                                 Select_Signal, Select_Timeout);

  function Xx_Select (Timeout_In : Duration) return Xx_Select_Result_List is
    Fd    : Integer;
    Read  : Bool_For_C;
    Timeout_Dur, Timeout_Tim : Duration;
    Final_Exp : Calendar.Time;
    Timeout_Ms : Integer;
    Dummy : Result;
    Cb_Searched : Cb_Rec;
    use Calendar;
  begin
    -- Compute final expiration
    if Timeout_In /= Infinite_Timeout then
      Final_Exp := Calendar.Clock + Timeout_In;
    end if;

    loop
      -- Final timeout from the one specified
      if Timeout_In /= Infinite_Timeout then
        Timeout_Dur := Final_Exp - Calendar.Clock;
        -- Reached?
        if Timeout_Dur < 0.0 then
            Timeout_Dur := 0.0;
        end if;
      else
        Timeout_Dur := 0.0;
      end if;

      -- Netx timer timeout
      Timeout_Tim := Timers.Wait_For;

      -- Compute smaller timeout between timers and the one requested
      if Timeout_Tim = Infinite_Timeout then
        -- No timer
        if Timeout_In = Infinite_Timeout then
          -- No timer and infinite timeout: infinite
          Timeout_Dur := Infinite_Timeout;
        else
          -- No timer and timeout set: keep timeout
          null;
        end if;
      else
        -- Some timer
        if Timeout_In = Infinite_Timeout then
          -- Some timer and infinite timeout: take timer
          Timeout_Dur := Timeout_Tim;
        else
          -- Some timer and a timeout set: take smallest
          if Timeout_Tim <= Timeout_Dur then
            -- Timer is smallest
            Timeout_Dur := Timeout_Tim;
          else
            -- Keep timeout
            null;
          end if;
        end if;
      end if;

      -- The real select
      Timeout_Ms := Integer (Timeout_Dur * 1000.0);
      Dummy := X_Select (Fd'Address, Read'Address, Timeout_Ms'Address);
      if Debug then
        if Dummy /= Ok then
          My_Io.Put_Line ("  XX_SELECT -> ERROR");
          return Select_Timeout;
        else
          My_Io.Put_Line ("  XX_SELECT -> " & Integer'Image(Fd)
                                      & " " & Bool_For_C'Image(Read));
        end if;
      end if;

      -- Results
      if Fd = C_Select_X_Event then
        -- A X event
        return Select_X_Event;
      elsif Fd = C_Select_No_Event then
        -- Nothing. Expire timers or return timeout
        if Timers.Expire then
          return Select_Timer;
        end if;
        if Timeout_In /= Infinite_Timeout
        and then Calendar.Clock > Final_Exp then
          -- Requested timeout reached
          return Select_Timeout;
        end if;
      elsif Fd = C_Select_Sig_Event then
        if Cb_Sig /= null then
          Cb_Sig.all;
          return Select_Signal;
        end if;
      else
        -- A FD event
        Cb_Searched.Fd := File_Desc(Fd);
        Cb_Searched.Read := Boolean(Read);
        Cb_Searched.Cb := null;
        begin
          -- Search and read callback
          Cb_Search (Cb_List, Cb_Searched, From_Current => False);
          Cb_Mng.Read (Cb_List, Cb_Searched,  Cb_Mng.Current);
          -- Call it and propagate event if callback returns true
          if Cb_Searched.Cb /= null then
            if Cb_Searched.Cb (Cb_Searched.Fd, Cb_Searched.Read) then
              return Select_Fd;
            end if;
          end if;
        exception
          when Cb_Mng.Not_In_List =>
          if Debug then
            My_Io.Put_Line ("**** XX_SELECT: " & Integer'Image(Fd) 
                          & " fd not found ****");
          end if;
        end;
        -- No callback or returned False
      end if;
    end loop;
  end Xx_Select;

  procedure Xx_Process_Event (Line_For_C_Id : out Line_For_C;
                              Kind : out Event_Kind;
                              Next : out Boolean) is
    Dummy : Result;
    Next_For_C : Bool_For_C;
  begin
    Dummy := X_Process_Event (Line_For_C_Id'Address,
                              Kind'Address, Next_For_C'Address);
    Next := For_Ada(Next_For_C);
  end Xx_Process_Event;

  task body Dispatcher is
    -- The clients
    Nb_Clients : Line_Range;
    Nb_Wait : Line_Range;
    type Client_Desc_Rec is record
      Known : Boolean;
      -- Will be LINE_FOR_C
      Line_For_C_Id : Line_For_C;
      Wait_Inf : Boolean;
      Wait_Exp : Calendar.Time;
      -- Refreshing this client due to a unregister
      Refresh : Boolean;
    end record;
    Clients : array (Client_Range) of Client_Desc_Rec;
    -- The TIMEOUT/EVENT and CLIENT from WAIT to SOME_EVENT to GET_EVENT
    Select_Result : Xx_Select_Result_List;
    Selected_Client : Client_Range;
    -- One event to give in SOME_EVENT
    Some_Event_Present : Boolean;
    Loc_Kind : Event_Kind;
    Loc_Next : Boolean;
    -- Local X line
    Loc_Line_For_C_Id : Line_For_C;
    use System, Calendar;
    -- Delay or INFINITE_TIMEOUT
    Delay_Dur : Duration;
  
    procedure Compute_Smaller_Delay(Smaller_Delay : out Duration;
                                    Selected_Client : out Client_Range) is
      Infinite : Boolean;
      Dur, Min_Dur : Duration; 
      Current_Time : Calendar.Time := Calendar.Clock;
    begin

      Infinite := True;
      Min_Dur := Duration'Last;
      Selected_Client := Client_Range'First;
      -- Look for smallest delay. Check if all infinite.
      for I in Client_Range loop
        if Clients(I).Known then
          if not Clients(I).Wait_Inf then
            Infinite := False;
            Dur := Clients(I).Wait_Exp - Current_Time;
            if Dur < 0.0 then
              Dur := 0.0;
            end if;
            if Dur < Min_Dur then
              Min_Dur := Dur;
              Selected_Client := I;
            end if;
          end if;
        end if;
      end loop;
      if Infinite then
        Smaller_Delay := Infinite_Timeout;
      else
        Smaller_Delay := Min_Dur;
      end if;
    end Compute_Smaller_Delay;

    procedure Get_Client_From_Line(Line_For_C_Id : in Line_For_C;
                                   Client : in out Client_Range;
                                   Found : out Boolean) is
    begin
      -- Same as current?
      if Clients(Client).Known
      and then Clients(Client).Line_For_C_Id = Line_For_C_Id then
        Found := True;
        return;
      end if;
      for I in Client_Range loop
        if Clients(I).Known
        and then Clients(I).Line_For_C_Id = Line_For_C_Id then
          Found := True;
          Client := I;
          return;
        end if;
      end loop;
      Client := Client_Range'First;
      Found := False;
    end Get_Client_From_Line;

  begin
    -- Do you need me?
    select
      accept Start;
    or
      terminate;
    end select;
    -- No client known
    Nb_Clients := 0;
    Nb_Wait := 0;
    for I in Client_Range loop
      Clients(I).Known := False;
    end loop;
    -- No event
    Some_Event_Present := False;
    -- To avoid a warning: may not have a value
    Selected_Client := Client_Range'First;
    Loc_Kind := Discard;
    Loc_Next := False;

    loop
      select
        -- Accept call to X, one at a time
        accept Call_On (Client : in Client_Range;
                        Line_For_C_Id : out Line_For_C) do
          Line_For_C_Id := Clients(Client).Line_For_C_Id;
        end Call_On;
        accept Call_Off (Client : in Client_Range;
                         New_Line_For_C_Id : in Line_For_C) do
          Clients(Client).Line_For_C_Id := New_Line_For_C_Id;
        end Call_Off;
      or
        accept Register (Client : out Line_Range) do
          -- Find a slot
          for I in Client_Range loop
            if not Clients(I).Known then
              Clients(I).Known := True;
              Clients(I).Refresh := False;
              Clients(I).Line_For_C_Id := No_Line_For_C;
              Client := I;
              Nb_Clients := Nb_Clients + 1;
              if Debug then
                My_Io.Put_Line ("Register -> " & Line_Range'Image(I));
              end if;
              return;
            end if;
          end loop;
          -- Too many clients
          Client := No_Client_No;
        end Register;
      or
        accept Unregister (Client : in out Line_Range) do
          if Debug then
            My_Io.Put_Line ("Unregister " & Line_Range'Image(Client));
          end if;
          -- Update administration
          if Client /= No_Client_No then
            Clients(Client).Known := False;
            Nb_Clients := Nb_Clients - 1;
          end if;
          Client := No_Client_No;
          -- Generate a dummy refresh event for all client
          -- Wake up all waiting clients
          for I in Client_Range loop
            if Clients(I).Known then
              Clients(I).Refresh := True;
              select
                accept Some_Event(I) (Some : out Boolean) do
                  Some := True;
                  Nb_Wait := Nb_Wait - 1;
                end Some_Event;
              or
                delay 0.0;
              end select; 
            end if;
          end loop;
        end Unregister;
      or
        -- Client is ready to wait
        accept Wait (Client : in Client_Range; Timeout : in Duration) do
          Nb_Wait := Nb_Wait + 1;
          if Debug then
            My_Io.Put_Line ("Wait " & Line_Range'Image(Client)
                          & "  timeout: " & Duration'Image(Timeout));
            My_Io.Put_Line ("    Waiting nb " & Line_Range'Image(Nb_Wait));
          end if;
          -- Some pending event for this client?
          if not Some_Event_Present or else Client /= Selected_Client then
            -- This client will wait
            -- Compute expiration
            if Timeout < 0.0 then
              Clients(Client).Wait_Inf := True;
              if Debug then
                My_Io.Put_Line ("    Wait inf");
              end if;
            else
              Clients(Client).Wait_Inf := False;
              Clients(Client).Wait_Exp := Calendar.Clock + Timeout;
              if Debug then
                My_Io.Put_Line ("    Wait timeout");
              end if;
            end if;
          elsif Debug then
            My_Io.Put_Line ("    Wait client is selected");
          end if;
        end Wait;
        -- Can we freeze the whole stuff?
        --  no event and all clients waiting
        if not Some_Event_Present and then Nb_Wait = Nb_Clients then
          -- Loop until timeout or valid client for the event is found
          loop
            -- This gives the client with smallest delay
            -- If all infinite, the first known
            Compute_Smaller_Delay(Delay_Dur, Selected_Client);
            if Debug then
              My_Io.Put_Line ("        Wait select " & Duration'Image(Delay_Dur));
            end if;
            Select_Result := Xx_Select (Delay_Dur);
            case Select_Result is
              when Select_Timeout =>
                -- Timeout
                Some_Event_Present := False;
                if Debug then
                  My_Io.Put_Line ("            Wait select timeout for -> "
                                & Line_Range'Image(Selected_Client));
                end if;
                exit;
              when Select_X_Event =>
                -- An event: Get&store it and it's client
                Xx_Process_Event (Loc_Line_For_C_Id, Loc_Kind, Loc_Next);
                Get_Client_From_Line(Loc_Line_For_C_Id, Selected_Client,
                                   Some_Event_Present);
                if Debug then
                  My_Io.Put_Line ("            Wait select event for -> "
                                & Line_Range'Image(Selected_Client)
                                & " found " & Boolean'Image(Some_Event_Present));
                end if;
                exit when Some_Event_Present;
              when Select_Fd =>
                Some_Event_Present := True;
                Loc_Kind := Fd_Event;
                Loc_Next := False;
                if Debug then
                  My_Io.Put_Line ("            Wait select fd event for -> "
                                & Line_Range'Image(Selected_Client));
                end if;
                exit;
              when Select_Timer =>
                Some_Event_Present := True;
                Loc_Kind := Timer_Event;
                Loc_Next := False;
                if Debug then
                  My_Io.Put_Line ("            Wait select timer event for -> "
                                & Line_Range'Image(Selected_Client));
                end if;
                exit;
              when Select_Signal =>
                Some_Event_Present := True;
                Loc_Kind := Signal_Event;
                Loc_Next := False;
                if Debug then
                  My_Io.Put_Line ("            Wait select signal event for -> "
                                & Line_Range'Image(Selected_Client));
                end if;
                exit;
            end case;
          end loop;
        end if;
      or
        when Nb_Clients /= 0 and then Nb_Wait = Nb_Clients =>
        -- Release the client for stored event
        accept Some_Event(Selected_Client) (Some : out Boolean) do
          if Debug then
            My_Io.Put_Line ("Some_event " & Line_Range'Image(Selected_Client)
                   & " -> "
                   & "Select result:" & Xx_Select_Result_List'Image(Select_Result)
                   & ", Event present:" & Boolean'Image(Some_Event_Present));
          end if;
          Some := Select_Result /= Select_Timeout or else Some_Event_Present;
          Nb_Wait := Nb_Wait - 1;
        end Some_Event;
      or
        accept Get_Event (Client : in Client_Range; Kind : out Event_Kind;
                                                    Some : out Boolean) do
          -- Artificial refresh for this client?
          if Clients(Client).Refresh then
            Clients(Client).Refresh := False;
            Kind := Refresh;
            Some := False;
            if Debug then
              My_Io.Put_Line ("Get_event " & Line_Range'Image(Client)
                            & " -> artificial refresh");
            end if;
            return;
          end if;
          if Client /= Selected_Client then
            -- Invalid client
            Kind := Discard;
            Some := False;
            if Debug then
              My_Io.Put_Line ("Get_event " & Line_Range'Image(Client)
                            & " -> not selected");
            end if;
            return;
          end if;
          -- Event got from previous wait or get_event?
          if Some_Event_Present then
            Kind := Loc_Kind;
            Some := Loc_Next;
            Some_Event_Present := False;
            if Debug then
              My_Io.Put_Line ("Get_event " & Line_Range'Image(Client)
                            & " -> from previous wait/get_event");
            end if;
          else
            -- No stored event
            Xx_Process_Event (Loc_Line_For_C_Id, Loc_Kind, Loc_Next);
            -- New event for the same client?
            if Loc_Line_For_C_Id /= Clients(Client).Line_For_C_Id then
              -- Current client has to give up
              Some := False;
              Kind := Discard;
              Some_Event_Present := True;
              -- Find client of the event
              Get_Client_From_Line(Loc_Line_For_C_Id, Selected_Client,
                                   Some_Event_Present);
              if Debug then
                My_Io.Put_Line ("Get_event " & Line_Range'Image(Client)
                              & " -> give up to "
                              & Line_Range'Image(Selected_Client));
              end if;
            else
              -- This event is for this client. Deliver.
              Some := Loc_Next;
              Kind := Loc_Kind;
              if Debug then
                My_Io.Put_Line ("Get_event " & Line_Range'Image(Client)
                              & " -> got it");
              end if;
            end if; -- event is for client
          end if; -- SOME_EVENT_PRESENT
          if Debug then
            My_Io.Put_Line ("    Get_event -> " & Event_Kind'Image(Loc_Kind)
                          & " next: " & Boolean'Image(Loc_Next));
          end if;
        end Get_Event;
      or
        terminate;
      end select;
    end loop;
  end Dispatcher;

  ------------------------------------------------------------------
  -- Specific select without X
  function Select_No_X (Timeout_Ms : Integer) return Boolean is
    Select_Result : Xx_Select_Result_List;
  begin
    if Initialised  then
      raise X_Failure;
    end if;
    Set_Debug;
    if Timeout_Ms < 0 then
      Select_Result := Xx_Select (Timers.Infinite_Seconds);
    else
      Select_Result := Xx_Select (Duration(Timeout_Ms) / 1000.0);
    end if;
    case Select_Result is
      when Select_X_Event =>
        if Debug then
          My_Io.Put_Line ("**** SELECT_NO_X: Got a X event");
        end if;
        raise X_Failure;
      when Select_Fd | Select_Timer | Select_Signal =>
        return True;
      when Select_Timeout =>
        return False;
    end case;
  end Select_No_X;

  ------------------------------------------------------------------
  procedure X_Wake_Up is
  begin
    C_X_Wake_Up;
  end X_Wake_Up;

end X_Mng;

