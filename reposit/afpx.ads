with Text_Handler, Con_Io, Dynamic_List;

package Afpx is

  -- Descriptor, field index
  type Descriptor_Range is new Positive range 1 .. 50;
  type Absolute_Field_Range is new Natural range 0 .. 200;
  subtype Field_Range is Absolute_Field_Range
          range 1 .. Absolute_Field_Range 'Last;
  List_Field_No : constant Absolute_Field_Range := 0;

  -- The content of one row of one field (encode, decode)
  subtype Str_Txt is Text_Handler.Text (Con_Io.Col_Range_Last + 1);

  -- Width and height of a field
  subtype Height_Range is Positive range 1 .. Con_Io.Row_Range_Last + 1;
  subtype Width_Range  is Positive range 1 .. Con_Io.Col_Range_Last + 1;

  -- Set current descriptor (read from file)
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- By default, the CON_IO screen is cleared
  -- Exceptions : NO_DESCRIPTOR (DESCRIPTOR not found)
  procedure Use_Descriptor (Descriptor_No : in Descriptor_Range;
                            Clear_Screen : in Boolean := True);

  -- Clear the content of a field
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure Clear_Field (Field_No : in Field_Range);

  -- Reset the field from initial definition in file 
  --  colors and / or  content,
  -- The field becomes activated and not protected.
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure Reset_Field (Field_No : in Absolute_Field_Range;
                         Reset_Colors : in Boolean := True;
                         Reset_String : in Boolean := True);

  -- Width of a field
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  function Get_Field_Width (Field_No : Absolute_Field_Range)
                           return Width_Range;

  -- Width and height of a field
  -- Exceptions : NO_DESCRIPTOR (no DESCRIPTOR in use),
  --              INVALID_FIELD (FIELD_NO too big)
  procedure Get_Field_Size (Field_No : in Absolute_Field_Range;
                            Height : out Height_Range;
                            Width  : out Width_Range);

  -- Encode a string in a field.
  -- The ROW is filled with spaces, then with STR starting at COL
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  --              INVALID_SQUARE (not in field),
  --              STRING_TOO_LONG (due to SQUARE.COL)
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in String);
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in Str_Txt);

  -- Decode the content of a row of a field
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD, INVALID_ROW
  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Row_Range)
                        return String;
  procedure Decode_Field (Field_No : in Field_Range;
                          Row      : in Con_Io.Row_Range;
                          Str      : in out Str_Txt);


  -- Get field colors
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure Get_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : out Con_Io.Effective_Colors;
    Blink_Stat : out Con_Io.Effective_Blink_Stats;
    Background : out Con_Io.Effective_Basic_Colors;
    Selected   : out Con_Io.Effective_Basic_Colors);

  -- Set field colors
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  --              INVALID_COLOR
  --       - FOREGROUND has to be BASIC_COLORS for list, get and button fields
  --       - SELECTED has to be CURRENT for put and button fields
  --       - BLINK_STAT has to be CURRENT except for put fields
  procedure Set_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : in Con_Io.Colors       := Con_Io.Current;
    Blink_Stat : in Con_Io.Blink_Stats  := Con_Io.Current;
    Background : in Con_Io.Basic_Colors := Con_Io.Current;
    Selected   : in Con_Io.Basic_Colors := Con_Io.Current);

  -- Activate/Desactivate a field for further put_then_gets
  -- All fields are activated by default (when USE_DESCRIPTOR or RESET_FIELD)
  -- A non active field is not displayed by put_then get
  --  (when USE_DESCRIPTOR or RESET_FIELD)
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure Set_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : in Boolean);
  procedure Get_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : out Boolean);

  -- Protect/Unprotect a GET or BUTTON for further put_then_gets
  -- A protected get field is displayed like a put field (but cannot blink)
  -- A protected button field is displayed like a put (but no click/release)
  -- A protected list is displayed (but no item can be selected)
  -- All get/button/list fields are unprotected by default
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure Set_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : in Boolean);
  procedure Get_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : out Boolean);

  -- Erase all the fields of the descriptor from the screen
  --  (Fill them with current screen's background color)
  -- Exceptions : NO_DESCRIPTOR
  procedure Erase;

  -- Put a descriptor content
  -- Any list has to be des activated
  -- Exceptions : NO_DESCRIPTOR, LIST_IN_PUT;
  procedure Put;

  -- Computes next cursor field after current one:
  --  The criteria is the next unprotected and active get field
  --  If FROM is 0, then the first field matching is returned
  --  Else the next matching after FROM is returned
  -- 0 is returned if no matching field is found
  function Next_Cursor_Field (From : Absolute_Field_Range)
  return Absolute_Field_Range;

  -- Same with previous field
  function Prev_Cursor_Field (From : Absolute_Field_Range)
  return Absolute_Field_Range;

  -- List of items to put in list field in put_then_get
  subtype Line_Len_Range is Natural range 0 .. Con_Io.Col_Range'Last+1;
  type Line_Rec is record
    Str : String (1 .. Line_Len_Range'Last);
    Len : Line_Len_Range;
  end record;

  package Line_List_Mng is new Dynamic_List (Line_Rec);
  Line_List : Line_List_Mng.List_Type;
  -- Actions on the list
  type List_Action_List is (Up, Down, Page_Up, Page_Down,
                            Top, Bottom, Center);

  -- Update the list due to an action
  -- Exceptions : INVALID_FIELD if no list in current descriptor,
  procedure Update_List (Action : in List_Action_List);

  -- See Con_io.Curs_Mvt
  type Event_List is (Keyboard, Mouse_Button,
                      Fd_Event, Timer_Event, Signal_Event, Refresh);
  type Keyboard_Key_List is (Return_Key, Escape_Key, Break_Key);

  type Result_Rec (Event : Event_List := Keyboard) is record
    Id_Selected : Natural;
    case Event is
      when Keyboard =>
        Keyboard_Key : Keyboard_Key_List;
      when Mouse_Button =>
        Field_No : Absolute_Field_Range;
      when Fd_Event | Timer_Event | Signal_Event | Refresh =>
        null;
    end case;
  end record;

  -- Print the fields and the list (if REDISPLAY), then gets.
  -- REDISPLAY should be set if modif if some other screen actions (con_io)
  --  justify a redisplay
  -- In LIST: mouse click changes current list element (ID_SELECTED)
  --      double click ends put_then get 
  --      up/down arrows, page up/down, Ctrl page up/down scroll list
  -- In PUT fields : nothing
  -- In GET fields : cursor right/left, characters, bakcspace, delete,
  --                  Home, end, insert edit field
  --                 Tab, shift Tab  change field
  --                 Return / Esc / Break to end put_then_get
  --                 mouse click to move at home of field
  -- In BUTTON fields : mouse click then release ends put_then_get
  -- This call affects the content of GET fields, the cursor field and col,
  -- and the current element of the list
  -- If no field is GET (or all protected or desactivated,
  --  then cursor field and col are not significant
  -- Exceptions :  NO_DESCRIPTOR,
  --               INVALID_FIELD, INVALID_COL (for cursor)
  --               STRING_TOO_LONG (if an item in list is too long)
  procedure Put_Then_Get (Cursor_Field : in out Field_Range;
                          Cursor_Col   : in out Con_Io.Col_Range;
                          Result       : out Result_Rec;
                          Redisplay    : in Boolean := False);

  -- At elaboration
  Afpx_File_Not_Found, Afpx_File_Read_Error, Afpx_File_Version_Error : exception;
  -- On call
  No_Descriptor, Invalid_Field, Invalid_Square, Invalid_Row, Invalid_Col,
  String_Too_Long, Invalid_Color, List_In_Put : exception;


end Afpx;

