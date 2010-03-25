with Text_Handler, Con_Io, Dynamic_List;

package Afpx is

  -- Descriptor, field index
  type Descriptor_Range is new Positive range 1 .. 50;
  type Absolute_Field_Range is new Natural range 0 .. 200;
  subtype Field_Range is Absolute_Field_Range
          range 1 .. Absolute_Field_Range 'Last;
  List_Field_No : constant Absolute_Field_Range := 0;

  -- Width and height of a field
  subtype Height_Range is Positive range 1 .. Con_Io.Full_Row_Range_Last + 1;
  subtype Width_Range  is Positive range 1 .. Con_Io.Full_Col_Range_Last + 1;

  -- The content of one row of one field (encode, decode)
  subtype Str_Txt is Text_Handler.Text (Width_Range'Last);

  -- Width and height of the screen
  procedure Get_Screen_Size (Height : out Height_Range;
                             Width  : out Width_Range);

  -- Set current descriptor (read from file)
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- The Con_Io screen from previous descriptor (if any) is re-used
  --  (no new window) cleared
  -- By default, the Con_Io screen is cleared
  -- Exceptions : No_Descriptor (Descriptor not found)
  procedure Use_Descriptor (Descriptor_No : in Descriptor_Range;
                            Clear_Screen : in Boolean := True);

  -- Close the Con_Io screen
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- Exceptions : No_Descriptor (no Descriptor in use)
  procedure Release_Descriptor;

  -- Suspend and resume a con_io
  -- If a program wants to open several con_io, (by example a graphical con_io
  --   after an afpx), there are two options:
  -- - One task for the con_io, the main uses afpx and the task uses con_io,
  --   each receives its own events
  -- - The main opens an afpx and a con_io, only one is active at a time.
  --   In this case the program must suspend and not use the afpx,
  --   then open and use the new con_io, then close the new con_io
  --   then resume and use the afpx.
  procedure Suspend;
  procedure Resume;

  -- Get descriptor background color
  procedure Get_Descriptor_Background (
      Background : out Con_Io.Effective_Basic_Colors);

  -- Check if current descriptor defines a list
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Has_List return Boolean;

  -- Returns the number of fields of current descriptor
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Nb_Fields return Absolute_Field_Range;

  -- Clear the content of a field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  procedure Clear_Field (Field_No : in Field_Range);

  -- Reset the field from initial definition in file
  --  colors and / or  content,
  -- The field becomes activated and not protected.
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  procedure Reset_Field (Field_No : in Absolute_Field_Range;
                         Reset_Colors : in Boolean := True;
                         Reset_String : in Boolean := True);

  -- Width of a field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  function Get_Field_Width (Field_No : Absolute_Field_Range)
                            return Width_Range;

  -- Width and height of a field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  procedure Get_Field_Size (Field_No : in Absolute_Field_Range;
                            Height : out Height_Range;
                            Width  : out Width_Range);

  -- Encode a string in a field.
  -- The Row is filled with spaces, then with Str starting at Col
  -- Exceptions : No_Descriptor, Invalid_Field
  --              Invalid_Square (not in field),
  --              String_Too_Long (due to Square.Col)
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Full_Square;
                          Str      : in String);
  procedure Encode_Wide_Field (Field_No : in Field_Range;
                               From_Pos : in Con_Io.Full_Square;
                               Str      : in Wide_String);
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Full_Square;
                          Str      : in Str_Txt);

  -- Decode the content of a row of a field
  -- If Adjust is set, then only Width string characters are
  --  returned, which may lead to skip last (in)put characters
  --  and possibly pad with a space
  -- Exceptions : No_Descriptor, Invalid_Field, Invalid_Row
  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Full_Row_Range;
                         Adjust   : Boolean := True)
                         return String;
  function Decode_Wide_Field (Field_No : Field_Range;
                              Row      : Con_Io.Full_Row_Range)
                              return Wide_String;
  procedure Decode_Field (Field_No : in Field_Range;
                          Row      : in Con_Io.Full_Row_Range;
                          Str      : in out Str_Txt;
                          Adjust   : in Boolean := True);


  -- Get field colors
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Get_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : out Con_Io.Effective_Colors;
    Blink_Stat : out Con_Io.Effective_Blink_Stats;
    Background : out Con_Io.Effective_Basic_Colors;
    Selected   : out Con_Io.Effective_Basic_Colors);

  -- Set field colors
  -- Exceptions : No_Descriptor, Invalid_Field
  --              Invalid_Color
  --       - Foreground has to be Basic_Colors for list, get and button fields
  --       - Selected has to be Current for put and button fields
  --       - Blink_Stat has to be Current except for put fields
  procedure Set_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : in Con_Io.Colors       := Con_Io.Current;
    Blink_Stat : in Con_Io.Blink_Stats  := Con_Io.Current;
    Background : in Con_Io.Basic_Colors := Con_Io.Current;
    Selected   : in Con_Io.Basic_Colors := Con_Io.Current);

  -- Activate/Desactivate a field for further put_then_gets
  -- All fields are activated by default (when Use_Descriptor or Reset_Field)
  -- A non active field is not displayed by put_then get
  --  (when Use_Descriptor or Reset_Field)
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Set_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : in Boolean);
  function  Get_Field_Activation (Field_No : in Absolute_Field_Range)
                                  return Boolean;

  -- Protect/Unprotect a Get or Button for further put_then_gets
  -- A protected get field is displayed like a put field (but cannot blink)
  -- A protected button field is displayed like a put (but no click/release)
  -- A protected list is displayed (but no item can be selected)
  -- All get/button/list fields are unprotected by default
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Set_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : in Boolean);
  function  Get_Field_Protection (Field_No : in Absolute_Field_Range)
                                  return Boolean;

  -- Get field kind
  -- Exceptions : No_Descriptor, Invalid_Field
  type Field_Kind_List is (Put, Button, Get);
  function Get_Field_Kind (Field_No : in Absolute_Field_Range)
                          return Field_Kind_List;
  function Is_Put_Kind    (Field_No : in Absolute_Field_Range) return Boolean;
  function Is_Button_Kind (Field_No : in Absolute_Field_Range) return Boolean;
  function Is_Get_Kind    (Field_No : in Absolute_Field_Range) return Boolean;

  -- Erase all the fields of the descriptor from the screen
  --  (Fill them with current screen's background color)
  -- Exceptions : No_Descriptor
  procedure Erase;

  -- Put a descriptor content
  -- Any list has to be de-activated
  -- Exceptions : No_Descriptor, List_In_Put.
  procedure Put;

  -- Computes next cursor field after current one:
  --  The criteria is the next unprotected and active get field
  --  If From is 0, then the first field matching is returned
  --  Else the next matching after From is returned
  -- 0 is returned if no matching field is found
  function Next_Cursor_Field (From : Absolute_Field_Range)
                              return Absolute_Field_Range;

  -- Same with previous field
  function Prev_Cursor_Field (From : Absolute_Field_Range)
                              return Absolute_Field_Range;

  -- List of items to put in list field in Put_Then_Get
  subtype Line_Len_Range is Natural range 0 .. Con_Io.Full_Col_Range'Last+1;
  type Line_Rec is record
    Str : Wide_String (1 .. Line_Len_Range'Last);
    Len : Line_Len_Range;
  end record;

  -- Encode a string in a line for the list
  -- Exceptions : String_Too_Long
  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in String);
  procedure Encode_Wide_Line (Line : in out Line_Rec;
                         Str  : in Wide_String);
  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in Str_Txt);

  package Line_Dyn_List_Mng is new Dynamic_List (Line_Rec);
  package Line_List_Mng renames Line_Dyn_List_Mng.Dyn_List;
  Line_List : Line_List_Mng.List_Type;
  -- Actions on the list
  type List_Action_List is (Up, Down, Page_Up, Page_Down,
                            Top, Bottom, Center);

  -- Update the list due to an action
  -- Exceptions : Invalid_Field if no list in current descriptor.
  procedure Update_List (Action : in List_Action_List);

  -- Result of Put_Then_Get:
  -- See Con_io.Curs_Mvt
  type Event_List is (Keyboard, Mouse_Button,
                      Fd_Event, Timer_Event, Signal_Event, Refresh);
  type Keyboard_Key_List is (Return_Key, Escape_Key, Break_Key);

  type Result_Rec (Event : Event_List := Keyboard) is record
    Id_Selected_Right : Natural;
    case Event is
      when Keyboard =>
        Keyboard_Key : Keyboard_Key_List;
      when Mouse_Button =>
        Field_No : Absolute_Field_Range;
      when Fd_Event | Timer_Event | Signal_Event | Refresh =>
        null;
    end case;
  end record;

  -- Call back called by Put_Then_Get when entering a new get field:
  -- Given the field no, the reason for entering field (see Con_Io)
  type Enter_Field_Cause_List is (Mouse, Right_Full, Left, Tab, Stab);
  --  given the cursor col when this is a Mouse click
  --  and given the content of the get field as by Decode_Field (Row => 0)
  --  the client specifies the column of the cursor.
  -- If the value returned is bigger than Str'Length - 1,
  --  then Str'Length - 1 is used.
  -- If no callback is provided, then cursor is set to end of field if Left and
  --  start of field otherwise.
  type Cursor_Set_Col_Cb is access
       function (Cursor_Field : Field_Range;
                 New_Field : Boolean;
                 Cursor_Col : Con_Io.Full_Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Str : Wide_String) return Con_Io.Full_Col_Range;

  -- Returns the index (from 0 to Str'Last-1) of the last character of Str
  --  or, if Significant, the index following last significant character
  --  (skipping trailing spaces and htabs).
  -- This can be usefully called by Cursor_Set_Col_Cb.
  function Last_Index (Str : Wide_String; Significant : Boolean)
                       return Con_Io.Full_Col_Range;

  -- Call back called by Put_Then_Get when something is changed in the list:
  --  - change of left or right selection
  --  - scroll by keyboard or wheel
  --  - Put_Then_Get redisplays list (list modified)
  type List_Change_List is (Left_Selection, Right_Selection, Scroll,
                            List_Modified);
  type List_Button_List is (List_Left, List_Right);
  type List_Ids_Selected_Array is array (List_Button_List) of Natural;
  type List_Status_Rec is record
    -- The number of items diplayed, 0 if no list field active
    -- Width if list_length >= width, list_length otherwise
    Nb_Rows : Natural;
    -- First and last items displayed in the window
    Id_Top    : Natural;
    Id_Bottom : Natural;
    -- Item selected (0 if no selection, if list no active...)
    Ids_Selected : List_Ids_Selected_Array;
  end record;
  type List_Change_Cb is access
    procedure (Action : in List_Change_List;
               Status : in List_Status_Rec);

  -- Print the fields and the list (if Redisplay), then gets.
  -- Redisplay should be set if modif of some other screen actions (con_io)
  --  justify a redisplay, by instance when Result.Event was Refresh.
  -- In List:
  --   mouse click changes current list element (or Id_Selected_Right),
  --   Up/Down arrow, Page Up/Down, Ctrl Page Up/Down scrolls the list,
  --   double click terminates Put_Then Get.
  -- In Put fields: nothing.
  -- In Get fields:
  --    Right/Left arrow, character, Backspace, Delete,
  --      Home, End, Ctrl Delete or Insert edits the field,
  --    Tab or Ctrl Tab changes field (like Next:Prev_Cursor_Field),
  --    Return / Esc / Break terminates Put_Then_Get,
  --    mouse click moves to the field.
  -- In Button fields: mouse click then release terminates Put_Then_Get.
  -- This call affects the content of Get fields, the cursor field and col,
  --  and the current element of the list, it calls Modification_Ack on the
  --  Line_List (see Dynamic_List).
  -- If no field is Get (or all protected or desactivated,
  --  then cursor field and col are not significant, otherwise
  --  they are used at initialisation and set before the end.
  -- No call to Put_Then_Get are allowed while already in Put_Then_Get
  --  (i.e. from an Event callback or Cursor_Col_Cb).
  -- Exceptions :  No_Descriptor,
  --               Invalid_Field, Invalid_Col (for cursor),
  --               String_Too_Long (if an item in list is too long),
  --               In_Put_Then_Get (already in Put_Then_Get).
  procedure Put_Then_Get (Cursor_Field  : in out Field_Range;
                          Cursor_Col    : in out Con_Io.Full_Col_Range;
                          Insert        : in out Boolean;
                          Result        : out Result_Rec;
                          Redisplay     : in Boolean := False;
                          Right_Select  : in Boolean := False;
                          Cursor_Col_Cb : access
       function (Cursor_Field : Field_Range;
                 New_Field : Boolean;
                 Cursor_Col : Con_Io.Full_Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Str : Wide_String) return Con_Io.Full_Col_Range := null;
                          List_Change_Cb : access
       procedure (Action : in List_Change_List;
                  Status : in List_Status_Rec) := null);

  -- Ring a bell on screen
  procedure Bell (Repeat : in Positive := 1);

  -- Propose (mouse) selection to to other applications
  -- Clears if empty string
  procedure Set_Selection (Selection : in String);

  -- At elaboration
  Afpx_File_Not_Found, Afpx_File_Read_Error,
  Afpx_File_Version_Error : exception;

  -- On call
  No_Descriptor, Invalid_Field, Invalid_Square, Invalid_Row, Invalid_Col,
  String_Too_Long, Invalid_Color, List_In_Put, In_Put_Then_Get : exception;


end Afpx;

