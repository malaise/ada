with As.U, Unicode, Con_Io, Dynamic_List;
package Afpx is

  -- See also the comments in Afpx.dtd for the definitions of descriptors,
  --  list and fields (kind, geometry, colors).

  -- Descriptor, field index
  type Descriptor_Range is new Positive range 1 .. 50;
  type Absolute_Field_Range is new Natural range 0 .. 500;
  subtype Field_Range is Absolute_Field_Range
          range 1 .. Absolute_Field_Range 'Last;
  List_Field_No : constant Absolute_Field_Range := 0;

  -- Width and height of a field
  subtype Height_Range is Positive range 1 .. Con_Io.Last_Row + 1;
  subtype Width_Range  is Positive range 1 .. Con_Io.Last_Col + 1;

  -- The content of one row of one field (encode, decode)
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;

  -- Width and height of the screen
  procedure Get_Screen_Size (Height : out Height_Range;
                             Width  : out Width_Range);

  -- Set current descriptor (read from file)
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- The Con_Io screen from previous descriptor (if any) is re-used
  --  (no new window)
  -- By default, the Con_Io screen is cleared
  -- Exceptions : No_Descriptor (Descriptor not found)
  procedure Use_Descriptor (Descriptor_No : in Descriptor_Range;
                            Clear_Screen : in Boolean := True);
  -- Get current descriptor no
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Get_Descriptor return Descriptor_Range;

  -- Close the Con_Io console
  -- Previous descriptor modifications (from encode, set_colors, put_then_get)
  --  are lost
  -- Exceptions : No_Descriptor (no Descriptor in use)
  procedure Release_Descriptor;

  -- Is a descriptor in use
  function Is_Descriptor_Set return Boolean;

  -- The Con_Io Console used by Afpx
  -- The pregram can re-use this Console (e.g. for graphics) as long as it
  --  doesn't call Afpx during this time, then it can Redisplay and use Afpx
  --  again.
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Get_Console return Con_Io.Console;

  -- Suspend and resume Afpx
  -- If a program wants to open several Con_Io Console, (by example a graphical
  --  Console and Afpx), there are two options:
  -- - One task for the Console: the main uses Afpx and the task uses the
  --   Console. Each task receives its own events
  -- - The main opens Afpx and a Console, but only one is active at a time.
  --   In this case the program must Suspend and not use Afpx,
  --   then open and use the new Console, then close the new Console
  --   then Resume, Redisplay and use Afpx again.
  -- Also, for using Event_Mng.Wait (directly or not) a program
  --  must Suspend and Resume Afpx (calling Redisplay is not necessary if no
  --  graphical change has been done meanwhile).
  -- Exceptions : No_Descriptor (no Descriptor in use)
  procedure Suspend;
  procedure Resume;
  function Is_Suspended return Boolean;

  -- Get descriptor background color
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Get_Descriptor_Background return Con_Io.Effective_Colors;

  -- Check if current descriptor defines a list
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Has_List return Boolean;

  -- Returns the number of fields (not the list) of current descriptor
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Nb_Fields return Absolute_Field_Range;

  -- Clear the content of a field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  procedure Clear_Field (Field_No : in Field_Range);

  -- Reset the field from initial definition in file
  --  colors and / or  content,
  -- The field becomes activated and not protected.
  -- Reset_String is meaningless if Field_No is List_Field_No
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  procedure Reset_Field (Field_No : in Absolute_Field_Range;
                         Reset_Colors : in Boolean := True;
                         Reset_String : in Boolean := True);

  -- Height and width of a field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big)
  function Get_Field_Height (Field_No : Absolute_Field_Range)
                             return Height_Range;
  function Get_Field_Width (Field_No : Absolute_Field_Range)
                            return Width_Range;
  procedure Get_Field_Size (Field_No : in Absolute_Field_Range;
                            Height : out Height_Range;
                            Width  : out Width_Range);
  -- Data len of a get field
  -- Exceptions : No_Descriptor (no Descriptor in use),
  --              Invalid_Field (Field_No too big or not a Get field)
  function Get_Data_Len (Field_No : Absolute_Field_Range)
                         return Width_Range;

  -- Get field kind
  -- Exceptions : No_Descriptor, Invalid_Field
  type Field_Kind_List is (Put, Button, Get);
  function Get_Field_Kind (Field_No : in Field_Range) return Field_Kind_List;
  function Is_Put_Kind    (Field_No : in Field_Range) return Boolean;
  function Is_Button_Kind (Field_No : in Field_Range) return Boolean;
  function Is_Get_Kind    (Field_No : in Field_Range) return Boolean;

  -- Encode a string in a field.
  -- The Row is filled with spaces, then with Str starting at Col
  -- Exceptions : No_Descriptor, Invalid_Field
  --              Invalid_Square (not in field),
  --              String_Too_Long (due to Square.Col)
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in String);
  procedure Encode_Wide_Field (Field_No : in Field_Range;
                               From_Pos : in Con_Io.Square;
                               Str      : in Wide_String);
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in Unicode_Sequence);
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in As.U.Asu_Us);

  -- Decode the content of a row of a field
  -- If Adjust is set, then only Data_Len characters are
  --  returned, which may lead to skip last (in)put characters
  --  and possibly pad with a space
  -- Exceptions : No_Descriptor, Invalid_Field, Invalid_Row
  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Row_Range;
                         Adjust   : Boolean := True)
                         return String;
  function Decode_Wide_Field (Field_No : Field_Range;
                              Row      : Con_Io.Row_Range)
                              return Wide_String;
  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Row_Range)
                         return Unicode_Sequence;
  procedure Decode_Field (Field_No : in Field_Range;
                          Row      : in Con_Io.Row_Range;
                          Str      : in out As.U.Asu_Us;
                          Adjust   : in Boolean := True);


  -- Get field colors
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Get_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : out Con_Io.Effective_Colors;
    Background : out Con_Io.Effective_Colors;
    Selected   : out Con_Io.Effective_Colors);

  -- Set field colors
  -- Exceptions : No_Descriptor, Invalid_Field
  --              Invalid_Color
  --       - Foreground has to be Colors for list, get and button fields
  --       - Selected has to be Current for put and button fields
  procedure Set_Field_Colors (
    Field_No   : in Absolute_Field_Range;
    Foreground : in Con_Io.Colors := Con_Io.Current;
    Background : in Con_Io.Colors := Con_Io.Current;
    Selected   : in Con_Io.Colors := Con_Io.Current);

  -- Activate/Desactivate a field for further put_then_gets
  -- All fields are activated by default (when Use_Descriptor or Reset_Field)
  -- A non active field is not displayed by put_then get
  --  (when Use_Descriptor or Reset_Field)
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Set_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : in Boolean);
  function  Get_Field_Activation (Field_No : in Absolute_Field_Range)
                                  return Boolean;

  -- Protect/Unprotect the List, a Get or Button for further put_then_gets
  -- A protected get field is displayed like a put field
  -- A protected button field is displayed like a put (but no click/release)
  -- A protected list is displayed (but no item can be selected)
  -- All get/button/list fields are unprotected by default
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Set_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : in Boolean);
  function  Get_Field_Protection (Field_No : in Absolute_Field_Range)
                                  return Boolean;

  -- Erase all the fields of the descriptor from the screen
  -- Fill the fields on the screen with current screen's background color,
  --  but the content of the fields is not affected
  -- The list if any has to be de-activated
  -- Exceptions : No_Descriptor, List_In_Put
  procedure Erase;

  -- Put a descriptor content
  -- The list is any has to be de-activated
  -- Exceptions : No_Descriptor, List_In_Put
  procedure Put;

  -- Computes next cursor field after current one:
  --  The criteria is the next unprotected and active get field
  --  If From is 0, then the first field matching is returned
  --  Else the next matching after From is returned
  -- When the last field is reached the search reastarts from beginning
  -- 0 is returned if no matching field is found
  -- Exceptions : No_Descriptor
  function Next_Cursor_Field (From : Absolute_Field_Range)
                              return Absolute_Field_Range;

  -- Same with previous field
  function Prev_Cursor_Field (From : Absolute_Field_Range)
                              return Absolute_Field_Range;

  -- List of items to put in list field in Put_Then_Get
  subtype Line_Len_Range is Natural range 0 .. Con_Io.Col_Range'Last + 1;
  type Line_Rec is record
    Str : Unicode_Sequence (1 .. Line_Len_Range'Last);
    Len : Line_Len_Range;
  end record;

  -- Encode a string in a line for the list
  -- Exceptions : String_Too_Long
  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in String);
  procedure Encode_Wide_Line (Line : in out Line_Rec;
                         Str  : in Wide_String);
  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in Unicode_Sequence);

  -- The list (displayed in List_Field)
  package Line_Dyn_List_Mng is new Dynamic_List (Line_Rec);
  package Line_List_Mng renames Line_Dyn_List_Mng.Dyn_List;
  Line_List : Line_List_Mng.List_Type;

  -- User actions on the list:
  ----------------------------
  -- These are the scrolling actions that are automatic bound on keys
  --  and mouse wheel (as soon as the list is not empty):
  --                    +---------------------------------------+
  --                    | Arrow keys |  Page keys | Mouse wheel |
  -- +------------------+------------+------------+-------------+
  -- | Line Up/Down     |      V     |            |      V      |
  -- | Page Up/Down     |    Shift   |      V     |    Shift    |
  -- | 10 Pages Up/Down |    Ctrl    |    Shift   |     Ctrl    |
  -- | Top/Bottom       |            |     Ctrl   |             |
  -- +----------------------------------------------------------+

  -- Selection in the list (with the mouse):
  -- When the list is active and not empty there is always one line selected,
  --  which is highlighted with the "Selected" color defined for the list.
  --  Left click on a line sets the left selection to this line.
  -- On option (Right_Select set to Put_Then_Get) it is possible to have a
  --  second line selected, which is highlighted by reversing the "Foreground"
  --  and "Background" colors defined for the list.
  --  Right click on a line (except the one already left selected) set or unsets
  --  the right selection to this line.
  --  Left selecting the right selected line also resets the right selection.

  -- During Put_Then_Get execution the List_Change_Cb callback reports all the
  --  changes in the list (scroll, selection, change of content).
  -- When Put_Then_get returns, some information can also be obtained but
  --   nothing about which part of the list was displayed:
  --  - The current element of the Line_List is the one left-selected
  --  - The right-selected is set in Put_Then_Get result (Id_Selected_Right)

  -- Caller actions on the list (caller asking Afpx to modify the list)
  -----------------------------
  -- Last 3 actions try to move the current selected item (left click)
  --  at a given position in list window
  type List_Action_List is (Up, Down, Page_Up, Page_Down,
                            Shift_Page_Up, Shift_Page_Down,
                            Top, Bottom,
                            Top_Selected, Center_Selected, Bottom_Selected);

  -- Update the list due to an action (e.g. a button)
  -- Exceptions : Invalid_Field if no list in current descriptor or not active.
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
        -- Field click and relative square of click and release
        Field_No : Absolute_Field_Range;
        Click_Pos, Release_Pos : Con_Io.Square;
      when Fd_Event | Timer_Event | Signal_Event | Refresh =>
        null;
    end case;
  end record;

  -- Call back called by Put_Then_Get when entering a new get field:
  -- Given the field no, the reason for entering field (see Con_Io)
  --  given the cursor col when this is a Mouse click
  --  and given the content of the get field as by Decode_Field (Row => 0)
  --  the client specifies the column of the cursor.
  -- If the value returned is bigger than Str'Length - 1,
  --  then Str'Length - 1 is used.
  -- If no callback is provided, then cursor is set to end of field if Left and
  --  start of field otherwise.
  -- Beware that modifying the list (including changing the current position)
  --  affects the ongoing Put_Then_Get
  type Enter_Field_Cause_List is (Mouse, Right_Full, Left, Tab, Stab);
  type Cursor_Set_Col_Cb is access
       function (Cursor_Field : Field_Range;
                 New_Field : Boolean;
                 Cursor_Col : Con_Io.Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Str : Unicode_Sequence) return Con_Io.Col_Range;

  -- Returns the index (from 0 to Str'Length-1) of the first character of Str
  --  or, if Significant, the index preceeding first significant character
  --  (skipping heading spaces and htabs).
  -- This can be usefully called by Cursor_Set_Col_Cb.
  function First_Index (Str : Unicode_Sequence; Significant : Boolean)
                        return Con_Io.Col_Range;

  -- Returns the index (from 0 to Str'Length-1) of the last character of Str
  --  or, if Significant, the index following last significant character
  --  (skipping trailing spaces and htabs).
  -- This can be usefully called by Cursor_Set_Col_Cb.
  function Last_Index (Str : Unicode_Sequence; Significant : Boolean)
                       return Con_Io.Col_Range;

  -- Call back called by Put_Then_Get when something is changed in the list:
  --  - Put_Then_Get is first displaying (init) the list
  --  - change of left or right selection
  --  - scroll by keyboard or wheel
  -- Beware that modifying the list (including changing the current position)
  --  affects the ongoing Put_Then_Get
  -- Note: A double click in a new line tiggers a Left_Selection notification,
  --  then the end of Put_Then_Get
  type List_Change_List is (Init, Left_Selection, Right_Selection, Scroll);
  type List_Button_List is (List_Left, List_Right);
  type List_Ids_Selected_Array is array (List_Button_List) of Natural;
  type List_Status_Rec is record
    -- The number of items diplayed, 0 if no list field active
    -- Width if list_length >= width, list_length otherwise
    Nb_Rows : Natural;
    -- First and last items displayed in the window
    Id_Top    : Natural;
    Id_Bottom : Natural;
    -- Item selected (0 if no selection, if list not active...)
    Ids_Selected : List_Ids_Selected_Array;
  end record;
  type List_Change_Cb is access
    procedure (Action : in List_Change_List;
               Status : in List_Status_Rec);

  -- Percent of position of list in list field
  -- 0 when list is shorter than field (including empty list)
  -- 1 => Top, 100 => Bottom
  subtype Percent_Range is Natural range 0 .. 100;
  function Get_List_Percent return Percent_Range;
  -- Get position in list corresponding to Percent
  function Get_List_Index (Percent : Percent_Range) return Natural;

  -- Redisplay can be called before Put_Then_Get to force the redraw
  --  of all fields. Normally this is not needed because it is automatically
  --  internally triggered when the list or a field is modified or on Refresh
  --  event
  procedure Redisplay;

  -- The 'in out' token of Get status between successive calls to Put_Then_Get
  -- If Cursor_Field is List_Field_No (0) then the cursor will be in the
  --  first cursor field: Next_Cursor_Field(0)
  type Get_Handle_Rec is record
    Cursor_Field : Absolute_Field_Range :=  List_Field_No;
    Cursor_Col   : Con_Io.Col_Range := Con_Io.Col_Range_First;
    Insert       : Boolean := False;
    Offset       : Con_Io.Col_Range := Con_Io.Col_Range_First;
  end record;

  -- Print the fields and the list (if Redisplay has been called or is needed),
  --  then gets.
  --  justify a redisplay, by instance when Result.Event was Refresh. It is
  --  always reset to False by Put_Then_Get.
  -- In List: See above section on "User actions on the list".
  --   mouse click changes current list element (or Id_Selected_Right),
  --   Up/Down arrow, Page Up/Down, Ctrl Page Up/Down scrolls the list,
  --   double click terminates Put_Then Get (Mouse_Button, List_Field_No).
  -- In Put fields: nothing.
  -- In Get fields:
  --   A character input, Backspace, Suppr, or Ctrl Suppr edits the field,
  --   A character input or arrow can also shit the field (by an Offset)
  --    if the data len is larger than the field width
  --   (Ctrl) Right/Left arrow, (Ctrl) Home/End navigates in the field,
  --   Insert toggles insert/replace mode
  --   Tab or Shift Tab changes field (like Next/Prev_Cursor_Field), a character
  --    input or left/right arrow can also lead to change field
  --   Only Return, Esc or Break key terminates the Put_Then_Get
  --   A mouse click moves to the field.
  -- In Button fields: mouse click then release terminates Put_Then_Get.
  -- This call affects the content of Get fields, the cursor field and col,
  --  and the current element of the list, it calls Modification_Ack on the
  --  Line_List (see Dynamic_List).
  -- If no field is Get (or all protected or desactivated,
  --  then the Get_Handle is not significant, otherwise it is used at
  --  initialisation and set before returning.
  -- While already in Put_Then_Get (i.e. from an Event callback or
  --  Cursor_Col_Cb or List_Change_Cb) it is forbidden to call:
  --  - Put_Then_Get, Erase, Put
  --  - Use/Release Descriptor, Suspend/Resume
  --  - Set_Field_Activation/Protection of the List or of a Get field
  --  This would raise the exception In_Put_Then_Get
  -- Exceptions :  No_Descriptor,
  --               Invalid_Field, Invalid_Col (for cursor),
  --               String_Too_Long (if an item in list is too long),
  --               Suspended (Afpx is currentlky suspended)
  --               In_Put_Then_Get (already in Put_Then_Get while calling Afpx)
  procedure Put_Then_Get (Get_Handle    : in out Get_Handle_Rec;
                          Result        : out Result_Rec;
                          Right_Select  : in Boolean := False;
                          Cursor_Col_Cb : access
       function (Cursor_Field : Field_Range;
                 New_Field : Boolean;
                 Cursor_Col : Con_Io.Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Str : Unicode_Sequence) return Con_Io.Col_Range := null;
                          List_Change_Cb : access
       procedure (Action : in List_Change_List;
                  Status : in List_Status_Rec) := null);

  -- Ring a bell on screen
  procedure Bell (Repeat : in Positive := 1);

  -- Propose (mouse) selection to other applications
  -- Clears if empty string
  procedure Set_Selection (Selection : in String);

  -- At elaboration
  Afpx_File_Not_Found, Afpx_File_Read_Error,
  Afpx_File_Version_Error : exception;

  -- On call
  No_Descriptor, Invalid_Field, Invalid_Square, Invalid_Row, Invalid_Col,
  String_Too_Long, Invalid_Color, List_In_Put, In_Put_Then_Get,
  Suspended : exception;

end Afpx;

