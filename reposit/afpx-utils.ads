package Afpx.Utils is

  -- Scroll the list according to button
  -- Assumption is that buttons are consecutive in the order:
  -- Top(1), PgUp(2), Up(3), Center(4), Down(5), PgDown(6), Bottom(7)
  subtype Offset_Range is Field_Range range 1 .. 7;
  procedure Scroll (Offset : in Offset_Range);


  -- Initialize the Afpx list
  generic
    -- Type of the element of the list
    type Element_Type is private;
    -- List manager of elements
    with package Element_List is new Dynamic_List(Element_Type);
    -- How to set the Afpx line from Element_Type
    with procedure Set (Line : in out Line_Rec; From : in Element_Type);
    Deallocate : in Boolean := True;
  -- Clear (and deallocate) the Afpx list and initialize it from From list,
  --  move to current position of From
  procedure Init_List (From : in out Element_List.Dyn_List.List_Type);


  -- If Str fits Width then return Str, padded with space if no Align_Left
  -- elsif Show_Cut return ">>" & tail to match Width (if Keep_Tail)
  --             or return head to match Width and "<<" (if not Keep_Tail)
  -- else return tail or head
  function Procuste (Str : String;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Keep_Tail : Boolean := True;
                     Show_Cut : Boolean := True) return String;


  -- Protect a field and "revert" its colors, or reset it to its default (keep
  --  its Activation status)
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range;
                           Protect  : in Boolean);

  -- Clean (fill with spaces) the Str field of the line
  procedure Clean_Line (Line : in out Afpx.Line_Rec);

  -- Clean then encode a line, procuste on Text, preserving tail or head of Text
  procedure Encode_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : in out Afpx.Line_Rec;
                         Keep_Tail : in Boolean := True;
                         Show_Cut : Boolean := True);

  -- Center Head+Text+Tail in Line, procuste on Text, preserving tail or head
  --  of Text, or fill gaps with spaces,
  procedure Center_Line (Head, Text, Tail : in String;
                         Width : in Afpx.Width_Range;
                         Line : in out Afpx.Line_Rec;
                         Keep_Head : in Boolean := True;
                         Show_Cut : Boolean := True);

  -- Encode Text in 1st column of Row of Field, procuste,
  --  preserve tail or head
  procedure Encode_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Clear : in Boolean := True;
                          Keep_Tail : in Boolean := True;
                          Show_Cut : Boolean := True);

  -- Clear field and Center Text in 1st column of Field (row 0 or 1)
  --  procuste, preserve tail or head
  procedure Center_Field (Text : in String;
                          Field : in Afpx.Field_Range;
                          Row : in Con_Io.Row_Range;
                          Keep_Head : in Boolean := True;
                          Show_Cut : Boolean := True);

end Afpx.Utils;

