with As.U, Afpx;
package Screen is

  -- Number of lines of text to match
  Nb_Line_Text : constant := 9;
  subtype Text_Range is Positive range 1 .. Nb_Line_Text;
  subtype Text_Array is As.U.Asu_Array (1 .. Nb_Line_Text);

  -- The field no of the first line of text
  function First_Text return Afpx.Absolute_Field_Range;

  -- The full content of input fields
  type Input_Rec is record
    -- The Regex;
    Regex : As.U.Asu_Us;
    -- The option
    Case_Sensitive : Boolean := False;
    -- The text to match
    Text : Text_Array;
  end record;

  -- Current cursor has changed
  procedure Cursor_Has_Changed;

  -- Has an input changed since previous call
  function Input_Changed return Boolean;

  -- Get (changed) input
  function Get_Input return Input_Rec;

  -- Clear clear regex and reset flags
  procedure Clear_Regex;

  -- Clear all lines of text
  procedure Clear_Text;

  -- Put result
  -- Result is the full matching substring and up to 9 substrings
  -- For each, the range (start, stop) on 2 digits and the string
  Nb_Results : constant := 10;
  subtype Index_Range is Natural range 0 .. 99;
  type Result_Rec is record
    Start, Stop : Index_Range := 0;
    Str : As.U.Asu_Us;
  end record;
  No_Result : constant Result_Rec := (others => <>);

  type Results_Array is array (1 .. Nb_Results) of Result_Rec;
  No_Results : constant Results_Array := (others => <>);

  procedure Put_Results (
    Cursor_Field : in Afpx.Absolute_Field_Range := 1;
    -- Line will be 0 if first (and other) result is empty
    Line : in Text_Range := 1;
    Results : in Results_Array);

  -- Put compilation error
  procedure Put_Error (Msg : in String);

  -- (Un) Toggle an option "X" <-> " "
  procedure Toggle (Fld : in Afpx.Field_Range);

end Screen;
