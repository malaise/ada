with Con_Io, Afpx.Utils, Int_Img, Str_Util, Normal;
with Afpx_Xref;

package body Screen is
  -- Previous and new input
  Prev_Input, Curr_Input : Input_Rec;
  -- Has cursor changed
  Prev_Cursor, Curr_Cursor : Afpx.Field_Range := 1;

  -- LOCAL: Is an option set
  function Is_Set (Fld : Afpx.Field_Range) return Boolean is
    (Afpx.Decode_Field (Fld, 0, True) = "X");

  -- LOCAL: Strip trailing spaces
  procedure Strip (Str : in out As.U.Asu_Us) is
  begin
    Str := As.U.Tus (Str_Util.Strip (Str.Image));
  end Strip;

  -- The field no of the first line of text
  function First_Text return Afpx.Absolute_Field_Range is
  begin
    return Afpx_Xref.Main.Text1;
  end First_Text;

  -- Current cursor has changed
  procedure Cursor_Has_Changed (Cursor_Field : in Afpx.Field_Range) is
    use type Afpx.Field_Range;
  begin
    if Cursor_Field >= First_Text then
      -- Valid new cursor field
      Curr_Cursor := Cursor_Field;
    elsif Curr_Cursor < First_Text then
      -- Default to first
      Curr_Cursor := First_Text;
    end if;
    -- Update cursor field
    Afpx.Clear_Field (Afpx_Xref.Main.Cursor);
    Afpx.Encode_Field (Afpx_Xref.Main.Cursor,
        (Con_Io.Row_Range (Curr_Cursor - First_Text), 0),
        "->");
  end Cursor_Has_Changed;

  -- Has a cursor changed since previous call
  function Has_Cursor_Changed return Boolean is
    use type Afpx.Field_Range;
  begin
    -- Force a first update ad First_Text
    Cursor_Has_Changed (1);
    return Curr_Cursor /= Prev_Cursor;
  end Has_Cursor_Changed;

  -- Has an input changed since previous call
  function Has_Input_Changed return Boolean is
    use type Afpx.Field_Range;
  begin
    -- Decode option
    Curr_Input.Case_Sensitive := Is_Set (Afpx_Xref.Main.Case_Sensitive);
    -- Decode Regex
    Afpx.Decode_Field (Afpx_Xref.Main.Regex, 0, Curr_Input.Regex, False);
    Strip (Curr_Input.Regex);
    -- Decode text
    for I in Text_Range loop
      Afpx.Decode_Field (Afpx_Xref.Main.Text1 + Afpx.Field_Range (I) - 1, 0,
                         Curr_Input.Text(I), False);
      Strip (Curr_Input.Text(I));
    end loop;
    return Curr_Input /= Prev_Input;
  end Has_Input_Changed;

  -- Get (changed) input
  function Get_Input return Input_Rec is
  begin
    Prev_Input := Curr_Input;
    return Prev_Input;
  end Get_Input;

  -- Get (changed) cursor
  function Get_Cursor return Afpx.Field_Range is
  begin
    -- Trigger first update
    Cursor_Has_Changed (1);
    Prev_Cursor := Curr_Cursor;
    return Curr_Cursor;
  end Get_Cursor;

  -- Clear clear regex and reset flag
  procedure Clear_Regex is
  begin
    Afpx.Reset_Field (Afpx_Xref.Main.Case_Sensitive, Reset_Colors => False);
    Afpx.Clear_Field (Afpx_Xref.Main.Regex);
  end Clear_Regex;

  -- Clear all lines of text
  procedure Clear_Text is
    use type Afpx.Field_Range;
  begin
    for I in Text_Range loop
      Afpx.Clear_Field (Afpx_Xref.Main.Text1 + Afpx.Field_Range (I) - 1);
    end loop;
  end Clear_Text;

  function Image (I : Index_Range) return String is
    (Normal (I, 2, Gap => '0'));

  -- Put result
  procedure Put_Results (
                         Line : in Text_Range := 1;
                         Results : in Results_Array) is
  begin
    -- Encode matching line
    Afpx.Encode_Field (Afpx_Xref.Main.Line, (0, 0),
        Int_Img (if Results = No_Results then 0 else Line));
    -- Reset result
    Afpx.Reset_Field (Afpx_Xref.Main.Ranges);
    Afpx.Reset_Field (Afpx_Xref.Main.Result);
    -- Encode line by line
    for I in Results'Range loop
      if Results(I) /= No_Result then
        Afpx.Utils.Encode_Field (
          "(" & Image (Results(I).Start) & "-" & Image (Results(I).Stop) & ")",
          Afpx_Xref.Main.Ranges, I - 1, Clear => False, Keep_Tail => False);
        Afpx.Utils.Encode_Field (
          Results(I).Str.Image,
          Afpx_Xref.Main.Result, I - 1, Clear => False, Keep_Tail => False);
      end if;
    end loop;
  end Put_Results;

  -- Put compilation error
  procedure Put_Error (Msg : in String) is
  begin
    -- Reset and encode error in red
    Afpx.Reset_Field (Afpx_Xref.Main.Ranges);
    Afpx.Reset_Field (Afpx_Xref.Main.Result);
    Afpx.Set_Field_Colors (Afpx_Xref.Main.Result,
                           Foreground => Con_Io.Color_Of ("Red"));
    Afpx.Utils.Encode_Field (Msg, Afpx_Xref.Main.Result, 0, Keep_Tail => False);
  end Put_Error;

  -- (Un) Toggle an option "X" <-> " "
  procedure Toggle (Fld : in Afpx.Field_Range) is
  begin
    Afpx.Encode_Field (Fld, (0, 0),
        (if Is_Set (Fld) then " " else "X"));
  end Toggle;

end Screen;

