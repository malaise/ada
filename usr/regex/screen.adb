with Con_Io, Afpx.Utils, Images, Str_Util;
with Afpx_Xref;

package body Screen is
  -- Previous and new input
  Prev_Input, Cur_Input : Input_Rec;

  -- LOCAL: Is an option set
  function Is_Set (Fld : Afpx.Field_Range) return Boolean is
    (Afpx.Decode_Field (Fld, 0, True) = "X");

  -- LOCAL: Strip trailing spaces
  procedure Strip (Str : in out As.U.Asu_Us) is
  begin
    Str := As.U.Tus (Str_Util.Strip (Str.Image));
  end Strip;

  -- Has an input changed since previous call
  function Input_Changed return Boolean is
    use type Afpx.Field_Range;
  begin
    -- Decode option
    Cur_Input.Case_Sensitive := Is_Set (Afpx_Xref.Main.Case_Sensitive);
    -- Decode Regex
    Afpx.Decode_Field (Afpx_Xref.Main.Regex, 0, Cur_Input.Regex, False);
    Strip (Cur_Input.Regex);
    -- Decode text
    for I in Text_Range loop
      Afpx.Decode_Field (Afpx_Xref.Main.Text1 + Afpx.Field_Range (I) - 1, 0,
                         Cur_Input.Text(I), False);
      Strip (Cur_Input.Text(I));
    end loop;
    -- Update Prev if intput has changed
    return Res : Boolean do
      Res := Cur_Input /= Prev_Input;
      if Res then
        Prev_Input := Cur_Input;
      end if;
    end return;
  end Input_Changed;

  -- Get (changed) input
  function Get_Input return Input_Rec is
  begin
    Prev_Input := Cur_Input;
    return Prev_Input;
  end Get_Input;

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

  -- Put result
  procedure Put_Results (Line : in Text_Range := 1;
                         Results : in Results_Array) is
    use type Afpx.Field_Range;
  begin
    -- Encode Line
    Afpx.Encode_Field (Afpx_Xref.Main.Line, (0, 0),
        Images.Integer_Image ( (if Results(1).Is_Null then 0 else Line)));
    -- Reset and encode result
    Afpx.Reset_Field (Afpx_Xref.Main.Result);
    for I in Results'Range loop
      if not Results(I).Is_Null then
        Afpx.Utils.Encode_Field (Results(I).Image, Afpx_Xref.Main.Result,
                                 I - 1, Clear => False, Keep_Tail => False);
      end if;
    end loop;
  end Put_Results;

  -- Put compilation error
  procedure Put_Error (Msg : in String) is
  begin
    -- Reset and encode error in red
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

