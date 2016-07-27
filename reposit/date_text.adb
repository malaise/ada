with Trace.Loggers, Mixed_Str, Normal, Images, Gets, As.U, Any_Def,
     Unbounded_Arrays, Perpet, Scanner;
package body Date_Text is

  -- Logger
  Logger : Trace.Loggers.Logger;

  -- Maps fields of Date_Rec from/to indexes
  package Indexes is
    -- Indexes of fields: 1 is year .. 7 is microseconds
    subtype Index_Range is Positive range 1 .. 8;
    -- Update To so that its Index field is set to Val
    procedure Set (To : in out Date_Rec;
                   Index : in Index_Range;
                   Val : in Natural);
    -- Get the Index field of Val
    function Get (Val : in Date_Rec; Index : in Index_Range) return Natural;
  end Indexes;

  package body Indexes is
    -- Update To so that its Index field is set to Val
    procedure Set (To : in out Date_Rec;
                   Index : in Index_Range;
                   Val : in Natural) is
    begin
      case Index is
        when 1 => To.Years    := Val;
        when 2 => To.Months   := Val;
        when 3 => To.Days     := Val;
        when 4 => To.Hours    := Val;
        when 5 => To.Minutes  := Val;
        when 6 => To.Seconds  := Val;
        when 7 => To.Millisec := Val;
        when 8 => To.Microsec := Val;
      end case;
    end Set;

    -- Get the Index field of Val
    function Get (Val : in Date_Rec; Index : in Index_Range) return Natural is
    begin
      return (case Index is
        when 1 => Val.Years,
        when 2 => Val.Months,
        when 3 => Val.Days,
        when 4 => Val.Hours,
        when 5 => Val.Minutes,
        when 6 => Val.Seconds,
        when 7 => Val.Millisec,
        when 8 => Val.Microsec);
    end Get;
  end Indexes;

  -- Convertion functions for month
  package Conv is
    type Func is access function (Str : String) return String;
    -- "January" -> "01" and reverse
    function Long2Num (Str : String) return String;
    function Num2Long (Str : String) return String;
    -- "Jan" -> "01" and reverse
    function Short2Num (Str : String) return String;
    function Num2Short (Str : String) return String;
  end Conv;

  package body Conv is
    -- "January" -> "01" and reverse
    function Long2Num (Str : String) return String is
      Month : Perpet.Month_Name_List;
    begin
      -- Convert to month and check casing
      Month := Perpet.Month_Name_List'Value (Str);
      if Str /= Mixed_Str (Month'Img) then
        raise Constraint_Error;
      end if;
      return Normal (Perpet.Month_Name_List'Pos (Month) + 1, 2, Gap => '0');
    end Long2Num;

    function Num2Long (Str : String) return String is
      Num : constant Natural := Gets.Get_Int (Str) - 1;
    begin
      return Mixed_Str (Perpet.Month_Name_List'Image
          (Perpet.Month_Name_List'Val (Num)));
    end Num2Long;

    -- "Jan" -> "01" and reverse
    type Short_Name_List is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct,
                             Nov, Dec);
    function Short2Num (Str : String) return String is
      Month : Short_Name_List;
    begin
      -- Convert to month and check casing
      Month := Short_Name_List'Value (Str);
      if Str /= Mixed_Str (Month'Img) then
        raise Constraint_Error;
      end if;
      return Normal (Short_Name_List'Pos (Month) + 1, 2, Gap => '0');
    end Short2Num;

    function Num2Short (Str : String) return String is
      Num : constant Natural := Gets.Get_Int (Str) - 1;
    begin
      return Mixed_Str (Short_Name_List'Image (Short_Name_List'Val (Num)));
    end Num2Short;
  end Conv;

  -- The structures describing the formats
  -- A field is either of kind num or enum
  type Field_Kind is (Num, Enum);
  -- How many fields are defined for format
  subtype Field_Range is Positive range 1 .. 10;
  -- A field
  type Field_Rec (Kind : Field_Kind := Num) is record
    -- The character that represents the field in Format (after '%')
    Char : Character;
    -- The index in Date_Rec
    Index : Indexes.Index_Range;
    -- The Scanner format
    Scanner_Format : As.U.Asu_Us;
    case Kind is
       when Num =>
         -- The min and max values
         Min, Max : Natural;
       when Enum =>
         -- The numeric equivalent field
         Target : Field_Range;
         -- The scanning and putting conversion function
         Conv_Scan, Conv_Put : Conv.Func;
    end case;
  end record;

  -- The fields in the Format
  Fields : constant array (Field_Range) of Field_Rec := (
    1 => (Num,  'Y', 1, As.U.Tus ("4l"), Day_Mng.T_Years'First,
                                         Day_Mng.T_Years'Last),
    2 => (Num,  'm', 2, As.U.Tus ("2l"), Day_Mng.T_Months'First,
                                         Day_Mng.T_Months'Last),
    3 => (Enum, 'b', 2, As.U.Tus ("3i"), 2, Conv.Short2Num'Access,
                                            Conv.Num2Short'Access),
    4 => (Enum, 'B', 2, As.U.Tus ("i"), 2, Conv.Long2Num'Access,
                                           Conv.Num2Long'Access),
    5 => (Num,  'd', 3, As.U.Tus ("2l"), Day_Mng.T_Days'First,
                                         Day_Mng.T_Days'Last),
    6 => (Num,  'H', 4, As.U.Tus ("2l"), Day_Mng.T_Hours'First,
                                         Day_Mng.T_Hours'Last),
    7 => (Num,  'M', 5, As.U.Tus ("2l"), Day_Mng.T_Minutes'First,
                                         Day_Mng.T_Minutes'Last),
    8 => (Num,  'S', 6, As.U.Tus ("2l"), Day_Mng.T_Seconds'First,
                                         Day_Mng.T_Seconds'Last),
    9 => (Num,  's', 7, As.U.Tus ("3l"), Day_Mng.T_Millisec'First,
                                         Day_Mng.T_Millisec'Last),
   10 => (Num,  'u', 8, As.U.Tus ("3l"), Day_Mng.T_Millisec'First,
                                         Day_Mng.T_Millisec'Last)
  );
  type Fields_Array is array (Positive range <>) of Field_Range;
  package Unbounded_Fields_Array_Mng is new Unbounded_Arrays (
      Field_Range, Fields_Array);
  subtype Field_Sequence is Unbounded_Fields_Array_Mng.Unb_Array;

  -- Init the trace logger if necessary
  procedure Init_Logger is
  begin
    if not Logger.Is_Init then
      Logger.Init ("Date_Text");
    end if;
  end Init_Logger;

  -- Find the field by char
  function Field_Of (Char : Character) return Field_Range is
  begin
    for I in Field_Range loop
      if Char = Fields(I).Char then
        return I;
      end if;
    end loop;
    Logger.Log_Debug ("Unknown Esc >" & Char & "<");
    raise Invalid_Format;
  end Field_Of;

  -- Get length of a field => 0 if unknwon
  function Get_Length (Fi : Field_Range) return Natural is
    Char : Character;
  begin
    -- Format start with a digit?
    Char := Fields(Fi).Scanner_Format.Element (1);
    if Char in '0' .. '9' then
      return Character'Pos (Char) - Character'Pos ('0');
    else
      return 0;
    end if;
  end Get_Length;

  -- Check a Format, raises Invalid_Format
  Esc : constant Character := '%';
  procedure Check_Format (Format : in String;
                          Scan_Fmt : out As.U.Asu_Us;
                          Flds : out Field_Sequence) is
    Ind : Positive;
    Char : Character;
    Fi : Field_Range;
    use type As.U.Asu_Us;
  begin
    Logger.Log_Debug ("Check_Format >" & Format & "<");
    Scan_Fmt.Set_Null;
    Flds.Set_Null;
    -- Look for Esc sequences
    Ind := Format'First;
    while Ind <= Format'Last loop
      if Format(Ind) = Esc then
        if Ind = Format'Last then
          -- Tailing %
          Logger.Log_Debug ("Tailing Esc");
          raise Invalid_Format;
        end if;
        -- Check this is a known Esc sequence
        Ind := Ind + 1;
        Char := Format(Ind);
        if Char = Esc then
          Scan_Fmt.Append (Scanner.Esc & Scanner.Esc);
        else
          Fi := Field_Of (Char);
          -- Check that, if Field has unknown length, then it is not immediately
          --  followed by an Esc (except EscEsc) nor by a lower-case letter
          if Get_Length (Fi) = 0
          and then Ind + 2 <= Format'Last then
            if Format(Ind + 1) = Esc and then Format(Ind + 2) /= Esc then
               Logger.Log_Debug ("Esc " & Char & " followed by "
                               & Format(Ind + 1 .. Ind + 2));
              raise Invalid_Format;
            elsif Format(Ind + 1) in 'a' .. 'z' then
              Logger.Log_Debug ("Esc " & Char & " followed by "
                              & Format(Ind + 1));
              raise Invalid_Format;
            end if;
          end if;
          Scan_Fmt.Append (Scanner.Esc & Fields(Fi).Scanner_Format);
          Flds.Append (Fi);
        end if;
      else
        Scan_Fmt.Append (Format(Ind));
      end if;
      Ind := Ind + 1;
    end loop;
  end Check_Format;

  -- Scan a String at a given Format
  -- Any field of Date_Rec that is not set n Format is set to its default
  -- Raise Invalid_Format if the format is not valid (invalid %X or tailing %,
  --  several occurences of the same field)
  -- Raise Invalid_String if the string does not match the format
  function Scan (Str : String; Format : String) return Date_Rec is
    -- The format for Scanner
    Scan_Fmt : As.U.Asu_Us;
    -- The list of field descriptors
    Flds : Field_Sequence;
    -- The result of scanning
    Anys : Scanner.Any_Sequence;
    -- The target field index
    Target : Field_Range;
    -- The fields in Result that are already set
    Set : array (Field_Range) of Boolean := (others => False);
    -- Extracted value
    Text : As.U.Asu_Us;
    Val : Natural;
    -- The result
    Result : Date_Rec;
    use type Conv.Func;
  begin
    -- Init and check
    Init_Logger;
    Logger.Log_Debug ("Scanning >" & Str & "< with format >" & Format & "<");
    Check_Format (Format, Scan_Fmt, Flds);
    if Format = "" then
      if Str = "" then
        return Result;
      else
        Logger.Log_Debug ("Expecting empty string");
        raise Invalid_String;
      end if;
    end if;
    -- Check validity of scanning format
    Logger.Log_Debug ("Scan format >" & Scan_Fmt.Image & "<");
    declare
      Dummy_Len : Natural;
    begin
      Dummy_Len := Scanner.Length (Scan_Fmt.Image);
    exception
      when Scanner.Invalid_Format =>
        Logger.Log_Debug ("Invalid scanning format");
        raise Invalid_Format;
      when Scanner.Unknown_Length =>
        null;
    end;

    -- Scan
    Anys := Scanner.Scan (Str, Scan_Fmt.Image);
    if Anys.Length /= Flds.Length then
      Logger.Log_Debug ("Got" & Anys.Length'Img
                      & " Anys for" & Flds.Length'Img & " Flds");
      raise Invalid_String;
    end if;
    for I in 1 .. Anys.Length loop
      -- Get the val
      Target := Flds.Element(I);
      if Fields(Flds.Element(I)).Kind = Enum then
        begin
          -- Convert into num string then to val
          Text := As.U.Tus (Fields(Target).Conv_Scan (
              Any_Def.Image (Anys.Element(I))));
          Logger.Log_Debug ("  Converted text >" & Text.Image & "<");
          Val := Gets.Get_Int (Text.Image);
        exception
          when Constraint_Error =>
            Logger.Log_Debug ("Exception when converting " & Text.Image);
            raise Invalid_String;
        end;
        Target := Fields(Flds.Element(I)).Target;
      else
        begin
          -- Convert to val
          Val := Natural (Anys.Element(I).Lint);
        exception
          when Constraint_Error =>
            Logger.Log_Debug ("Invalid num " & Text.Image);
            raise Invalid_String;
        end;
      end if;

      -- Check value
      if Val < Fields(Target).Min or else Val > Fields(Target).Max then
        Logger.Log_Debug ("Invalid value " & Images.Integer_Image (Val));
        raise Invalid_String;
      end if;

      -- Check, if already set, that same value
      if Set (Fields(Target).Index)
      and then Val /= Indexes.Get (Result, Fields(Target).Index) then
          Logger.Log_Debug ("New value " & Images.Integer_Image (Val)
              & " differs from previous "
              & Images.Integer_Image (Indexes.Get (Result, Fields(Target).Index)));
          raise Invalid_String;
        end if;
        -- Store value and set
        Indexes.Set (Result, Fields(Target).Index, Val);
        Set (Fields(Target).Index) := True;
    end loop;
    -- Done
    return Result;
  end Scan;

  -- Put a date at a given format
  -- Raise Invalid_Format if the format is not valid (invalid %X or tailing %)
  function Put (Date : Date_Rec; Format : String) return String is
    -- Dummy format for Scanner and the list of field descriptors
    Dummy_Scan_Fmt : As.U.Asu_Us;
    Dummy_Flds : Field_Sequence;
    -- Index in Format
    Ifor : Positive;
    -- Is current char a field
    Is_Field : Boolean;
    Fi : Field_Range;
    -- Tempo extracted text
    Text : As.U.Asu_Us;
    -- Extracted value
    Val : Natural;
    -- The result
    Result : As.U.Asu_Us;
    use type Conv.Func;
  begin
    -- Init
    Init_Logger;
    Check_Format (Format, Dummy_Scan_Fmt, Dummy_Flds);
    -- Scan the format
    Ifor := Format'First;
    while  Ifor <= Format'Last loop
      -- Check if this is supposed to be a field
      if Format(Ifor) = Esc then
        if Format(Ifor + 1) = Esc then
          -- %% in Format -> % in Str
          Ifor := Ifor + 1;
          Is_Field := False;
        else
          -- A field to scan
          Ifor := Ifor + 1;
          Is_Field := True;
        end if;
      else
        Is_Field := False;
      end if;
      if Is_Field then
        Fi := Field_Of (Format(Ifor));
        -- Get the value
        Val := Indexes.Get (Date, Fields(Fi).Index);
        if Fields(Fi).Kind = Enum then
          -- Apply conversion
          Text := As.U.Tus (Images.Integer_Image (Val));
          Text := As.U.Tus (Fields(Fi).Conv_Put (Text.Image));
        else
          -- Put num pad with 0
          Text := As.U.Tus (Normal (Val, Get_Length (Fi), Gap => '0'));
        end if;
        -- Put text
        Result.Append (Text);
      else
        -- Put char
        Result.Append (Format(Ifor));
      end if;

      Ifor := Ifor + 1;
    end loop;
    -- Done
    return Result.Image;
  end Put;

  -- Compute the lenght of text generated by a format
  -- Raise Invalid_Format if the format is not valid
  -- Raise Unknown_Length if the format refers to %B (variable length)
  function Length (Format : String) return Natural is
    -- Dummy format for Scanner and the list of field descriptors
    Dummy_Scan_Fmt : As.U.Asu_Us;
    Dummy_Flds : Field_Sequence;
    -- Index in Format
    Ifor : Positive;
    -- Is current char a field
    Is_Field : Boolean;
    Fi : Field_Range;
    -- The result
    Result : Natural;
    use type Conv.Func;
  begin
    -- Init
    Init_Logger;
    Check_Format (Format, Dummy_Scan_Fmt, Dummy_Flds);
    -- Scan the format
    Ifor := Format'First;
    Result := 0;
    while Ifor <= Format'Last loop
      -- Check if this is supposed to be a field
      if Format(Ifor) = Esc then
        if Format(Ifor + 1) = Esc then
          -- %% in Format -> % in Str
          Ifor := Ifor + 1;
          Is_Field := False;
        else
          -- A field to scan
          Ifor := Ifor + 1;
          Is_Field := True;
        end if;
      else
        Is_Field := False;
      end if;
      if Is_Field then
        Fi := Field_Of (Format(Ifor));
        -- Get the value
        if Get_Length (Fi) = 0 then
          raise Unknown_Length;
        end if;
        Result := Result + Get_Length(Fi);
      else
        Result := Result + 1;
      end if;

      Ifor := Ifor + 1;
    end loop;
    -- Done
    return Result;
  end Length;

end Date_Text;

