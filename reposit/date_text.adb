with Trace.Loggers, Mixed_Str, Normal, Images, Gets, As.U, Str_Util,
     Regular_Expressions, Perpet;
package body Date_Text is

  -- Logger
  Logger : Trace.Loggers.Logger;

  -- Maps fields of Date_Rec from/to indexes
  package Indexes is
    -- Indexes of fields: 1 is year .. 7 is milliseconds
    subtype Index_Range is Positive range 1 .. 7;
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
        when 7 => Val.Millisec);
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
  subtype Field_Range is Positive range 1 .. 9;
  -- A field
  type Field_Rec (Kind : Field_Kind := Num) is record
    -- The character that represents the field in Format (after '%')
    Char : Character;
    -- The index in Date_Rec
    Index : Indexes.Index_Range;
    -- The length  of the field (0 if unknown)
    Length : Natural;
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
    1 => (Num,  'Y', 1, 4, Ada.Calendar.Year_Number'First,
                           Ada.Calendar.Year_Number'Last),
    2 => (Num,  'm', 2, 2, Ada.Calendar.Month_Number'First,
                           Ada.Calendar.Month_Number'Last),
    3 => (Enum, 'b', 2, 3, 2, Conv.Short2Num'Access, Conv.Num2Short'Access),
    4 => (Enum, 'B', 2, 0, 2, Conv.Long2Num'Access,  Conv.Num2Long'Access),
    5 => (Num,  'd', 3, 2, Ada.Calendar.Day_Number'First,
                           Ada.Calendar.Day_Number'Last),
    6 => (Num,  'H', 4, 2, Day_Mng.T_Hours'First,    Day_Mng.T_Hours'Last),
    7 => (Num,  'M', 5, 2, Day_Mng.T_Minutes'First,  Day_Mng.T_Minutes'Last),
    8 => (Num,  'S', 6, 2, Day_Mng.T_Seconds'First,  Day_Mng.T_Seconds'Last),
    9 => (Num,  's', 7, 3, Day_Mng.T_Millisec'First, Day_Mng.T_Millisec'Last)
  );

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

  -- Check a Format, raises Invalid_Format
  Esc : constant Character := '%';
  procedure Check_Format (Format : String) is
    Ind : Positive;
    Char : Character;
    Fi : Field_Range;
  begin
    Logger.Log_Debug ("Check_Format >" & Format & "<");
    -- Look for Esc sequences
    Ind := Format'First;
    loop
      if Format(Ind) = Esc then
        if Ind = Format'Last then
          -- Tailing %
          Logger.Log_Debug ("Tailing Esc");
          raise Invalid_Format;
        end if;
        -- Check this is a known Esc sequence
        Ind := Ind + 1;
        Char := Format(Ind);
        if Char /= Esc then
          Fi := Field_Of (Char);
          -- Check that, if Field has unknonw length, then it is not immeditely
          --  followed by an Esc
          if Fields(Fi).Length = 0
          and then Ind + 2 <= Format'Last
          and then Format(Ind + 1) = Esc
          and then Format(Ind + 2) /= Esc then
             Logger.Log_Debug ("Esc " & Char & " followed by "
                             & Format(Ind + 1 .. Ind + 2));
            raise Invalid_Format;
          end if;
        end if;
      end if;
      exit when Ind = Format'Last;
      Ind := Ind + 1;
    end loop;
  end Check_Format;

  -- Scan a String at a given Format
  -- Any field of Date_Rec that is not set n Format is set to its default
  -- Raise Invalid_Format if the format is not valid (invalid %X or tailing %,
  --  several occurences of the same field)
  -- Raise Invalid_String if the string does not match the format
  function Scan (Str : String; Format : String) return Date_Rec is
    -- The fields in Result that are already set
    Set : array (Field_Range) of Boolean := (others => False);
    -- Indexes in Str and Format
    Istr, Ifor : Positive;
    -- Is current char a field
    Is_Field : Boolean;
    Fi : Field_Range;
    -- Tempo extracted text
    Text : As.U.Asu_Us;
    -- Expected character, and its index in Str
    Next_Char : Character;
    Next_Index : Natural;
    -- Extracted value
    Val : Natural;
    -- The result
    Result : Date_Rec;
    use type Conv.Func;
  begin
    -- Init
    Init_Logger;
    Check_Format (Format);
    -- Scan the format and the string
    Ifor := Format'First;
    Istr := Str'First;
    loop
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
        -- Find the field by char
        Fi := Field_Of (Format(Ifor));
        -- Scan text
        if Fields(Fi).Length /= 0 then
          -- Scan len chars (check enough Str length)
          if Istr > Str'Last - Fields(Fi).Length  + 1 then
            Logger.Log_Debug ("Str is too long for " & Fields(Fi).Char);
            raise Invalid_String;
          end if;
          Text := As.U.Tus (Str (Istr .. Istr + Fields(Fi).Length  - 1));
        else
          -- Length unknown, depending on format, up to non-field or end
          if Ifor = Format'Last then
            -- End of Format (and of Str)
            Text := As.U.Tus (Str (Istr .. Str'Last));
          else
            -- Format defines the following character
            Next_Char := Format(Ifor + 1);
            Next_Index := Str_Util.Locate (Str, Next_Char & "", Ifor + 1);
            if Next_Index = 0 then
              Logger.Log_Debug ("Str does not provide expected " & Next_Char);
              raise Invalid_String;
            end if;
            Text := As.U.Tus (Str (Istr .. Next_Index - 1));
          end if;
        end if;
        Logger.Log_Debug ("  Parsed text >" & Text.Image
                        & "< for %" & Format(Ifor));
        -- Increment in Str to point to last char read
        Istr := Istr + Text.Length  - 1;
        -- Handle indirection to a Num field
        if Fields(Fi).Kind = Enum then
          begin
            Text := As.U.Tus (Fields(Fi).Conv_Scan (Text.Image));
          exception
            when Constraint_Error =>
              Logger.Log_Debug ("Exception when converting " & Text.Image);
              raise Invalid_String;
          end;
          Fi := Fields(Fi).Target;
          Logger.Log_Debug ("  Converted text >" & Text.Image & "<");
        end if;
        -- Check length of digits
        if Fields(Fi).Length /= 0
        and then not Regular_Expressions.Match (
            "[0-9]{" & Images.Integer_Image (Fields(Fi).Length) & "}",
            Text.Image, True) then
          Logger.Log_Debug ("Invalid num " & Text.Image);
          raise Invalid_String;
        end if;
        -- Convert to val
        Val := Gets.Get_Int (Text.Image);
        -- Check value
        if Val < Fields(Fi).Min or else Val > Fields(Fi).Max then
          Logger.Log_Debug ("Invalid value " & Images.Integer_Image (Val));
          raise Invalid_String;
        end if;
        -- Check, if already set, that same value
        if Set (Fields(Fi).Index)
        and then Val /= Indexes.Get (Result, Fields(Fi).Index) then
          Logger.Log_Debug ("New value " & Images.Integer_Image (Val)
              & " differs from previous "
              & Images.Integer_Image (Indexes.Get (Result, Fields(Fi).Index)));
          raise Invalid_String;
        end if;
        -- Store value and set
        Indexes.Set (Result, Fields(Fi).Index, Val);
        Set (Fields(Fi).Index) := True;
      elsif Str(Istr) /= Format(Ifor) then
        -- Check that non-field character matches
        Logger.Log_Debug ("Str(" & Images.Integer_Image (Istr)
            & ")=" & Str(Istr)
            & " /= Fmt(" & Images.Integer_Image (Ifor)
            & ")=" & Format(Ifor));
        raise Invalid_String;
      end if;

      -- Chek end conditions
      if Ifor = Format'Last then
        -- Check that end of Str
        if Istr = Str'Last then
          exit;
        else
          Logger.Log_Debug ("Str longer that Format");
          raise Invalid_String;
        end if;
      end if;
      if Istr = Str'Last and then Ifor /= Format'Last then
        -- Check that end of format
        Logger.Log_Debug ("Str shorter that Format");
        raise Invalid_String;
      end if;
      Ifor := Ifor + 1;
      Istr := Istr + 1;
    end loop;
    -- Done
    return Result;
  end Scan;

  -- Put a date at a given format
  -- Raise Invalid_Format if the format is not valid (invalid %X or tailing %)
  function Put (Date : Date_Rec; Format : String) return String is
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
    Check_Format (Format);
    -- Scan the format
    Ifor := Format'First;
    loop
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
          Text := As.U.Tus (Normal (Val, Fields(Fi).Length, Gap => '0'));
        end if;
        -- Put text
        Result.Append (Text);
      else
        -- Put char
        Result.Append (Format(Ifor));
      end if;

      -- Chek end conditions
      exit when Ifor = Format'Last;
      Ifor := Ifor + 1;
    end loop;
    -- Done
    return Result.Image;
  end Put;

  -- Compute the lenght of text generated by a format
  -- Raise Invalid_Format if the format is not valid
  -- Raise Unknown_Length if the format refers to %B (variable length)
  function Length (Format : String) return Natural is
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
    Check_Format (Format);
    -- Scan the format
    Ifor := Format'First;
    Result := 0;
    loop
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
        if Fields(Fi).Length = 0 then
          raise Unknown_Length;
        end if;
        Result := Result + Fields(Fi).Length;
      else
        Result := Result + 1;
      end if;

      -- Chek end conditions
      exit when Ifor = Format'Last;
      Ifor := Ifor + 1;
    end loop;
    -- Done
    return Result;
  end Length;

end Date_Text;

