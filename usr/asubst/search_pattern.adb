with Ada.Characters.Latin_1, Ada.Strings.Unbounded, Utf_8;
with Sys_Calls, Argument, Unique_List, String_Mng, Text_Line, Debug,
     Char_To_Hexa;
package body Search_Pattern is

  package Asu renames Ada.Strings.Unbounded;

  -- 0 to 16 substring indexes
  subtype Substr_Array is Regular_Expressions.Match_Array (Nb_Sub_String_Range);

  -- Unique list of patterns
  type Line_Pat_Rec is record
    -- Pattern number: 1, 2...
    Num : Positive;
    -- Is it a Delim (\n) or a string/regex
    Is_Delim : Boolean;
    -- Regex or string to search (depending on Is_regex)
    Pat : Regular_Expressions.Compiled_Pattern;
    Find_Str : Asu.Unbounded_String;
    -- The complete string from input flow that matches
    Match_Str : Asu.Unbounded_String;
    -- Number of sub-matching strings
    Nb_Substr : Nb_Sub_String_Range := 0;
    -- Indexes of matching string (at index 0),
    --  then indexes of sub-matching strings
    Substrs : Substr_Array := (others => (0, 0));
  end record;
  procedure Set (To : out Line_Pat_Rec; Val : in Line_Pat_Rec) is
  begin
    -- Setting a pattern (in Parse) for use in Check
    To.Num := Val.Num;
    To.Is_Delim := Val.Is_Delim;
    -- Regexp cannot be copied, so it will be assigned
    --  to the Line_Pat_Rec access
    -- To.Pat := Val.Pat;
    To.Find_Str := Val.Find_Str;
    -- The following are not required because set by Check and 
    --  later on read by Substring... always by access
    To.Match_Str := Val.Match_Str;
    To.Nb_Substr := Val.Nb_Substr;
    To.Substrs := Val.Substrs;
  end Set;
  -- Unicity of pattern num
  function Image (Line_Pat : Line_Pat_Rec) return String is
  begin
    return Line_Pat.Num'Img;
  end Image;
  function "=" (Current : Line_Pat_Rec; Criteria : Line_Pat_Rec)
               return Boolean is
  begin
    -- Unicity of Num
    return Current.Num = Criteria.Num;
  end "=";
  package Unique_Pattern is new Unique_List (Line_Pat_Rec, Set, Image, "=");
  -- True only after Check(Number) called and OK.
  Check_Completed : Boolean := False;
  Expected_Index : Positive := 1;

  -- The patterns ans associated check results
  Pattern_List : Unique_Pattern.List_Type;

  -- True if one unique pattern and with no '^' nor '$'
  Is_Multiple : Boolean;

  -- True if utf8 encoding
  Utf8 : Boolean;

  -- True if find pattern is regexes
  Is_Regex : Boolean;

  -- Line_Feed String
  Line_Feed : constant String := Text_Line.Line_Feed & "";

  -- Reports a parsing error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR parsing search pattern: "
        & Msg & ".");
    raise Parse_Error;
  end Error;

  -- Add a line pattern
  procedure Add (Crit : in String;
                 Extended, Case_Sensitive : in Boolean) is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    Ok : Boolean;
  begin
    -- Compute new pattern number and type
    Upat.Num := Unique_Pattern.List_Length (Pattern_List) + 1;
    Upat.Is_Delim := Crit = "";
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search adding regex "
             &  Upat.Num'Img & " >" & Crit & "<");
    end if;
    -- Empty pattern is a delimiter
    if Upat.Is_Delim then
      -- Insert delimiter
      Unique_Pattern.Insert (Pattern_List, Upat);
      return;
    end if;
    -- Store string if this is not a regex
    if not Is_regex then
      Upat.Find_Str := Asu.To_Unbounded_String (Crit);
      Upat.Nb_Substr := 0;
      Unique_Pattern.Insert (Pattern_List, Upat);
      return;
    end if;
    -- Regex: Count max number of substrings, i.e. number of '('
    for I in Sub_String_Range loop
      -- Locate succcessive occurences of "("
      exit when String_Mng.Locate (Crit, Crit'First, "(", I) = 0;
      Upat.Nb_Substr := I;
    end loop;
    -- Regex compiled patterns cannot be copied and Substrs are used later
    -- Insert a pattern without compiled pattern and substrs
    Unique_Pattern.Insert (Pattern_List, Upat);
    -- Get access to it and compile in this access
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    Regular_Expressions.Compile (Upat_Access.Pat, Ok, Crit,
                                 Extended => Extended,
                                 Case_Sensitive => Case_Sensitive);
    if not Ok then
      Error ("Invalid pattern """ & Crit
        & """. Error is " & Regular_Expressions.Error(Upat_Access.Pat));
    end if;
  end Add;

  -- Check that the string does not contain the fragment
  procedure Check (Str : in String; Frag : in String) is
  begin
    if String_Mng.Locate (Str, Str'First, Frag) /= 0 then
      Error ("Pattern """ & Str & """ cannot contain """ & Frag & """");
    end if;
 end Check;

  -- Start line and stop line strings in regex
  function Start_String (Delim : in Boolean) return String is
  begin
    if Delim then return "^";
    else return "";
    end if;
  end Start_String;
  function Stop_String (Delim : in Boolean) return String is
  begin
    if Delim then return "$";
    else return "";
    end if;
  end Stop_String;

  -- Parses the search patern
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String;
                   Extended, Case_Sensitive, Utf8, Is_Regex : in Boolean) is

    The_Pattern : Asu.Unbounded_String;

    -- Check and get an hexa code from The_Pattern (Index .. Index + 1)
    subtype Byte is Natural range 0 .. 255;
    function Get_Hexa (Index : Positive) return Byte is
     Result : Byte;
    begin
      -- First digit: 16 * C
      if Index > Asu.Length (The_Pattern) then
        Error ("No hexadecimal sequence at the end of search pattern");
      end if;
      begin
        Result := 16#10# * Char_To_Hexa (Asu.Element (The_Pattern, Index));
      exception
        when Constraint_Error =>
          Error ("Invalid hexadecimal sequence "
               & Asu.Slice (The_Pattern, Index, Index + 1)
               & " in search pattern");
      end;
      -- First digit: 1 * C
      if Index + 1 > Asu.Length (The_Pattern) then
        Error ("Uncomplete hexadecimal sequence at the end of search pattern");
        raise Parse_Error;
      end if;
      begin
        Result := Result + Char_To_Hexa (Asu.Element (The_Pattern, Index + 1));
      exception
        when Constraint_Error =>
          Error ("Invalid hexadecimal sequence "
               & Asu.Slice (The_Pattern, Index, Index + 1)
               & " in search pattern");
      end;
      if Result = 0 and then Is_regex then
        Error ("Invalid null hexadecimal sequence in regex"
             & Asu.Slice (The_Pattern, Index, Index + 1)
             & " in search pattern");
      end if;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, got hexadecimal sequence "
                                 & Asu.Slice (The_Pattern, Index, Index + 1));
      end if;
      return Result;
    end Get_Hexa;

    -- Returns character class (e.g. [:alnum:] associated to a key char
    --  or "\Key"
    function Char_Class_Of (Key : Character) return String is
    begin
      if not Is_regex then
        -- If not a regex, no interpretation
        return '\' & Key;
      end if;
      case Key is
        when 'M' => return "[:alnum:]";
        when 'A' => return "[:alpha:]";
        when 'B' => return "[:blank:]";
        when 'C' => return "[:cntrl:]";
        when 'D' => return "[:digit:]";
        when 'G' => return "[:graph:]";
        when 'L' => return "[:lower:]";
        when 'P' => return "[:print:]";
        when 'T' => return "[:punct:]";
        when 'S' => return "[:space:]";
        when 'U' => return "[:upper:]";
        when 'X' => return "[:xdigit:]";
        when others => return '\' & Key;
      end case;
    end Char_Class_Of;

    -- Indexes in Pattern
    Start_Index : Positive;
    Stop_Index : Natural;
    -- Was previous item a delimiter
    Prev_Delim : Boolean;
    -- Is next item a delimiterA (true except at the end)
    Next_Delim : Boolean;
  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search parsing pattern >" & Pattern & "<");
    end if;
    -- Reset pattern characteristics
    The_Pattern := Asu.To_Unbounded_String (Pattern);
    Unique_Pattern.Delete_List (Pattern_List);
    Is_Multiple := False;
    Check_Completed := False;
    Expected_Index := 1;
    Search_Pattern.Utf8 := Utf8;
    Search_Pattern.Is_Regex := Is_Regex;
    -- Reject empty pattern
    if Pattern = "" then
      Error ("Empty pattern");
    end if;

    -- Replace escape sequences (\n, \t and \xIJ) in the pattern
    Stop_Index := 1;
    loop
      -- Locate sequence
      Stop_Index := String_Mng.Locate_Escape (Asu.To_String (The_Pattern),
                               Stop_Index,
                               "\nstxABCDEFGHIJKLMNOPQRSTUVWXYZ");
      exit when Stop_Index = 0;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, found Esc char >"
                                & Asu.Element (The_Pattern, Stop_Index) & "<");
      end if;
      -- Replace sequence
      case Asu.Element (The_Pattern, Stop_Index) is
        when 'n' =>
          -- "\n" replaced by line_feed
          Asu.Replace_Slice (The_Pattern, Stop_Index - 1, Stop_Index, Line_Feed);
        when 's' =>
          -- "\s" replaced by space
          Asu.Replace_Slice (The_Pattern, Stop_Index - 1, Stop_Index, " ");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          Asu.Replace_Slice (The_Pattern, Stop_Index - 1, Stop_Index,
                             Ada.Characters.Latin_1.Ht & "");
        when 'x' =>
          -- "\xIJ" hexa replaced by byte or "\n"
          Asu.Replace_Slice (The_Pattern, Stop_Index - 1, Stop_Index + 2,
                             Character'Val (Get_Hexa (Stop_Index + 1)) & "");
        when 'A' .. 'Z' =>
          -- Some \A to \Z replaced by [:<CharClass>:]
          declare
            Class : constant String := Char_Class_Of (
                    Asu.Element (The_Pattern, Stop_Index));
          begin
            Asu.Replace_Slice (The_Pattern, Stop_Index - 1, Stop_Index, Class);
            Stop_Index := Stop_Index + Class'Length - 1;
          end;
        when others =>
          -- Impossible. Leave sequence as it is, skip it
          Stop_Index := Stop_Index + 1;
      end case;
      exit when Stop_Index >= Asu.Length (The_Pattern);
    end loop;

    -- Locate all Line_Feed and split pattern (one per line)
    Start_Index := 1;
    Prev_Delim := False;
    loop
      Stop_Index := String_Mng.Locate (Asu.To_String (The_Pattern),
                                       Start_Index,
                                       Line_Feed);
      if Stop_Index = Start_Index then
        -- A Delim
        Add ("", Extended, Case_Sensitive);
        Prev_Delim := True;
      else
        -- A Regex: see if it followed by a delim (always except at the end)
        Next_Delim := Stop_Index /= 0;
        -- Make Stop_Index be the last index of regex,
        if Stop_Index = 0 then
          Stop_Index := Asu.Length (The_Pattern);
        else
          -- This delim will be located at next iteration of the loop
          Stop_Index := Stop_Index - 1;
        end if;
        declare
          Slice : constant String
                := Asu.Slice (The_Pattern, Start_Index, Stop_Index);
        begin
          if Is_regex then
            -- It must not contain Start_String if preeceded by a delim
            Check (Slice, Start_String (Prev_Delim));
            -- It must not contain Stop_String if preeceded by a delim
            Check (Slice, Stop_String (Next_Delim));
          end if;
          -- Add this regex
          Add (Start_String (Prev_Delim) & Slice & Stop_String (Next_Delim),
             Extended, Case_Sensitive);
        end;
        if Is_Regex then
          -- See if this is a single regex and if it can apply several times
          --  to one line of input (no ^ not $)
          if not Prev_Delim
             and then not Next_Delim
             and then Asu.Element (The_Pattern, Start_Index) & ""
                 /= Start_String (True)
             and then Asu.Element (The_Pattern, Stop_Index) & ""
                 /= Stop_String (True) then
            Is_Multiple := True;
          end if;
        else
          -- Same, but only check delimiters (start and stop strings are
          --  not interpreted) 
          if not Prev_Delim
             and then not Next_Delim then
            Is_Multiple := True;
          end if;
        end if;
      end if;
      -- Done
      exit when Stop_Index = Asu.Length (The_Pattern);
      Start_Index := Stop_Index + 1;
    end loop;

    -- Done
  exception
    when Parse_Error =>
      -- Free previous pattern
      Unique_Pattern.Delete_List (Pattern_List);
      raise;
  end Parse;

  -- Returns the number of lines that it covered by the
  --  search pattern (one per regex and one per New_Line.
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive is
    N : constant Natural := Unique_Pattern.List_Length (Pattern_List);
  begin
    if N = 0 then
      raise No_Regex;
    else
      return N;
    end if;
  end Number;

  -- Tells if the search pattern can be applied several times
  --  on one line of input (i.e. does not contain '\n', '^' or '$'
  -- Raises No_Regex if the pattern was not parsed OK
  function Multiple return Boolean is
  begin
    -- Must be some pattern compiled
    if Unique_Pattern.List_Length (Pattern_List) = 0 then
      raise No_Regex;
    end if;
    return Is_Multiple;
  end Multiple;

  -- Returns the number of substrings of one regex
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  function Nb_Substrings (Regex_Index : Positive) return Nb_Sub_String_Range is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
  begin
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    return Upat_Access.Nb_Substr;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Nb_Substrings;

  -- Checks if the input string from Start to its end
  --  matches the regex no Regex_Index
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (retruned by Parse)
  function Check (Str : String; Start : Positive;
                  Regex_Index : Positive) return Boolean is
    -- The pattern to check with
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    -- Check result
    Nmatch : Natural;
    Match : Regular_Expressions.Match_Array
               (1 .. Nb_Sub_String_Range'Last + 1);
  begin
    -- Check that this index follows previous
    if Regex_Index /= Expected_Index then
      raise No_Regex;
    end if;
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    -- Check not completed by default
    Check_Completed := False;
    -- Reset substring array if not a delim
    if not Upat_Access.Is_Delim then
      Upat_Access.Substrs := (others => (0, 0));
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search check pattern No " & Regex_Index'Img);
    end if;
    -- Delimiter matches delimiter
    if Upat_Access.Is_Delim then
      if Str = Line_Feed then
        Upat_Access.Nb_Substr := 0;
        Upat_Access.Substrs(0) := (1, 1);
        Upat_Access.Match_Str := Asu.To_Unbounded_String (Line_Feed);
        if Regex_Index = Unique_Pattern.List_Length (Pattern_List) then
          -- Last pattern and matches
          Check_Completed := True;
          Expected_Index := 1;
        else
          Expected_Index := Expected_Index + 1;
        end if;
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Search check pattern is delim vs delim");
        end if;
        return True;
      else
        Expected_Index := 1;
        if Debug.Set then
          Sys_Calls.Put_Line_Error (
                    "Search check pattern is delim vs not delim");
        end if;
        return False;
      end if;
    elsif Str = Line_Feed then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern is not delim vs delim");
      end if;
      Expected_Index := 1;
      return False;
    else
      if Debug.Set then
        Sys_Calls.Put_Line_Error (
                  "Search check pattern is not delim vs not delim");
      end if;
      -- Check. Note that indexes will relative to Str
      if Is_Regex then
        Regular_Expressions.Exec (Upat_Access.Pat,
                                  Str(Start .. Str'Last),
                                  Nmatch, Match);
      else
        -- Not a regex, locate string
        Nmatch := String_Mng.Locate (Str, Start,
                     Asu.To_String (Upat_Access.Find_Str));
        if Nmatch /= 0 then
          -- Fill matching info as if from a regex
          Match (1) := (
             Start_Offset => Nmatch,
             End_Offset   => Nmatch + Asu.Length (Upat_Access.Find_Str) - 1);
          Nmatch := 1;
        end if;                           
      end if;
      if Nmatch >= 1 and then Match(1).Start_Offset <= Match(1).End_Offset then
        -- Copy the slice of substrings
        Upat_Access.Nb_Substr := Nmatch - 1;
        Upat_Access.Substrs(0) := Match(1);
        Upat_Access.Substrs(1 .. Upat_Access.Nb_Substr)
                   := Match(2 .. Nmatch);
        Upat_Access.Match_Str := Asu.To_Unbounded_String (Str);
        if Regex_Index = Unique_Pattern.List_Length (Pattern_List) then
          -- Last pattern and matches
          Check_Completed := True;
          Expected_Index := 1;
        else
          Expected_Index := Expected_Index + 1;
        end if;
        return True;
      else
        -- Not match
        Expected_Index := 1;
        return False;
      end if;
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Check;

  -- Check if Char is the startup of a valid UTF-8 sequence
  --  and increment Offset accordingly
  procedure Apply_Utf8 (Char : in Character;
                        Offset : in out Regular_Expressions.Offset_Range) is
    Len : Utf_8.Len_Range;
  begin
    -- Get lenght of sequence
    Len := Utf_8.Nb_Chars (Char);
    -- Apply offset
    Offset := Offset + Len - 1;
  exception
    when Utf_8.Invalid_Sequence =>
      -- Leave Offset unchanged
      null;
  end Apply_Utf8;

  -- Returns the Nth sub-matching string of one regex
  -- Returns the complete matching string of one regex if Sub_String_Index is 0
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  --  or if the Sub_String_Index is higher than the number
  --  of substring of this regex (returned by Nb_Substrings)
  --  or if last Checks did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  (sub) string length
  function Substring (Regex_Index : Positive;
                      Sub_String_Index : Nb_Sub_String_Range)
           return String is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    Cell : Regular_Expressions.Match_Cell;
  begin
    -- Check that previous check completed
    if not Check_Completed then
      raise No_Regex;
    end if;
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    -- Check number of substrings and get cell
    if Sub_String_Index > Upat_Access.Nb_Substr then
      raise No_Regex;
    end if;
    Cell := Upat_Access.Substrs(Sub_String_Index);
    -- Check if end of cell is the start of a Utf8 sequence
    if Utf8 and then Is_Regex then
      Apply_Utf8 (Asu.Element (Upat_Access.Match_Str, Cell.End_Offset),
                  Cell.End_Offset);
      if Cell.End_Offset > Asu.Length (Upat_Access.Match_Str) then
        raise Substr_Len_Error;
      end if;
    end if;
    -- Return the slice
    return Asu.Slice (Upat_Access.Match_Str,
                      Cell.Start_Offset, Cell.End_Offset);

  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Substring;

  -- Returns the Match_Cell of the complete matching string
  -- i.e. (Substr_Indexes(1, 0).Start_Offset,
  --       Substr_Indexes(Number, 0).End_Offset)
  -- Raises No_Regex if last Check did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  string length
  function Str_Indexes return Regular_Expressions.Match_Cell is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    Cell : Regular_Expressions.Match_Cell;
    Nbre : Natural;
  begin
    -- Check that previous check completed
    if not Check_Completed then
      raise No_Regex;
    end if;

    -- Get access to the first pattern
    Upat.Num := 1;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);

    -- Get start of string matching first pattern
    Cell.Start_Offset := Upat_Access.Substrs(0).Start_Offset;

    -- Get access to the last pattern if needed (if more than one pattern)
    Nbre := Unique_Pattern.List_Length (Pattern_List);
    if Nbre /= 1 then
      -- Get access to the last pattern
      Upat.Num := Nbre;
      Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    end if;

    -- Get end of string matching last pattern
    Cell.End_Offset := Upat_Access.Substrs(0).End_Offset;

    -- Apply UTF8 correction
    -- Check if end of cell is the start of a Utf8 sequence
    if Utf8 and then Is_Regex then
      Apply_Utf8 (Asu.Element (Upat_Access.Match_Str, Cell.End_Offset),
                  Cell.End_Offset);
      if Cell.End_Offset > Asu.Length (Upat_Access.Match_Str) then
        raise Substr_Len_Error;
      end if;
    end if;
    return Cell;
  end Str_Indexes;

end Search_Pattern;

