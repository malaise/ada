with Ada.Characters.Latin_1, Ada.Strings.Unbounded;
with Sys_Calls, Argument, Unique_List, String_Mng, Text_Line, Debug,
     Char_To_Hexa;
package body Search_Pattern is

  -- 1 to 16 substring indexes
  subtype Substr_Array is Regular_Expressions.Match_Array (Sub_String_Range);

  -- Unique list of patterns
  type Line_Pat_Rec is record
    Num : Positive;
    Is_Delim : Boolean;
    Pat : Regular_Expressions.Compiled_Pattern;
    Nb_Substr : Nb_Sub_String_Range := 0;
    Substrs : Substr_Array := (others => (0, 0));
  end record;
  procedure Set (To : out Line_Pat_Rec; Val : in Line_Pat_Rec) is
  begin
    To.Num := Val.Num;
    To.Is_Delim := Val.Is_Delim;
    -- Regexp cannot be copied, so it will be assigned
    --  to the Line_Pat_Rec access
    -- To.Pat := Val.Pat;
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
  Pattern_List : Unique_Pattern.List_Type;

  -- True if one unique pattern and with no '^' nor '$'
  Is_Multiple : Boolean;

  -- Line_Feed String
  Line_Feed : constant String := Text_Line.Line_Feed & "";

  package Asu renames Ada.Strings.Unbounded;

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
    -- Count number of substrings, i.e. number of '('
    for I in Sub_String_Range loop
      -- locate succcessive occurences of "("
      exit when String_Mng.Locate (Crit, Crit'First, "(", I) = 0;
      Upat.Nb_Substr := I;
    end loop;
    -- Regex compiled patterns cannot be set and Substrs are sued later
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
                   Extended, Case_Sensitive : in Boolean) is

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
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, got hexadecimal sequence "
                                 & Asu.Slice (The_Pattern, Index, Index + 1));
      end if;
      return Result;
    end Get_Hexa;

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
    -- Free previous pattern
    The_Pattern := Asu.To_Unbounded_String (Pattern);
    Unique_Pattern.Delete_List (Pattern_List);
    Is_Multiple := False;
    -- Reject empty pattern
    if Pattern = "" then
      Error ("Empty pattern");
    end if;

    -- Replace escape sequences (\n, \t and \xIJ) in the pattern
    Stop_Index := 1;
    loop
      -- Locate sequence
      Stop_Index := String_Mng.Locate_Escape (Asu.To_String (The_Pattern),
                                               Stop_Index , "\nstx");
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
          -- It must not contain Start_String if preeceded by a delim
          Check (Slice, Start_String (Prev_Delim));
          Check (Slice, Stop_String (Next_Delim));
          -- Add this regex
          Add (Start_String (Prev_Delim) & Slice & Stop_String (Next_Delim),
             Extended, Case_Sensitive);
        end;
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
  -- search pattern (one per regex and one per New_Line.
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
  -- on one line of input (i.e. does not contain '\n', '^' or '$'
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
  -- the number of regex (returned by Number)
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

  -- Checks if the input string matches one regex
  -- Returns a Match_Cell (set to (0, 0) if no match)
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (retruned by Parse)
  function Check (Str : String;
                  Regex_Index : Positive)
           return Regular_Expressions.Match_Cell is
    -- The pattern to check with
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    -- Check result
    Nmatch : Natural;
    Match : Regular_Expressions.Match_Array
               (1 .. Nb_Sub_String_Range'Last + 1);
  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search checking str >" & Str & "< ");
    end if;
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    -- Reset substring array if not a delim
    if not Upat_Access.Is_Delim then
      Upat_Access.Substrs := (others => (0, 0));
    end if;
    -- Delimiter matches delimiter
    if Upat_Access.Is_Delim then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern " & Regex_Index'Img &
                                  " is delim");
      end if;
      if Str = Line_Feed then
        return (1, 1);
      else
        return (0, 0);
      end if;
    elsif Str = Line_Feed then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check empty vs not delim");
      end if;
      return (0, 0);
    else
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern " & Regex_Index'Img);
      end if;
      Regular_Expressions.Exec (Upat_Access.Pat,
                                Str,
                                Nmatch, Match);
      if Nmatch >= 1 and then Match(1).Start_Offset <= Match(1).End_Offset then
        -- Copy the slice of substrings
        Upat_Access.Nb_Substr := Nmatch - 1;
        Upat_Access.Substrs(1 .. Upat_Access.Nb_Substr)
                   := Match(2 .. Nmatch);
        return Match(1);
      else
        return (0, 0);
      end if;
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Check;

  -- Returns the Match_Cell of the Nth sub-matching string
  -- of one regex
  -- Raises No_Regex if the Regex_Index is higher than
  -- the number of regex (returned by Number)
  -- or if the Sub_String_Index is higher than the number
  -- of substring of this regex (returned by Nb_Substrings)
  function Substr_Indexes (Regex_Index : Positive;
                           Sub_String_Index : Sub_String_Range)
           return Regular_Expressions.Match_Cell is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
  begin
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    -- Check number of substrings and return cell
    if Sub_String_Index > Upat_Access.Nb_Substr then
      raise No_Regex;
    end if;
    return Upat_Access.Substrs(Sub_String_Index);
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Substr_Indexes;

end Search_Pattern;

