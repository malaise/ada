with Ada.Characters.Latin_1;
with As.U, Sys_Calls, Argument, Hashed_List.Unique, Str_Util, Text_Line,
     Hexa_Utils, Language, Images, Unbounded_Arrays;
with Debug;
package body Search_Pattern is

  -- 0 to 16 substring indexes
  subtype Substr_Array is Regular_Expressions.Match_Array (Nb_Sub_String_Range);

  -- Unbounded array of back references (inter-regex)
  subtype Byte is Natural range 0 .. 255;
  type Backref_Rec is record
    -- Regex = 0 for not a backref
    -- Substring = 0 for text matching all regex
    Regex : Byte := 0;
    Substr : Nb_Sub_String_Range := 0;
  end record;
  type Backref_Array is array (Positive range <>) of Backref_Rec;
  package Backref_Mng is new Unbounded_Arrays (Backref_Rec,
                                               Backref_Array);
  subtype Backref_Ua is Backref_Mng.Unb_Array;

  -- Add (append) a backref at a given index
  procedure Add_Backref (To : in out Backref_Ua;
                         Backref : in Backref_Rec;
                         Index : in Positive) is
  begin
    if To.Length >= Index then
      -- Normally never here (backrefs are appended one after the other)
      To.Replace_Element (Index, Backref);
    else
      declare
        -- Gap from Len + 1 to Index - 1
        Gap : constant Backref_Array (1 .. Index - To.Length - 1)
            := (others => (0, 0));
      begin
        To.Append (Gap & Backref);
      end;
    end if;
  end Add_Backref;

  -- Unique list of patterns
  type Line_Pat_Rec is record
    -- Pattern number: 1, 2...
    Num : Positive;
    -- Is it a Delim (\n) or a string/regex
    Is_Delim : Boolean;
    -- Are Prev and next patterns delimiters
    Prev_Delim, Next_Delim : Boolean;
    -- String to search (and Regex depending on Is_regex)
    Find_Str : As.U.Asu_Us;
    Case_Sensitive, Dot_All : Boolean;
    Pat : Regular_Expressions.Compiled_Pattern;
    -- Inter-regex back references
    Backrefs : Backref_Ua;
    -- The complete string from input flow that matches
    Match_Str : As.U.Asu_Us;
    -- Number of sub-matching strings
    Nb_Substr : Nb_Sub_String_Range := 0;
    -- Indexes of matching string (at index 0),
    --  then indexes of sub-matching strings
    Substrs : Substr_Array := (others => (0, 0, 0));
  end record;
  type Line_Pat_Acc is access all Line_Pat_Rec;

  procedure Set (To : out Line_Pat_Rec; Val : in Line_Pat_Rec) is
  begin
    -- Setting a pattern (in Parse) for use in Check
    To.Num := Val.Num;
    To.Is_Delim := Val.Is_Delim;
    To.Prev_Delim := Val.Prev_Delim;
    To.Next_Delim := Val.Next_Delim;
    To.Find_Str := Val.Find_Str;
    To.Case_Sensitive := Val.Case_Sensitive;
    To.Dot_All := Val.Dot_All;
    -- Regexp pattern cannot be copied, so it will be assigned
    --  to the Line_Pat_Rec access
    -- To.Pat := Val.Pat;
    To.Backrefs := Val.Backrefs;
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
  package H_List_Pattern  is new Hashed_List (Line_Pat_Rec, Line_Pat_Acc,
                                            Set, "=", Image);
  package Unique_Pattern  is new H_List_Pattern.Unique;

  -- The search and exclude patterns
  Search_List  : aliased Unique_Pattern.Unique_List_Type;
  Exclude_List : aliased Unique_Pattern.Unique_List_Type;

  -- True if one unique pattern and with no '^' nor '$'
  Is_Iterative : Boolean;

  -- True if milti pattern and first and last patterns are not delimiter
  Is_Overlapping : Boolean;

  -- True if find pattern is regexes
  Is_Regex : Boolean;

  -- Is first char of first pattern, last char of last pattern, a delim
  First_First_Delim, Last_Last_Delim : Boolean;

  -- The official Line Feed
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

  -- Delimiter
  Delimiter : As.U.Asu_Us;
  -- Get the delimiter
  function Get_Delimiter return String is
  begin
    return Delimiter.Image;
  end Get_Delimiter;

  -- Reports a parsing error
  type Pattern_Kind_List is (Search_Kind, Exclude_Kind, Delimiter_Kind);
  Pattern_Kind : Pattern_Kind_List := Search_Kind;
  procedure Error (Msg : in String) is
  begin
    case Pattern_Kind is
      when Search_Kind =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
            & " ERROR parsing search pattern: "
            & Msg & ".");
      when Exclude_Kind =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
            & " ERROR parsing exclude pattern: "
            & Msg & ".");
      when Delimiter_Kind =>
        Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
            & " ERROR parsing delimiter string: "
            & Msg & ".");
    end case;
    raise Parse_Error;
  end Error;

  -- Get access to pattern num N
  function Get_Search_Access (N : in Positive) return Line_Pat_Acc is
    Upat : Line_Pat_Rec;
    Upat_Access : Line_Pat_Acc;
  begin
    Upat.Num := N;
    Search_List.Get_Access (Upat, Upat_Access);
    return Upat_Access;
  end Get_Search_Access;

  -- Add a line pattern
  procedure Add (Crit : in String;
                 Regex_Mode, Case_Sensitive, Dot_All : in Boolean;
                 Prev_Delim, Next_Delim : in Boolean;
                 Backrefs : in Backref_Ua;
                 List : in out Unique_Pattern.Unique_List_Type) is
    Upat : Line_Pat_Rec;
    Upat_Access : Line_Pat_Acc;
    Ok : Boolean;
  begin
    -- Compute new pattern number and type
    Upat.Num := List.List_Length + 1;
    Upat.Is_Delim := Crit = Delimiter.Image;
    Upat.Prev_Delim := Prev_Delim;
    Upat.Next_Delim := Next_Delim;
    Upat.Backrefs := Backrefs;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search adding regex "
             &  Upat.Num'Img & " >" & Crit & "<");
    end if;
    -- Empty pattern is a delimiter
    if Upat.Is_Delim then
      -- Insert delimiter
      List.Insert (Upat);
      return;
    end if;
    -- Store string
    Upat.Find_Str := As.U.Tus (Crit);
    Upat.Case_Sensitive := Case_Sensitive;
    Upat.Dot_All := Dot_All;
    if not Regex_Mode then
      -- Only string if this is not a regex
      Upat.Nb_Substr := 0;
      List.Insert (Upat);
      return;
    end if;
    -- Regex: Count max number of substrings, i.e. number of '('
    -- This is a majorant because some may be \( or [(]
    for I in Sub_String_Range loop
      -- Locate succcessive occurences of "("
      exit when Str_Util.Locate (Crit, "(", Occurence => I) = 0;
      Upat.Nb_Substr := I;
    end loop;
    -- Regex compiled patterns cannot be copied and Substrs are used later
    -- Insert a pattern without compiled pattern and substrs
    List.Insert (Upat);
    -- Get access to it and compile in this access
    List.Get_Access (Upat, Upat_Access);
    Regular_Expressions.Compile (Upat_Access.Pat, Ok, Crit,
                                 Case_Sensitive => Case_Sensitive,
                                 Dot_All => Dot_All);
    if not Ok then
      Error ("Invalid pattern """ & Crit
        & """." & Line_Feed
        & "Error (from PCRE) is: "
        & Regular_Expressions.Error(Upat_Access.Pat));
    end if;
  end Add;

  -- Start line and stop line strings in regex
  Start_Char : constant Character := '^';
  function Start_String (Delim : in Boolean) return String is
  begin
    if Delim then return "" & Start_Char;
    else return "";
    end if;
  end Start_String;
  Stop_Char : constant Character := '$';
  function Stop_String (Delim : in Boolean) return String is
  begin
    if Delim then return "" & Stop_Char;
    else return "";
    end if;
  end Stop_String;

  -- Parses a pattern (splits it or not in several items of List)
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse_One (Pattern : in String;
                       Case_Sensitive : in Boolean;
                       Regex_Mode : in Boolean;
                       Split : in Boolean;
                       Dot_All : in Boolean;
                       Search : in Boolean;
                       List : in out Unique_Pattern.Unique_List_Type) is

    The_Pattern : As.U.Asu_Us;

    -- Check and get an hexa code from The_Pattern (Index .. Index + 1)
    function Get_Hexa (Index : Positive) return Byte is
     Result : Byte;
    begin
      -- First digit: 16 * C
      if Index > The_Pattern.Length then
        Error ("No hexadecimal sequence at the end of pattern");
      end if;
      begin
        Result := 16#10#
                * Hexa_Utils.Char_To_Hexa (The_Pattern.Element (Index));
      exception
        when Constraint_Error =>
          Error ("Invalid hexadecimal sequence "
               & The_Pattern.Slice (Index, Index + 1)
               & " in pattern");
      end;
      -- First digit: 1 * C
      if Index + 1 > The_Pattern.Length then
        Error ("Uncomplete hexadecimal sequence at the end of pattern");
        raise Parse_Error;
      end if;
      begin
        Result := Result
                + Hexa_Utils.Char_To_Hexa (The_Pattern.Element (Index + 1));
      exception
        when Constraint_Error =>
          Error ("Invalid hexadecimal sequence "
               & The_Pattern.Slice (Index, Index + 1)
               & " in pattern");
      end;
      if Result = 0 and then Regex_Mode then
        Error ("Invalid null hexadecimal sequence in regex"
             & The_Pattern.Slice (Index, Index + 1)
             & " in pattern");
      end if;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, got hexadecimal sequence "
                                 & The_Pattern.Slice (Index, Index + 1));
      end if;
      return Result;
    end Get_Hexa;

    -- Returns character class (e.g. [:alnum:]) associated to a key char
    --  or "\Key"
    function Char_Class_Of (Key : Character) return String is
    begin
      if not Regex_Mode then
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

    -- Return True if Pattern(Start) = '^' and Patern(Stop) = '$' (not "\$")
    function Check_Start (Start : Positive) return Boolean is
    begin
      return The_Pattern.Element (Start) = Start_Char;
    end Check_Start;
    function Check_Stop (Stop : Positive) return Boolean is
    begin
      return The_Pattern.Element (Stop) = Stop_Char
        and then not Str_Util.Is_Backslashed (The_Pattern.Image, Stop);
    end Check_Stop;
    function Check_Iterative (Start, Stop : Positive) return Boolean is
    begin
      return not Check_Start (Start) and then not Check_Stop (Stop);
    end Check_Iterative;

    -- Indexes in Pattern
    Start_Index : Positive;
    Stop_Index : Natural;
    Tmp_Index : Natural;
    -- Was previous item a delimiter
    Prev_Delim : Boolean;
    -- Is next item a delimiterA (true except at the end)
    Next_Delim : Boolean;
    -- Are we processing first chunk
    First_Chunk : Boolean;
    -- Byte read after \R or \r
    Byt : Byte;
    -- Array of back references
    Backref : Backref_Rec;
    Backrefs : Backref_Ua;
  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search parsing pattern >" & Pattern & "<");
    end if;
    -- Reset pattern characteristics
    The_Pattern := As.U.Tus (Pattern);
    List.Delete_List;
    -- Reject empty pattern
    if Pattern = "" then
      Error ("Empty pattern");
    end if;

    -- Replace escape sequences (\n, \t and \xIJ) in the pattern
    Stop_Index := 1;
    loop
      -- Locate sequences (not \R nor \r)
      Stop_Index := Str_Util.Locate_Escape (The_Pattern.Image,
                               Stop_Index,
                               "\nstxABCDEFGHIJKLMNOPQSTUVWXYZ");
      exit when Stop_Index = 0;
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, found Esc char >"
                                & The_Pattern.Element (Stop_Index) & "<");
      end if;
      -- Replace sequence
      case The_Pattern.Element (Stop_Index) is
        when 'n' =>
          -- "\n" replaced by line_feed
          The_Pattern.Replace (Stop_Index - 1, Stop_Index, Line_Feed);
        when 's' =>
          -- "\s" replaced by space
          The_Pattern.Replace (Stop_Index - 1, Stop_Index, " ");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          The_Pattern.Replace (Stop_Index - 1, Stop_Index,
                               Ada.Characters.Latin_1.Ht & "");
        when 'x' =>
          -- "\xIJ" hexa replaced by byte
          The_Pattern.Replace (Stop_Index - 1, Stop_Index + 2,
                             Character'Val (Get_Hexa (Stop_Index + 1)) & "");
        when 'A' .. 'Q' | 'S' .. 'Z' =>
          -- Some \A to \Z (but not \R) replaced by [:<CharClass>:]
          declare
            Class : constant String := Char_Class_Of (
                    The_Pattern.Element (Stop_Index));
          begin
            The_Pattern.Replace (Stop_Index - 1, Stop_Index, Class);
            Stop_Index := Stop_Index + Class'Length - 1;
          end;
        when others =>
          -- "\\" or impossible. Leave sequence as it is, skip it
          Stop_Index := Stop_Index + 1;
      end case;
      exit when Stop_Index >= The_Pattern.Length;
    end loop;

    if Split then
      -- Locate all Delimiters and split pattern (one per line)
      Start_Index := 1;
      Prev_Delim := False;
      First_Chunk := True;
      loop
        if Delimiter.Is_Null then
          Stop_Index := 0;
        else
          Stop_Index := Str_Util.Locate (The_Pattern.Image,
                                         Delimiter.Image, Start_Index);
        end if;
        if Stop_Index = Start_Index then
          -- A Delim
          if Prev_Delim then
            -- Two successive Delims
            if Regex_Mode then
              Add (Start_String (True) & Stop_String (True), Regex_Mode,
                   Case_Sensitive, Dot_All, False, False, Backrefs, List);
            else
              Add ("", Regex_Mode, Case_Sensitive, False, False, False,
                   Backrefs, List);
            end if;
          end if;
          Add (Delimiter.Image, Regex_Mode, Case_Sensitive,
               False, False, False, Backrefs, List);
          Prev_Delim := True;
        else
          -- A pattern: see if it is followed by a delim (always, except at the
          --  end)
          Next_Delim := Stop_Index /= 0;

          -- Make Stop_Index be the last index of regex,
          if Stop_Index = 0 then
            Stop_Index := The_Pattern.Length;
          else
            -- This delim will be located at next iteration of the loop
            Stop_Index := Stop_Index - 1;
          end if;

          -- Handle back references
          Tmp_Index := Start_Index;
          loop
            -- Tmp is index in full The_Pattern
            Tmp_Index := Str_Util.Locate_Escape (
              The_Pattern.Slice (Start_Index, Stop_Index),
                                 Tmp_Index, "\Rr");
            exit when Tmp_Index = 0;
            if The_Pattern.Element (Tmp_Index) /= '/' then
              if Debug.Set then
                Sys_Calls.Put_Line_Error ("Search, found Esc char >"
                                & The_Pattern.Element (Tmp_Index) & "<");
              end if;
              -- "\RIJ" or "\rIJ' replaced by 'R' or 'r', and store backref
              Byt := Get_Hexa (Tmp_Index + 1);
              The_Pattern.Replace (Tmp_Index - 1, Tmp_Index + 2,
                                   The_Pattern.Element (Tmp_Index) & "");
              Stop_Index := Stop_Index - 3;
              if The_Pattern.Element (Tmp_Index - 1) = 'R' then
                -- Add backref to full regex Byt
                Backref := (Byt, 0);
              else
                -- Add backref to substr Low_Byt of regex High_Byt
                Backref := (Byt / 16, Byt mod 16);
              end if;
              if Backref.Regex = 0
              or else Backref.Regex > List.List_Length then
                Error ("Invalid regex index "
                     & Images.Integer_Image (Backref.Regex)
                     & " in back reference");
              end if;
              if Backref.Substr /= 0
              and then Backref.Substr > Nb_Substrings (Backref.Regex) then
                Error ("Invalid substring index "
                     & Images.Integer_Image (Backref.Substr)
                     & " in back reference");
              end if;
              if Debug.Set then
                Sys_Calls.Put_Line_Error ("Search, adding backref "
                   & Backref.Regex'Img & " :" & Backref.Substr'Img);
              end if;
              -- Index in Backrefs is relative to this slice
              Add_Backref (Backrefs, Backref, Tmp_Index - Start_Index + 1);
            end if;
          end loop;

          -- Insert pattern (maybe with ' and '$')
          declare
            Slice : constant String
                  := The_Pattern.Slice (Start_Index, Stop_Index);
            Needs_Start, Needs_Stop : Boolean;
          begin
            if Regex_Mode then
              -- It is not mandatory to avoid ^^toto and toto$$, but cleaner
              -- Add '^' if a delimiter preceeds and no leading '^' yet
              Needs_Start := Prev_Delim and then not Check_Start (Start_Index);
              -- Add '$' if a delimiter follows and no trailing '$' yet
              --  (or if it is "\$")
              Needs_Stop := Next_Delim and then not Check_Stop (Stop_Index);
              -- Add this regex with start/stop strings
              Add (Start_String (Needs_Start) & Slice
                   & Stop_String (Needs_Stop),
                 True, Case_Sensitive, Dot_All, Prev_Delim, Next_Delim,
                 Backrefs, List);
            else
              -- Add this regex with no start/stop strings
              Add (Slice, False, True, False, Prev_Delim, Next_Delim, Backrefs,
                   List);
            end if;
          end;

          if Search then
            -- Set global characteristics when parsing search
            if Regex_Mode then
              -- See if this is a single regex and if it can apply several times
              --  to one line of input (not start by ^ nor end by $, except
              --  "\$")
              -- Note that ^ or $ cannot be followed by ?, * or +, so no need to
              --  check "^?"
              -- Also note that "(^|@)toto(/|$)" is potentially iterative and
              --  is handled as iterative
              if Search
              and then not Prev_Delim
              and then not Next_Delim
              and then Check_Iterative (Start_Index, Stop_Index) then
                Is_Iterative := True;
              end if;
            else
              -- Same, but only check delimiters (start and stop strings are
              --  not interpreted)
              if Search
              and then not Prev_Delim
              and then not Next_Delim then
                Is_Iterative := True;
              end if;
            end if;
          end if;
          Prev_Delim := False;
        end if;
        if Search and then First_Chunk then
          -- Is first chunk a delim or starting by '^'
          First_First_Delim := Prev_Delim
                      or else (Regex_Mode and then Check_Start (Start_Index));
          First_Chunk := False;
        end if;
        -- Done
        exit when Stop_Index = The_Pattern.Length;
        Start_Index := Stop_Index + 1;
      end loop;
      if Search then
        -- Is last chunk a delim or ending by '$' (not backslashed)
        Last_Last_Delim := Prev_Delim
                          or else (Regex_Mode and then Check_Stop (Stop_Index));
      end if;
    else
      -- No split (delimiter)
      if Regex_Mode then
        Add (The_Pattern.Image, True, Case_Sensitive, Dot_All,
             False, False, Backrefs, List);
      else
        Add (The_Pattern.Image, False, True, False, False, False, Backrefs,
             List);
      end if;
    end if;
    -- Done
  exception
    when Constraint_Error =>
      -- Not enough characters after '\'
      Error ("Invalid pattern " & Pattern);
  end Parse_One;

  -- Checks and sets the delimiter
  procedure Parse_Delimiter (Delim : String) is
    Delim_List  : aliased Unique_Pattern.Unique_List_Type;
    Upat : Line_Pat_Rec;
    Acc : Line_Pat_Acc;
  begin
    -- Optim and safe way
    if Delim = Line_Feed then
      Delimiter := As.U.Tus (Line_Feed);
      return;
    elsif Delim = "" then
      -- Empty delimiter
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search, parsed empty delimiter");
      end if;
      Delimiter.Set_Null;
      return;
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search, parsing delimiter");
    end if;

    -- Parse Delim as a non-regex string, not splitted
    Pattern_Kind := Delimiter_Kind;
    Parse_One (Delim, False, False, False, False, False,Delim_List);
    -- One string in list: the delimiter
    Upat.Num := 1;
    Delim_List.Get_Access (Upat, Acc);
    Delimiter := Acc.Find_Str;
    if Delimiter.Length > Text_Line.Max_Line_Feed_Len then
      Error ("Delimiter is too long");
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search, parsed delimiter >"
           & Delimiter.Image & "<");
    end if;
  end Parse_Delimiter;


  -- Parses and compiles the search patern
  -- Parses and compiles the exclude patern (if any)
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Search  : in String;
                   Exclude : in String;
                   Delimiter : in String;
                   Case_Sensitive, Is_Regex, Dot_All : in Boolean) is
    Upat : Line_Pat_Rec;
    Search_Access, Exclude_Access : Line_Pat_Acc;
    use type Language.Language_Set_List;
  begin
    -- Init global variables and 'constants'
    Search_Pattern.Is_Regex := Is_Regex;
    -- Parse the delimiter
    Parse_Delimiter (Delimiter);
    -- Parse the search pattern
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search, parsing search pattern");
    end if;

    -- Parse and check the find pattern
    Pattern_Kind := Search_Kind;
    Is_Iterative := False;
    Parse_One (Search, Case_Sensitive, Is_Regex, True, Dot_All,
               True, Search_List);

    -- See if searches need to overlap
    Is_Overlapping := not First_First_Delim
             and then not Last_Last_Delim;

    -- Global attributes of find pattern
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search, Is_Iterative " & Is_Iterative'Img
                             &  ", Is_Overlapping " & Is_Overlapping'Img);
    end if;

    if Exclude = "" then
      -- No exclude
      Exclude_List.Delete_List;
      return;
    end if;
    -- Parse the exclude pattern
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search, parsing exclude pattern");
    end if;
    Pattern_Kind := Exclude_Kind;
    Parse_One (Exclude, Case_Sensitive, Is_Regex, True, Dot_All,
               False, Exclude_List);

    -- Both patterns must have same length and have delims at same pos
    if Search_List.List_Length /= Exclude_List.List_Length then
      Error ("Exclude must have the same number of regex as the find pattern");
    end if;
    for I in 1 .. Search_List.List_Length loop
      Upat.Num := I;
      Search_List.Get_Access (Upat, Search_Access);
      Exclude_List.Get_Access (Upat, Exclude_Access);
      if Search_Access.Is_Delim /= Exclude_Access.Is_Delim then
        Error ("Exclude must have the same delimiters as the find pattern");
      end if;
    end loop;
  exception
    when others =>
      -- Cleanup
      Search_List.Delete_List;
      Exclude_List.Delete_List;
      Search_Pattern.Delimiter := As.U.Tus (Line_Feed);
      raise;
  end Parse;

  -- Returns the number of lines that it covered by the
  --  search pattern (one per regex and one per New_Line.
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive is
    N : constant Natural := Search_List.List_Length;
  begin
    if N = 0 then
      raise No_Regex;
    else
      return N;
    end if;
  end Number;

  -- Is search pattern a Regex
  -- Raises No_Regex if the pattern was not parsed OK
  function Search_Regex return Boolean is
  begin
    if Search_List.List_Length = 0 then
      raise No_Regex;
    else
      return Is_Regex;
    end if;
  end Search_Regex;

  -- Return the Nts pattern or delimiter
  -- Raises No_Regex if the pattern was not parsed OK
  -- Raises Contraint_Error if N > Number;
  function Get_Pattern (Regex_Index : Positive) return String is
    L : constant Natural := Search_List.List_Length;
    Upat_Access : Line_Pat_Acc;
  begin
    if L = 0 then
      raise No_Regex;
    elsif Regex_Index > L then
      raise Constraint_Error;
    end if;
    -- Get access to the pattern
    Upat_Access := Get_Search_Access (Regex_Index);
    if Upat_Access.Is_Delim then
      -- This is a delimiter
      return Delimiter.Image;
    else
      -- This is a String or Regex
      return Upat_Access.Find_Str.Image;
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Get_Pattern;

  -- Tells if the search pattern can be applied several times
  --  on one line of input (i.e. does not contain '\n', '^' or '$'
  -- Raises No_Regex if the pattern was not parsed OK
  function Iterative return Boolean is
  begin
    -- Must be some pattern compiled
    if Search_List.List_Length = 0 then
      raise No_Regex;
    end if;
    return Is_Iterative;
  end Iterative;

  -- Tells if the search pattern (if multi regex, otherwise False)
  --  can re-applied to the last text chunk, i.e. does not start nor stop
  --  with delimiter
  function Overlaps return Boolean is
  begin
   -- Must be some pattern compiled
    if Search_List.List_Length = 0 then
      raise No_Regex;
    end if;
    return Is_Overlapping;
  end Overlaps;

  -- Is an exclude pattern set
  function Has_Exclude return Boolean is
  begin
    return Exclude_List.List_Length /= 0;
  end Has_Exclude;

  -- Returns the number of substrings of one regex
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  function Nb_Substrings (Regex_Index : Positive) return Nb_Sub_String_Range is
  begin
    -- Get access to the pattern
    return Get_Search_Access (Regex_Index).Nb_Substr;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Nb_Substrings;

  -- Checks if the input string from Start to its end
  --  matches the regex no Regex_Index
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (retruned by Parse)
  Upat : aliased Line_Pat_Rec;
  function Check (Str : String; Start : Positive;
                  Search : in Boolean;
                  Regex_Index : Positive) return Boolean is
    -- The pattern to check with and the one to update
    Crit_Access, Upat_Access : Line_Pat_Acc;
    -- Check result
    Nmatch : Natural;
    Match : Regular_Expressions.Match_Array
               (1 .. Nb_Sub_String_Range'Last + 1);
    -- Backref, Index of Backref in string, and result of recompilation
    Backref : Backref_Rec;
    Str_Index : Positive;
    Ok : Boolean;
    -- The list
    type List_Access is access all Unique_Pattern.Unique_List_Type;
    List : List_Access;

  begin
    -- Search or exclude list?
    if Search then
      -- Check match in search list
      List := Search_List'Access;
      -- Check not completed by default
    elsif Exclude_List.List_Length = 0 then
      -- No exclude list, so Str is OK (does not match)
      return False;
    else
      -- Check match in exclude list
      List := Exclude_List'Access;
    end if;
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    List.all.Get_Access (Upat, Upat_Access);
    -- Reset substring array if not a delim
    if not Upat_Access.Is_Delim then
      Upat_Access.Substrs := (others => (0, 0, 0));
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search check pattern No " & Regex_Index'Img);
    end if;
    -- Delimiter matches delimiter
    if Upat_Access.Is_Delim then
      if Str = Delimiter.Image then
        Upat_Access.Nb_Substr := 0;
        Upat_Access.Substrs(0) := (1, 1, 1);
        Upat_Access.Match_Str := Delimiter;
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Search check pattern is delim vs delim");
        end if;
        return True;
      else
        if Debug.Set then
          Sys_Calls.Put_Line_Error (
                    "Search check pattern is delim vs not delim");
        end if;
        return False;
      end if;
    elsif Str = Delimiter.Image then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern is not delim vs delim");
      end if;
      return False;
    else
      if Debug.Set then
        Sys_Calls.Put_Line_Error (
                  "Search check pattern is not delim vs not delim");
      end if;

      -- Replace back references in Upat (and compile pattern if regex)
      if Upat_Access.Backrefs.Is_Null then
        Crit_Access := Upat_Access;
        if  Debug.Set then
          Sys_Calls.Put_Line_Error ("Search pattern is "
                                  & Crit_Access.Find_Str.Image);
        end if;
      else
        Set (Upat, Upat_Access.all);
        Crit_Access := Upat'Access;
        Str_Index := 1;
        for I in 1 .. Upat.Backrefs.Length loop
          Backref := Upat.Backrefs.Element (I);
          if Backref.Regex /= 0 then
            -- Replace the letter (R or r) by the matching (sub) string
            declare
              Rep_Str : constant String := Substring (Backref.Regex,
                                                      Backref.Substr);
            begin
              Upat.Find_Str.Replace (Str_Index, Str_Index, Rep_Str);
              Str_Index := Str_Index + Rep_Str'Length;
            end;
          else
            -- Keep this char
            Str_Index := Str_Index + 1;
          end if;
        end loop;
        if  Debug.Set then
          Sys_Calls.Put_Line_Error ("Search pattern is " & Upat.Find_Str.Image);
        end if;
        if Is_Regex then
          -- Compile regex
          Regular_Expressions.Compile (Upat.Pat, Ok, Upat.Find_Str.Image,
                                       Case_Sensitive => Upat.Case_Sensitive,
                                       Dot_All => Upat.Dot_All);
          if not Ok then
            Sys_Calls.Put_Line_Error (
                Argument.Get_Program_Name
               & "ERROR: Invalid regex "
                         & Upat.Find_Str.Image
                         & " after replacing back reference.");
            raise Regex_Error;
          end if;
        end if;
      end if;

      -- Check. Note that indexes will be relative to Str
      if Is_Regex then
        Regular_Expressions.Exec (Crit_Access.Pat,
                                  Str(Start .. Str'Last),
                                  Nmatch, Match);
        -- For exclusion, the match must be strict
        if not Search and then Nmatch >= 1 and then
            (Match(1).First_Offset /= Start
             or else Match(1).Last_Offset_Stop /= Str'Last) then
          -- Not strict matching
          Nmatch := 0;
        end if;
      elsif Str = "" and then Crit_Access.Find_Str.Image = "" then
        -- Noregex: Empty string versus empty pattern
        Match(1) := (Start, 0, 0);
        Nmatch := 1;
      else
        -- Noregex, locate string in tail of Str
        -- If pattern is followed by a delim then search bawards to find
        --  last occurence (usefull if pattern is not preceeded by a delim)
        Nmatch := Str_Util.Locate (Str (Start .. Str'Last),
                                   Crit_Access.Find_Str.Image,
                                   0, Forward => not Crit_Access.Next_Delim);
        if Nmatch /= 0 then
          -- Fill matching info as if from a regex
          Match(1) := (
           First_Offset => Nmatch,
           Last_Offset_Start => Nmatch + Crit_Access.Find_Str.Length - 1,
           Last_Offset_Stop  => Nmatch + Crit_Access.Find_Str.Length - 1);
          Nmatch := 1;
          if Search then
            if Crit_Access.Prev_Delim
            and then Match(1).First_Offset /= Start then
              -- If a delim before, then must match from start
              Nmatch := 0;
            end if;
            if Crit_Access.Next_Delim
            and then Match(1).Last_Offset_Stop /= Str'Last then
              -- If a delim after the must match up to stop
              Nmatch := 0;
            end if;
          elsif Match(1).First_Offset /= Start
                or else Match(1).Last_Offset_Stop /= Str'Last then
            -- For exclusion, the match must be strict
            Nmatch := 0;
          end if;
        end if;
      end if;
      if Nmatch >= 1
      and then (Match(1).First_Offset <= Match(1).Last_Offset_Stop
                or else Str = "") then
        -- Normal match or empty string
        -- Copy the slice of substrings
        Upat_Access.Nb_Substr := Nmatch - 1;
        Upat_Access.Substrs(0) := Match(1);
        Upat_Access.Substrs(1 .. Upat_Access.Nb_Substr)
                   := Match(2 .. Nmatch);
        Upat_Access.Match_Str := As.U.Tus (Str);
        return True;
      else
        -- Not match
        return False;
      end if;
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Check;

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
    Upat_Access : Line_Pat_Acc;
    Cell : Regular_Expressions.Match_Cell;
    use type Regular_Expressions.Match_Cell;
  begin
    -- Get access to the pattern
    Upat_Access := Get_Search_Access (Regex_Index);
    -- Check number of substrings and get cell
    if Sub_String_Index > Upat_Access.Nb_Substr then
      return "";
    end if;
    Cell := Upat_Access.Substrs(Sub_String_Index);
    -- Check if end of cell is the start of a Utf8 sequence
    if Cell.Last_Offset_Stop > Upat_Access.Match_Str.Length then
      raise Substr_Len_Error;
    end if;
    -- Return the slice
    if Cell = Regular_Expressions.No_Match then
      -- Empty match
      return "";
    else
      return Upat_Access.Match_Str.Slice (
                        Cell.First_Offset, Cell.Last_Offset_Stop);
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Substring;

  -- Returns the complete matching string of all regexes
  -- Raises No_Regex if last Checks did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  string length
  function Allstring return String is
    Result : As.U.Asu_Us;
  begin
    for I in 1 .. Number loop
      Result.Append (Substring (I, 0));
    end loop;
    return Result.Image;
  end Allstring;

  -- Returns the Match_Cell of the complete matching string
  -- i.e. (Substr_Indexes(1, 0).Start_Offset,
  --       Substr_Indexes(Number, 0).End_Offset)
  -- Raises No_Regex if last Check did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  string length
  function Str_Indexes return Regular_Expressions.Match_Cell is
    Upat_Access : Line_Pat_Acc;
    Cell : Regular_Expressions.Match_Cell;
    Nbre : Natural;
  begin
    -- Get access to the first pattern
    Upat_Access := Get_Search_Access (1);

    -- Get start of string matching first pattern
    Cell.First_Offset := Upat_Access.Substrs(0).First_Offset;

    -- Get access to the last pattern if needed (if more than one pattern)
    Nbre := Search_List.List_Length;
    if Nbre /= 1 then
      -- Get access to the last pattern
      Upat_Access := Get_Search_Access (Nbre);
    end if;

    -- Get end of string matching last pattern
    Cell.Last_Offset_Start := Upat_Access.Substrs(0).Last_Offset_Start;
    Cell.Last_Offset_Stop  := Upat_Access.Substrs(0).Last_Offset_Stop;

    -- Apply UTF8 correction
    -- Check if end of cell is the start of a Utf8 sequence
    if Cell.Last_Offset_Stop > Upat_Access.Match_Str.Length then
      raise Substr_Len_Error;
    end if;
    return Cell;
  end Str_Indexes;

end Search_Pattern;

