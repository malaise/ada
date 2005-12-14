with Ada.Strings.Unbounded, Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Sys_Calls, String_Mng, Text_Line, Unique_List, Debug,
     Char_To_Hexa, Regular_Expressions;
with Search_Pattern;
package body Replace_Pattern is

  package Asu renames Ada.Strings.Unbounded;

  -- The pattern to replace
  The_Pattern : Asu.Unbounded_String;

  -- The character in the pattern that code "\IJ (-> matching (sub)string)
  Match_Char : constant Character := Ada.Characters.Latin_1.Bs;

  -- The line feed string
  Line_Feed : constant String  :=Text_Line.Line_Feed & "";

  -- Regex and subbstring indexes of \R and \r
  subtype Byte is Natural range 0 .. 255;

  -- Record describing an action to perform during substitution
  type Subtit_Action_List is (Replace_Match_Regex,
                              Replace_Match_Substring);
  -- @@@ will add UPPER_CASE, lower_case and Mixed_Case conversions
  -- The chars in The_Pattern qualifying the action
  type Match_Action_Rec is record
    -- Index in the pattern
    Index : Positive;
    Action : Subtit_Action_List;
    Info : Byte;
  end record;
  procedure Set (To : out Match_Action_Rec; Val : in Match_Action_Rec) is
  begin
    To := Val;
  end Set;
  -- Access by index in The_Pattern
  function Image (Element : Match_Action_Rec) return String is
  begin
    return Element.Index'Img;
  end Image;
  function "=" (Current : Match_Action_Rec; Criteria : Match_Action_Rec)
               return Boolean is
  begin
    return Current.Index = Criteria.Index;
  end "=";
  package Matches_List is new Unique_List (Match_Action_Rec, Set, Image);
  Matches : Matches_List.List_Type;

  -- Reports a parsing error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
    raise Parse_Error;
  end Error;

  -- Check and get an hexa code from The_Pattern (Index .. Index + 1)
  function Get_Hexa (Index : Positive) return Byte is
   Result : Byte;
  begin
    -- First digit: 16 * C
    if Index > Asu.Length (The_Pattern) then
      Error ("No hexadecimal sequence at the end of replace pattern");
    end if;
    begin
      Result := 16#10# * Char_To_Hexa (Asu.Element (The_Pattern, Index));
    exception
      when Constraint_Error =>
        Error ("Invalid hexadecimal sequence "
             & Asu.Slice (The_Pattern, Index, Index + 1)
             & " in replace pattern");
    end;
    -- First digit: 1 * C
    if Index + 1 > Asu.Length (The_Pattern) then
      Error ("Uncomplete hexadecimal sequence at the end of replace pattern");
      raise Parse_Error;
    end if;
    begin
      Result := Result + Char_To_Hexa (Asu.Element (The_Pattern, Index + 1));
    exception
      when Constraint_Error =>
        Error ("Invalid hexadecimal sequence "
             & Asu.Slice (The_Pattern, Index, Index + 1)
             & " in replace pattern");
    end;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace, got hexadecimal sequence "
                               & Asu.Slice (The_Pattern, Index, Index + 1));
    end if;
    return Result;
  end Get_Hexa;

  -- Parse the replace pattern
  procedure Parse (Pattern : in String) is
    Start : Positive;
    Got : Natural;
    Esc_Char : Character;
    Match : Match_Action_Rec;
    Hexa_Byte : Byte;

  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace parsing pattern >" & Pattern & "<");
    end if;
    -- Store pattern
    The_Pattern := Asu.To_Unbounded_String (Pattern);
    -- Replace escape sequences by coding chars
    Start := 1;
    loop
      -- Locate an escape sequence, exit when no more
      Got := String_Mng.Locate_Escape (Asu.To_String (The_Pattern),
                                       Start, "\nRrstx");
      exit when Got = 0;
      -- Set corresponding code
      Esc_Char := Asu.Element (The_Pattern, Got);
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Replace, found Esc char >" & Esc_Char & "<");
      end if;
      -- In all case of replacement of \... by a char:
      Start := Got;
      case Esc_Char is
        when '\' =>
          -- "\\" replaced by '\'
          Asu.Replace_Slice (The_Pattern, Got - 1, Got, "\");
        when 'n' =>
          -- "\n" replaced by Line_Feed
          Asu.Replace_Slice (The_Pattern, Got - 1, Got, Line_Feed);
        when 's' =>
          -- "\s" replaced by Space
          Asu.Replace_Slice (The_Pattern, Got - 1, Got, " ");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          Asu.Replace_Slice (The_Pattern, Got - 1, Got,
                             Ada.Characters.Latin_1.Ht & "");
        when 'x' =>
          -- "\xIJ" hexa replaced by byte
          Hexa_Byte := Get_Hexa (Got + 1);
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 2,
                             Character'Val (Hexa_Byte) & "");
        when 'R' | 'r' =>
          -- "\RIJ" or \rIJ, IJ in hexa, replaced by matching (sub) string
          -- Check IJ is a valid byte in hexa
          Hexa_Byte := Get_Hexa (Got + 1);
          Match.Index := Got - 1;
          Match.Info := Hexa_Byte;
          if Esc_Char = 'R' then
            -- Replace by regex index IJ, check IJ
            if Hexa_Byte > Search_Pattern.Number then
              Sys_Calls.Put_Line_Error (
                "Invalid (too high) regex index "
                & Asu.Slice (The_Pattern, Got + 1, Got + 2)
                & " in replace pattern");
              raise Parse_Error;
            end if;
            Match.Action := Replace_Match_Regex;
          else
            -- Replace by regex I substring J
            -- Check I
            if Hexa_Byte / 16#10# = 0
            or else Hexa_Byte / 16#10# > Search_Pattern.Number then
              Sys_Calls.Put_Line_Error (
                "Invalid (null or too high) regex index "
                & Asu.Element (The_Pattern, Got + 1)
                & " in replace pattern");
              raise Parse_Error;
            end if;
            -- Check J
            if Hexa_Byte rem 16#10#
               > Search_Pattern.Nb_Substrings (Hexa_Byte / 16#10#) then
              Sys_Calls.Put_Line_Error (
                "Invalid (too high) substr index "
                & Asu.Element (The_Pattern, Got + 2)
                & " of regex index " & Asu.Element (The_Pattern, Got + 1)
                & " in replace pattern");
              raise Parse_Error;
            end if;
            Match.Action := Replace_Match_Substring;
          end if;
          -- Store that, at this index, there is a (sub) string match
          Matches_List.Insert (Matches, Match);
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 2,
                             Match_Char & "");
        when others =>
          -- Impossible. Leave sequence as it is, skip it
          Start := Got + 1;
      end case;
      -- Also done if end of pattern
      exit when Start >= Asu.Length (The_Pattern);
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace stored pattern >"
                               & Asu.To_String (The_Pattern) & "<");
    end if;
  end Parse;

  -- Return the Substr_Index substring (all if 0)
  -- of Str matching Match_Index regex
  function Substr (Str : String;
                   Match_Index : Search_Pattern.Sub_String_Range;
                   Substr_Index : Search_Pattern.Nb_Sub_String_Range)
                  return String is
    -- String (1 .. N)
    Loc_Str : constant String := Str;
    Cell : Regular_Expressions.Match_Cell;
  begin
    if Substr_Index = 0 then
      -- Full string matching Match_Index th regex
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Replace, got substring >"
                                & Loc_Str & "<");
      end if;
      return Loc_Str;
    end if;
    -- Get substr indexes
    Cell := Search_Pattern.Substr_Indexes (Match_Index, Substr_Index);
    if Cell.Start_Offset <= Cell.End_Offset then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Replace, got substring >"
                    & Loc_Str(Cell.Start_Offset .. Cell.End_Offset) & "<");
      end if;
      return Loc_Str (Cell.Start_Offset .. Cell.End_Offset);
    else
      raise Replace_Error;
    end if;
  exception
    when others =>
      raise Replace_Error;
  end Substr;

  -- Extract a substring of string matching a regex
  subtype Extraction_List is Subtit_Action_List
          range Replace_Match_Regex .. Replace_Match_Substring;
  function Matchstring (Str : String; Kind : Extraction_List; Nth : Byte)
                       return String is
    Prev_Delim, Got_Delim : Natural;
    Ith, Rth : Byte;
    Sth : Search_Pattern.Nb_Sub_String_Range;
  begin
    -- Regex 0 => complete matching string
    if Kind = Replace_Match_Regex and then Nth = 0 then
      return Str;
    end if;
    if Kind = Replace_Match_Regex then
      -- Nth is the regex index
      Rth := Nth;
      Sth := 0;
    else
      Rth := Nth / 16;
      Sth := Nth rem 16;
    end if;
    -- Locate the string matching the Rth regex
    Prev_Delim := Str'First - 1;
    Ith := 0;
    loop
      -- Search next delimiter
      Got_Delim := String_Mng.Locate (Str, Prev_Delim + 1, Line_Feed);
      if Got_Delim = 0 then
        -- No (more) delim
        if Ith = Rth - 1 then
          -- Last matching string is the one
          return Substr (Str(Prev_Delim + 1 .. Str'Last), Rth, Sth);
        else
          -- Less than Rth matching strings in Str???
          -- Should not occure because checked during parsing
          raise Replace_Error;
        end if;
      end if;
      if Got_Delim = Prev_Delim + 1 then
        -- A delim after a delim
        Ith := Ith + 1;
        if Ith = Rth then
          if Kind = Replace_Match_Substring then
            -- Parsing should not let substring of delim
            raise Replace_Error;
          end if;
          -- The substring is this new delim
          return Line_Feed;
        end if;
      else
        -- A delim after a string
        Ith := Ith + 1;
        if Ith = Rth then
          -- The substring is between delims
          return Substr (Str(Prev_Delim + 1 .. Got_Delim - 1), Rth, Sth);
        else
          Ith := Ith + 1;
          if Ith = Rth then
            if Kind = Replace_Match_Substring then
              -- Parsing should not let substring of delim
              raise Replace_Error;
            end if;
            -- The substring is this new delim
            return Line_Feed;
          end if;
        end if;
      end if;
      Prev_Delim := Got_Delim;
    end loop;
  exception
    when others =>
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                              & " INTERNAL ERROR. Cannot find match string "
                              & Rth'Img & "-" & Sth'Img
                              & " of >" & Str & "<");
      raise Replace_Error;
  end Matchstring;

  -- Replace the input string by the replace pattern
  -- Input string is used to substitute \rIJ and \RIJ in pattern
  function Replace (Str : String) return String is
    Result : Asu.Unbounded_String;
    -- Current index in result
    Start : Positive;
    -- Index of next Match_Char in result
    Got : Natural;
    -- Offset between result and Matches list due to previous substitutions:
    --  Substitution replaces "Got" Match_Char character of Result
    --  by Sub_Str, so potential next Match_Char in Result
    --  will not match any more the index stored in Matches
    Offset : Natural;
    -- Matching item
    Match : Match_Action_Rec;
  begin
    -- Init result with replace pattern
    Result := The_Pattern;
    -- Replace all occurences of replace code
    Start := 1;
    Offset := 0;
    loop
      -- Locate replace code
      Got := String_Mng.Locate (Asu.To_String (Result),
                                Start, Match_Char & "");
      exit when Got = 0;
      -- Check and get substring index
      Try_Match:
      begin
        Match.Index := Got - Offset;
        Matches_List.Read (Matches, Match, Match);
        -- Replace by sub Str pattern
        declare
         -- Get full or partial substring
          Sub_Str : constant String
                  := Matchstring (Str, Match.Action, Match.Info);
        begin
          if Debug.Set then
            Sys_Calls.Put_Line_Error ("Replace, got match string >"
                                    & Sub_Str & "<");
          end if;
          Asu.Replace_Slice (Result, Got, Got, Sub_Str);
          -- Restart locate from first char after replacement
          Start := Got + Sub_Str'Length;
          Offset := Offset + Sub_Str'Length - 1;
        end;
      exception
        when Matches_List.Not_In_List =>
          -- This Match_Char is a normal char
          Start := Got + 1;
      end Try_Match;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace, replaced >" & Str
         & "< by >"
         & Asu.To_String (Result) & "<");
    end if;
    return Asu.To_String (Result);
  exception
    when Replace_Error =>
      raise;
    when Error:others =>
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                               & " INTERNAL ERROR: "
                               & Ada.Exceptions.Exception_Name (Error)
                               & " while replacing string >"
                               & Str & "< by >"
                               & Asu.To_String (The_Pattern) & "<");
      raise Replace_Error;
  end Replace;

end Replace_Pattern;

