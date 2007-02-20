with Ada.Strings.Unbounded, Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Sys_Calls, String_Mng, Text_Line, Unique_List, Debug,
     Char_To_Hexa, Regular_Expressions, Upper_Str, Lower_Str, Mixed_Str;
with Search_Pattern;
package body Replace_Pattern is

  package Asu renames Ada.Strings.Unbounded;

  -- The pattern to replace
  The_Pattern : Asu.Unbounded_String;

  -- The character in the pattern that code a substitution
  Subst_Char : constant Character := Ada.Characters.Latin_1.Bs;

  -- The line feed string
  Line_Feed : constant String  :=Text_Line.Line_Feed & "";

  -- Regex and subbstring indexes of \R and \r
  subtype Byte is Natural range 0 .. 255;

  -- Record describing an action to perform during substitution
  -- Replace by string matching a regex (Info)
  -- Replace by substring No (Info rem 16) of string matching No (Info / 16)
  -- Start/stop upper/lower/mixed case conversion
  type Subtit_Action_List is (
       Replace_Match_Regex, Replace_Match_Substring,
       Start_Uppercase, Start_Lowercase, Start_Mixedcase, Stop_Case);
  -- The chars in The_Pattern qualifying the action
  type Substit_Action_Rec is record
    -- Index in the pattern
    Index : Positive;
    Action : Subtit_Action_List;
    Info : Byte;
  end record;
  type Substit_Action_Access is access all Substit_Action_Rec;
  procedure Set (To : out Substit_Action_Rec; Val : in Substit_Action_Rec) is
  begin
    To := Val;
  end Set;
  -- Access by index in The_Pattern
  function Image (Element : Substit_Action_Rec) return String is
  begin
    return Element.Index'Img;
  end Image;
  function "=" (Current : Substit_Action_Rec; Criteria : Substit_Action_Rec)
               return Boolean is
  begin
    return Current.Index = Criteria.Index;
  end "=";
  package Substites_List is new Unique_List (Substit_Action_Rec,
                    Substit_Action_Access, Set, Image);
  Substites : Substites_List.List_Type;

  -- What is current case substitution mode
  subtype Case_Mode_List is Subtit_Action_List
            range Start_Uppercase .. Stop_Case;

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

  -- Check and activates a Case switch
  procedure Switch_Case (Subst : in Substit_Action_Rec;
                         Case_Action : in out Case_Mode_List) is
  begin
    -- Check that the requested mode (in action) is not current
    if Subst.Action = Case_Action then
      Error ("Invalid " & Mixed_Str (Subst.Action'Img)
           & " while already in this case conversion");
    end if;
    -- Activate
    Substites_List.Insert (Substites, Subst);
    Asu.Replace_Slice (The_Pattern,
                       Subst.Index, Subst.Index + 1,
                       Subst_Char & "");
    Case_Action := Subst.Action;
  end Switch_Case;

  -- Parse the replace pattern
  procedure Parse (Pattern : in String) is
    -- Start index when looking for next \
    Start : Positive;
    -- Got \ index
    Got : Natural;
    -- The char after \
    Esc_Char : Character;
    -- The Substit_Action_Rec to store
    Subst : Substit_Action_Rec;
    -- IJ hexa value
    Hexa_Byte : Byte;
    -- Current case action
    Case_Action : Case_Mode_List;

  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace parsing pattern >" & Pattern & "<");
    end if;
    -- Store pattern
    The_Pattern := Asu.To_Unbounded_String (Pattern);
    -- Replace escape sequences by coding chars
    Start := 1;
    Case_Action := Stop_Case;
    loop
      -- Locate an escape sequence, exit when no more
      Got := String_Mng.Locate_Escape (Asu.To_String (The_Pattern),
                                       Start, "\clmnRrstux");
      exit when Got = 0;
      -- Set corresponding code
      Esc_Char := Asu.Element (The_Pattern, Got);
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Replace, found Esc char >" & Esc_Char & "<");
      end if;
      -- In all case of replacement of \... by a char:
      Start := Got;
      Subst.Index := Got - 1;
      Subst.Info := 0;
      case Esc_Char is
        when '\' =>
          -- "\\" replaced by '\'
          Asu.Replace_Slice (The_Pattern, Got - 1, Got, "\");
        when 'c' =>
          -- "\c" XXX case switch off
          Subst.Action := Stop_Case;
          Switch_Case (Subst, Case_Action);
        when 'l' =>
          -- "\l" lower_case switch on
          Subst.Action := Start_Lowercase;
          Switch_Case (Subst, Case_Action);
        when 'm' =>
          -- "\m" Mixed_Case switch on
          Subst.Action := Start_Mixedcase;
          Switch_Case (Subst, Case_Action);
        when 'n' =>
          -- "\n" replaced by Line_Feed
          if Case_Action /= Stop_Case then
            -- Case substitution stops with the line, add subst marker
            Case_Action := Stop_Case;
            Subst.Action := Stop_Case;
            Substites_List.Insert (Substites, Subst);
            Asu.Replace_Slice (The_Pattern, Got - 1, Got, Subst_Char & Line_Feed);
          else
            Asu.Replace_Slice (The_Pattern, Got - 1, Got, Line_Feed);
          end if;
        when 's' =>
          -- "\s" replaced by Space
          Asu.Replace_Slice (The_Pattern, Got - 1, Got, " ");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          Asu.Replace_Slice (The_Pattern, Got - 1, Got,
                             Ada.Characters.Latin_1.Ht & "");
        when 'u' =>
          -- "\u" UPPER_CASE switch on
          Subst.Action := Start_Uppercase;
          Switch_Case (Subst, Case_Action);
        when 'x' =>
          -- "\xIJ" hexa replaced by byte
          Hexa_Byte := Get_Hexa (Got + 1);
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 2,
                             Character'Val (Hexa_Byte) & "");
        when 'R' | 'r' =>
          -- "\RIJ" or \rIJ, IJ in hexa, replaced by matching (sub) string
          -- Check IJ is a valid byte in hexa
          Hexa_Byte := Get_Hexa (Got + 1);
          Subst.Info := Hexa_Byte;
          if Esc_Char = 'R' then
            -- Replace by regex index IJ, check IJ
            if Hexa_Byte = 0
            or else Hexa_Byte > Search_Pattern.Number then
              Error ( "Invalid (null or too high) regex index "
                & Asu.Slice (The_Pattern, Got + 1, Got + 2)
                & " in replace pattern");
            end if;
            Subst.Action := Replace_Match_Regex;
          else
            -- Replace by regex I substring J
            -- Check I
            if Hexa_Byte / 16#10# = 0
            or else Hexa_Byte / 16#10# > Search_Pattern.Number then
              Error ( "Invalid (null or too high) regex index "
                & Asu.Element (The_Pattern, Got + 1)
                & " in replace pattern");
            end if;
            -- Check J
            if Hexa_Byte rem 16#10#
               > Search_Pattern.Nb_Substrings (Hexa_Byte / 16#10#) then
              Error ("Invalid (too high) substr index "
                & Asu.Element (The_Pattern, Got + 2)
                & " of regex index " & Asu.Element (The_Pattern, Got + 1)
                & " in replace pattern");
            end if;
            Subst.Action := Replace_Match_Substring;
          end if;
          -- Store that, at this index, there is a (sub) string match
          Substites_List.Insert (Substites, Subst);
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 2,
                             Subst_Char & "");
        when others =>
          -- Impossible. Leave sequence as it is, skip it
          Start := Got + 1;
      end case;
      -- Also done if end of pattern
      exit when Start >= Asu.Length (The_Pattern);
    end loop;
    -- Stop case substitution at end of line, add subst marker
    if Case_Action /= Stop_Case then
      Subst.Index := Asu.Length (The_Pattern) + 1;
      Subst.Action := Stop_Case;
      Subst.Info := 0;
      Substites_List.Insert (Substites, Subst);
      Asu.Append (The_Pattern, Subst_Char & "");
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace stored pattern >"
                               & Asu.To_String (The_Pattern) & "<");
    end if;
  end Parse;

  -- Extract a substring of string matching a regex
  subtype Extraction_List is Subtit_Action_List
          range Replace_Match_Regex .. Replace_Match_Substring;
  function Matchstring (Kind : Extraction_List; Nth : Byte) return String is
    Rth : Positive;
    Sth : Search_Pattern.Nb_Sub_String_Range;
  begin
    if Kind = Replace_Match_Regex then
      -- Nth is the regex index
      Rth := Nth;
      Sth := 0;
    else
      -- Nth is the regex index / substr index
      Rth := Nth / 16;
      Sth := Nth rem 16;
    end if;
    return Search_Pattern.Substring (Rth, Sth);
  exception
    when Error:others =>
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Replace.Matchstring: exception "
                                & Ada.Exceptions.Exception_Name (Error));
      end if;
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                & " INTERNAL ERROR. Cannot find matching (sub)string "
                & Rth'Img & "," & Sth'Img);
      raise Replace_Error;
  end Matchstring;

  -- Apply XXX case conversion to Str(Start .. Stop)
  -- Return the converted Str(Start .. Stop)
  function Casestring (Str : String;
                       Action : Case_Mode_List) return String is
  begin
    case Action is
      when Start_Uppercase =>
        return Upper_Str (Str);
      when Start_Lowercase =>
        return Lower_Str (Str);
      when Start_Mixedcase =>
        return Mixed_Str (Str);
      when Stop_Case =>
       return Str;
    end case;
  end Casestring;
  -- Return the replacing string
  function Replace return String is
    Result : Asu.Unbounded_String;
    -- Current index in result
    Start : Positive;
    -- Index of next Subst_Char in result
    Got : Natural;
    -- Offset between result and Substites list due to previous substitutions:
    --  Substitution replaces "Got" Subst_Char character of Result
    --  by Sub_Str, or removes it. So potential next Subst_Char in Result
    --  will not match any more the index stored in Substites
    Offset : Integer;
    -- Substitution action item
    Valid_Subst : Boolean;
    Subst : Substit_Action_Rec;
    -- Case substitution start index (0 if none) and mode (Stop if none)
    Case_Index : Natural;
    Case_Mode : Case_Mode_List;
  begin
    -- Init result with replace pattern
    Result := The_Pattern;
    if Debug.Set then
      Sys_Calls.Put_Error ("Replace, working with >");
      for I in 1 .. Asu.Length (Result) loop
        if Asu.Element (Result, I) = Subst_Char then
          Sys_Calls.Put_Error ("<Subst>");
        else
          Sys_Calls.Put_Error (Asu.Element (Result, I) & "");
        end if;
      end loop;
      Sys_Calls.Put_Line_Error ("<");
    end if;
    -- Replace all occurences of replace code, toggle case substitution...
    Start := 1;
    Offset := 0;
    Case_Index := 0;
    Case_Mode := Stop_Case;
    loop
      -- Locate replace code
      Got := String_Mng.Locate (Asu.To_String (Result),
                                Start, Subst_Char & "");
      exit when Got = 0;
      -- Check that this is a match action record
      Try_Subst:
      begin
        Subst.Index := Got - Offset;
        Substites_List.Read (Substites, Subst, Subst);
        Valid_Subst := True;
      exception
        when Substites_List.Not_In_List =>
          -- This Subst_Char is a normal char
          Valid_Subst := False;
      end Try_Subst;
      -- Process match action
      if Valid_Subst then
        case Subst.Action is
          when Replace_Match_Regex | Replace_Match_Substring =>
            -- Replace by (sub) Str matching regex
            declare
             -- Get full or partial substring
              Sub_Str : constant String
                      := Matchstring (Subst.Action, Subst.Info);
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
          when Start_Uppercase | Start_Lowercase
             | Start_Mixedcase | Stop_Case =>
            if Case_Index /= 0 then
              -- End of a casing: apply
              if Debug.Set then
                Sys_Calls.Put_Line_Error ("Replace, converting >"
                       & Asu.Slice (Result, Case_Index, Got - 1) & "< to "
                       & Mixed_Str(Case_Mode'Img));
              end if;
              -- New or stop casing: replace from Start to Subst_Char
              -- This is compatible with utf8 without explicit checks
              declare
                Case_Str : constant String
                         := Casestring (Asu.Slice (Result, Case_Index, Got - 1),
                                        Case_Mode);
              begin
                Asu.Replace_Slice (Result, Case_Index, Got, Case_Str);
              end;
            else
              -- No casing active: remove subst tag
              Asu.Replace_Slice (Result, Got, Got, "");
            end if;
            if Subst.Action = Stop_Case then
              -- Stop casing
              Case_Index := 0;
              Case_Mode := Stop_Case;
            else
              -- New case
              Case_Index := Got;
              Case_Mode := Subst.Action;
            end if;
            if Debug.Set then
              Sys_Calls.Put_Line_Error ("Replace, marking start case at"
                       & Case_Index'Img
                       & " with " & Subst.Action'Img);
            end if;
            -- Restart locate from first char after case switch
            Start := Got;
            Offset := Offset - 1;
        end case;
      else
        -- This Subst_Char is, in fact, not a subst action
        Start := Got + 1;
      end if;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace, replacing by >"
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
                               & " while replacing string by >"
                               & Asu.To_String (The_Pattern) & "<");
      raise Replace_Error;
  end Replace;

end Replace_Pattern;

