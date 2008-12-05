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
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

  -- Regex and subbstring indexes of \R and \r
  subtype Byte is Natural range 0 .. 255;

  -- Record describing an action to perform during substitution
  -- Replace by string matching a regex (Info)
  -- Replace by substring No (Info rem 16) of string matching No (Info / 16)
  -- Start/stop upper/lower/mixed case conversion
  type Subtit_Action_List is (
       -- These ones lead to substring extraction
       Replace_Match_Regex, Replace_Match_Substring,
       If_Match_Substring, Elsif_Match_Substring,
       And_Then_Match_Substring, Or_Else_Match_Substring,
       -- These ones don't
       Else_Match_Substring, End_If_Match_Substring,
       Start_Uppercase, Start_Lowercase, Start_Mixedcase, Stop_Case);
  -- Substring extraction kind
  subtype Extraction_List is Subtit_Action_List
          range Replace_Match_Regex .. Or_Else_Match_Substring;
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
    -- Current If action
    type If_Mode_List is (None, In_If, In_Else);
    If_Mode : If_Mode_List;
    If_Index : Positive;

  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace parsing pattern >" & Pattern & "<");
    end if;
    -- Store pattern
    The_Pattern := Asu.To_Unbounded_String (Pattern);
    -- Replace escape sequences by coding chars
    Start := 1;
    Case_Action := Stop_Case;
    If_Mode := None;
    loop
      -- Locate an escape sequence, exit when no more
      Got := String_Mng.Locate_Escape (Asu.To_String (The_Pattern),
                                       Start, "\acefilmnoRrstux");
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
        when 'e' =>
          -- "\e" else of if
          Subst.Action := Else_Match_Substring;
          if If_Mode /= In_If then
            Error ("Invalid " & Mixed_Str (Subst.Action'Img)
                 & " while not in condition");
          end if;
          Substites_List.Insert (Substites, Subst);
          Asu.Replace_Slice (The_Pattern, Subst.Index, Subst.Index + 1,
                             Subst_Char & "");
          If_Mode := In_Else;
        when 'f' =>
          -- "\f" end of if
          Subst.Action := End_If_Match_Substring;
          if If_Mode = None then
            Error ("Invalid " & Mixed_Str (Subst.Action'Img)
                 & " while not in condition or else part");
          end if;
          Substites_List.Insert (Substites, Subst);
          Asu.Replace_Slice (The_Pattern, Subst.Index, Subst.Index + 1,
                             Subst_Char & "");
          If_Mode := None;
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
        when 'R' | 'r' | 'i' | 'a' | 'o' =>
          -- "\RIJ" or \rIJ, IJ in hexa, replaced by matching (sub) string
          -- "\iIJ", IJ in hexa, replaced by text if substring is matching
          -- Check IJ is a valid byte in hexa
          Hexa_Byte := Get_Hexa (Got + 1);
          Subst.Info := Hexa_Byte;
          if Esc_Char = 'R' then
            -- Replace by regex index IJ, check IJ
            if Hexa_Byte > Search_Pattern.Number then
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
            if Esc_Char = 'r' then
              Subst.Action := Replace_Match_Substring;
            elsif Esc_Char = 'i' then
              -- Check 'if' or 'elsif' directive
              case If_Mode is
                when None =>
                  Subst.Action := If_Match_Substring;
                when In_If =>
                  Subst.Action := Elsif_Match_Substring;
                when In_Else =>
                  Error ("Invalid 'if' or 'elsif' directive while in 'else' "
                         & "of condition");
              end case;
              If_Mode := In_If;
              If_Index := Subst.Index;
            else
              -- Check 'and then' or 'or else '
              if If_Mode /= In_If then
                Error ("Invalid 'and then' or 'or else ' directive while "
                     & " not in 'if' or 'elsif' condition");
              end if;
              if Subst.Index /= If_Index + 1 then
                Error ("Invalid 'and then' or 'or else ' directive "
                     & " not part of 'if' or 'elsif' condition");
              end if;
              If_Index := Subst.Index;
              if Esc_Char = 'a' then
                Subst.Action := And_Then_Match_Substring;
              else
                Subst.Action := Or_Else_Match_Substring;
              end if;
            end if;
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
      Subst.Action := Stop_Case;
      Subst.Index := Asu.Length (The_Pattern) + 1;
      Subst.Info := 0;
      Substites_List.Insert (Substites, Subst);
      Asu.Append (The_Pattern, Subst_Char & "");
    end if;
    -- Stop conditional section at end of line, add subst marker
    if If_Mode /= None then
      Subst.Action := End_If_Match_Substring;
      Subst.Index := Asu.Length (The_Pattern) + 1;
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
  function Matchstring (Kind : Extraction_List; Nth : Byte) return String is
    Rth : Positive;
    Sth : Search_Pattern.Nb_Sub_String_Range;
  begin
    if Kind = Replace_Match_Regex and then Nth = 0 then
      -- \R00
      return Search_Pattern.Allstring;
    else
      if Kind = Replace_Match_Regex then
        -- \RIJ
        -- Nth is the regex index
        Rth := Nth;
        Sth := 0;
      else
        -- \rIJ or \iIJ or \aIJ or \oIJ
        -- Nth is the regex index / substr index
        Rth := Nth / 16;
        Sth := Nth rem 16;
      end if;
      return Search_Pattern.Substring (Rth, Sth);
    end if;
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
  type If_Status_List is (None, If_Ok, If_Ko, In_Else);
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
    -- If substring does not match, skip from start_skip to end
    Start_Skip : Natural;
    -- None outside If (\i), then If_Ko as long as no If is OK,
    -- then Is_Ok if a If is Ok, then possible Else_Ok, then None
    If_Status : If_Status_List;
    -- Anonymous exception when Subst.Action and If_Status don't match
    If_Action_Mismatch : exception;
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
    Start_Skip := 0;
    If_Status := None;
    loop
      -- Locate replace code
      Got := String_Mng.Locate (Asu.To_String (Result),
                                Subst_Char & "", Start);
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
          when If_Match_Substring .. End_If_Match_Substring =>
            -- End of a previous section to skip?
            if Start_Skip /= 0 then
              -- Delete from Start_Skip to current
              Asu.Delete (Result, Start_Skip, Got);
              Offset := Offset - (Got - Start_Skip);
              -- Start_Skip remains unchanged
              Got := Start_Skip;
            else
              -- Remove this tag
              Asu.Delete (Result, Got, Got);
            end if;
            -- Restart locate from first char after case switch
            Start_Skip := Got;
            Start := Got;
            Offset := Offset - 1;
            if Debug.Set then
              if Subst.Action = End_If_Match_Substring
              or else Subst.Action = Else_Match_Substring then
                Sys_Calls.Put_Line_Error (
                  "Replace, got 'if' action " & Subst.Action'Img);
              else
                Sys_Calls.Put_Line_Error (
                  "Replace, got 'if' action " & Subst.Action'Img
                  & " on string >"
                  & Matchstring (Subst.Action, Subst.Info) & "<");
              end if;
            end if;
            case If_Status is
              when None =>
                if Subst.Action /= If_Match_Substring then
                  raise If_Action_Mismatch;
                end if;
                -- Entering if
                if Matchstring (Subst.Action, Subst.Info) /= "" then
                  -- This if is Ok
                  If_Status := If_Ok;
                  Start_Skip := 0;
                else
                  If_Status := If_Ko;
                end if;
              when If_Ko =>
                case Subst.Action is
                  when If_Match_Substring =>
                    raise If_Action_Mismatch;
                  when Elsif_Match_Substring =>
                    if Matchstring (Subst.Action, Subst.Info) /= "" then
                      -- After one or several if were Ko, this elsif is Ok
                      If_Status := If_Ok;
                      Start_Skip := 0;
                    end if;
                    -- otherwise it remains If_Ko
                  when Else_Match_Substring =>
                    -- After one or several if were Ko, the else is Ok
                    If_Status := In_Else;
                    Start_Skip := 0;
                  when End_If_Match_Substring =>
                    If_Status := None;
                    Start_Skip := 0;
                  when And_Then_Match_Substring =>
                    -- And_Then after If_Ko -> remains If_Ko
                    null;
                  when Or_Else_Match_Substring =>
                    if Matchstring (Subst.Action, Subst.Info) /= "" then
                      -- Or_Else Ok after If_Ko -> If_Ok
                      If_Status := If_Ok;
                      Start_Skip := 0;
                    end if;
                  when others =>
                    -- Impossible
                    raise If_Action_Mismatch;
                end case;
              when If_Ok =>
                case Subst.Action is
                  when If_Match_Substring =>
                    raise If_Action_Mismatch;
                  when Elsif_Match_Substring =>
                    -- it remains If_Ko
                    null;
                  when Else_Match_Substring =>
                    -- A If was Ok, everything is skipped
                    If_Status := In_Else;
                  when End_If_Match_Substring =>
                    If_Status := None;
                    Start_Skip := 0;
                  when And_Then_Match_Substring =>
                    if Matchstring (Subst.Action, Subst.Info) /= "" then
                      -- And_Then Ok after If_Ok -> If_Ok
                      Start_Skip := 0;
                    else
                      -- And_Then Ko after If_Ok -> If_Ko
                      If_Status := If_Ko;
                    end if;
                  when Or_Else_Match_Substring =>
                    -- Or_Else after If_Ok -> remains If_Ok
                    Start_Skip := 0;
                  when others =>
                    -- Impossible
                    raise If_Action_Mismatch;
                end case;
              when In_Else =>
                if Subst.Action /= End_If_Match_Substring then
                  raise If_Action_Mismatch;
                end if;
                If_Status := None;
                Start_Skip := 0;
            end case;
          when Replace_Match_Regex | Replace_Match_Substring =>
            if Start_Skip = 0 then
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
            else
              Start := Got + 1;
            end if;
          when Start_Uppercase | Start_Lowercase
             | Start_Mixedcase | Stop_Case =>
            if Start_Skip = 0 then
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
                Asu.Delete (Result, Got, Got);
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
            end if;
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

