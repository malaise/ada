with Ada.Characters.Latin_1, Ada.Exceptions;
with As.U, Argument, Sys_Calls, Str_Util, Text_Line, Hashed_List.Unique,
     Hexa_Utils, Upper_Str, Lower_Str, Mixed_Str, Command, Images;
with Search_Pattern, Debug;
package body Replace_Pattern is

  function Code_Image is new Images.Int_Image (Command.Exit_Code_Range);

  -- The pattern to replace
  The_Pattern : As.U.Asu_Us;

  -- The character in the pattern that code a substitution
  Subst_Char : constant Character := Ada.Characters.Latin_1.Bs;

  -- The line feed string
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

  -- Regex and substring indexes of \R and \r
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
       Start_Command, Stop_Command,
       Start_File, Stop_File,
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
  package H_Substites_List is new Hashed_List (Substit_Action_Rec,
                    Substit_Action_Access, Set, "=", Image);
  package Substites_List is new H_Substites_List.Unique;
  Substites : Substites_List.Unique_List_Type;

  -- What is current case substitution mode
  subtype Case_Mode_List is Subtit_Action_List
            range Start_Uppercase .. Stop_Case;

  -- Put error message (no raise)
  procedure Put_Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR: " & Msg & ".");
  end Put_Error;

  -- Reports a parsing error
  procedure Error (Msg : in String) is
  begin
    Put_Error (Msg);
    raise Parse_Error;
  end Error;

  -- Check and get an hexa code from The_Pattern (Index .. Index + 1)
  function Get_Hexa (Index : Positive; Hexa : in Boolean) return Byte is
   Error_Msg : As.U.Asu_Us;
   Result : Byte;
  begin
    -- Set error message
    if Hexa then
      Error_Msg := As.U.Tus ("hexadecimal sequence");
    else
      Error_Msg := As.U.Tus ("regex or substring index");
    end if;
    -- First digit: 16 * C
    if Index > The_Pattern.Length then
      Error ("No " & Error_Msg.Image & " at the end of replace pattern");
    end if;
    begin
      Result := 16#10# * Hexa_Utils.Char_To_Hexa (The_Pattern.Element (Index));
    exception
      when Constraint_Error =>
        Error ("Invalid " & Error_Msg.Image & " "
             & The_Pattern.Slice (Index, Index + 1)
             & " in replace pattern");
    end;
    -- First digit: 1 * C
    if Index + 1 > The_Pattern.Length then
      Error ("Uncomplete " & Error_Msg.Image & " at the end of replace pattern");
      raise Parse_Error;
    end if;
    begin
      Result := Result
              + Hexa_Utils.Char_To_Hexa (The_Pattern.Element (Index + 1));
    exception
      when Constraint_Error =>
        Error ("Invalid " & Error_Msg.Image & " "
             & The_Pattern.Slice (Index, Index + 1)
             & " in replace pattern");
    end;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace, got " & Error_Msg.Image & " "
                               & The_Pattern.Slice (Index, Index + 1));
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
    Substites.Insert (Subst);
    The_Pattern.Replace (Subst.Index, Subst.Index + 1, Subst_Char & "");
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
    -- Currently in command, in file
    In_Command, In_File : Boolean;

  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace parsing pattern >" & Pattern & "<");
    end if;
    -- Store pattern
    The_Pattern := As.U.Tus (Pattern);
    -- Replace escape sequences by coding chars
    Start := 1;
    Case_Action := Stop_Case;
    If_Mode := None;
    In_File := False;
    In_Command := False;
    loop
      -- Locate an escape sequence, exit when no more
      Got := Str_Util.Locate_Escape (The_Pattern.Image,
                                       Start, "\acefiKklmnoPpRrstux");
      exit when Got = 0;
      -- Set corresponding code
      Esc_Char := The_Pattern.Element (Got);
      -- In command or file, skip unexpected characters. Handle k|p, R and r
      if In_Command and then
        (Esc_Char /= 'k'and then Esc_Char /= 'R' and then Esc_Char /= 'r') then
        -- Skip this char by settting Esc_Char tu Subst_Char, which:
        --  should not be in the original replace pattern
        --  is not in the Locate_Escape above
        --  is not in the case statement below
        Esc_Char := Subst_Char;
      elsif In_File and then
        (Esc_Char /= 'p'and then Esc_Char /= 'R' and then Esc_Char /= 'r') then
        -- Skip this char by settting Esc_Char tu Subst_Char, which:
        --  should not be in the original replace pattern
        --  is not in the Locate_Escape above
        --  is not in the case statement below
        Esc_Char := Subst_Char;
      else
        if Debug.Set then
          Sys_Calls.Put_Line_Error ("Replace, found Esc char >" & Esc_Char & "<");
        end if;
      end if;
      -- In all case of replacement of \... by a char:
      Start := Got;
      Subst.Index := Got - 1;
      Subst.Info := 0;
      case Esc_Char is
        when '\' =>
          -- "\\" replaced by '\'
          The_Pattern.Replace (Got - 1, Got, "\");
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
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          If_Mode := In_Else;
        when 'f' =>
          -- "\f" end of if
          Subst.Action := End_If_Match_Substring;
          if If_Mode = None then
            Error ("Invalid " & Mixed_Str (Subst.Action'Img)
                 & " while not in condition or else part");
          end if;
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          If_Mode := None;
        when 'K' =>
          -- "\K" to start command
          Subst.Action := Start_Command;
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          In_Command := True;
        when 'k' =>
          -- "\k" to stop command
          Subst.Action := Stop_Command;
          if not In_Command then
            Error ("Invalid " & Mixed_Str (Subst.Action'Img)
                 & " while not in command");
          end if;
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          In_Command := False;
        when 'P' =>
          -- "\P" to start file insertion
          Subst.Action := Start_File;
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          In_File := True;
        when 'p' =>
          -- "\p" to stop file insertion
          Subst.Action := Stop_File;
          if not In_File then
            Error ("Invalid " & Mixed_Str (Subst.Action'Img)
                 & " while not in file");
          end if;
          Substites.Insert (Subst);
          The_Pattern.Replace (Subst.Index, Subst.Index + 1,
                                     Subst_Char & "");
          In_File := False;
        when 'l' =>
          -- "\l" lower_case switch on
          Subst.Action := Start_Lowercase;
          Switch_Case (Subst, Case_Action);
        when 'm' =>
          -- "\m" Mixed_Case switch on
          Subst.Action := Start_Mixedcase;
          Switch_Case (Subst, Case_Action);
        when 'n' =>
          The_Pattern.Replace (Got - 1, Got, Line_Feed);
        when 's' =>
          -- "\s" replaced by Space
          The_Pattern.Replace (Got - 1, Got, " ");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          The_Pattern.Replace (Got - 1, Got, Ada.Characters.Latin_1.Ht & "");
        when 'u' =>
          -- "\u" UPPER_CASE switch on
          Subst.Action := Start_Uppercase;
          Switch_Case (Subst, Case_Action);
        when 'x' =>
          -- "\xIJ" hexa replaced by byte
          Hexa_Byte := Get_Hexa (Got + 1, True);
          The_Pattern.Replace (Got - 1, Got + 2,
                               Character'Val (Hexa_Byte) & "");
        when 'R' | 'r' | 'i' | 'a' | 'o' =>
          -- "\RIJ" or \rIJ, IJ in hexa, replaced by matching (sub) string
          -- "\iIJ", IJ in hexa, replaced by text if substring is matching
          -- Check IJ is a valid byte in hexa
          Hexa_Byte := Get_Hexa (Got + 1, False);
          Subst.Info := Hexa_Byte;
          if Esc_Char = 'R' then
            -- Replace by regex index IJ, check IJ
            if Hexa_Byte > Search_Pattern.Number then
              Error ( "Invalid (null or too high) regex index "
                & The_Pattern.Slice (Got + 1, Got + 2)
                & " in replace pattern");
            end if;
            Subst.Action := Replace_Match_Regex;
          else
            -- Replace by regex I substring J
            -- Check I
            if Hexa_Byte / 16#10# = 0
            or else Hexa_Byte / 16#10# > Search_Pattern.Number then
              Error ( "Invalid (null or too high) regex index "
                & The_Pattern.Element (Got + 1)
                & " in replace pattern");
            end if;
            -- Check J
            if Hexa_Byte rem 16#10#
               > Search_Pattern.Nb_Substrings (Hexa_Byte / 16#10#) then
              Error ("Invalid (too high) substr index "
                & The_Pattern.Element (Got + 2)
                & " of regex index " & The_Pattern.Element (Got + 1)
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
          Substites.Insert (Subst);
          The_Pattern.Replace (Got - 1, Got + 2, Subst_Char & "");
        when others =>
          -- Impossible or a \* to skip within command
          -- Leave sequence as it is, skip it
          Start := Got + 1;
      end case;
      -- Also done if end of pattern
      exit when Start >= The_Pattern.Length;
    end loop;
    -- Check not in case substitution
    if Case_Action /= Stop_Case then
      Error ("Un-terminated case substitution");
    end if;
    -- Check not in conditional section
    if If_Mode /= None then
      Error ("Un-terminated 'if', 'elsif' or 'else' directive");
    end if;
    -- Check not in command or file section
    if In_File then
      Error ("Un-terminated file insertion");
    end if;
    if In_Command then
      Error ("Un-terminated command");
    end if;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace stored pattern >"
                               & The_Pattern.Image & "<");
    end if;
  end Parse;

  -- Returns if pattern is empty
  function Is_Empty return Boolean is
  begin
    return The_Pattern.Is_Null;
  end Is_Empty;

  -- Return replace pattern
  function Get return String is
  begin
    return The_Pattern.Image;
  end Get;

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

  -- Issue a Shell command and return its stdout, raises Command_Error
  --  if command did not exit with code 0
  Out_Flow, Err_Flow : aliased Command.Flow_Rec (Command.Str);
  function Shell_Command (Cmd : String) return String is separate;

  -- Open a file and return its content, raises File_Error
  -- open, read or close fails
  function File_Content (Path : String) return String is separate;

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
    Result : As.U.Asu_Us;
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
    -- Command start index (0 if none)
    Command_Index : Natural;
    -- File start index (0 if none)
    File_Index : Natural;
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
      for I in 1 .. Result.Length loop
        if Result.Element (I) = Subst_Char then
          Sys_Calls.Put_Error ("<Subst>");
        else
          Sys_Calls.Put_Error (Result.Element (I) & "");
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
    Command_Index := 0;
    File_Index := 0;
    If_Status := None;
    loop
      -- Locate replace code
      Got := Str_Util.Locate (Result.Image, Subst_Char & "", Start);
      exit when Got = 0;
      -- Check that this is a match action record
      Try_Subst:
      begin
        Subst.Index := Got - Offset;
        Substites.Read (Subst);
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
              Result.Delete (Start_Skip, Got);
              Offset := Offset - (Got - Start_Skip);
              -- Start_Skip remains unchanged
              Got := Start_Skip;
            else
              -- Remove this tag
              Result.Delete (Got, Got);
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
          when Start_Command =>
            if Start_Skip = 0 then
              Command_Index := Got;
              -- Go on searching from next char
              Start := Got + 1;
            end if;
          when Stop_Command =>
            if Start_Skip = 0 then
              Result.Replace (Command_Index, Got,
                  Shell_Command (Result.Slice (Command_Index + 1, Got - 1)));
            else
              -- Go on searching from next char
              Start := Got + 1;
            end if;
          when Start_File =>
            if Start_Skip = 0 then
              File_Index := Got;
              -- Go on searching from next char
              Start := Got + 1;
            end if;
          when Stop_File =>
            if Start_Skip = 0 then
              Result.Replace (File_Index, Got,
                  File_Content (Result.Slice (File_Index + 1, Got - 1)));
            else
              -- Go on searching from next char
              Start := Got + 1;
            end if;
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
                Result.Replace (Got, Got, Sub_Str);
                -- Restart locate from first char after replacement
                Start := Got + Sub_Str'Length;
                Offset := Offset + Sub_Str'Length - 1;
              end;
            else
              -- Go on searching from next char
              Start := Got + 1;
            end if;
          when Start_Uppercase | Start_Lowercase
             | Start_Mixedcase | Stop_Case =>
            if Start_Skip = 0 then
              if Case_Index /= 0 then
                -- End of a casing: apply
                if Debug.Set then
                  Sys_Calls.Put_Line_Error ("Replace, converting >"
                         & Result.Slice (Case_Index, Got - 1) & "< to "
                         & Mixed_Str(Case_Mode'Img));
                end if;
                -- New or stop casing: replace from Start to Subst_Char
                -- This is compatible with utf8 without explicit checks
                declare
                  Case_Str : constant String
                           := Casestring (Result.Slice (Case_Index, Got - 1),
                                          Case_Mode);
                begin
                  Result.Replace (Case_Index, Got, Case_Str);
                end;
              else
                -- No casing active: remove subst tag
                Result.Delete (Got, Got);
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
            else
              -- Go on searching from next char
              Start := Got + 1;
            end if;
        end case;
      else
        -- This Subst_Char is, in fact, not a subst action
        Start := Got + 1;
      end if;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace, replacing by >" & Result.Image & "<");
    end if;
    return Result.Image;
  exception
    when Replace_Error =>
      raise;
    when File_Error =>
      raise;
    when Command_Error =>
      raise;
    when Terminate_Request =>
      raise;
    when Error:others =>
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                               & " INTERNAL ERROR: "
                               & Ada.Exceptions.Exception_Name (Error)
                               & " while replacing string by >"
                               & The_Pattern.Image & "<");
      raise Replace_Error;
  end Replace;

end Replace_Pattern;

