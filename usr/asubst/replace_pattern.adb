with Ada.Strings.Unbounded, Ada.Characters.Latin_1, Ada.Exceptions;
with Argument, Sys_Calls, String_Mng, Text_Line, Unique_List, Debug,
     Char_To_Hexa;
with Search_Pattern;
package body Replace_Pattern is

  package Asu renames Ada.Strings.Unbounded;

  -- The pattern to replace
  The_Pattern : Asu.Unbounded_String;

  -- The character in the pattern that code "\IJ (-> matching (sub)string)
  Match_Char : constant Character := Ada.Characters.Latin_1.Bs;

  -- The indexes, in The_Pattern, of matching (sub)strings
  subtype Byte is Natural range 0 .. 255;
  subtype Substr_Index is Byte;
  type Match_Info is record
    Index : Positive;
    Substr : Substr_Index;
  end record;
  procedure Set (To : out Match_Info; Val : in Match_Info) is
  begin
    To := Val;
  end Set;
  -- Access by index in The_Pattern
  function Image (Element : Match_Info) return String is
  begin
    return Element.Index'Img;
  end Image;
  function "=" (Current : Match_Info; Criteria : Match_Info)
               return Boolean is
  begin
    return Current.Index = Criteria.Index;
  end "=";
  package Matches_List is new Unique_List (Match_Info, Set, Image);
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
    Match : Substr_Index;

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
                                       Start, "\nt0123456789ABCDEFabcdefx");
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
          Asu.Replace_Slice (The_Pattern, Got - 1, Got,
                             Text_Line.Line_Feed & "");
        when 't' =>
          -- "\t" replaced by (horiz) tab
          Asu.Replace_Slice (The_Pattern, Got - 1, Got,
                             Ada.Characters.Latin_1.Ht & "");
        when '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' =>
          -- "\IJ" hexa replaced by matching (sub) string
          Match := Get_Hexa (Got);
          if Match > Search_Pattern.Number then
            Sys_Calls.Put_Line_Error (
              "Invalid (too high) matching index "
              & Asu.Slice (The_Pattern, Got, Got + 1)
              & " in replace pattern");
            raise Parse_Error;
          end if;
          -- Store that, at this index, there is a (sub) string match
          Matches_List.Insert (Matches, (Got - 1, Match));
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 1,
                             Match_Char & "");
        when 'x' =>
          -- "\xIJ" hexa replaced by byte
          Match := Get_Hexa (Got + 1);
          Asu.Replace_Slice (The_Pattern, Got - 1, Got + 2,
                             Character'Val (Match) & "");
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

  -- Extract Nth substring of string (all if 0)
  function Substring (Str : String; Nth : Substr_Index) return String is
    Prev_Delim, Got_Delim : Natural;
    Ith : Substr_Index;
  begin
    -- Index 0 => complete matching string
    if Nth = 0 then
      return Str;
    end if;
    -- Locate the Nth matching substring
    Prev_Delim := Str'First - 1;
    Ith := 0;
    loop
      -- Search next delimiter
      Got_Delim := String_Mng.Locate (Str, Prev_Delim + 1,
                                      Text_Line.Line_Feed & "");
      if Got_Delim = 0 then
        -- No (more) delim
        if Ith = Nth - 1 then
          -- Last substring is the one
          return Str(Prev_Delim + 1 .. Str'Last);
        else
          -- Less than Nth substrings in Str???
          -- Should not occure because checked during parsing
          raise Replace_Error;
        end if;
      end if;
      if Got_Delim = Prev_Delim + 1 then
        -- A delim after a delim
        Ith := Ith + 1;
        if Ith = Nth then
          -- The substring is this new delim
          return Text_Line.Line_Feed & "";
        end if;
      else
        -- A delim after a string
        Ith := Ith + 1;
        if Ith = Nth then
          -- The substring is between delims
          return Str(Prev_Delim + 1 .. Got_Delim - 1);
        else
          Ith := Ith + 1;
          if Ith = Nth then
            -- The substring is this new delim
            return Text_Line.Line_Feed & "";
          end if;
        end if;
      end if;
      Prev_Delim := Got_Delim;
    end loop;
  exception
    when others =>
      Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
                              & " INTERNAL ERROR. Cannot find " & Nth'Img
                              & " substring of >" & Str & "<");
      raise Replace_Error;  
  end Substring;
    

  function Replace (Str : String) return String is
    Result : Asu.Unbounded_String;
    Start : Positive;
    Got : Natural;
    Match : Match_Info;
  begin
    -- Init result with replace pattern
    Result := The_Pattern;
    -- Replace all occurences of replace code
    Start := 1;
    loop
      -- Locate replace code
      Got := String_Mng.Locate (Asu.To_String (Result),
                                Start, Match_Char & "");
      exit when Got = 0;
      -- Check and get substring index
      Try_Match:
      begin
        Match.Index := Got;
        Matches_List.Read (Matches, Match, Match);
        -- Replace by sub Str pattern
        declare
          Sub_Str : constant String := Substring (Str, Match.Substr);
        begin
          if Debug.Set then
            Sys_Calls.Put_Line_Error ("Replace, got sub string >"
                                    & Sub_Str & "<");
          end if;
          Asu.Replace_Slice (Result, Got, Got, Sub_Str);
          -- Restart locate from first char after replacement
          Start := Got + Sub_Str'Length;
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

