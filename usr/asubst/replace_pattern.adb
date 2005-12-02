with Ada.Strings.Unbounded, Ada.Characters.Latin_1;
with Argument, Sys_Calls, String_Mng, Text_Line, Debug;
package body Replace_Pattern is

  package Asu renames Ada.Strings.Unbounded;

  -- The pattern to replace
  The_Pattern : Asu.Unbounded_String;

  -- The character in the pattern that code "\&" (-> found string)
  Found_Char : constant Character := Ada.Characters.Latin_1.Bs;

  procedure Parse (Pattern : in String) is
    Start : Positive;
    Got : Natural;
    New_Char : Character;
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
                                       Start, "\nt&");
      exit when Got = 0;
      -- Set corresponding code
      New_Char := Asu.Element (The_Pattern, Got + 1);
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Found char >" & New_Char & "<");
      end if;
      case New_Char is
        when 'n' => New_Char := Text_Line.Line_Feed;
        when 't' => New_Char := Ada.Characters.Latin_1.Ht;
        when '&' => New_Char := Found_Char;
        when others => New_Char := '\';
      end case;
      if New_Char = '\' then
        -- Leave sequence as it is, skip it
        Start := Got + 2;
      else
        -- Replace the sequence
        Asu.Replace_Slice (The_Pattern, Got, Got + 1, New_Char & "");
        Start := Got + 1;
      end if;
      -- Also done if end of pattern
      exit when Start = Asu.Length (The_Pattern);
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replace stored pattern >"
                               & Asu.To_String (The_Pattern) & "<");
    end if;
  end Parse;

  function Replace (Str : String) return String is
    Result : Asu.Unbounded_String;
    Start : Positive;
    Got : Natural;
  begin
    -- Init result with replace pattern
    Result := The_Pattern;
    -- Replace all occurences of replace code
    Start := 1;
    loop
      -- Locate replace code
      Got := String_Mng.Locate (Asu.To_String (Result),
                                Start, Found_Char & "");
      exit when Got = 0;
      -- Replace by Str pattern
      Asu.Replace_Slice (Result, Got, Got, Str);
      -- Restart locate from first char after replacement
      Start := Got + Str'Length;
    end loop;
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Replacing >" & Str & "< by >"
         & Asu.To_String (Result) & "<");
    end if;
    return Asu.To_String (Result);
  exception
    when others =>
      raise Replace_Error;
  end Replace;

end Replace_Pattern;

