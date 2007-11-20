-- Allows navigation (lookup chars) within a string
-- with Ada.Strings.Unbounded;

package body String_Mng.Navigator is
  package Asu renames Ada.Strings.Unbounded;

  -- Constructor, re-inits position to start of string
  procedure Set (Navig : in out Navigator_Type; Str : in String) is
  begin
    Navig.Str := Asu.To_Unbounded_String (Str);
    Navig.Start := Str'First;
    Navig.Current := Str'First;
    Navig.No_Char := Default_No_Char;
  end Set;


  -- Current position. Index is relative to the string
  -- (Str'First at the beginning)
  function Position (Navig : Navigator_Type) return Integer is
  begin
    return Navig.Current;
  end Position;


  -- Move forward or backwards
  -- May move out of the string
  procedure Move (Navig : in out Navigator_Type; By : in Integer := 1) is
  begin
    Navig.Current := Navig.Current + By;
  end Move;


  -- Rewind (move to first or last char)
  procedure Rewind (Navig : in out Navigator_Type;
                    To_Start : in Boolean := True) is
  begin
    if To_Start or else Asu.Length (Navig.Str) = 0 then
      -- Rewind to start or empty string
      Navig.Current := Navig.Start;
    else
      -- Rewind to end of not empty string
      Navig.Current := Navig.Start + Asu.Length (Navig.Str) - 1;
    end if;
  end Rewind;


  -- Set/get the "No_Char" character that is returned when out of string
  procedure Set_No_Char (Navig : in out Navigator_Type;
                         Char : in Character := Default_No_Char) is
  begin
    Navig.No_Char := Char;
  end Set_No_Char;

  function Get_No_Char (Navig : Navigator_Type) return Character is
  begin
    return Navig.No_Char;
  end Get_No_Char;


  -- Lookup a character
  -- Returns No_Char if out the string
  function Lookup (Navig : Navigator_Type; Offset : Integer := 0)
           return Character is
    -- Index in Str (from 1 to ...)
    Index : constant Integer := Navig.Current - Navig.Start + 1 + Offset;
  begin
    -- Check if Index is within string
    if Index <= 0 or else Index > Asu.Length (Navig.Str) then
      return Navig.No_Char;
    else
      return Asu.Element (Navig.Str, Index);
    end if;
  end Lookup;

end String_Mng.Navigator;

