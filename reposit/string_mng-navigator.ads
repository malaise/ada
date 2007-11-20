-- Allows navigation (lookup chars) within a string
with Ada.Strings.Unbounded;
package String_Mng.Navigator is

  -- Returned when out of string
  Default_No_Char : constant Character := Ada.Characters.Latin_1.Nul;

  type Navigator_Type is tagged private;

  -- Constructor. Re-inits position to start of string and resets
  --  No_Char to default
  procedure Set (Navig : in out Navigator_Type; Str : in String);

  -- Current position. Index is relative to the string
  -- (Str'First at the beginning)
  function Position (Navig : Navigator_Type) return Integer;

  -- Move forward or backwards
  -- May move out of the string
  procedure Move (Navig : in out Navigator_Type; By : in Integer := 1);

  -- Rewind (move to first or last char)
  procedure Rewind (Navig : in out Navigator_Type;
                    To_Start : in Boolean := True);

  -- Set/get the "No_Char" character that is returned when out of string
  procedure Set_No_Char (Navig : in out Navigator_Type;
                         Char : in Character := Default_No_Char);
  function Get_No_Char (Navig : Navigator_Type) return Character;

  -- Lookup a character
  -- Returns No_Char if out the string
  function Lookup (Navig : Navigator_Type; Offset : Integer := 0)
           return Character;

private

  type Navigator_Type is tagged record
    Str : Ada.Strings.Unbounded.Unbounded_String;
    Start : Natural := 0;
    Current : Integer := 0;
    No_Char : Character := Default_No_Char;
  end record;

end String_Mng.Navigator;

