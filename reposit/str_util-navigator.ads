-- Allows navigation (lookup chars) within and possibly outside (around)
--  a string
with As.U;
package Str_Util.Navigator is

  -- Returned when out of string
  Default_No_Char : constant Character := Ada.Characters.Latin_1.Nul;

  type Navigator_Type is tagged private;

  -- Constructor. Re-inits position to start of string and resets
  --  No_Char to default
  procedure Set (Navig : in out Navigator_Type; Str : in String);

  -- Length
  function Length (Navig : Navigator_Type) return Natural;

  -- Current position. Index is relative to the string
  -- (Str'First at the beginning)
  function Position (Navig : Navigator_Type) return Integer;

  -- Is a position (Current + Offset) within bounds
  function In_Bounds (Navig : Navigator_Type;
                      Offset : Integer := 0) return Boolean;

  -- Move forward or backwards
  -- If Current + By is out of the string,
  --  if Check then raises Out_Of_Bounds
  --  otherwise move out of the string
  procedure Move (Navig : in out Navigator_Type;
                  By    : in Integer := 1;
                  Check : in Boolean := False);

  -- Rewind (move to first or last char)
  procedure Rewind (Navig : in out Navigator_Type;
                    To_Start : in Boolean := True);

  -- Set/get the "No_Char" character that is returned when out of string
  procedure Set_No_Char (Navig : in out Navigator_Type;
                         Char : in Character := Default_No_Char);
  function Get_No_Char (Navig : Navigator_Type) return Character;

  -- Lookup a character
  -- If Current + Offset is out the string, raise Out_Of_Bounds if Check
  --  and return No_Char otherwise
  function Lookup (Navig  : Navigator_Type;
                   Offset : Integer := 0;
                   Check  : Boolean := False)
           return Character;

  -- Return the String (from 1 to Length) stored in the Navigator
  function Image (Navig  : Navigator_Type) return String;

  -- Raised when Check is set and moving/looking out of string
  Out_Of_Bounds : exception;

private

  type Navigator_Type is tagged record
    Str : As.U.Asu_Us;
    Start : Natural := 0;
    Current : Integer := 0;
    No_Char : Character := Default_No_Char;
  end record;

end Str_Util.Navigator;

