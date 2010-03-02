with Ada.Strings.Unbounded;
with Afpx;
package Utils is

  -- Asu stuff
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  -- If Str fits Width then return Str
  -- else return ">> " & tail
  function Normalize (Str : String; Width : Positive) return String;

  -- Remove trailing spaces and Htabs
  function Parse_Spaces (Str : String) return String;
  function Last_Index (Str : String) return Natural;

  -- The scroll buttons
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range 2 .. 8;

  -- Exception on Ctrl C
  Exit_Requested : exception;

end Utils;

