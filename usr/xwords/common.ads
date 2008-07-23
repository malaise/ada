with Ada.Strings.Unbounded;
-- Common definitions
package Common is

  -- Asu stuff
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

end Common;
