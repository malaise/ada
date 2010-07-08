with Ada.Strings.Unbounded;
with Dynamic_List, Unique_List;
package As.U is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Tus (Char : Character) return Asu_Us;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;

  -- Dynamic_List of Asu_Us
  package Asu_List_Mng is new Dynamic_List (Asu_Us);
  package Asu_Dyn_List_Mng renames Asu_List_Mng.Dyn_List;

  -- Unique_List of Asu_Us
  type Asu_Us_Access is access all Asu_Us;
  procedure Set (To : out Asu_Us; Val : in Asu_Us);
  function Image (Element : Asu_Us) return String;
  package Asu_Unique_List_Mng is new Unique_List (
       Asu_Us, Asu_Us_Access, Set, Asu."=" , Image);

end As.U;

