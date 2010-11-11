with Ada.Strings.Unbounded;
with Dynamic_List, Hashed_List.Unique, Unbounded_Arrays;
package As.U is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Is_Null (Str : Asu_Us) return Boolean;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Tus (Char : Character) return Asu_Us;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;
  function Asu_Uslice (Source : Asu_Us; Low : Positive;
                       High   : Natural) return Asu_Us
                       renames Asu.Unbounded_Slice;

  -- Dynamic_List of Asu_Us
  package Asu_List_Mng is new Dynamic_List (Asu_Us);
  subtype Asu_Us_Access is Asu_List_Mng.Element_Access;
  package Asu_Dyn_List_Mng renames Asu_List_Mng.Dyn_List;

  -- Hahsed_List and Unique_List of Asu_Us
  procedure Set (To : out Asu_Us; Val : in Asu_Us);
  function Image (Element : Asu_Us) return String;
  package Asu_Hashed_List_Mng is new Hashed_List (
       Asu_Us, Asu_Us_Access, Set, Asu."=" , Image);
  package Asu_Unique_List_Mng is new Asu_Hashed_List_Mng.Unique;

  -- Unbounded array of Asu_Us
  type Asu_Array is array (Positive range <>) of Asu_Us;
  package Asu_Unbounded_Arrays is new Unbounded_Arrays (Asu_Us, Asu_Array);
  package Asu_Ua renames Asu_Unbounded_Arrays;

end As.U;

