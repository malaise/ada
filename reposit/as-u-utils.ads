with Dynamic_List, Hashed_List.Unique, Unbounded_Arrays;
package As.U.Utils is

  -- Dynamic_List of Asu_Us
  package Asu_List_Mng is new Dynamic_List (Asu_Us);
  package Asu_Dyn_List_Mng renames Asu_List_Mng.Dyn_List;

  -- Hahsed_List and Unique_List of Asu_Us
  type Asu_Us_Access is access all Asu_Us;
  package Asu_Hashed_List_Mng is new Hashed_List (
       Asu_Us, Asu_Us_Access, Set, As.U."=" , Image);
  package Asu_Unique_List_Mng is new Asu_Hashed_List_Mng.Unique;

  -- Unbounded array of Asu_Us
  type Asu_Array is array (Positive range <>) of Asu_Us;
  package Asu_Unbounded_Arrays is new Unbounded_Arrays (Asu_Us, Asu_Array);
  package Asu_Ua renames Asu_Unbounded_Arrays;

end As.U.Utils;

