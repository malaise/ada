with Dynamic_List, Long_Long_Limited_List, Hashed_List.Unique, Unbounded_Arrays;
package As.U.Utils is

  -- Dynamic_List of Asu_Us
  package Asu_List_Mng is new Dynamic_List (Asu_Us);
  package Asu_Dyn_List_Mng renames Asu_List_Mng.Dyn_List;
  package Asu_Long_Long_List_Mng is new Long_Long_Limited_List (Asu_Us, Set);

  -- Hahsed_List and Unique_List of Asu_Us
  subtype Asu_Us_Access is As.U.Asu_Us_Access;
  package Asu_Hashed_List_Mng is new Hashed_List (
       Asu_Us, Asu_Us_Access, Set, As.U."=" , Image);
  package Asu_Unique_List_Mng is new Asu_Hashed_List_Mng.Unique;

  -- Unbounded array of Asu_Us
  subtype Asu_Array is As.U.Asu_Array;
  package Asu_Unbounded_Arrays is new Unbounded_Arrays (Asu_Us, Asu_Array);
  package Asu_Ua renames Asu_Unbounded_Arrays;

end As.U.Utils;

