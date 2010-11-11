with As.U; use As.U;
with Hashing, Hashed_List;
package File_Hash is

  -- The goal is to store the hash of word (0 to FFF) and a reasonable length
  --  of the word (x?) in *FFF, so x is either F (15!) of FF (255) which is OK
  -- Longer names are not stored
  Max_Str_Len : constant := 16#FF#;

  Hash_Max : constant Hashing.Max_Hash_Range := 16#FFFFF#;
  function Hash_Func (Key : String) return Hashing.Max_Hash_Range;

  package List_Mng is new Hashed_List (
       Asu_Us, Asu_Us_Access, Set, Asu."=" , Image, Hash_Max, Hash_Func);


  -- Load the content of the file in the list
  Init_Error : exception;
  procedure Load (File_Name : in String;
                  List : in out List_Mng.List_Type);

end File_Hash;

