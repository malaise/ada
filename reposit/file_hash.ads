with As.U.Utils;
with Hashing, Hashed_List;
package File_Hash is

  -- The goal is to store the hash of word (0 to FFF) and a reasonable length
  --  of the word (x?) in *FFF, so x is either F (15!) of FF (255) which is OK
  -- Longer names are not stored
  Max_Str_Len : constant := 16#FF#;

  Hash_Max : constant Hashing.Max_Hash_Range := 16#FFFFF#;
  subtype Max_Hash_Range is Hashing.Max_Hash_Range
          range Hashing.Max_Hash_Range'First .. Hash_Max;
  function Hash_Func (Key : String) return Max_Hash_Range;

  package List_Mng is new Hashed_List (
       As.U.Asu_Us, As.U.Utils.Asu_Us_Access, As.U.Set, As.U."=", As.U.Image,
       Hash_Max, Hash_Func);


  -- Load the content of the file in the list
  Init_Error : exception;
  procedure Load (File_Name : in String;
                  List : in out List_Mng.List_Type);

end File_Hash;

