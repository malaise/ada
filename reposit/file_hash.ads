with As.U, Long_Longs, Hashing, Hashed_List;
package File_Hash is

  -- Stores the content of a file in a hashed list, one entry per line

  -- The goal is to store the hash of the line (0 to FFF) and a reasonable
  --  length of the line (x?) in *FFF.
  -- x could be (15) but this is too small, so it is FF (255), which is OK
  -- Longer lines are truncated
  Max_Str_Len : constant := 16#FF#;

  Hash_Max : constant Hashing.Max_Hash_Range := 16#FFFFF#;
  subtype Max_Hash_Range is Hashing.Max_Hash_Range
          range Hashing.Max_Hash_Range'First .. Hash_Max;
  function Hash_Func (Key : String) return Max_Hash_Range;

  -- Record stored for each line: line no and content
  -- The key is the content
  type Line_Rec is record
    No : Long_Longs.Ll_Positive;
    Txt : As.U.Asu_Us;
  end record;
  type Line_Access is access all Line_Rec;
  procedure Set (To : out Line_Rec; Val : in Line_Rec);
  -- Match if Txt matches
  function "=" (Current : Line_Rec; Criteria : Line_Rec) return Boolean;
  -- Image of Txt
  function Key_Image (Element : Line_Rec) return String;

  -- Hashed list of lines
  package List_Mng is new Hashed_List (
       Line_Rec, Line_Access, Set, "=", Key_Image, Hash_Max, Hash_Func);


  -- Load the content of the file in the list (insert after current position)
  Init_Error : exception;
  procedure Load (File_Name : in String;
                  List : in out List_Mng.List_Type);

end File_Hash;

