package File_Hash is

  -- The goal is to store the hash of word (0 to 3FF) and a reasonable length
  --  of the word (x?) in FFFF, so x is on 3F (IIII II) => 63
  -- Longer names are not stored
  Max_Str_Len : constant := 16#3F#;

  -- Init the table from a file
  Init_Error : exception;
  procedure Init (File_Name : in String);

  -- See if a word exists
  Too_Long : exception;
  function Exists (Word : String) return Boolean;

end File_Hash;

