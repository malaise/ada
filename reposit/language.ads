with Unicode, Utf_8;
package Language is

  -- When Get_Env, Lang_Utf_8 is set if a Getenv on "LANG" gives a value
  --  containing "UTF-8" (any casing), otherwise it is Lang_C that is set.
  -- Get_Env is the default behaviour of Get_Language if Set_Language has not
  --  been called.
  -- Once set or got, the language should not be changed because several text
  --  processing utilities depend on it, so any attempt to set language twice
  --  will raise Language_Already_Set
  -- Use Force_Language to bypass this check at your own risks.
  type Language_Selection_List is (Lang_C, Lang_Utf_8, Get_Env);
  Language_Already_Set : exception;
  procedure Set_Language (Language : in Language_Selection_List);
  procedure Force_Language (Language : in Language_Selection_List);

  subtype Language_List is Language_Selection_List range Lang_C .. Lang_Utf_8;
  function Get_Language return Language_List;


  -- Independant from language:
  -----------------------------
  -- Conversion to and from wide character, to and from unicode number
  -- Last wide character and last unicode number can be casted to character
  --   without decoding
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  Wide_Last_Char : constant Wide_Character := Wide_Character'Val (16#7F#);
  Unicode_Last_Char : constant Unicode_Number := 16#7F#;
  Unicode_Last_Wide : constant Unicode_Number := 16#FFFF#;
  function Is_Char (W : Wide_Character) return Boolean;
  function Is_Char (U : Unicode_Number) return Boolean;
  function Is_Wide (U : Unicode_Number) return Boolean;
  function Char_To_Wide (C : Character) return Wide_Character;
  function Char_To_Unicode (C : Character) return Unicode_Number;
  function Wide_To_Unicode (W : Wide_Character) return Unicode_Number;
  -- Character when translation Unicode/Wide -> Char or Unicode -> Wide fails
  Default_Char : constant Character := '#';
  function Wide_To_Char (W : Wide_Character) return Character;
  function Unicode_To_Char (U : Unicode_Number) return Character;
  function Unicode_To_Wide (U : Unicode_Number) return Wide_Character;
  -- Output has same range (so same len) as input
  function Copy (W : Wide_String) return String;
  function Copy (S : String) return Wide_String;
  function Copy (U : Unicode_Sequence) return String;
  function Copy (S : String) return Unicode_Sequence;
  function Copy (U : Unicode_Sequence) return Wide_String;
  function Copy (W : Wide_String) return Unicode_Sequence;

  -- Dependant from language:
  ---------------------------
  Invalid_Utf_8_Sequence : exception renames Utf_8.Invalid_Sequence;
  Not_Wide_Character : exception renames Utf_8.Not_Wide_Character;
  -- Conversion from wide string
  function Wide_To_String (Str : Wide_String) return String;
  function "&" (Left : String; Right : Wide_String) return String;
  function "&" (Left : Wide_String; Right : String) return String;
  -- Conversion to wide string
  -- May raise Invalid_Sequence or Not_Wide_Character exception
  function String_To_Wide (Str : String) return Wide_String;
  function "&" (Left : String; Right : Wide_String) return Wide_String;
  function "&" (Left : Wide_String; Right : String) return Wide_String;
  function "=" (Left : String; Right : Wide_String) return Boolean;
  function "=" (Left : Wide_String; Right : String) return Boolean;

  -- Conversion from unicode sequence
  function Unicode_To_String (Str : Unicode_Sequence) return String;
  function "&" (Left : String; Right : Unicode_Sequence) return String;
  function "&" (Left : Unicode_Sequence; Right : String) return String;
  -- Conversion to unicode sequence
  -- May raise Invalid_Sequence exception
  function String_To_Unicode (Str : String) return Unicode_Sequence;
  function "&" (Left : String; Right : Unicode_Sequence)
               return Unicode_Sequence;
  function "&" (Left : Unicode_Sequence; Right : String)
               return Unicode_Sequence;
  function "=" (Left : String; Right : Unicode_Sequence) return Boolean;
  function "=" (Left : Unicode_Sequence; Right : String) return Boolean;

  -- The following operations take into account the
  --  fact that several characters of String may be used
  --  to define one put character (slot).
  -- They may raise Invalid_Sequence exception

  -- Number of chars needed to complete Char and put it (in one slot)
  function Nb_Chars (Char : Character) return Positive;

  -- Compute the number of slots to put Str
  function Put_Length (Str : String) return Natural;

  -- Compute the last index of Str to put a given number of slots max
  function Last_Index_For (Str : String; Put_Pos : Natural) return Natural;

  -- Adjust String so that it contains only Len valid characters
  --  (result has a length of Len or is shorter)
  function Adjust (Str : String; Len : Natural) return String;

  -- Compute all the indexes of Str corresponding to successive slots
  type Index_Array is array (Positive range <>) of Natural;
  function All_Indexes_Of (Str : String) return Index_Array;

  -- Return the slice of Str from First_Pos to Last_Pos slots included
  function Slice (Str : String;
                  First_Pos : Positive;
                  Last_Pos  : Natural) return String;

end Language;

