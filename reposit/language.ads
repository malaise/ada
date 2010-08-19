with Unicode, Utf_8;
package Language is

  -- When ENV, UTF_8 is set if a Getenv on "LANG" gives a value
  --  containing "UTF-8". This is the default behaviour.
  -- Once set or got, the language should not be changed because
  --  several text processing utilities depend on it.
  -- Attempts to set or get language twice will raise Language_Already_Set
  -- Use Force_Language to bypass this check at your own risks.
  type Language_List is (Lang_C, Lang_Utf_8, Get_Env);
  Language_Already_Set : exception;
  procedure Set_Language (Language : in Language_List);
  procedure Force_Language (Language : in Language_List);

  subtype Language_Set_List is Language_List range Lang_C .. Lang_Utf_8;
  function Get_Language return Language_Set_List;


  -- Independant from language
  -- Conversion to and from wide character, to and from uncide number
  -- Last wide character and last unicode number can be casted to character
  --   without decoding
  subtype Unicode_Number is Unicode.Unicode_Number;
  Last_Wide_Char : constant Wide_Character := Wide_Character'Val (16#7F#);
  Last_Unicode_Char : constant Unicode_Number := 16#7F#;
  function Is_Char (W : Wide_Character) return Boolean;
  function Is_Char (U : Unicode_Number) return Boolean;
  function Char_To_Wide (C : Character) return Wide_Character;
  function Char_To_Unicode (C : Character) return Unicode_Number;
  -- Return '#' if not Is_Char
  function Wide_To_Char (W : Wide_Character) return Character;
  function Unicode_To_Char (U : Unicode_Number) return Character;


  -- Dependant from language
  Invalid_Utf_8_Sequence : exception renames Utf_8.Invalid_Sequence;
  -- Conversion from wide string
  -- May raise Utf_8.Invalid_Sequence exception
  function Wide_To_String (Str : Wide_String) return String;
  function "&" (Left : String; Right : Wide_String) return String;
  function "&" (Left : Wide_String; Right : String) return String;
  -- Conversion to wide string according to language
  -- May raise Utf_8.Invalid_Sequence exception
  function String_To_Wide (Str : String) return Wide_String;
  function "&" (Left : String; Right : Wide_String) return Wide_String;
  function "&" (Left : Wide_String; Right : String) return Wide_String;
  function "=" (Left : String; Right : Wide_String) return Boolean;
  function "=" (Left : Wide_String; Right : String) return Boolean;

  -- Conversion from unicode sequence
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  function Unicode_To_String (Str : Unicode_Sequence) return String;
  function "&" (Left : String; Right : Unicode_Sequence) return String;
  function "&" (Left : Unicode_Sequence; Right : String) return String;
  -- Conversion to unicode sequence according to language
  -- May raise Utf_8.Invalid_Sequence exception
  function String_To_Unicode (Str : String) return Unicode_Sequence;
  function "&" (Left : String; Right : Unicode_Sequence)
               return Unicode_Sequence;
  function "&" (Left : Unicode_Sequence; Right : String)
               return Unicode_Sequence;
  function "=" (Left : String; Right : Unicode_Sequence) return Boolean;
  function "=" (Left : Unicode_Sequence; Right : String) return Boolean;

  -- The following operations take into account the
  --  fact that several characters of String may be used
  --  to define one put character.
  -- They may raise Utf_8.Invalid_Sequence exception

  -- Number of characters needed to complete Char and put it
  function Nb_Chars (Char : Character) return Positive;

  -- Compute the put length of Str
  function Put_Length (Str : String) return Natural;

  -- Compute the last index of Str to put a given
  --  number of characters
  function Last_Index_For (Str : String; Put_Pos : Natural) return Natural;

  -- Adjust String so that it contains only Max valid characters
  --  (result has a length of Max or is shorter)
  function Adjust (Str : String; Len : Natural) return String;

  -- Compute all the indexes of Str corresponding to successive
  --  put offset
  type Index_Array is array (Positive range <>) of Natural;
  function All_Indexes_Of (Str : String) return Index_Array;

  -- Return the slice of Str from First_Pos to Last_Pos included
  function Slice (Str : String;
                  First_Pos : Positive;
                  Last_Pos  : Natural) return String;

end Language;

