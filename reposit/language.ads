package Language is

  -- When ENV, UTF_8 is set if a Getenv on "LANG" gives a value
  --  ending by ".UTF-8". This is the default behaviour.
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


  -- Conversion to and from wide character
  -- Last wide character that can be casted to character without decoding
  Last_Char : constant Wide_Character := Wide_Character'Val (16#7F#);
  function Is_Char (W : Wide_Character) return Boolean;
  function Char_To_Wide (C : Character) return Wide_Character;
  -- Raises Constraint_Error if not Is_Char (W)
  function Wide_To_Char (W : Wide_Character) return Character;

  -- Conversion from wide string
  -- May raise Utf_8 exceptions
  function Wide_To_String (Str : Wide_String) return String;
  function "&" (Left : String; Right : Wide_String) return String;
  function "&" (Left : Wide_String; Right : String) return String;

  -- Conversion to wide string
  -- May raise Utf_8 exceptions
  function String_To_Wide (Str : String) return Wide_String;
  function "&" (Left : String; Right : Wide_String) return Wide_String;
  function "&" (Left : Wide_String; Right : String) return Wide_String;
  function "=" (Left : String; Right : Wide_String) return Boolean;
  function "=" (Left : Wide_String; Right : String) return Boolean;

  -- The following operations take into account the
  --  fact that several characters of String may be used
  --  to define one put character.
  -- They may raise Utf_8.Invalid_Sequence

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

