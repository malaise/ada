-- Utilities for handling languages
package Language_Utils is

  -- The unicode sequence to provide a language name in its language
   -- A Unicode number
  subtype Unicode_Number is Natural range 0 .. 16#10FFFF#;
  type Unicode_Sequence is array (Positive range <>) of Unicode_Number;
  function To_Unicode (Str : String) return Unicode_Sequence;

  -- A variable unicode sequence stored
  Text_Max_Length : constant := 15;
  subtype Text_Len is Natural range 0 .. Text_Max_Length;
  type Text (Len : Text_Len := 0) is record
    Txt : Unicode_Sequence (1 .. Len);
  end record;

  -- The descriptor of a language
  -- Char set of a language
  -- Array (A..Z, 0..9) of wide characters
  subtype Char_Set is Wide_String (1 .. 36);
  -- Array of codes for a language and construction of char set
  subtype Char_Code_Range is Natural
     range 0 .. Wide_Character'Pos (Wide_Character'Last);
  type Char_Code is array (Char_Set'Range) of Char_Code_Range;
  -- The descriptor
  type Language_Desc (Len : Text_Len := 0) is record
    -- The name of the language in it own language
    Name : Text (Len);
    -- The conversion characters in the language for A .. Z, 0 .. 9
    Set : Char_Set;
    -- First and last character  of the set
    First, Last : Wide_Character;
  end record;
  function Build_Desc (Name : Unicode_Sequence;
                       Code : Char_Code) return Language_Desc;

end Language_Utils;

