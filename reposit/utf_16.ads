-- Utf_16 encoding/decoding
with Unicode;
package Utf_16 is

  -- Utf-16 sequence of bytes, Wide_String of 1 to 2 Wide_Character
  Max_Chars : constant := 2;
  subtype Len_Range is Positive range 1 .. Max_Chars;
  subtype Sequence is Wide_String;
  -- A sequence of 1 or 2 Wide_Character
  subtype Word is Sequence;

  -- Unicode: Natural range 0 .. 16#10FFFF#;
  subtype Unicode_Number is Unicode.Unicode_Number;

  -- All the following operations suppose that the UTF-16 sequence
  --  is in big endian (UTF-16BE)

  -- Raised if incorrect UTF-16 Sequence or incorrect First_Char
  Invalid_Sequence : exception;

  -- Returns the number of chars of a word (coded in the 1st char)
  -- May raise Invalid_Sequence
  function Nb_Chars (First_Char : Wide_Character) return Len_Range;

  -- Checks that a Utf-16 word is valid
  function Is_Valid (W : Word) return Boolean;
  -- Checks that a Utf-16 word is valid, raise Invalid_Sequence if not
  procedure Check_Valid (W : in Word);


  -- Raised when encoding excluded non-character Unicode (U+D800 .. U+DFFF)
  Excluded_Non_Character : exception;

  -- Decodes a Utf-16 sequence to Unicode. May raise Invalid_Sequence
  function Decode (W : Word) return Unicode_Number;
  -- Encodes a Unicode as a Utf-16 sequence
  function Encode (Unicode : Unicode_Number) return Word;

  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  -- Decodes a Utf-16 sequence (of words) to Unicode sequence.
  -- May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Sequence;
  -- Encodes a Unicode sequence as a Utf-16 sequence (of words)
  function Encode (Unicode : Unicode_Sequence) return Sequence;


  -- Raised if a Utf-16 sequence leads to a Unicode above last
  --  Wide_Character (16#FFFF#)
  Not_Wide_Character : exception;
  -- Decodes a Utf-16 word to Wide_Character.
  -- May raise Invalid_Sequence or Not_Wide_Character
  function Decode (W : Word) return Wide_Character;
  -- Encodes a Unicode as a Utf-16 word
  function Encode (Wide_Char : Wide_Character) return Word;

  -- Swap a sequence BE <-> LE (big endian <-> little endian)
  procedure Swap (Wide_Char : in out Wide_Character);
  function Swap (Wide_Char : Wide_Character) return Wide_Character;
  procedure Swap (Seq : in out Sequence);
  function Swap (Seq : Sequence) return Sequence;

  -- Split / merge a UTF-16 sequence into a sequence of bytes (chars)
  function Split (Seq : Sequence) return String;
  -- The string length must be multiple of 2
  Odd_Length : exception;
  function Merge (Str : String) return Sequence;

end Utf_16;

