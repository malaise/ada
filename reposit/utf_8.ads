-- Utf_8 encoding/decoding
with Unicode;
package Utf_8 is

  -- Utf-8 sequence of bytes, String of 1 to 4 chars
  Max_Chars : constant := 4;
  subtype Len_Range is Positive range 1 .. Max_Chars;
  subtype Sequence is String;
  -- A sequence of 1 or 4 Character
  subtype Word is Sequence;


  -- Unicode
  subtype Unicode_Number is Unicode.Unicode_Number;

  -- Raised if incorrect UTF-8 Sequence or incorrect First_Char
  Invalid_Sequence : exception;

  -- Returns the number of chars of a sequence (coded in the 1st char)
  -- May raise Invalid_Sequence
  function Nb_Chars (First_Char : Character) return Len_Range;

  -- Checks that a Utf-8 word is valid
  function Is_Valid (W : Word) return Boolean;
  -- Checks that a Utf-8 word is valid, raise Invalid_Sequence if not
  procedure Check_Valid (W : in Word);
  -- Checks that a Utf-8 word is safe (valid and not uselessly long...)
  function Is_Safe (W : Word) return Boolean;
  -- Checks that a Utf-8 word is safe, raise Invalid_Sequence if not
  procedure Check_Safe (W : in Word);


  -- Decodes a Utf-8 word to Unicode. May raise Invalid_Sequence
  function Decode (W : Word) return Unicode_Number;
  -- Encodes a Unicode as a Utf-8 word
  function Encode (Unicode : Unicode_Number) return Word;

  subtype Unicode_Sequence is Unicode.Unicode_Sequence;
  -- Decodes a Utf-8 sequence (of sequences) to Unicode sequence.
  -- May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Sequence;
  -- Encodes a Unicode sequence as a Utf-8 sequence (of sequences)
  function Encode (Unicode : Unicode_Sequence) return Sequence;


  -- Raised if a Utf-8 sequence leads to a Unicode above last
  --  Wide_Character (16#FFFF#)
  Not_Wide_Character : exception;

  -- Decodes a Utf-8 word to Wide_Character.
  -- May raise Invalid_Sequence or Not_Wide_Character
  function Decode (W : Word) return Wide_Character;
  -- Encodes a Unicode as a Utf-8 sequence
  function Encode (Wide_Char : Wide_Character) return Word;

end Utf_8;

