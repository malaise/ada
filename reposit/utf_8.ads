-- Utf_8 encoding/decoding
package Utf_8 is

  -- Utf-8 sequence of bytes, String of 1 to 4 chars
  Max_Chars : constant := 4;
  subtype Len_Range is Positive range 1 .. Max_Chars;
  subtype Sequence is String;

  -- Unicode
  subtype Unicode_Number is Natural range 0 .. 16#10FFFF#;

  -- Raised if incorrect UTF-8 Sequence or incorrect First_Char
  Invalid_Sequence : exception;

  -- Returns the number of chars of a sequence (coded in the 1st char)
  -- May raise Invalid_Sequence
  function Nb_Chars (First_Char : Character) return Len_Range;

  -- Checks that a Utf-8 sequence is valid
  function Is_Valid (Seq : Sequence) return Boolean;
  -- Checks that a Utf-8 sequence is valid, raise Invalid_Sequence if not
  procedure Check_Valid (Seq : in Sequence);
  -- Checks that a Utf-8 sequence is safe (valid and not uselessly long...)
  function Is_Safe (Seq : Sequence) return Boolean;
  -- Checks that a Utf-8 sequence is safe, raise Invalid_Sequence if not
  procedure Check_Safe (Seq : in Sequence);


  -- Decodes a Utf-8 sequence to Unicode. May raise Invalid_Sequence
  function Decode (Seq : Sequence) return Unicode_Number;
  -- Encodes a Unicode as a Utf-8 sequence
  function Encode (Unicode : Unicode_Number) return Sequence;


  -- Raised if a Utf-8 sequence leads to a Unicode above last
  --  Wide_Character (16#FFFF#)
  Not_Wide_Character : exception;

  -- Decodes a Utf-8 sequence to Wide_Character.
  -- May raise Invalid_Sequence or Not_Wide_Character
  function Decode (Seq : Sequence) return Wide_Character;
  -- Encodes a Unicode as a Utf-8 sequence
  function Encode (Wide_Char : Wide_Character) return Sequence;

end Utf_8;

