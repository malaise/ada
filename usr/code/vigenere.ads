with Long_Longs;
package Vigenere is

  -- Any (printable) character is allowed in the key
  Max_Key_Length : constant := 80;
  -- Raised when encoding or decoding with an empty key string
  Empty_Key : exception;

  type Long_String is array (Long_Longs.Ll_Positive range <>) of Character;

  -- Ony letters of input string are encoded, and lowerchar are converted to upperchar
  -- Other characters are encoded as a space
  procedure Encode (Key : in String; Str : in out Long_String);

  -- Ony uppercase letters are accepted in input string
  -- Otherwise raises
  Decode_Error : exception;
  procedure Decode (Key : in String; Str : in out Long_String);

end Vigenere;

