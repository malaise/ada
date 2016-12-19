with Long_Longs;
package Vigenere is

  Max_Key_Length : constant := 80;

  type Long_String is array (Long_Longs.Ll_Positive range <>) of Character;

  procedure Encode (Key : in String; Str : in out Long_String);
  procedure Decode (Key : in String; Str : in out Long_String);

  Empty_Key, Decode_Error : exception;
end Vigenere;

