with My_Math;
package Vigenere is

  Max_Key_Length : constant := 80;

  subtype Long_Positive is My_Math.Inte range 1 .. My_Math.Inte'Last;
  type Long_String is array (Long_Positive range <>) of Character;

  procedure Encode (Key : in String; Str : in out Long_String);
  procedure Decode (Key : in String; Str : in out Long_String);

  Empty_Key, Decode_Error : exception;
end Vigenere;

