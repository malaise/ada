with Vigenere;
package Grid_2 is

  subtype Long_String is Vigenere.Long_String;

  function Encode (Key : in String; Text : Long_String) return Long_String;

  function Decode (Key : in String; Text : Long_String) return Long_String;

  Long_String_Too_Long : exception;

end Grid_2;

