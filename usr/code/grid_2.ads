with My_Math;
package Grid_2 is

  subtype Long_Positive is My_Math.Inte range 1 .. My_Math.Inte'Last;
  type Long_String is array (Long_Positive range <>) of Character;

  function Encode (Key : in String; Text : Long_String)
                  return Long_String;

  function Decode (Key : in String; Text : Long_String)
                  return Long_String;

  Long_String_Too_Long : exception;

end Grid_2;

