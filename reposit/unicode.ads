with Unbounded_Arrays;
package Unicode is

  -- A Unicode number
  subtype Unicode_Number is Natural range 0 .. 16#10FFFF#;

  -- An array of Unicode numbers
  type Unicode_Sequence is array (Positive range <>) of Unicode_Number;

  -- An unbounded array of Unicode numbers
  package Unbounded_Unicode is new Unbounded_Arrays (Unicode_Number,
                                                     Unicode_Sequence);

end Unicode;

