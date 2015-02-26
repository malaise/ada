-- Conversion Number -> letters (long scale)
-- Trilliards, Trillions, Billiars, Billions, Milliards, Millions,
--  Thousands, Units
--  e.g. 9789  -> "nine thousand seven hundred eighty nine"
with Long_Longs;
package Num_Letters is

  subtype Number is Long_Longs.Llu_Natural;
  function Letters_Of (N : Number) return String;

end Num_Letters;

