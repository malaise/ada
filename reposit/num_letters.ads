-- Conversion Number -> letters
--  e.g. 9789  -> "nine thousand seven hundred eighty nine"
package Num_Letters is

  subtype Number is Natural range 0 .. 999_999;
  function Letters_Of (N : Number) return String;

end Num_Letters;

