-- Conversion Number -> letters (long scale): Decilliards, Decillions...
-- Trilliards, Trillions, Billiars, Billions, Milliards, Millions, Thousands
-- and Units
--  e.g. 9789  -> "nine thousand seven hundred eighty nine"
with Arbitrary;
package Num_Letters is
  -- nine thousand nine hundred ninety one decilliards.... = 10 ** 66 - 1
  Max_Number : constant Arbitrary.Number := Arbitrary.Set (
--          6         5         4         3         2         1         0
-- 9876543210987654321098765432109876543210987654321098765432109876543210
      "999999999999999999999999999999999999999999999999999999999999999999");
  subtype Number is Arbitrary.Number;
  function Letters_Of (N : Number) return String;

end Num_Letters;

