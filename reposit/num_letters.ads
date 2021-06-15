-- Conversion Number <-> letters
--  e.g. 9789  <-> nine thousand seven hundred eighty nine
with Arbitrary;
package Num_Letters is

  -- Number to convert
  subtype Number is Arbitrary.Number;

  -- long scale:  Centilliard .. Milliard, Million, Thousand   Max 10^606-1
  -- short scale: Centillion  .. Billion,  Million, Thousand   Max 10^306-1
  -- common: Million, Thousand, Hundred, Tenth and Unit
  type Scale_List is (Long, Short);

  -- Raises Constraint_Error if N is too large for the scale (see "Max" above)
  function Letters_Of (N : Number; Scale : Scale_List := Long) return String;

  -- Raises Constraint_Error if Words is an invalid number
  function Num_Of (Words : String; Scale : Scale_List := Long) return Number;
end Num_Letters;

