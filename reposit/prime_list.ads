with Arbitrary;
package Prime_List is

  subtype Prime_Number is Arbitrary.Number;
  subtype Prime_Positive is Prime_Number;

  Zero : constant Prime_Number := Arbitrary.Set (Integer'(0));
  One  : constant Prime_Positive := Arbitrary.Set (Integer'(1));

  -- Rewind the list of prime numbers found so far
  procedure Rewind;

  -- Get next prime number
  function Next return Prime_Positive;

end Prime_List;

