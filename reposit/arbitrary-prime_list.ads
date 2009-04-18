package Arbitrary.Prime_List is

  subtype Positive_Number is Number;

  -- Rewind the list of prime numbers found so far
  procedure Rewind;

  -- Get next prime number
  function Next return Positive_Number;

end Arbitrary.Prime_List;

