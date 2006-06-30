with Arbitrary;
package Prime_List is

  subtype Prime_Number is Arbitrary.Number;
  subtype Prime_Positive is Prime_Number;

  -- Rewind the list of prime numbers found so far
  procedure Rewind;

  -- Get next prime number
  function Next return Prime_Positive;

end Prime_List;

