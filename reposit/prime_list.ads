package Prime_List is

  subtype Long_Long_Positive is Long_Long_Integer
      range 1 .. Long_Long_Integer'Last;

  -- Rewind the list of prime numbers found so far
  procedure Rewind;

  -- Get next prime number
  function Next return Long_Long_Positive;

end Prime_List;

