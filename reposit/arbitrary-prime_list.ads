package Arbitrary.Prime_List is

  subtype Positive_Number is Number;

  type Iterator is tagged private;

  -- Rewind the list of prime numbers found so far
  procedure Rewind (It : in out Iterator);

  -- Get next prime number
  function Next (It : in out Iterator) return Positive_Number;

private

  type Iterator is tagged record
     Position : Positive_Number := One;
  end record;

end Arbitrary.Prime_List;

