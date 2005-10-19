package Num_Letters is

  -- Conversion Number -> letters
  --  e.g. 9789  nine thousand seven hundred eighty nine


  subtype Number is Natural range 0 .. 99_999;

  function Letters_Of (N : Number) return String;

end Num_Letters;

