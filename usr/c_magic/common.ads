package Common is

  -- Maximum dimension of square
  Max_Dim : constant := 9;
  subtype Dim_Range is Positive range 1 .. Max_Dim;

  -- Search all magic squares of a given dimension
  procedure Search (Dim : in Dim_Range);

end Common;

