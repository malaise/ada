package COMMON is

  -- Maximum dimension of square
  MAX_DIM : constant := 9;
  subtype DIM_RANGE is POSITIVE range 1 .. MAX_DIM;

  -- Search all magic squares of a given dimension
  procedure SEARCH (DIM : in DIM_RANGE);

end COMMON;

