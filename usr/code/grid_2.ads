with MY_MATH;
package GRID_2 is

  subtype LONG_POSITIVE is MY_MATH.INTE range 1 .. MY_MATH.INTE'LAST;
  type LONG_STRING is array (LONG_POSITIVE range <>) of CHARACTER;

  function ENCODE (KEY : in STRING; TEXT : LONG_STRING)
                  return LONG_STRING;

  function DECODE (KEY : in STRING; TEXT : LONG_STRING)
                  return LONG_STRING;

  LONG_STRING_TOO_LONG : exception;

end GRID_2;
