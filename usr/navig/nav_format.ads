with NAV_TYPES;
-- to convert got strings in data fields, and fields in string to be put
package NAV_FORMAT is

  type FORMAT_RESULT is (SET, UNSET, ERROR);

  -- from speed to string
  function IMAG (SPEED : NAV_TYPES.T_SPEED; SET : BOOLEAN := TRUE)
   return STRING;
  -- from string to angle
  function IMAG (ANGLE : NAV_TYPES.T_ANGLE; SET : BOOLEAN := TRUE)
   return STRING;
  -- from string to drift
  function IMAG (DRIFT : NAV_TYPES.T_DRIFT; SET : BOOLEAN := TRUE)
   return STRING;

  -- from string to speed
  -- if error, then pos is it's position
  procedure VALUE (STR : in STRING;
   SPEED : out NAV_TYPES.T_SPEED; RES : out FORMAT_RESULT; POS : out POSITIVE);
  -- from string to angle
  procedure VALUE (STR : in STRING;
   ANGLE : out NAV_TYPES.T_ANGLE; RES : out FORMAT_RESULT; POS : out POSITIVE);
  -- from drift to speed
  procedure VALUE (STR : in STRING;
   DRIFT : out NAV_TYPES.T_DRIFT; RES : out FORMAT_RESULT; POS : out POSITIVE);

end NAV_FORMAT;