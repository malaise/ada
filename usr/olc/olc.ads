-- Open Location Codes
package Olc is

  -----------
  -- Types --
  -----------
  -- Real type (for latitude and longitude)
  type Real is digits 15 range -1.79E308 .. 1.79E308;

  -- Coordinate in fraction of degrees
  subtype Lat_Range is Real range  -90.0 ..  90.0;
  subtype Lon_Range is Real range -180.0 .. 180.0;
  type Coordinate is record
    Lat : Lat_Range;
    Lon : Lon_Range;
  end record;

  -- Precision (number of digits) can be 2, 4, 6, 8, 10, 11, 12, 13, 14, 15
  subtype Precision_Range is Positive range 2 .. 15;
  Default_Precision : constant Precision_Range := 10;

  -- Open Location Code (full)
  -- Notes: codes with less than 8 digits are padded with pairs of '0'
  --        and a '+' is inserted after the 8th digit
  -- The possible code lengths for a full code are 9, 11, 12 .. 16
  subtype Code_Length is Positive range 9 .. 16;
  -- Codes contain the characters 2 3 4 5 6 7 8 9 C F G H J M P Q R V W X
  -- Not case sensitive
  subtype Code_Type is String;

  ----------------------
  -- Basic operations --
  ----------------------
  -- Encode a coordinate into a Open Location Code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type;

  -- Verify the syntaxic validity of a code (code length and character set)
  function Is_Valid (Code : Code_Type) return Boolean;

  -- Verify that the code is full and valid: syntaxic, full (not short) and
  --  decodes to valid coordinate
  -- Raises, if Code is invalid
  function Is_Full (Code : Code_Type) return Boolean;

  -- Decode a Open Location Code into two coordinates (south-west and
  --  north-east corner)
  -- Raises, if Code is invalid: Invalid_Code
  Invalid_Code : exception;
  procedure Decode (Code : in Code_Type;
                    South_West, North_East: out Coordinate);

  -- Return the center of two coordinates
  function Center_Of (South_West, North_East: Coordinate) return Coordinate;

  -- Return the precision of a code (full or short)
  -- Raises, if Code is invalid: Invalid_Code
  function Precision_Of (Code : Code_Type) return Precision_Range;

  -----------------
  -- Short codes --
  -----------------
  -- Shorten the code (remove 2, 4 or 6 first digits) so that the resulting code
  --  is the closest matching code to the passed location
  -- Return the input code if it cannot be shorten
  -- Raises, if Code is invalid: Invalid_Code
  type Factor_Range is new Positive range 1 .. 3;
  function Shorten (Code : Code_Type;
                    Reference : Coordinate) return Code_Type;

  -- Verify the syntaxic validity of a short code, i.e. a full code, possibliy
  -- with extra precision (more than 2 digits after the '+') without the
  -- 2, 4 or 6 first digits
  -- Returns False for a full code
  -- Raises, if Code is invalid: Invalid_Code
  function Is_Short (Code : Code_Type) return Boolean;

  -- Return the nearest full code matching the provided short code and location
  -- Raises, if Code is invalid or not short: Invalid_Code
  function Nearest (Code : Code_Type; Reference : Coordinate) return Code_Type;

end Olc;

