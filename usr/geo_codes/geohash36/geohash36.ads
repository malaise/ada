-- Geohash-36
package Geohash36 is

  -----------
  -- Types --
  -----------
  -- Real type (for latitude and longitude)
  type Real is new Long_Float;

  -- Coordinate in fraction of degrees
  subtype Lat_Range is Real range  -90.0 ..  90.0;
  subtype Lon_Range is Real range -180.0 .. 180.0;
  type Coordinate is record
    Lat : Lat_Range;
    Lon : Lon_Range;
  end record;

  -- Precision (number of digits) can be 1 to 15
  subtype Precision_Range is Positive range 1 .. 15;
  Default_Precision : constant Precision_Range := 10;

  -- Code
  subtype Code_Length is Precision_Range;
  -- Codes contain the characters
  -- 2 3 4 5 6 7 8 9 b B C d D F g G h H
  -- j J K l L M n N P q Q r R t T V W X
  subtype Code_Type is String;

  ----------------
  -- Operations --
  ----------------
  -- Encode a coordinate into a Geohash-36 code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type;

  -- Decode a Geohash-36 code into a coordinate
  -- Raises, if Code is invalid: Invalid_Code
  Invalid_Code : exception;
  function Decode (Code : Code_Type) return Coordinate;

  -- Get the precision (in meters) for a given code precision
  procedure Precision_Of (Precision : in Precision_Range;
                          Lat_Prec, Lon_Prec : out Real);

  -- Get the neighbor of a given code in a given direction
  -- Same precision as input, Center leads to return the input Code
  -- Raises, if Code is invalid: Invalid_Code
  type Direction_List is (
    North_West, North,  North_East,
    West,       Center, East,
    South_West, South,  South_East);
  function Neighbor_Of (Code : Code_Type; Direction : Direction_List)
           return Code_Type;

  -- Compute the checksup letter ('a' to 'z') of a code
  -- Raises, if Code is invalid: Invalid_Code
  function Checksum_Of (Code : Code_Type) return Character;

end Geohash36;

