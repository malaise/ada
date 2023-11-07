-- Geohash (32)
package Geohash is

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

  -- Precision (number of digits) can be 1 to 12
  subtype Precision_Range is Positive range 1 .. 12;
  Default_Precision : constant Precision_Range := 10;

  -- Code
  subtype Code_Length is Precision_Range;
  -- Codes contain the characters 0 to 9 and
  -- b c d e f g h j k m n p q r s t u v w x y z
  subtype Code_Type is String;

  ----------------
  -- Operations --
  ----------------
  -- Encode a coordinate into a Geohash code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type;

  -- Decode a Geohash code into bounds
  -- Raises, if Code is invalid: Invalid_Code
  Invalid_Code : exception;
  procedure Bounds_Of (Code : in Code_Type;
                       South_West, North_East : out Coordinate);

  -- Decode a Geohash code into a coordinate (center of bounds)
  function Decode (Code : Code_Type) return Coordinate;

  -- Get the adjacent cell of a given code in a given direction
  -- Same precision as input
  -- Raises, if Code is invalid: Invalid_Code
  type Direction_List is (North,  South, East, West);
  function Adjacent_Of (Code : Code_Type; Direction : Direction_List)
           return Code_Type;

end Geohash;

