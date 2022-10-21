with My_Math;
package body Geohash36 is

  -- Constants
  ------------
  -- Earth radius in meters
  Earth_Radius : constant := 6370000.0;

  -- Mattrix 6 x 6 of characters
  Dim : constant := 6;
  subtype Dim_Range is Natural range 0 .. Dim - 1;
  Base36 : constant array (Dim_Range, Dim_Range) of Character :=
      ( ('2', '3', '4', '5', '6', '7'),
        ('8', '9', 'b', 'B', 'C', 'd'),
        ('D', 'F', 'g', 'G', 'h', 'H'),
        ('j', 'J', 'K', 'l', 'L', 'M'),
        ('n', 'N', 'P', 'q', 'Q', 'r'),
        ('R', 't', 'T', 'V', 'W', 'X') );

  -- Hashed reversed values to mattrix
  -- 36 pairs of crossed references
  Base36_Inv : constant array (0 .. Dim * Dim * 2 - 1) of Natural :=
      (16#91#, 16#15#, 16#FF#, 16#FF#, 16#93#, 16#18#,
       16#14#, 16#00#, 16#16#, 16#00#, 16#97#, 16#1B#,
       16#99#, 16#1D#, 16#FF#, 16#FF#, 16#9A#, 16#1F#,
       16#1C#, 16#00#, 16#1E#, 16#00#, 16#FF#, 16#FF#,
       16#20#, 16#00#, 16#FF#, 16#FF#, 16#80#, 16#21#,
       16#81#, 16#22#, 16#82#, 16#23#, 16#03#, 16#00#,
       16#04#, 16#00#, 16#05#, 16#00#, 16#06#, 16#00#,
       16#07#, 16#00#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
       16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#08#, 16#00#,
       16#FF#, 16#FF#, 16#0B#, 16#00#, 16#FF#, 16#FF#,
       16#09#, 16#00#, 16#8A#, 16#0E#, 16#8C#, 16#10#,
       16#FF#, 16#FF#, 16#8D#, 16#12#, 16#0F#, 16#00#);

  -- Local operations
  -------------------
  Not_Found : constant Integer := -1;
  procedure Char2Index (C : in Character; Row, Col : out Integer) is
    Index : Integer;
    Hash, Val :  Natural;
    function Val_Of (H : Natural) return Natural is
      (if H < 16#80# then H else H - 16#80#);

  begin
    Index := Character'Pos (C) rem 36;

    -- Get the Hash entry
    Hash := Base36_Inv (Index * 2);
    if Hash = 16#FF# then
      Row := Not_Found;
      Col := Not_Found;
      return;
    end if;

    -- Get the value
    Val := Val_Of (Hash);
    Row := Val / Dim;
    Col := Val rem Dim;
    if Base36(Row, Col) = C then
      -- Got it
      return;
    elsif Hash >= 16#80# then
      Hash := Base36_Inv (Index * 2 + 1);
      Val := Val_Of (Hash);
      Row := Val / Dim;
      Col := Val rem Dim;
      if Base36(Row, Col) = C then
        -- Got it
        return;
      end if;
    end if;
    -- Not match found
    Row := Not_Found;
    Col := Not_Found;
  end Char2Index;

  -- Public operation
  -------------------
  -- Encode a coordinate into a Geohash-36 code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type is
    Lat1, Lat2, Lon1, Lon2, Bound1, Bound2 : Real;
    Row, Col : Dim_Range := 0;
    Slice : Real;
    Result : Code_Type(1 .. Precision);
  begin
    Lat1 := -90.0;
    Lat2 := +90.0;
    Lon1 := -180.0;
    Lon2 := +180.0;

    for P in 1 .. Precision loop

      -- Find Row for Lon
      Slice := abs (Lon1 - Lon2) / Real (Dim);
      for I in Dim_Range loop
        Bound1 := Lon1 + Real (I) * Slice;
        Bound2 := Lon1 + Real (I + 1) * Slice;
        if Coord.Lon > Bound1 and then Coord.Lon <= Bound2 then
          -- Coord is in boundaries
          Col := I;
          Lon1 := Bound1;
          Lon2 := Bound2;
          exit;
        end if;
      end loop;

      -- Find Col for Lat
      Slice := abs (Lat1 - Lat2) / Real (Dim);
      for I in Dim_Range loop
        Bound1 := Lat1 + Real (I) * Slice;
        Bound2 := Lat1 + Real (I + 1) * Slice;
        if Coord.Lat > Bound1 and then Coord.Lat <= Bound2 then
          -- Coord is in boundaries
          Row := Dim - 1 - I;
          Lat1 := Bound1;
          Lat2 := Bound2;
          exit;
        end if;
      end loop;

      Result(P) := Base36(Row, Col);
      Row := 0;
      Col := 0;
    end loop;

    return Result;
  end Encode;

  -- Decode a Geohash-36 code into a coordinate
  -- Raises, if Code is invalid: Invalid_Code
  function Decode (Code : Code_Type) return Coordinate is
    Lat1, Lat2, Lon1, Lon2 : Real;
    Row, Col : Integer;
    Slice : Real;
  begin
    Lat1 := -90.0;
    Lat2 := +90.0;
    Lon1 := -180.0;
    Lon2 := +180.0;

    for C of Code loop
      -- Find entry in mattrix
      Char2Index (C, Row, Col);
      if Row = Not_Found then
        raise Invalid_Code;
      end if;
      Row := Dim - 1 - Row;

      Slice := abs (Lon1 - Lon2) / Real (Dim);
      Lon2 := Lon1 + Slice * Real (Col + 1);
      Lon1 := Lon1 + Slice * Real (Col);

      Slice := abs (Lat1 - Lat2) / Real (Dim);
      Lat2 := Lat1 + Slice * Real (Row + 1);
      Lat1 := Lat1 + Slice * Real (Row);
    end loop;

    return (Lat => (Lat2 + Lat1) / 2.0,
            Lon => (Lon2 + Lon1) / 2.0);

  end Decode;

  -- Get the precision (in meters) for a given code precision
  procedure Precision_Of (Precision : in Precision_Range;
                          Lat_Prec, Lon_Prec : out Real) is
  begin
    Lat_Prec := 90.0 / Real (Dim ** Precision)
              * 2.0 * My_Math.Pi * Earth_Radius / 360.0;
    Lon_Prec := Lat_Prec * 2.0;
  end Precision_Of;

  -- type Direction_List is (
  --   North_West, North,  North_East,
  --   West,       Center, East,
  --   South_West, South,  South_East);
  type Offset_Rec is record
    Lat, Lon : Natural;
  end record;
  Offsets : constant array (Direction_List) of Offset_Rec := (
    North_West => (5, 5), North  => (5, 0), North_East => (5, 1),
    West       => (0, 5), Center => (0, 0), East       => (0, 1),
    South_West => (1, 5), South  => (1, 0), South_East => (1, 1));
  -- Get the neighbor of a given code in a given direction
  -- Same precision as input
  -- Raises, if Code is invalid: Invalid_Code
  function Neighbor_Of (Code : Code_Type; Direction : Direction_List)
           return Code_Type is
    Lat_Diff, Lon_Diff : Integer;
    Row, Col : Integer;
  begin
    -- Mattrix pos of last char
    Char2Index (Code(Code'Last), Row, Col);
    if Row = Not_Found then
      raise Invalid_Code;
    end if;

    -- Apply offset
    Lat_Diff := Offsets(Direction).Lat;
    Lon_Diff := Offsets(Direction).Lon;
    Row := (Row + Lat_Diff) rem Dim;
    Col := (Col + Lon_Diff) rem Dim;

    -- Update last char
    return Code(Code'First .. Integer'Pred (Code'Last)) & Base36 (Row, Col);
  end Neighbor_Of;


  -- Compute the checksup letter ('a' to 'z') of a code
  function Checksum_Of (Code : Code_Type) return Character is
    Row, Col : Integer;
    Res : Natural := 0;
    Factor : Positive := 1;
  begin
    for C of reverse Code loop
      -- Value for the char
      Char2Index (C, Row, Col);
      if Row = Not_Found then
        raise Invalid_Code;
      end if;
      -- Factor is Length for the first char ... 1 for the last char
      Res := (Res + Factor * (Row * Dim + Col)) rem 26;
      Factor := Factor + 1;
    end loop;
    return Character'Val (Character'Pos ('a') + Res);
  end Checksum_Of;

end Geohash36;

