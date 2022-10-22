with Str_Util;
package body Geohash is

  -- Constants
  ------------
  Base32 : constant String := "0123456789bcdefghjkmnpqrstuvwxyz";

  -- Local operations
  -------------------
  procedure Check (Code : in Code_Type) is
  begin
    if Code'Length = 0 then
      raise Invalid_Code;
    end if;
  end Check;

  -- Public operations
  --------------------
  -- Encode a coordinate into a Geohash code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type is
    Lat, Lon, Lat_Min, Lat_Max, Lon_Min, Lon_Max, Lat_Mid, Lon_Mid : Real;
    Index, Bit : Natural := 0;
    Even_Bit : Boolean := True;
    Result : Code_Type(1 .. Precision);
    P : Precision_Range := 1;
  begin
    Lat := Coord.Lat;
    Lon := Coord.Lon;
    Lat_Min := -90.0;
    Lat_Max := +90.0;
    Lon_Min := -180.0;
    Lon_Max := +180.0;

    loop

      if Even_Bit then
        -- Bisect E-W longitude
        Lon_Mid := (Lon_Min + Lon_Max) / 2.0;
        if Lon >= Lon_Mid then
          Index := Index * 2 + 1;
          Lon_Min := Lon_Mid;
        else
          Index := Index * 2;
          Lon_Max := Lon_Mid;
        end if;
      else
        -- Bisect N-S latitude
        Lat_Mid := (Lat_Min + Lat_Max) / 2.0;
        if Lat >= Lat_Mid then
          Index := Index * 2 + 1;
          Lat_Min := Lat_Mid;
        else
          Index := Index * 2;
          Lat_Max := Lat_Mid;
        end if;
      end if;
      Even_Bit := not Even_Bit;

      Bit := Bit + 1;
      if Bit = 5 then
        -- 5 bits gives us a character: append it and start over
        Result(P) := Base32(Index + 1);
        exit when P = Precision;
        P := P + 1;
        Bit := 0;
        Index := 0;

      end if;
    end loop;

    return Result;
  end Encode;

  -- Decode a Geohash code into bounds
  -- Raises, if Code is invalid: Invalid_Code
  procedure Bounds_Of (Code : in Code_Type;
                       South_West, North_East : out Coordinate) is
    Lat_Min, Lat_Max, Lon_Min, Lon_Max, Lat_Mid, Lon_Mid : Real;
    Bit_N : Natural := 0;
    Even_Bit : Boolean := True;
    Index : Natural;
  begin
    Check (Code);
    Lat_Min := -90.0;
    Lat_Max := +90.0;
    Lon_Min := -180.0;
    Lon_Max := +180.0;

    for C of Code loop
      -- Find C in map Base32
      Index := Str_Util.Locate (Base32, C & "");
      if Index = 0 then
        raise Invalid_Code;
      end if;
      Index := Index - 1;

      -- Decode char
      for N in reverse 0 .. 4 loop
        Bit_N := (Index / (2 ** N)) rem 2;
        if Even_Bit then
          -- Longitude
          Lon_Mid := (Lon_Min + Lon_Max) / 2.0;
          if Bit_N = 1 then
            Lon_Min := Lon_Mid;
          else
            Lon_Max := Lon_Mid;
          end if;
        else
          -- Latitude
          Lat_Mid := (Lat_Min + Lat_Max) / 2.0;
          if Bit_N = 1 then
            Lat_Min := Lat_Mid;
          else
            Lat_Max := Lat_Mid;
          end if;
        end if;
        Even_Bit := not Even_Bit;
      end loop;
    end loop;

    South_West := (Lat_Min, Lon_Min);
    North_East := (Lat_Max, Lon_Max);

  end Bounds_Of;

  -- Decode a Geohash code into a coordinate
  -- Raises, if Code is invalid: Invalid_Code
  function Decode (Code : Code_Type) return Coordinate is
    Sw, Ne : Coordinate;
    Res : Coordinate;
  begin
    Bounds_Of (Code, Sw, Ne);
    Res.Lat :=( Sw.Lat + Ne.Lat) / 2.0;
    Res.Lon :=( Sw.Lon + Ne.Lon) / 2.0;
    return Res;
  end Decode;

  type Neighbour is array (1 .. 2) of String (1 .. 32);
  Neighbours : constant array (Direction_List) of Neighbour := (
    North => ("p0r21436x8zb9dcf5h7kjnmqesgutwvy",
              "bc01fg45238967deuvhjyznpkmstqrwx"),
    South => ("14365h7k9dcfesgujnmqp0r2twvyx8zb",
              "238967debc01fg45kmstqrwxuvhjyznp"),
    East  => ("bc01fg45238967deuvhjyznpkmstqrwx",
              "p0r21436x8zb9dcf5h7kjnmqesgutwvy"),
    West  => ("238967debc01fg45kmstqrwxuvhjyznp",
              "14365h7k9dcfesgujnmqp0r2twvyx8zb") );

  type Border is array (1 .. 2) of String (1 .. 8);
  Borders : constant array (Direction_List) of Border := (
    North => ("prxz    ", "bcfguvyz"),
    South => ("028b    ", "0145hjnp"),
    East  => ("bcfguvyz", "prxz    "),
    West  => ("0145hjnp", "028b    ") );

  -- Get the neighbor of a given code in a given direction
  -- Same precision as input
  -- Raises, if Code is invalid: Invalid_Code
  function Adjacent_Of (Code : Code_Type; Direction : Direction_List)
           return Code_Type is
    Last_Char : Character;
    Parent : String (1 .. Code'Length - 1);
    Hash_Type, Index : Natural;
  begin
    Check (Code);
    Last_Char := Code(Code'Last);
    Parent := Code(Code'First .. Integer'Pred (Code'Last));
    Hash_Type := Code'Length rem 2 + 1;

    -- Check for edge-cases which don't share common prefix
    if Str_Util.Locate (Borders(Direction)(Hash_Type), Last_Char & "") /= 0
    and then Parent /= "" then
      Parent := Adjacent_Of (Parent, Direction);
    end if;

    -- Append letter for direction to parent
    Index := Str_Util.Locate (Neighbours(Direction)(Hash_Type), Last_Char & "");
    return Parent & Base32(Index);
  end Adjacent_Of;

end Geohash;

