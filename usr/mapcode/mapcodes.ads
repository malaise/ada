-- Mapcode management
with As_U;
package Mapcodes is

  Mapcode_C_Version : constant String := "2.0.2";
  Mapcode_Data_Version : constant String := "2.3.0";
  Mapcode_Ada_Version  : constant String := "1.0.6/Data"
                                          & Mapcode_Data_Version;

  -- Real type (for latitude and longitude)
  type Real is digits 15 range -1.79E308 .. 1.79E308;

  -----------------
  -- TERRITORIES --
  -----------------

  -- Valid territory number
  subtype Territory_Range is Natural range 0 .. 532;

  -- Given an alphacode (such as US-AL), return the territory number
  --  or raise Unknown_Territory
  -- A Context territory helps to interpret ambiguous (abbreviated)
  --  alphacodes, such as "AL"
  -- Raise, if Territory or Context is not known:
  Unknown_Territory : exception;
  function Get_Territory_Number (Territory : String;
                                 Context : String := "")
           return Territory_Range;

  -- Return the alphacode (usually an ISO 3166 code) of a territory
  -- Format: Local (often ambiguous), International (full and unambiguous,
  --  DEFAULT), or Shortest
  type Territory_Formats is (Local, International, Shortest);
  function Get_Territory_Alpha_Code (
      Territory_Number : Territory_Range;
      Format : Territory_Formats := International) return String;

  -- Return the full name of a territory
  function Get_Territory_Fullname (Territory_Number : Territory_Range)
           return String;

  -- Return the parent country of a subdivision
  -- Raise, if Territory is not a subdivision:
  Not_A_Subdivision : exception;
  function Get_Parent_Of (Territory_Number : Territory_Range)
           return Territory_Range;

  -- Return True if Territory is a state
  function Is_Subdivision (Territory_Number : Territory_Range) return Boolean;

  -- Return True if Territory is a country that has states
  function Has_Subdivision (Territory_Number : Territory_Range) return Boolean;

  --------------------------
  -- Encoding to mapcodes --
  --------------------------
  -- Coordinate in fraction of degrees
  type Coordinate is record
     Lat, Lon : Real;
  end record;

  -- One mapcode-related information bloc
  type Mapcode_Info is record
    -- Territory code (AAA for Earth)
    Territory_Alpha_Code : As_U.Asu_Us;
    -- Simple mapcode
    Mapcode : As_U.Asu_Us;
    -- Territory, then a space and the mapcode,
    --  or simple mapcode if it is valid on Earth
    Full_Mapcode : As_U.Asu_Us;
    -- Territory number
    Territory_Number : Territory_Range;
  end record;
  type Mapcode_Infos is array (Positive range <>) of Mapcode_Info;

  -- Encode a coordinate
  -- Return an array of mapcodes, each representing the specified coordinate.
  -- If a Territory alphacode is specified, then only mapcodes (if any) within
  --   that territory are returned. If Earth is provided as territory then
  --  only the 9-letter "international" mapcode is returned
  -- If Shortest is set, then at most one mapcode (the "default" and
  --   "shortest possible" mapcode) in any territory are returned
  -- The Precision option leads to produce mapcodes extended with high-precision
  --  letters (the parameter specifies how many letters: 0, 1, or 2
  -- If Sort is set, then The returned array will contain first the shortest
  --  mapcode, then possibly the other mapcodes for the same territory,
  --  then possibly mapcodes for other territories, then possibly the
  --  international (Earth) mapcode
  subtype Precisions is Natural range 0 .. 2;
  Earth : constant String := "AAA";
  function Encode (Coord : Coordinate;
                   Territory : String := "";
                   Shortest : Boolean := False;
                   Precision : Precisions := 0;
                   Sort : Boolean := False) return Mapcode_Infos;

  ------------------------
  -- Decoding a mapcode --
  ------------------------
  -- Decode a string containing a mapcode
  -- The optional Context territory alphacode shall be set if the mapcode is
  --  ambiguous (not "international")
  -- Return a coordinate or, if the mapcode is incorrect or ambiguous, raise:
  Decode_Error : exception;
  function Decode (Mapcode, Context : String) return Coordinate;

end Mapcodes;

