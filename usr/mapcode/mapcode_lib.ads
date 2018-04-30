with As_U;
package Mapcode_Lib is

  -- All functions returning a Integer return a natural or Error
  Error : constant Integer := -1;

  -- All functions returning a string may return Undefined, on error
  Undefined : constant String := "";

  -- Real type
  type Real is digits 15 range -1.79E308 .. 1.79E308;

  -----------------
  -- TERRITORIES --
  -----------------
  -- Territory function taking a name may raise
  Unknown_Territory : exception;

  -- Valid territory number
  subtype Territory_Range is Natural range 0 .. 532;

  -- Given an alphacode (such as US-AL), return the territory number
  --  or Error.
  -- A context Territory helps to interpret ambiguous (abbreviated)
  --  alphacodes, such as "AL"
  function Get_Territory_Number (Territory : String;
                                 Context_Territory : String := Undefined)
           return Territory_Range;

  -- Return full name of territory or Undefined
  function Get_Territory_Fullname (Territory : String) return String;

  -- Return the alphacode (usually an ISO 3166 code) of a territory
  -- Format: Local (often ambiguous), International (full and unambiguous,
  --  DEFAULT), or Shortest
  type Territory_Formats is (Local, International, Shortest);
  function Get_Territory_Alpha_Code (
      Territory_Number : Territory_Range;
      Format : Territory_Formats := International) return String;
  function Get_Territory_Alpha_Code (
      Territory : String;
      Format : Territory_Formats := International) return String;

  -- Return parent country of subdivision
  -- Raise if Territory is not a subdivision
  Not_A_Subdivision : exception;
  --  subdivision)
  function Get_Parent_Of (Territory_Number : Territory_Range)
           return Territory_Range;
  function Get_Parent_Of (Territory : String) return Territory_Range;

  -- Return True if Territory is a state
  function Is_Subdivision (Territory : String) return Boolean;

  -- Return True if Territory is a country that has states
  function Has_Subdivision (Territory : String) return Boolean;

  -- All in one
  type Territory_Info is record
    Name : As_U.Asu_Us;
    Fullname : As_U.Asu_Us;
    Parent : As_U.Asu_Us;
    Has_Subdivision : Boolean;
  end record;
  function Get_Info (
      Territory_Number : Territory_Range;
      Format : Territory_Formats := International) return Territory_Info;

  -- Coordinate in fraction of degrees
  type Coordinate is record
     Lat, Lon : Real;
  end record;

  -- Encoding to Mapcodes
  type Map_Code_Info is record
    Map_Code : As_U.Asu_Us;
    Territory_Alpha_Code : As_U.Asu_Us;
    Full_Map_Code : As_U.Asu_Us;
    Territory_Number : Territory_Range;
  end record;
  type Map_Code_Infos is array (Positive range <>) of Map_Code_Info;

  -- Encode variants.
  -- Return an array of mapcodes, each representing the specified coordinate.
  -- If a territory is specified, only mapcodes (if any) within
  --   that territory are returned. if Earth is provided as territory then
  --  the 9-letter "international" mapcode is returned
  -- The Shortest variants return at most one mapcode (the "default" and
  --   "shortest possible" mapcode) in any territory.
  -- The WithPrecision variants produce mapcodes extended with high-precision
  --  letters (the parameter specifies how many letters: 0, 1, or 2
  subtype Precisions is Natural range 0 .. 2;
  Earth : constant String := "Earth";
  function Encode (Coord : Coordinate;
                   Territory : String := Undefined;
                   Shortest : Boolean := False;
                   Precision : Precisions := 0) return Map_Code_Infos;
  -- Decoding Mapcode
  Decode_Error : exception;
  -- Decode a string (which may contain a full mapcode, including a territory)
  --  the optional contextTerritoryNumber is used in case the mapcode
  --  specifies no (unambiguous) territory.
  -- Returns coordinate, or raises Decode_Error
  function Decode (Mapcode, Context : String) return Coordinate;

end Mapcode_Lib;

