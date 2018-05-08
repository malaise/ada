-- Mapcode management
with As_U;
package Mapcodes is

  Mapcode_C_Version : constant String := "2.0.2";
  Mapcode_Data_Version : constant String := "2.3.0";
  Mapcode_Ada_Version  : constant String := "1.4/Data"
                                          & Mapcode_Data_Version;

  -- Real type
  type Real is digits 15 range -1.79E308 .. 1.79E308;

  -----------------
  -- TERRITORIES --
  -----------------
  -- Get_Territory_Number may raise, if Territory or Context is not known:
  Unknown_Territory : exception;

  -- Valid territory number
  subtype Territory_Range is Natural range 0 .. 532;

  -- Given an alphacode (such as US-AL), return the territory number
  --  or raise Unknown_Territory
  -- A context territory helps to interpret ambiguous (abbreviated)
  --  alphacodes, such as "AL"
  function Get_Territory_Number (Territory : String;
                                 Context : String := "")
           return Territory_Range;

  -- Return the alphacode (usually an ISO 3166 code) of a territory
  -- Format: Local (often ambiguous), International (full and unambiguous,
  --  DEFAULT), or Shortest (non ambiguous)
  type Territory_Formats is (Local, International, Shortest);
  function Get_Territory_Alpha_Code (
      Territory_Number : Territory_Range;
      Format : Territory_Formats := International) return String;

  -- Return full name of a territory
  function Get_Territory_Fullname (Territory_Number : Territory_Range)
           return String;

  -- Return parent country of a rterritory that is a subdivision
  -- May raise, if the territory is not a subdivision:
  Not_A_Subdivision : exception;
  --  subdivision)
  function Get_Parent_Of (Territory_Number : Territory_Range)
           return Territory_Range;

  -- Return True if a territory is a state
  function Is_Subdivision (Territory_Number : Territory_Range) return Boolean;

  -- Return True if a territory is a country that has states
  function Has_Subdivision (Territory_Number : Territory_Range) return Boolean;

  ------------------------------------
  -- ENCODING and DECODING MAPCODES --
  ------------------------------------
  -- Coordinate in fraction of degrees
  type Coordinate is record
     Lat, Lon : Real;
  end record;

  -- Array of mapcode information returned as the result of encoding
  type Mapcode_Info is record
    Mapcode : As_U.Asu_Us;
    Full_Mapcode : As_U.Asu_Us;
    Territory_Alpha_Code : As_U.Asu_Us;
    Territory_Number : Territory_Range;
  end record;
  type Mapcode_Infos is array (Positive range <>) of Mapcode_Info;

  -- Encode a coordinate into mapcodes
  -- Returns an array of mapcodes, each representing the specified coordinate.
  -- If a territory is specified, then only mapcodes (if any) within
  --   that territory are returned. if Earth is provided as territory then
  --  the 9-letter "international" mapcode is returned
  -- The Shortest option make it return at most one mapcode (the "default" and
  --   "shortest possible" mapcode) in any territory
  -- The Precision option produces mapcodes extended with high-precision
  --  letters (the parameter specifies how many letters: 0, 1, or 2
  subtype Precisions is Natural range 0 .. 2;
  Earth : constant String := "Earth";
  function Encode (Coord : Coordinate;
                   Territory : String := "";
                   Shortest : Boolean := False;
                   Precision : Precisions := 0) return Mapcode_Infos;

  -- Decode may raise, if the mapcode is not valid (in the context):
  Decode_Error : exception;
  -- Decode a mapcode
  -- The optional context territory must be set when the mapcode denotes a local
  --  (ambiguous) mapcode
  -- Returns the coordinate of the mapcode, or raises Decode_Error if the
  --  mapcode is not valid (or not valid in the privided context)
  function Decode (Mapcode : String;
                   Context : String := "") return Coordinate;

end Mapcodes;

