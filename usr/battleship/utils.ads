with Afpx;
package Utils is

  -- Exception on CtrlC or SigTerm to abort the game
  Abort_Game : exception;

  -- Debug modes
  procedure Init;
  function Debug_Comm return Boolean;
  function Debug_Setup return Boolean;
  function Debug_Play return Boolean;

  -- Debug message
  procedure Debug (Msg : in String);

  -- A Coordinate
  type Row_Range is (A, B, C, D, E, F, G, H, I, J);
  type Col_Range is new Positive range 1 .. 10;
  type Coord is record
    Row : Row_Range;
    Col : Col_Range;
  end record;

  -- Image in 2 chars (10 -> a)
  subtype Str2 is String (1 .. 2);
  function Image (C : Coord) return Str2;
  function Value (S : Str2) return Coord;

  -- Cell of a field
  function Fld2Coord (Start : Afpx.Field_Range; Fld : Afpx.Field_Range)
                     return Coord;
  -- Field of a cell
  function Coord2Fld (Start : Afpx.Field_Range; C : Coord)
                     return Afpx.Field_Range;
  -- Is a cell in grid
  function In_Grid (C : Coord; Row_Offset, Col_Offset : Integer) return Boolean;

end Utils;

