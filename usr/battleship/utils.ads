with Afpx;
package Utils is

  -- Exception on CtrlC or SigTerm to abort the game
  Abort_Game : exception;

  procedure Init;
  -- Debug traces
  procedure Dbg_Comm  (Msg : in String);
  procedure Dbg_Setup (Msg : in String);
  procedure Dbg_Play  (Msg : in String);
  -- Error traces
  procedure Err_Comm  (Msg : in String);
  procedure Err_Setup (Msg : in String);
  procedure Err_Play  (Msg : in String);

  -- Is debug set
  function Dbg_Comm return Boolean;
  function Dbg_Setup return Boolean;
  function Dbg_Play return Boolean;

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

