package Grid_1 is


  subtype Row_Coordinate is Character range 'A' .. 'J';
  subtype Col_Coordinate is Character range 'K' .. 'T';


  type Coordinate_Rec is record
    Row : Row_Coordinate;
    Col : Col_Coordinate;
  end record;


  procedure Initialize (Key : in String);

  -- C can be any char from ' ' to '~' or Ascii.Cr
  -- Any other char is discarded
  function Encode (C : Character) return Coordinate_Rec;

  function Decode (Coordinate : Coordinate_Rec) return Character;

  Grid_Not_Init : exception;
  Invalid_Character : exception;

  procedure Dump;

end Grid_1;

