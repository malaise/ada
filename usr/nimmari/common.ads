package Common is

  -- The rows
  subtype Row_Range is Positive range 1 .. 4;

  -- The bars
  subtype Full_Bar_Range is Natural range 0 .. 7;
  subtype Bar_Range is Full_Bar_Range range 1 .. Full_Bar_Range'Last;

  -- Number of Bars per Row
  type Bar_Per_Row_Tab is array (Row_Range) of Bar_Range;
  Bar_Per_Row : constant Bar_Per_Row_Tab := (
   1 => 7,
   2 => 5,
   3 => 3,
   4 => 1);

  -- King of game
  type Game_List is (Nim, Marienbad);

end Common;
