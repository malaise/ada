package COMMON is

  -- The rows
  subtype ROW_RANGE is POSITIVE range 1 .. 4;

  -- The bars
  subtype FULL_BAR_RANGE is NATURAL range 0 .. 7;
  subtype BAR_RANGE is FULL_BAR_RANGE range 1 .. FULL_BAR_RANGE'LAST;

  -- Number of Bars per ROW
  type BAR_PER_ROW_TAB is array (ROW_RANGE) of BAR_RANGE;
  BAR_PER_ROW : constant BAR_PER_ROW_TAB := (
   1 => 7,
   2 => 5,
   3 => 3,
   4 => 1);

  -- King of game
  type GAME_LIST is (NIM, MARIENBAD);

end COMMON;
