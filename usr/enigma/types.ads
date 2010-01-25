package Types is

  -- The letters known by the Coder
  subtype Letter is Character range 'A' .. 'Z';
  Nb_Letters : constant Positive
             := Letter'Pos(Letter'Last) - Letter'Pos(Letter'First) + 1;
  -- Letter index: A->0 .. Z->25
  type Lid is mod Nb_Letters;

  -- Conversions
  function Id_Of (L : Letter) return Lid;
  function Letter_Of (I : Lid) return Letter;
  -- 1 -> 0 .. 26 -> 25, 27 -> 0 ..
  function Id_Of (P : Positive) return Lid;

end Types;

