package body Types is

  -- Pos of first letter
  Offset : constant := Letter'Pos(Letter'First);

  -- Conversions
  function Id_Of (L : Letter) return Lid is
    (Lid (Letter'Pos(L) - Offset)); --## rule line off Conversion
  function Id_Of (P : Positive) return Lid is
    (Lid ((P - 1) mod Nb_Letters));

  -- Letter index:
  function Letter_Of (I : Lid) return Letter is
    (Letter'Val(Natural(I) + Offset));

end Types;

