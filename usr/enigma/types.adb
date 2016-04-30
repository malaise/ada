package body Types is

  -- Pos of first letter
  Offset : constant := Letter'Pos(Letter'First);

  -- Conversions
  function Id_Of (L : Letter) return Lid is
  begin
    return Lid (Letter'Pos(L) - Offset);
  end Id_Of;
  function Id_Of (P : Positive) return Lid is
  begin
    return Lid ((P - 1) mod Nb_Letters);
  end Id_Of;

  -- Letter index:
  function Letter_Of (I : Lid) return Letter is
  begin
    return Letter'Val(Natural(I) + Offset);
  end Letter_Of;

end Types;

