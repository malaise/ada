package body AFPX_TYP is

  -- Check is square (relative to field) is in field
  function IN_FIELD (FIELD  : in FIELD_REC;
                     SQUARE : in CON_IO.SQUARE) return BOOLEAN is
  begin
    return   SQUARE.ROW < FIELD.HEIGHT
    and then SQUARE.COL < FIELD.WIDTH;
  end IN_FIELD;

  -- Check is square (absolute) is in field
  function IN_FIELD_ABSOLUTE (FIELD  : in FIELD_REC;
                              SQUARE : in CON_IO.SQUARE) return BOOLEAN is
  begin
    return   SQUARE.ROW >= FIELD.UPPER_LEFT.ROW
    and then SQUARE.ROW <= FIELD.LOWER_RIGHT.ROW
    and then SQUARE.COL >= FIELD.UPPER_LEFT.COL
    and then SQUARE.COL <= FIELD.LOWER_RIGHT.COL;
  end IN_FIELD_ABSOLUTE;

end AFPX_TYP;

