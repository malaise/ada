package TYPES is
  type MATTRIX_KIND_LIST is (WISH, REGRET);

  -- 0.00 to 100.00 multiplied by 100
  subtype CELL_RANGE is NATURAL range 0 .. 10_000;

  MAX_DIM : constant := 100;
  subtype INDEX_RANGE is NATURAL range 0 .. MAX_DIM;
  type MATTRIX_TAB is array (INDEX_RANGE range <>, INDEX_RANGE range <>)
  of CELL_RANGE;

  type MATTRIX_REC (DIM : INDEX_RANGE := 0) is record
    NOTES : MATTRIX_TAB(1 .. DIM, 1 .. DIM);
  end record;

end TYPES;

