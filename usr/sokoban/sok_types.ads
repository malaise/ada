-- Common types for sokoban
package SOK_TYPES is

  -- Array of a frame
  subtype ROW_RANGE is POSITIVE range 1 .. 16;
  subtype COL_RANGE is positive range 1 .. 19;

  -- Fixed background of the square
  type PATTERN_LIST is (WALL, FREE, TARGET);
  -- changeable content of a square
  type CONTENT_LIST is (MAN, BOX, NOTHING);

  -- A square
  type SQUARE_REC (PATTERN : PATTERN_LIST := FREE) is record
    case PATTERN is
      -- nothing on a wall
      when WALL => null;
      -- man or box or nothing
      when FREE | TARGET =>
        CONTENT : CONTENT_LIST;
    end case;
  end record;

  -- A frame
  type FRAME_TAB is array (ROW_RANGE, COL_RANGE) of SQUARE_REC;

  -- Frame number
  subtype FRAME_RANGE is POSITIVE range 1 .. 50;

  -- square position in frame
  type COORDINATE_REC is record
    ROW : ROW_RANGE;
    COL : COL_RANGE;
  end record;


  -- best score of each frame
  type SCORE_REC is record
    SET : BOOLEAN;
    DAY : NATURAL;
    DUR : DURATION;
    MOVES : NATURAL;
    PUSHES : NATURAL;
  end record;
end SOK_TYPES;
