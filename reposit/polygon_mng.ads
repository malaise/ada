package POLYGON_MNG is

  -- Representation of a point
  type FLOAT_POINT_REC is record
    X : FLOAT;
    Y : FLOAT;
  end record;
  -- Representation of a polygon
  type FLOAT_POINTS_ARRAY is array (POSITIVE range <>) of FLOAT_POINT_REC;

  -- Representations of a point
  type INT_POINT_REC is record
    X : INTEGER;
    Y : INTEGER;
  end record;
  -- Representation of a polygon
  type INT_POINTS_ARRAY is array (POSITIVE range <>) of INT_POINT_REC;

  -- Relationships between points and areas
  type BELONGING_RESULTS is (OUT_OF_AREA, SUMMIT, BOUNDARY, INSIDE_AREA);

  procedure BELONG_TO_AREA
      (POLYGON        : in FLOAT_POINTS_ARRAY;
       POINT_TO_CHECK : in FLOAT_POINT_REC;
       ACCURACY       : in FLOAT;
       RESULT         : out BELONGING_RESULTS);

  function IS_CONNEXE (POLYGON : FLOAT_POINTS_ARRAY) return BOOLEAN;

  procedure BELONG_TO_AREA
      (POLYGON        : in INT_POINTS_ARRAY;
       POINT_TO_CHECK : in INT_POINT_REC;
       ACCURACY       : in FLOAT;
       RESULT         : out BELONGING_RESULTS);

  function IS_CONNEXE (POLYGON : INT_POINTS_ARRAY) return BOOLEAN;

  NOT_A_POLYGON : exception;

end POLYGON_MNG;

