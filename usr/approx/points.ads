-- Point types definition and point data set management
with CURVE;
package POINTS is

  -- Maxi number of points
  MAX_NUMBER : constant POSITIVE := 9999;

  -- One coordinate, one point, point array
  subtype P_T_COORDINATE is CURVE.T_COORDINATE;
  subtype P_T_ONE_POINT is CURVE.T_ONE_POINT;
  subtype P_T_THE_POINTS is CURVE.T_THE_POINTS;

  -- Store, clear, sort, return current point array, ont point
  function P_THE_POINTS return P_T_THE_POINTS;
  function P_ONE_POINT (INDEX : in POSITIVE) return P_T_ONE_POINT;
  procedure P_STORE (THE_POINTS : in P_T_THE_POINTS);
  procedure P_CLEAR;
  procedure P_SORT;

  -- Modify, add remove a point
  type P_T_UPD_ACTION is (ADD, REMOVE, MODIFY);
  procedure P_UPD_POINT (ACTION : in P_T_UPD_ACTION;
    INDEX : in POSITIVE := 1; POINT : in P_T_ONE_POINT := (X => 0.0, Y=> 0.0));

  -- Indicate that points are saved, tells if points are saved
  -- Tells if there is some points stored.
  procedure P_SAVED;
  function P_SAVED return BOOLEAN;
  function P_EMPTY return BOOLEAN;
  function P_NB    return NATURAL;

  -- Storage size exceeded (P_STORE, ADD)
  P_TOO_MANY : exception;
  -- INDEX greater than number of point (REMOVE, MODIFY)
  P_INDEX_OUT : exception;

end POINTS;
