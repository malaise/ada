-- Point types definition and point data set management
with Curve;
package Points is

  -- Maxi number of points
  Max_Number : constant Positive := 9999;

  -- One coordinate, one point, point array
  subtype P_T_Coordinate is Curve.T_Coordinate;
  subtype P_T_One_Point is Curve.T_One_Point;
  subtype P_T_The_Points is Curve.T_The_Points;

  -- Store, clear, sort, return current point array, ont point
  function P_The_Points return P_T_The_Points;
  function P_One_Point (Index : in Positive) return P_T_One_Point;
  procedure P_Store (The_Points : in P_T_The_Points);
  procedure P_Clear;
  procedure P_Sort;

  -- Modify, add remove a point
  type P_T_Upd_Action is (Add, Remove, Modify);
  procedure P_Upd_Point (Action : in P_T_Upd_Action;
    Index : in Positive := 1; Point : in P_T_One_Point := (X => 0.0, Y=> 0.0));

  -- Indicate that points are saved, tells if points are saved
  -- Tells if there is some points stored.
  procedure P_Saved;
  function P_Saved return Boolean;
  function P_Empty return Boolean;
  function P_Nb    return Natural;

  -- Storage size exceeded (P_Store, Add)
  P_Too_Many : exception;
  -- Index greater than number of point (Remove, Modify)
  P_Index_Out : exception;

end Points;
