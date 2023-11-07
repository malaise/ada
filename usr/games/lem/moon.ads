with Space;
package Moon is

  -- Moon attraction acceleration (constant, in m/s2)
  Acceleration : constant := -1.635;

  -- Init a new moon
  procedure Init;

  -- Higest possible point
  Y_Ground_Max : constant Space.Y_Range := 50.0;

  -- The array of points defining the ground
  -- First point has X = 0.0, last point has X = Space.X_Max
  --  and points are equi-distant on X
  -- Ys are from 0.0 to Y_Ground_Max
  type Ground_Array is array (Positive range <>) of Space.Position_Rec;
  function Get_Ground return Ground_Array;

end Moon;

