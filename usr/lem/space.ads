with My_Math;
package Space is

  -- Position in space
  type Position_Range is new My_Math.Real;
  type Position_Rec is record
    X_Pos : Position_Range;
    Y_Pos : Position_Range;
  end record;

  -- X from 0 to 500 meters
  X_Max : constant Position_Range := 500.0;
  subtype X_Range is Position_Range range 0.0 .. X_Max;

  -- Y from 0 to 200 meters
  Y_Max : constant Position_Range := 200.0;
  subtype Y_Range is Position_Range range 0.0 .. Y_Max;

end Space;

