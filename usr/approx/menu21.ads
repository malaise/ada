with CURVE;
package MENU21 is

  -- Return the bounds previoulsy set (if set)
  function BOUNDS_SET return BOOLEAN;
  procedure GET_BOUNDS (SET : out BOOLEAN; BOUNDS : out CURVE.T_BOUNDARIES);

  -- Interactive selection of bounds
  procedure MAIN_SCREEN;

end MENU21;
