with Points, My_Syslin;
package Resol is
  -- Degree of the polynomial
  subtype R_T_Degree is Natural range 0 .. 999;

  -- Vector : factors of the polynomial solution
  subtype Vector is My_Syslin.Vector;

  -- Compute polynomial
  function R_Resolution (The_Points : Points.P_T_The_Points) return Vector;

  -- Indicate new degree for next computation
  procedure R_Set_Degree (Degree : in R_T_Degree);

  -- Returns current degree
  function R_Degree return R_T_Degree;

  -- Indicate a modification of data set
  -- (so previous solution cannot be used any more)
  procedure R_Points_Modification;

  -- Problem during computation
  R_Resol_Error : exception;
  -- Degree greater than N points - 1 (raised by R_RESOLUTION)
  R_Degree_Out  : exception;

end Resol;
