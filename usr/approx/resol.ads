with POINTS, MY_SYSLIN;
package RESOL is

  -- Vector : factors of the polynomial solution
  subtype VECTOR is MY_SYSLIN.VECTOR;

  -- Degree : Degree of the polynomial
  subtype R_T_DEGREE is NATURAL;

  -- Compute polynomial
  function R_RESOLUTION (THE_POINTS : POINTS.P_T_THE_POINTS) return VECTOR;

  -- Indicate new degree for next computation
  procedure R_SET_DEGREE (DEGREE : in R_T_DEGREE);

  -- Returns current degree
  function R_DEGREE return R_T_DEGREE;

  -- Indicate a modification of data set
  -- (so previous solution cannot be used any more)
  procedure R_POINTS_MODIFICATION;

  -- Problem during computation
  R_RESOL_ERROR : exception;
  -- Degree greater than N points - 1 (raised by R_RESOLUTION)
  R_DEGREE_OUT  : exception;

end RESOL;
