package body RESOL is

  -- Degree of the polynomial
  subtype T_DEGREE is R_T_DEGREE range 1..50;
  THE_DEGREE : T_DEGREE := 1;
  -- Previous solution still accurate
  RESOLVED : BOOLEAN := FALSE;
  PREVIOUS_SOLUTION : VECTOR (1..T_DEGREE'lAST);

  -- Computation of the solution
  package R_COMPUTE is
    subtype MATRIX is MY_SYSLIN.MATRIX;
    -- Build At * A matrix and B * Y vector from points
    procedure DO_MATRIXES (THE_POINTS : in POINTS.P_T_THE_POINTS;
      T_A_A : out MATRIX; A_Y : out VECTOR);
    -- Solve (At*A)*X=B*Y
    function SYSTEM (A : MATRIX; B : VECTOR) return VECTOR;
  end R_COMPUTE;
  package body R_COMPUTE is separate;

  -- Try to re-use previous result, otherwise solve
  function R_RESOLUTION (THE_POINTS : POINTS.P_T_THE_POINTS) return VECTOR is
  begin
    if not RESOLVED then
      if THE_DEGREE > THE_POINTS'LENGTH then raise R_DEGREE_OUT; end if;
      declare
        T_A_A : R_COMPUTE.MATRIX(1..THE_DEGREE, 1..THE_DEGREE);
        A_Y : VECTOR(1..THE_DEGREE);
      begin
        RESOLVED := TRUE;
        R_COMPUTE.DO_MATRIXES (THE_POINTS, T_A_A, A_Y);
        PREVIOUS_SOLUTION (1..THE_DEGREE) := R_COMPUTE.SYSTEM (T_A_A, A_Y);
      exception
        when others =>
          RESOLVED := FALSE;
          raise R_RESOL_ERROR;
      end;
    end if;
    return PREVIOUS_SOLUTION (1..THE_DEGREE);
  exception
    when others =>
      RESOLVED := FALSE;
      raise R_RESOL_ERROR;
  end R_RESOLUTION;

  -- The System degree is polynomial degree + 1
  procedure R_SET_DEGREE (DEGREE : in R_T_DEGREE) is
  begin
    if DEGREE + 1 /= THE_DEGREE then
      RESOLVED := FALSE;
      THE_DEGREE := DEGREE + 1;
    end if;
  exception
    when others => raise R_DEGREE_OUT;
  end R_SET_DEGREE;

  function R_DEGREE return R_T_DEGREE is
  begin
    return THE_DEGREE - 1;
  end R_DEGREE;

  procedure R_POINTS_MODIFICATION is
  begin
    RESOLVED := FALSE;
  end R_POINTS_MODIFICATION;

end RESOL;
