package body Resol is

  subtype T_Degree is Natural range 1 .. R_T_Degree'Last + 1;

  The_Degree : T_Degree := 1;
  -- Previous solution still accurate
  Resolved : Boolean := False;
  Previous_Solution : Vector (1..T_Degree'Last);

  -- Computation of the solution
  package R_Compute is
    subtype Matrix is My_Syslin.Matrix;
    -- Build At * A matrix and B * Y vector from points
    procedure Do_Matrixes (The_Points : in Points.P_T_The_Points;
      T_A_A : out Matrix; A_Y : out Vector);
    -- Solve (At*A)*X=B*Y
    function System (A : Matrix; B : Vector) return Vector;
  end R_Compute;
  package body R_Compute is separate;

  -- Try to re-use previous result, otherwise solve
  function R_Resolution (The_Points : Points.P_T_The_Points) return Vector is
  begin
    if not Resolved then
      if The_Degree > The_Points'Length then raise R_Degree_Out; end if;
      declare
        T_A_A : R_Compute.Matrix(1..The_Degree, 1..The_Degree);
        A_Y : Vector(1..The_Degree);
      begin
        Resolved := True;
        R_Compute.Do_Matrixes (The_Points, T_A_A, A_Y);
        Previous_Solution (1..The_Degree) := R_Compute.System (T_A_A, A_Y);
      exception
        when others =>
          Resolved := False;
          raise R_Resol_Error;
      end;
    end if;
    return Previous_Solution (1..The_Degree);
  exception
    when others =>
      Resolved := False;
      raise R_Resol_Error;
  end R_Resolution;

  -- The System degree is polynomial degree + 1
  procedure R_Set_Degree (Degree : in R_T_Degree) is
  begin
    if Degree + 1 /= The_Degree then
      Resolved := False;
      The_Degree := Degree + 1;
    end if;
  exception
    when others => raise R_Degree_Out;
  end R_Set_Degree;

  function R_Degree return R_T_Degree is
  begin
    return The_Degree - 1;
  end R_Degree;

  procedure R_Points_Modification is
  begin
    Resolved := False;
  end R_Points_Modification;

end Resol;
