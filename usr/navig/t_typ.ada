with NAV_TYPES, MY_IO;
procedure T_TYP is
  A1, A2, A3 : NAV_TYPES.T_ANGLE;
  D : NAV_TYPES.T_DRIFT;
  I : INTEGER;
begin
  my_io.put ("A1 : deg, min ");
  my_io.get (I);
  A1.DEGREES := NAV_TYPES.T_DEGREE(I);
  my_io.get (I);
  A1.MINUTES := NAV_TYPES.T_MINUTE(I);
  my_io.put ("A2 : deg, min ");
  my_io.get (I);
  A2.DEGREES := NAV_TYPES.T_DEGREE(I);
  my_io.get (I);
  A2.MINUTES := NAV_TYPES.T_MINUTE(I);

  my_io.new_line;
  A3 := NAV_TYPES."+" (A1, A2);
  my_io.put ("A1 + A2 "); my_io.put (INTEGER(A3.DEGREES));
  my_io.put (INTEGER(A3.MINUTES));

  my_io.new_line;
  A3 := NAV_TYPES."-" (A1, A2);
  my_io.put ("A1 - A2 "); my_io.put (INTEGER(A3.DEGREES));
  my_io.put (INTEGER(A3.MINUTES));

  my_io.new_line;
  D := NAV_TYPES."-" (A1, A2);
  my_io.put ("A1 - A2 (drift)"); my_io.put (D.POSITIV);
  my_io.put (INTEGER(D.DEGREES));
  my_io.put (INTEGER(D.MINUTES));
  my_io.new_line;
end t_typ;