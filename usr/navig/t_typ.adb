with Nav_Types, My_Io;
procedure T_Typ is
  A1, A2, A3 : Nav_Types.T_Angle;
  D : Nav_Types.T_Drift;
  I : Integer;
begin
  my_io.put ("A1 : deg, min ");
  my_io.get (I);
  A1.Degrees := Nav_Types.T_Degree(I);
  my_io.get (I);
  A1.Minutes := Nav_Types.T_Minute(I);
  my_io.put ("A2 : deg, min ");
  my_io.get (I);
  A2.Degrees := Nav_Types.T_Degree(I);
  my_io.get (I);
  A2.Minutes := Nav_Types.T_Minute(I);

  my_io.new_line;
  A3 := Nav_Types."+" (A1, A2);
  my_io.put ("A1 + A2 "); my_io.put (Integer(A3.Degrees));
  my_io.put (Integer(A3.Minutes));

  my_io.new_line;
  A3 := Nav_Types."-" (A1, A2);
  my_io.put ("A1 - A2 "); my_io.put (Integer(A3.Degrees));
  my_io.put (Integer(A3.Minutes));

  my_io.new_line;
  D := Nav_Types."-" (A1, A2);
  my_io.put ("A1 - A2 (drift)"); my_io.put (D.Positiv);
  my_io.put (Integer(D.Degrees));
  my_io.put (Integer(D.Minutes));
  my_io.new_line;
end t_typ;