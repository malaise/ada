with Nav_Types, My_Io;
procedure T_Typ is
  A1, A2, A3 : Nav_Types.T_Angle;
  D : Nav_Types.T_Drift;
  I : Integer;
begin
  My_Io.Put ("A1 : deg, min ");
  My_Io.Get (I);
  A1.Degrees := Nav_Types.T_Degree(I);
  My_Io.Get (I);
  A1.Minutes := Nav_Types.T_Minute(I);
  My_Io.Put ("A2 : deg, min ");
  My_Io.Get (I);
  A2.Degrees := Nav_Types.T_Degree(I);
  My_Io.Get (I);
  A2.Minutes := Nav_Types.T_Minute(I);

  My_Io.New_Line;
  A3 := Nav_Types."+" (A1, A2);
  My_Io.Put ("A1 + A2 "); My_Io.Put (Integer(A3.Degrees));
  My_Io.Put (Integer(A3.Minutes));

  My_Io.New_Line;
  A3 := Nav_Types."-" (A1, A2);
  My_Io.Put ("A1 - A2 "); My_Io.Put (Integer(A3.Degrees));
  My_Io.Put (Integer(A3.Minutes));

  My_Io.New_Line;
  D := Nav_Types."-" (A1, A2);
  My_Io.Put ("A1 - A2 (drift)"); My_Io.Put (D.Positiv);
  My_Io.Put (Integer(D.Degrees));
  My_Io.Put (Integer(D.Minutes));
  My_Io.New_Line;
end T_Typ;

