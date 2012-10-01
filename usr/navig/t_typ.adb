with Basic_Proc, Get_Int;
with Nav_Types;
procedure T_Typ is
  A1, A2, A3 : Nav_Types.T_Angle;
  D : Nav_Types.T_Drift;
  I : Integer;
begin
  Basic_Proc.Put_Output ("A1 : deg, min ");
  I := Get_Int (Basic_Proc.Get_Line);
  A1.Degrees := Nav_Types.T_Degree(I);
  I := Get_Int (Basic_Proc.Get_Line);
  A1.Minutes := Nav_Types.T_Minute(I);
  Basic_Proc.Put_Output ("A2 : deg, min ");
  I := Get_Int (Basic_Proc.Get_Line);
  A2.Degrees := Nav_Types.T_Degree(I);
  I := Get_Int (Basic_Proc.Get_Line);
  A2.Minutes := Nav_Types.T_Minute(I);

  Basic_Proc.New_Line_Output;
  A3 := Nav_Types."+" (A1, A2);
  Basic_Proc.Put_Output ("A1 + A2 ");
  Basic_Proc.Put_Output (A3.Degrees'Img);
  Basic_Proc.Put_Output (A3.Minutes'Img);

  Basic_Proc.New_Line_Output;
  A3 := Nav_Types."-" (A1, A2);
  Basic_Proc.Put_Output ("A1 - A2 ");
  Basic_Proc.Put_Output (A3.Degrees'Img);
  Basic_Proc.Put_Output (A3.Minutes'Img);

  Basic_Proc.New_Line_Output;
  D := Nav_Types."-" (A1, A2);
  Basic_Proc.Put_Output ("A1 - A2 (drift)");
  Basic_Proc.Put_Output (D.Positiv'Img);
  Basic_Proc.Put_Output (D.Degrees'Img);
  Basic_Proc.Put_Output (D.Minutes'Img);
  Basic_Proc.New_Line_Output;
end T_Typ;

