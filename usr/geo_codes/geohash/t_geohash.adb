with Argument, Basic_Proc, My_Math, Normalization;
with Geohash;
procedure T_Geohash is

  procedure Help is
  begin
    Basic_Proc.Put_Line_Output (
        "Usage: " & Argument.Get_Program_Name & " <command>");
    Basic_Proc.Put_Line_Output (
        "  <commnd> ::= <encode> | <decode> | <bounds> | <adjacents> | <help>");
    Basic_Proc.Put_Line_Output ("  <encode>    ::= -c <coord> <precision>");
    Basic_Proc.Put_Line_Output ("  <coord>     ::= <lat> <lon>");
    Basic_Proc.Put_Line_Output ("  <precision> ::= 1..12");
    Basic_Proc.Put_Line_Output ("  <decode>    ::= -d <code>");
    Basic_Proc.Put_Line_Output ("  <bounds>    ::= -B <code>");
    Basic_Proc.Put_Line_Output ("  <adjacents> ::= -A <code>");
    Basic_Proc.Put_Line_Output ("  <help>      ::= -h");
  end Help;

  procedure Encode (Coord : in Geohash.Coordinate;
                    Prec  : in Geohash.Precision_Range) is
    Code : constant String := Geohash.Encode (Coord, Prec);
  begin
    Basic_Proc.Put_Line_Output (Code);
  end Encode;

  procedure Decode (Code : Geohash.Code_Type) is
    Coord : constant Geohash.Coordinate := Geohash.Decode (Code);
  begin
    Basic_Proc.Put_Line_Output (
        Normalization.Normal_Fixed (My_Math.Real (Coord.Lat), 17, 4, '0')
      & " "
      & Normalization.Normal_Fixed (My_Math.Real (Coord.Lon), 17, 4, '0'));
  end Decode;

  procedure Bounds (Code : Geohash.Code_Type) is
    Sw, Ne : Geohash.Coordinate;
  begin
    Geohash.Bounds_Of (Code, Sw, Ne);
    Basic_Proc.Put_Line_Output  (
        Normalization.Normal_Fixed (Float (Sw.Lat), 10, 6) & " "
      & Normalization.Normal_Fixed (Float (Sw.Lon), 10, 6) & ", "
      & Normalization.Normal_Fixed (Float (Ne.Lat), 10, 6) & " "
      & Normalization.Normal_Fixed (Float (Ne.Lon), 10, 6));
  end Bounds;

  procedure Adjacents (Code : Geohash.Code_Type) is
    use type Geohash.Direction_List;
  begin
    for D in Geohash.Direction_List loop
      Basic_Proc.Put_Output (Geohash.Adjacent_Of (Code, D));
      if D = Geohash.Direction_List'Last then
        Basic_Proc.New_Line_Output;
      else
        Basic_Proc.Put_Output (" ");
      end if;
    end loop;
  end Adjacents;

begin
  if Argument.Get_Parameter (1) = "-c" then
    Encode (
        Coord => (Lat => Geohash.Real'Value (Argument.Get_Parameter (2)),
                  Lon => Geohash.Real'Value (Argument.Get_Parameter (3))),
        Prec  => Geohash.Precision_Range'Value (Argument.Get_Parameter (4)));
  elsif Argument.Get_Parameter  (1) = "-d" then
    Decode (Argument.Get_Parameter (2));
  elsif Argument.Get_Parameter  (1) = "-B" then
    Bounds (Argument.Get_Parameter (2));
  elsif Argument.Get_Parameter  (1) = "-A" then
    Adjacents (Argument.Get_Parameter (2));
  elsif Argument.Get_Parameter (1) = "-h" then
    Help;
  else
    raise Constraint_Error;
  end if;
exception
  when others =>
    Basic_Proc.Put_Line_Output ("ERROR: Invalid argument");
    Help;
    Basic_Proc.Set_Error_Exit_Code;
end T_Geohash;

