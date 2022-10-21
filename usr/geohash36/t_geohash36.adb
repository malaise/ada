with Argument, Basic_Proc, My_Math, Normalization;
with Geohash36;
procedure T_Geohash36 is

  procedure Help is
  begin
    Basic_Proc.Put_Line_Output (
        "Usage: " & Argument.Get_Program_Name & " <command>");
    Basic_Proc.Put_Line_Output (
        "  <commnd> ::= <encode> | <decode> | <precision> | <neighbors>"
      & " | <checksup> | <help>");
    Basic_Proc.Put_Line_Output ("  <encode>    ::= -c <coord> <precision>");
    Basic_Proc.Put_Line_Output ("  <coord>     ::= <lat> <lon>");
    Basic_Proc.Put_Line_Output ("  <precision> ::= -P <precision>");
    Basic_Proc.Put_Line_Output ("  <neighbors> ::= -N <code>");
    Basic_Proc.Put_Line_Output ("  <checksum>  ::= -C <code>");
    Basic_Proc.Put_Line_Output ("  <help>      ::= -h");
  end Help;

  procedure Encode (Coord : in Geohash36.Coordinate;
                    Prec  : in Geohash36.Precision_Range) is
    Code : constant String := Geohash36.Encode (Coord, Prec);
  begin
    Basic_Proc.Put_Line_Output (Code);
  end Encode;

  procedure Decode (Code : Geohash36.Code_Type) is
    Coord : constant Geohash36.Coordinate := Geohash36.Decode (Code);
  begin
    Basic_Proc.Put_Line_Output (
        Normalization.Normal_Fixed (My_Math.Real (Coord.Lat), 17, 4, '0')
      & " "
      & Normalization.Normal_Fixed (My_Math.Real (Coord.Lon), 17, 4, '0'));
  end Decode;

  procedure Precision (Prec  : in Geohash36.Precision_Range) is
    Lat, Lon : Geohash36.Real;
  begin
    Geohash36.Precision_Of (Prec, Lat, Lon);
    Basic_Proc.Put_Line_Output  (
        Normalization.Normal_Fixed (Float (Lat), 10, 6) & "m x "
      & Normalization.Normal_Fixed (Float (Lon), 10, 6) & "m");
  end Precision;

  procedure Neighbors (Code : Geohash36.Code_Type) is
  begin
    for D in Geohash36.Direction_List loop
      Basic_Proc.Put_Output (Geohash36.Neighbor_Of (Code, D));
      if Geohash36.Direction_List'Pos (D) rem 3 = 2 then
        Basic_Proc.New_Line_Output;
      else
      Basic_Proc.Put_Output ("  ");
      end if;
    end loop;
  end Neighbors;

  procedure Checksum (Code : Geohash36.Code_Type) is
  begin
    Basic_Proc.Put_Line_Output (Geohash36.Checksum_Of (Code) & "");
  end Checksum;

begin
  if Argument.Get_Parameter (1) = "-c" then
    Encode (
        Coord => (Lat => Geohash36.Real'Value (Argument.Get_Parameter (2)),
                  Lon => Geohash36.Real'Value (Argument.Get_Parameter (3))),
        Prec  => Geohash36.Precision_Range'Value (Argument.Get_Parameter (4)));
  elsif Argument.Get_Parameter  (1) = "-d" then
    Decode (Argument.Get_Parameter (2));
  elsif Argument.Get_Parameter  (1) = "-P" then
    Precision (Geohash36.Precision_Range'Value (Argument.Get_Parameter (2)));
  elsif Argument.Get_Parameter  (1) = "-N" then
    Neighbors (Argument.Get_Parameter (2));
  elsif Argument.Get_Parameter  (1) = "-C" then
    Checksum (Argument.Get_Parameter (2));
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
raise;
end T_Geohash36;

