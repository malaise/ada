with Afpx;
with Points;
package Point_Str is

  Coordinate_String_Len : constant := 16;
  subtype Coordinate_String is String ( 1 .. Coordinate_String_Len);

  function Coordinate_Image (Coordinate : Points.P_T_Coordinate)
                            return Coordinate_String;

  -- May raise Constraint_Error
  function Coordinate_Value (Str : String) return Points.P_T_Coordinate;


  function Encode_Rec (Point : Points.P_T_One_Point) return Afpx.Line_Rec;

  function Decode_Rec (Rec : Afpx.Line_Rec) return Points.P_T_One_Point;

end Point_Str;
