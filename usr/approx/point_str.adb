with My_Math, Language, Normalization;
package body Point_Str is

  Exp_Len : constant := 3;
  function Coordinate_Image (Coordinate : Points.P_T_Coordinate)
                            return Coordinate_String is
  begin
    return Normalization.Normal_Digits (Coordinate, Coordinate_String_Len,
                                        Exp_Len);
  end Coordinate_Image;

  -- May raise Constraint_Error
  function Coordinate_Value (Str : String) return Points.P_T_Coordinate is
    I : My_Math.Inte;
  begin

    begin
      -- Float format
      return My_Math.Get (Str);
    exception
      when My_Math.Data_Error =>
         -- Int format
        I := My_Math.Get (Str);
        return Points.P_T_Coordinate (I);
    end;
  exception
    when others =>
      raise Constraint_Error;
  end Coordinate_Value;

  function Encode_Rec (Point : Points.P_T_One_Point) return Afpx.Line_Rec is
    Rec : Afpx.Line_Rec;
  begin
    Rec.Len := 2 * Coordinate_String_Len + 1;
    Rec.Str (1 .. Rec.Len) := Language.Copy (
        Coordinate_Image(Point.X) & " " & Coordinate_Image(Point.Y));
    return Rec;
  end Encode_Rec;

  function Decode_Rec (Rec : Afpx.Line_Rec) return Points.P_T_One_Point is
  begin
    return (X => Coordinate_Value(Language.Copy (
       Rec.Str(1 .. Coordinate_String_Len))),
            Y => Coordinate_Value(Language.Copy (
       Rec.Str(Coordinate_String_Len + 2 ..  2 * Coordinate_String_Len + 1))) );
  end Decode_Rec;


end Point_Str;

