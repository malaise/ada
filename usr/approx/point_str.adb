with Ada.Text_Io;
with Language;
package body Point_Str is

  package Coo_Io is new Ada.Text_Io.Float_Io(Points.P_T_Coordinate);
  package Coo_Int_Io is new Ada.Text_Io.Integer_Io(Integer);

  function Coordinate_Image (Coordinate : Points.P_T_Coordinate)
                            return Coordinate_String is
    Str : Coordinate_String;
  begin
    Coo_Io.Put (Str, Coordinate, 8, 4);
    return Str;
  end Coordinate_Image;

  -- May raise Constraint_Error
  function Coordinate_Value (Str : String) return Points.P_T_Coordinate is
    C : Points.P_T_Coordinate;
    L : Positive;
    I : Integer;
    Str_Len : Natural;
  begin

    -- Locate last significant character of Str
    Str_Len := 0;
    for J in reverse Str'Range loop
      if Str(J) /= ' ' then
        Str_Len := J + 1 - Str'First;
        exit;
      end if;
    end loop;
    if Str_Len = 0 then
      raise Constraint_Error;
    end if;

    Try_To_Convert:
    begin
      -- Float format
      Coo_Io.Get(Str, C, L);
    exception
      when Ada.Text_Io.Data_Error =>
         -- Int format
        Coo_Int_Io.Get(Str, I, L);
        C := Points.P_T_Coordinate (I);
    end Try_To_Convert;

    if L /= Str'Last then
      raise Constraint_Error;
    else
      return C;
    end if;
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
