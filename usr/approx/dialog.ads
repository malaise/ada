with As.U;
with Points, Screen, Resol;
package Dialog is

  -- If points are not saved, ask for confirmation
  function Confirm_Lost return Boolean;

  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  function Parse_Spaces (Txt : in out As.U.Asu_Us) return Boolean;

  -- Get a coordinate
  --  If Set is set in, then a Put_Then_Get is performed, else a get
  --  Validity is checked and Set is set out according to the final result
  subtype D_Coordinate_List is Screen.S_Coord_List;
  procedure Read_Coordinate (Kind : in D_Coordinate_List;
           Set : in out Boolean; Coordinate : in out Points.P_T_Coordinate;
           Subtitle : in Boolean := False);

  -- Get/update degree
  procedure Read_Degree;

  -- Display polynom
  procedure Put_Polynom (Polynom : Resol.Vector);

  -- Display y=f(x). Continue with another x?
  function Put_Yfx (Point : Points.P_T_One_Point) return Boolean;

end Dialog;

