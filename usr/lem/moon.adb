with My_Math, Rnd;
with Lem;
package body Moon is

  -- type Ground_Array is array (Positive range <>) of Space.Position_Rec;
  use type Space.Position_Range;
  -- Number of points on the ground
  Nb_Points : constant := 25;
  subtype Point_Range is Positive range 1 .. Nb_Points;

  -- Distance on X between 2 points
  Delta_Point : constant Space.X_Range
              := Space.X_Max / Space.Position_Range (Nb_Points - 1);

  -- The ground array
  Ground : Ground_Array (Point_Range);

  -- Init a new moon
  procedure Init is
    -- Index of the central point of the landing site (2 .. Last-2)
    --  Last-2 because Last-1 to Last may not be wide enough for LEM
    subtype Landing_Range is Point_Range
            range Point_Range'First + 1 .. Point_Range'Last - 2;
    Index_Landing : Point_Range;
    use type Space.X_Range;
  begin
    -- Raise anonymous (un-catchable) exception
    --  if LEM cannot land between 3 points
    declare
      Delta_Point_Lem_Width_Error : exception;
    begin
      if Delta_Point * 2.0 < Lem.Width then
        raise Delta_Point_Lem_Width_Error;
      end if;
    end;
    -- Set random height for all points
    for I in Point_Range loop
      Ground(I).X_Pos := Space.X_Range (I - 1) * Delta_Point;
      Ground(I).Y_Pos := Space.Y_Range (Rnd.Float_Random (
        Float(Space.Y_Range'First),
        Float(Y_Ground_Max)));
    end loop;
    -- Ensure last point has X = last X_Range
    Ground(Point_Range'Last).X_Pos := Space.X_Range'Last;

    -- Set index of landing point
    Index_Landing := Rnd.Int_Random (Landing_Range'First, Landing_Range'Last);
    -- Level the landing site
    Ground(Index_Landing - 1).Y_Pos := Ground(Index_Landing).Y_Pos;
    Ground(Index_Landing + 1).Y_Pos := Ground(Index_Landing).Y_Pos;
  end Init;


  -- The array of points defining the ground
  -- First point has X = 0.0, last point has X = Space.X_Max
  --  and points are equi-distant on X
  function Get_Ground return Ground_Array is
  begin
    return Ground;
  end Get_Ground;


end Moon;


