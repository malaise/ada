with Rnd, Argument;
with Lem;
package body Moon is

  -- type Ground_Array is array (Positive range <>) of Space.Position_Rec;
  use type Space.Position_Range;
  -- Number of points on the ground
  Nb_Points : constant := 18;
  subtype Point_Range is Positive range 1 .. Nb_Points;

  -- Distance on X between 2 points
  Delta_Point : constant Space.X_Range
              := Space.X_Max / Space.Position_Range (Nb_Points - 1);

  -- The ground array
  Ground : Ground_Array (Point_Range);

  -- Add Ground(Of) so that
  --     abs (Ground(Of).Y_Pos - Ground(Ref).Y_Pos) < Y_Min_Delta
  -- and Y_Ground_Min <= Ground(Of).Y_Pos <= Y_Ground_Max
  procedure Add_Ground (Of_Index, Ref_Index : in Point_Range) is
    -- Minimum thickness of ground
    Y_Ground_Min : constant Space.Y_Range := 2.0;
    -- Minimum delta between 2 points (before levelling of the landing site)
    Y_Min_Delta : constant Space.Y_Range := 5.0;
    Ref_Y : constant Space.Y_Range := Ground(Ref_Index).Y_Pos;
  begin
    loop
      Ground(Of_Index).Y_Pos := Space.Y_Range (Rnd.Gen.Float_Random (
          Float(Space.Y_Range'First+ Y_Ground_Min), Float(Y_Ground_Max)));
      exit when abs (Ground(Of_Index).Y_Pos - Ref_Y) >= Y_Min_Delta;
    end loop;
  end Add_Ground;

  -- Init a new moon
  procedure Init is
    -- Normal or hard mode
    Hard_Level : Boolean;
    -- Index of landing site
    -- If hard, ground is flat from I-1 to I, else from I-1 to I+1
    Index_Landing : Point_Range;
    -- First and last possible index for landing site (depending on hard)
    -- Then indexes of landing site
    First_Index : Point_Range := Point_Range'First + 2;
    Last_Index : Point_Range := Point_Range'Last - 1;

  begin
    -- Check if Hard_Level (-h or --hard argument)
    Hard_Level := False;
    if Argument.Is_Set (1, "h")
    and then Argument.Get_Parameter (1, "h") = "" then
      -- "-h"
      Hard_Level := True;
    elsif Argument.Is_Set (1, "-hard")
    and then Argument.Get_Parameter (1, "-hard") = "" then
      -- "--hard"
      Hard_Level := True;
    end if;
    -- Raise anonymous (un-catchable) exception
    --  if LEM cannot land between 2 points
    declare
      Delta_Point_Lem_Width_Error : exception;
    begin
      pragma Warnings (Off, "condition is always False");
      if Delta_Point < Lem.Width then
        raise Delta_Point_Lem_Width_Error;
      end if;
      pragma Warnings (On, "condition is always False");
    end;

    -- Set X of each point, ensure last point has X = last X_Range
    for I in Point_Range loop
      Ground(I).X_Pos := Space.X_Range (I - 1) * Delta_Point;
    end loop;
    Ground(Point_Range'Last).X_Pos := Space.X_Range'Last;

    -- Set index of landing point
    if not Hard_Level then
      Last_Index := Last_Index - 1;
    end if;
    Index_Landing := Rnd.Gen.Int_Random (First_Index, Last_Index);

    -- Set random landing altitude
    Ground(Index_Landing).Y_Pos := Space.Y_Range'First;
    Add_Ground (Index_Landing, Index_Landing);

    -- Level the landing site. 2 consecutive points if hard, 3 if normal.
    First_Index := Index_Landing - 1;
    Ground(First_Index).Y_Pos := Ground(Index_Landing).Y_Pos;
    Last_Index := Index_Landing;
    if not Hard_Level then
      Last_Index := Last_Index + 1;
      Ground(Last_Index).Y_Pos := Ground(Index_Landing).Y_Pos;
    end if;

    -- Add points around landing array
    for I in reverse Point_Range'First .. First_Index - 1 loop
      Add_Ground (I, I + 1);
    end loop;
    for I in Last_Index + 1 .. Point_Range'Last loop
      Add_Ground (I, I - 1);
    end loop;

  end Init;

  -- The array of points defining the ground
  -- First point has X = 0.0, last point has X = Space.X_Max
  --  and points are equi-distant on X
  function Get_Ground return Ground_Array is
  begin
    return Ground;
  end Get_Ground;


end Moon;


