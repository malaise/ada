with Moon, Lem;
package body Flight is

  -- Max horizontal and vertical speed
  Max_Horiz_Speed : constant Lem.Speed_Range := 3.0;
  Max_Verti_Speed : constant Lem.Speed_Range := 10.0;

  -- Get index of point (of moon ground) before LEM
  package Locate is
    function Get_Index (Left, Right : Space.Position_Rec;
                        Ground : Moon.Ground_Array) return Positive;
  end Locate;
  package body Locate is
    Prev_Index : Positive := Positive'First;

    function Get_Index (Left, Right : Space.Position_Rec;
                        Ground : Moon.Ground_Array) return Positive is
      use type Space.Position_Range;
      Start, Stop : Positive;
    begin
      -- Optim: look if LEM is around Prev_Index
      if Prev_Index < Ground'Last 
      and then Ground(Prev_Index).X_Pos <= Left.X_Pos
      and then Ground(Prev_Index + 1).X_Pos > Left.X_Pos then
        -- Unchanged
        null;
      elsif Prev_Index > 1
      and then Ground(Prev_Index - 1).X_Pos <= Left.X_Pos
      and then Ground(Prev_Index).X_Pos > Left.X_Pos then
        -- Lem has moved left
        Prev_Index := Prev_Index - 1;
      elsif Prev_Index < Ground'Last - 1
      and then Ground(Prev_Index + 1).X_Pos <= Left.X_Pos
      and then Ground(Prev_Index + 2).X_Pos > Left.X_Pos then
        -- Lem has moved right
        Prev_Index := Prev_Index + 1;
      else
        -- We don't know -> dichotomy
        if Ground(Prev_Index).X_Pos <= Left.X_Pos then
          Start := Prev_Index;
          Stop := Ground'Last;
        else
          Start := 1;
          Stop := Prev_Index;
        end if;
        loop
          Prev_Index := Start + Stop / 2;
          exit when Ground(Prev_Index).X_Pos <= Left.X_Pos
          and then  Ground(Prev_Index + 1).X_Pos > Left.X_Pos;
          if Ground(Prev_Index).X_Pos > Left.X_Pos then
            Stop := Prev_Index;
          else
            Start := Prev_Index;
          end if;
        end loop;
      end if;
      -- Raise anonymous (un-catchable) exception if this is last index
      declare
        Ground_Index_Not_Found : exception;
      begin
        if Prev_Index = Ground'Last then
          raise Ground_Index_Not_Found;
        end if;
      end;
      return Prev_Index;
    end Get_Index;
  end Locate;

  -- Check that a point is strictly above a segment (defined by two points)
  function Check_Above (Point, Left, Right : Space.Position_Rec) return Boolean is
    -- Height on segment at Point.X
    Y_Point : Space.Position_Range;
    use type Space.Position_Range, Lem.Speed_Range;
  begin
    -- Proportion: (P.Y - L.Y) / (P.X - L.X) = (R.Y - L.Y) / (R.X - L.X)
    Y_Point := Left.Y_Pos + (Right.Y_Pos - Left.Y_Pos)
                          / (Right.X_Pos - Left.X_Pos)
                          * (Point.X_Pos - Left.X_Pos);
    return Point.Y_Pos > Y_Point;
  end Check_Above;

  -- Check bottom of LEM versus ground
  function Check_Ground (Left, Right : Space.Position_Rec;
                         Speed : Lem.Speed_Rec) return Status_Rec is
    Ground : constant Moon.Ground_Array := Moon.Get_Ground;
    P1, P2 : Positive;
    Land :  Space.Position_Rec;
    use type Space.Position_Range, Lem.Speed_Range;
  begin
    -- 1. Check if LEM is above ground (flying)
    -- Locate last point P1 left of the LEM left (X1 <= LX)
    P1 := Locate.Get_Index (Left, Right, Ground);
    P2 := P1 + 1;
    if Ground(P2).X_Pos >= Right.X_Pos then
      -- LEM is completely between two successive points P1 and P2 (X2 >= RX)
      if Ground(P1).Y_Pos <= Ground(P2).Y_Pos then
        -- Ground is climbing: check right corner is above (not equal)
        if Check_Above (Right, Ground(P1), Ground(P2)) then
          return (Status => Flying);
        end if;
      else
        -- Ground is descending: check left corner is above (not equal)
        if Check_Above (Left, Ground(P1), Ground(P2)) then
          return (Status => Flying);
        end if;
      end if;
    else
      -- P2 is between L and R: check it is below
      if Ground(P2).Y_Pos < Right.X_Pos then
        return (Status => Flying);
      end if;
    end if;

    -- Now LEM is either landed or crashed
    -- 2. Check if LEM is landed
    -- Flat ground? For i in P1 to Pn (while Xi <= RX), Yi must be constant to Y1
    loop
      if Ground(P2).Y_Pos /= Ground(P1).Y_Pos then
        return (Status => Crashed);
      end if;
      P2 := P2 + 1;
      exit when Ground(P2).X_Pos > Right.X_Pos;
    end loop;
    -- Check Speeds (|X| and Y) are below minima, else crash
    if abs Speed.X_Speed > Max_Horiz_Speed 
    or else (Speed.Y_Speed < 0.0 and then abs Speed.Y_Speed > Max_Verti_Speed) then
      return (Status => Crashed);
    end if;

    -- 3. Landed
    -- Return the Lem landing position (LX + Lem.Width / 2.0, Y1 + Lem.Height / 2.0)
    Land.X_Pos := Left.X_Pos + Lem.Width / 2.0;
    Land.Y_Pos := Ground(P1).Y_Pos + Lem.Height / 2.0;
    return (Status => Landed, Landed_Pos => Land);
  end Check_Ground;

  -- Check current LEM status
  function Get_Status return Status_Rec is
    Lem_Position : constant Space.Position_Rec := Lem.Get_Position;
    Lem_Speed : constant Lem.Speed_Rec := Lem.Get_Speed;
    use type Space.Position_Range;
    -- Lem left and right bottom corners
    Left, Right : Space.Position_Rec;
  begin
    -- Compute LEM bottom corners
    Left.X_Pos := Lem_Position.X_Pos - Lem.Width / 2.0;
    Right.X_Pos := Lem_Position.X_Pos + Lem.Width / 2.0;
    Left.Y_Pos := Lem_Position.Y_Pos - Lem.Height / 2.0;
    Right.Y_Pos := Left.Y_Pos;

    -- Check if Lem is lost
    if Left.X_Pos < Space.X_Range'First
    or else Right.X_Pos > Space.X_Range'Last
    or else Left.Y_Pos < Space.Y_Range'First
    or else Lem_Position.Y_Pos + Lem.Height / 2.0 > Space.Y_Range'Last then
      return (Status => Lost);
    end if;

    -- Check if Lem is flying above highest point
    if Left.Y_Pos > Moon.Y_Ground_Max then
      return (Status => Flying);
    end if;

    -- Check status versus ground in details
    return Check_Ground (Left, Right, Lem_Speed);
  end Get_Status;


end Flight;


