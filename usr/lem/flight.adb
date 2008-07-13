with Ada.Text_Io;
with Rnd;
with Moon, Debug, Screen;
package body Flight is

  -- Part of margin for a landing to be "safe"
  Safe_Ratio : constant := 0.75;

  -- Get a valid init position for the LEM
  function Get_Init_Position return Space.Position_Rec is
    Init_Pos : Space.Position_Rec;
    use type Space.X_Range;
  begin
    Init_Pos.X_Pos :=  Space.X_Range (Rnd.Float_Random (
       Float(Space.X_Range'First + Lem.Width),
       Float(Space.X_Range'Last - Lem.Width) ));
    Init_Pos.Y_Pos := Space.Y_Range'Last - Lem.Height;
    return Init_Pos;
  end Get_Init_Position;

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
          Prev_Index := (Start + Stop) / 2;
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
  procedure Check_Ground (Left, Right : in Space.Position_Rec;
                          Result : in out Status_Rec) is
    -- The Moon ground
    Ground : constant Moon.Ground_Array := Moon.Get_Ground;
    -- The points of ground left, belaow and right the Lem
    P1, P2, P3 : Positive;
    -- The index of lowest point when flat ground (0 => not flat)
    Flat_Index : Natural;
    -- Iteration index
    Iter : Positive;
    use type Space.Position_Range, Lem.Speed_Range;
  begin
    -- Locate last point P1 left of the LEM left (X1 <= LX)
    P1 := Locate.Get_Index (Left, Right, Ground);
    P2 := P1 + 1;
    P3 := P2 + 1;
    if Debug.Set_Flight then
      Ada.Text_Io.Put_Line ("FLIGHT Lem is "
         & Left.X_Pos'Img & " - " & Right.X_Pos'Img & " / " & Left.Y_Pos'Img);
      Ada.Text_Io.Put_Line ("P" & P1'Img & " is " & Ground(P1).X_Pos'Img
                          & " / " & Ground(P1).Y_Pos'Img);
      Ada.Text_Io.Put_Line ("P" & P2'Img & " is " & Ground(P2).X_Pos'Img
                          & " / " & Ground(P2).Y_Pos'Img);
    end if;

    -- Check if ground is flat (at least as it is displayed on screen)
    -- For i in P1 to Pn (while Xi <= RX), Yi must be "=" to Y1
    Flat_Index := P1;
    Iter := P2;
    loop
      -- Check current (Iter) is at same visual height as P1
      if not Screen.Same_Height (Ground(Iter).Y_Pos, Ground(P1).Y_Pos) then
        if Debug.Set_Flight then
          Ada.Text_Io.Put_Line ("FLIGHT Ground not flat for " & Iter'Img);
        end if;
        Flat_Index := 0;
        exit;
      end if;
      -- Keep the lowest of points as the landing one
      if Ground(Iter).Y_Pos < Ground(Flat_Index).Y_Pos then
        Flat_Index := Iter;
      end if;
      -- Last point to check is the first one with X >= Right
      exit when Ground(Iter).X_Pos >= Right.X_Pos;
      Iter := Iter + 1;
    end loop;

    -- Set status to flying or approaching, according to height and flat
    if Flat_Index /= 0
    and then Right.Y_Pos - Ground(Flat_Index).Y_Pos < Moon.Y_Ground_Max then
      -- Ground is flat and less than Y_Ground_Max below
      Result.Status := Approaching;
    else
      -- Ground is not flat or too far below
      Result.Status := Flying;
    end if;

    -- 1. Check if LEM is above ground (flying)
    if Ground(P2).X_Pos >= Right.X_Pos then
      -- LEM is completely between two successive points P1 and P2 (X2 >= RX)
      if Ground(P1).Y_Pos <= Ground(P2).Y_Pos then
        -- Ground is climbing: check right corner is above (not equal)
        if Check_Above (Right, Ground(P1), Ground(P2)) then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 <= P2");
          end if;
          return;
        end if;
      else
        -- Ground is descending: check left corner is above (not equal)
        if Check_Above (Left, Ground(P1), Ground(P2)) then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 > P2");
          end if;
          return;
        end if;
      end if;
    else
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT P" & P3'Img & " is "
                     & Ground(P3).X_Pos'Img
             & " / " & Ground(P3).Y_Pos'Img);
      end if;
      -- P2 is between L and R: check depends on P1, P2 and P3
      if Ground(P1).Y_Pos <= Ground(P2).Y_Pos
      and then Ground(P2).Y_Pos <= Ground(P3).Y_Pos then
        -- P1 P2 P3 climbing: check right > (P2, P3)
        if Check_Above (Right, Ground(P2), Ground(P3)) then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 <= P2 <= P3");
          end if;
          return;
        end if;
      elsif Ground(P1).Y_Pos >= Ground(P2).Y_Pos
      and then Ground(P2).Y_Pos >= Ground(P3).Y_Pos then
        -- P1 P2 P3 climbing: check left > (P1, P2)
        if Check_Above (Left, Ground(P1), Ground(P2)) then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 >= P2 >= P3");
          end if;
          return;
        end if;
      elsif Ground(P1).Y_Pos <= Ground(P2).Y_Pos
      and then Ground(P2).Y_Pos >= Ground(P3).Y_Pos then
        -- P2 above P1 and P3: check P2 < Lem
        if Ground(P2).Y_Pos < Right.Y_Pos then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 <= P2 >= P3");
          end if;
          return;
        end if;
      elsif Ground(P1).Y_Pos >= Ground(P2).Y_Pos
      and then Ground(P2).Y_Pos <= Ground(P3).Y_Pos then
        -- P2 below P1 and P3: check left > (P1, P2) and right > (P2, P3)
        if Check_Above (Left, Ground(P1), Ground(P2))
        and then Check_Above (Right, Ground(P2), Ground(P3)) then
          if Debug.Set_Flight then
            Ada.Text_Io.Put_Line ("FLIGHT Above P1 >= P2 <= P3");
          end if;
          return;
        end if;
      end if;
    end if;

    -- Now LEM is either landed or crashed
    -- 2. Check if LEM is landed
    Result.Status := Crashed;
    -- Check ground is flat
    if Flat_Index = 0 then
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT Crashed not flat");
      end if;
      return;
    end if;
    -- Check Speeds (|X| and Y) are below minima, else crash
    if abs Result.Speed.X_Speed > Max_Horiz_Speed
    or else (Result.Speed.Y_Speed < 0.0
             and then abs Result.Speed.Y_Speed > Max_Verti_Speed) then
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT Crashed speed "
                                 & Result.Speed.X_Speed'Img
                         & " / " & Result.Speed.Y_Speed'Img);
      end if;
      return;
    end if;
    -- Speed must be negative or nul, otherwise approaching
    if Result.Speed.Y_Speed > 0.0 then
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT On ground but climbing");
      end if;
      Result.Status := Approaching;
      return;
    end if;

    -- 3. Landed
    -- Return the Lem landing position
    --  (LX + Lem.Width / 2.0, Yfalt + Lem.Height / 2.0)
    -- Flat_Index has been set to the lowest of the "flat" points
    if Debug.Set_Flight then
      Ada.Text_Io.Put_Line ("FLIGHT Landed with speeds "
                                 & Result.Speed.X_Speed'Img
                         & " / " & Result.Speed.Y_Speed'Img);
    end if;
    -- Safe or normal landing?
    if abs Result.Speed.Y_Speed <= Max_Verti_Speed * Safe_Ratio
    and then abs Result.Speed.X_Speed <= Max_Horiz_Speed * Safe_Ratio then
      Result.Status := Safe_Landed;
    else
      Result.Status := Landed;
    end if;
    Result.Pos.X_Pos := Left.X_Pos + Lem.Width / 2.0;
    Result.Pos.Y_Pos := Ground(Flat_Index).Y_Pos + Lem.Height / 2.0;
    Result.Speed := (0.0, 0.0);
    return;
  end Check_Ground;

  -- Check current LEM status
  function Get_Status return Status_Rec is
    Result : Status_Rec;
    use type Space.Position_Range;
    -- Lem left and right bottom corners
    Left, Right : Space.Position_Rec;
  begin
    Result.Pos := Lem.Get_Position;
    Result.Speed := Lem.Get_Speed;
    -- Compute LEM bottom corners
    Left.X_Pos := Result.Pos.X_Pos - Lem.Width / 2.0;
    Right.X_Pos := Result.Pos.X_Pos + Lem.Width / 2.0;
    Left.Y_Pos := Result.Pos.Y_Pos - Lem.Height / 2.0;
    Right.Y_Pos := Left.Y_Pos;

    -- Check if Lem is lost or crashed
    if Left.X_Pos < Space.X_Range'First
    or else Right.X_Pos > Space.X_Range'Last
    or else Result.Pos.Y_Pos + Lem.Height / 2.0 > Space.Y_Range'Last then
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT Lost at "
                      & Result.Pos.X_Pos'Img & "/" & Result.Pos.Y_Pos'Img);
      end if;
      Result.Status := Lost;
      return Result;
    end if;
    if Left.Y_Pos < Space.Y_Range'First then
      if Debug.Set_Flight then
        Ada.Text_Io.Put_Line ("FLIGHT Crashed at "
                      & Result.Pos.X_Pos'Img & "/" & Result.Pos.Y_Pos'Img);
      end if;
      Result.Status := Crashed;
      return Result;
    end if;

    -- Check if Lem is flying far above highest point
    if Left.Y_Pos > Moon.Y_Ground_Max * 1.5 then
      Result.Status := Flying;
      return Result;
    end if;

    -- Check status versus ground in details
    Check_Ground (Left, Right, Result);
    return Result;
  end Get_Status;

end Flight;

