with Moon, Lem;
package body Flight is

  -- Kind of LEM flight status
  -- type Status_List is (Flying, Landed, Crashed, Lost);

  -- LEM status and pos when landed
  -- type Status_Rec (Status : Status_List := Flying) is record
  --   case Status is
  --     when Flying | Crashed | Lost => null;
  --     when Landed =>
  --       Landed_Pos : Space.Position_Rec;
  --   end case;
  -- end record;

  -- Check bottom of LEM versus ground
  function Check_Ground (Left, Right : Space.Position_Rec) return Status_Rec is
    Ground : constant Moon.Ground_Array := Moon.Get_Ground;
  begin
    -- @@@
    -- 1. Check if LEM is above
    -- Locate last point P1 left of the LEM left (X1 <= LX)
    -- If LEM is completely between two successive points P1 and P2
    --    (X2 >= RX)
    --  if ascending (Y1 <= Y2) check right corner is above (not equal)
    --  if descending (Y1 > Y2) check left corner is above (not equal)
    -- Else check bottom is above P2 (Y2 < RY)
    -- If these checks pass => flying
    -- 2. Check if LEM is landed
    -- Flat ground? For i in P1 to Pn (while Xi <= RX), Yi must be constant,
    --  else crash
    -- Check Speed (X and Y) are below minima, else crash
    -- 3. Landed
    -- Return the Lem landing position (LX + Lem.Width / 2.0, Y1 + Lem.Height / 2.0)
    return (Status => Flying);
  end Check_Ground;

  -- Check current LEM status
  function Get_Status return Status_Rec is
  Lem_Position : constant Space.Position_Rec := Lem.Get_Position;
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

    return Check_Ground (Left, Right);
  end Get_Status;


end Flight;


