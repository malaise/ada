package body Team is

  Team_Error : exception;

  -- Number of pieces per team
  Nb_Pieces : constant := 2 * Integer(Space.Row_Range'Last);
  type Curr_Range is new Positive range 1 .. Nb_Pieces + 1;
  subtype Nb_Range is Curr_Range range 1 .. Nb_Pieces;

  -- Array of pieces per team
  Teams : array (Space.Color_List, Nb_Range) of Pieces.Piece_Access
        := (others => (others => null) );
  -- Current piece per team
  Curr  : array (Space.Color_List) of Curr_Range
        := (others => Nb_Range'First);

  -- Add a new piece to a team
  procedure Add (Piece : in Pieces.Piece_Access) is
    Id : constant Pieces.Piece_Id := Pieces.Id_Of(Piece.all);
    use type Pieces.Piece_Access;
  begin
    for I in Nb_Range loop
      if Teams(Id.Color, I) = null then
        Teams(Id.Color, I) := Piece;
        return;
      end if;
    end loop;
    raise Team_Error;
  end Add;

  -- Delete a piece from a team
  procedure Del (Piece : in Pieces.Piece_Access) is
    Id : constant Pieces.Piece_Id := Pieces.Id_Of(Piece.all);
    use type Pieces.Piece_Access;
  begin
    for I in Nb_Range loop
      if Teams(Id.Color, I) = Piece then
        if I = Nb_Range'Last then
          -- Last piece
          Teams(Id.Color, I) := null;
          return;
        end if;
        -- Shift
        for J in I+1 .. Nb_Range'Last loop
          Teams(Id.Color, J-1) := Teams(Id.Color, J);
          if Teams(Id.Color, J) = null then
            return;
          end if;
        end loop;
        Teams(Id.Color, Nb_Range'Last) := null;
        return;
      end if;
    end loop;
    raise Team_Error;
  end Del;

  -- Move to first piece of a team
  procedure Rewind (Color : in Space.Color_List) is
  begin
    Curr(Color) := Nb_Range'First;
  end Rewind;

  -- Get current piece of a team (or null) and to move to next
  function Get (Color : Space.Color_List) return Pieces.Piece_Access is
    Piece : Pieces.Piece_Access;
  begin
    if Curr(Color) > Nb_Range'Last then
      return null;
    else
      Piece := Teams(Color, Curr(Color));
      Curr(Color) := Curr(Color) + 1;
      return Piece;
    end if;
  end Get;

end Team;

