with Normal, Lower_Str;
with Space, Pieces;
function Image (Action : Game.Action_Rec;
                Result : Game.Move_Status_List) return String is
  -- Result
  Str : String (1 .. 9) := (others => ' ');
  First : Positive := 1;

  function Image (Square : in Space.Square_Coordinate) return  String is
  begin
    return Lower_Str (Space.Col_Range'Image(Square.Col))
                   & Normal(Integer(Square.Row), 1);
  end Image;

  function Image (Kind : Pieces.Promotion_Piece_List) return Character is
  begin
    return Pieces.Promotion_Piece_List'Image(Kind)(1);
  end Image;

  use type Space.Col_Range;
  use type Pieces.Action_Kind_List;
begin
  case Action.To.Kind is
    when Pieces.Move =>
      Str(1 .. 5) := Image (Action.From) & "-" & Image (Action.To.Dest);
      First := 6;
    when Pieces.Take =>
      Str(1 .. 5) := Image (Action.From) & "X" & Image (Action.To.Dest);
      First := 6;
    when Pieces.Take_En_Passant =>
      Str(1 .. 7) := Image (Action.From) & "X" & Image (Action.To.Dest) & "ep";
      First := 8;
    when Pieces.Castle =>
      if Action.To.Rook_From.Col = Space.H then
        -- Small castle
        Str(1 .. 3) := "o-o";
        First := 4;
      else
        -- Big castle
        Str(1 .. 5) := "o-o-o";
        First := 6;
      end if;
    when Pieces.Promote =>
      Str(1 .. 7) := Image (Action.From) & "-" & Image (Action.To.Dest)
                   & "=" & Image(Action.To.New_Piece);
      First := 8;
    when Pieces.Take_And_Promote =>
      Str(1 .. 7) := Image (Action.From) & "X" & Image (Action.To.Dest)
                   & "=" & Image(Action.To.New_Piece);
      First := 8;
    when Pieces.Cover =>
      Str(1 .. 5) := "Cover";
      First := 6;
  end case;
  -- Check, checkmate, stalemate
  case Result is
    when Game.Nok =>
      Str (First .. First + 1) := "Nk";
    when Game.Ok =>
      null;
    when Game.Check =>
      Str (First .. First) := "+";
    when Game.Stalemate =>
      Str (First .. First + 1) := "==";
    when Game.CheckMate =>
      Str (First .. First + 1) := "++";
  end case;
  return Str;
end Image;
      


