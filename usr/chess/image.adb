with Normal, Lower_Str, Text_Handler;
with Space.Board;

package body Image is

  function Piece_Image (Piece_Kind : Pieces.Piece_Kind_List) return String is
    use type Pieces.Piece_Kind_List;
  begin
    if Piece_Kind = Pieces.Knight then
      return "N";
    elsif Piece_Kind = Pieces.Pawn then
      return "";
    else
      return Pieces.Promotion_Piece_List'Image(Piece_Kind)(1 .. 1);
    end if;
  end Piece_Image;

  function Move_Image (Action : Game.Action_Rec;
                       Result : Game.Move_Status_List) return Move_Str is
    -- Acting piece
    Piece : constant Pieces.Piece_Kind_List
          := Pieces.Id_Of (Space.Board.Piece_At (Action.To.Dest).all).Kind;
    -- Result
    Res : Text_Handler.Text(10);
    Str : Move_Str := (others => ' ');

    function Image (Square : in Space.Square_Coordinate) return  String is
    begin
      return Lower_Str (Space.Col_Range'Image(Square.Col))
                     & Normal(Integer(Square.Row), 1);
    end Image;


    use type Space.Col_Range;
    use type Pieces.Action_Kind_List;
  begin
    Text_Handler.Set (Res, Piece_Image (Piece));
    case Action.To.Kind is
      when Pieces.Move =>
        Text_Handler.Append (Res, Image (Action.From) & "-" & Image (Action.To.Dest) );
      when Pieces.Take =>
        Text_Handler.Append (Res, Image (Action.From) & "X" & Image (Action.To.Dest) );
      when Pieces.Take_En_Passant =>
        Text_Handler.Append (Res, Image (Action.From) & "X" & Image (Action.To.Dest) & "ep");
      when Pieces.Castle =>
        -- Overwrite piece
        if Action.To.Rook_From.Col = Space.H then
          -- Small castle
          Text_Handler.Set (Res, "o-o");
        else
          -- Big castle
           Text_Handler.Set (Res, "o-o-o");
        end if;
      when Pieces.Promote =>
        -- Overwrite piece
        Text_Handler.Set (Res, Image (Action.From) & "-" & Image (Action.To.Dest)
                               & "=" & Piece_Image (Action.To.New_Piece) );
      when Pieces.Take_And_Promote =>
        -- Overwrite piece
        Text_Handler.Set (Res, Image (Action.From) & "X" & Image (Action.To.Dest)
                          & "=" & Piece_Image(Action.To.New_Piece) );
      when Pieces.Cover =>
        Text_Handler.Set (Res, "Cover");
    end case;
    -- Check, checkmate, stalemate
    case Result is
      when Game.Nok =>
        Text_Handler.Append (Res, "Nk");
      when Game.Ok =>
        null;
      when Game.Check =>
        Text_Handler.Append (Res, "+");
      when Game.Stalemate =>
        Text_Handler.Append (Res, "==");
      when Game.CheckMate =>
        Text_Handler.Append (Res, "++");
    end case;
    Str (1 .. Text_Handler.Length(Res)) := Text_Handler.Value (Res);
    return Str;
  end Move_Image;
        
end Image;
