with Team;
package body Space.Board is

  function Is_Empty (Square : in Square_Coordinate) return Boolean is
    use type Pieces.Piece_Access;
  begin
    return The_Board(Square.Col, Square.Row) /= null;
  end Is_Empty;

  procedure Create_Piece (Kind   : in Pieces.Piece_Kind_List;
                          Color  : in Color_List;
                          Square : in Square_Coordinate) is
  begin
    The_Board(Square.Col, Square.Row) :=  Pieces.Create(Kind, Color, Square);
    Team.Add (The_Board(Square.Col, Square.Row));
  end Create_Piece;

  procedure Delete_Piece (Square : in Square_Coordinate;
                          Commit : in Boolean) is
  begin
    Team.Del (The_Board(Square.Col, Square.Row));
    if Commit then
      Pieces.Delete (The_Board(Square.Col, Square.Row));
    end if;
    The_Board(Square.Col, Square.Row) := null;
  end Delete_Piece;

  procedure Restore_Piece (Piece : in Pieces.Piece_access) is
    Pos : constant Square_Coordinate := Pieces.Pos_Of (Piece.all);
  begin
    The_Board(Pos.Col, Pos.Row) :=  Piece;
    Team.Add (Piece);
  end Restore_Piece;

  procedure Move_Piece (From   : in Square_Coordinate;
                        To     : in Square_Coordinate;
                        Commit : in Boolean) is
    use type Pieces.Action_Kind_List;
  begin
    The_Board(To.Col, To.Row) := The_Board(From.Col, From.Row);
    The_Board(From.Col, From.Row) := null;
    Pieces.Move (The_Board(To.Col, To.Row), To, Commit);
  end Move_Piece;


  -- Initialization of board
  procedure Init is
  begin
    Create_Piece (Pieces.Rook,   White, (A, 1) );
    Create_Piece (Pieces.Knight, White, (B, 1) );
    Create_Piece (Pieces.Bishop, White, (C, 1) );
    Create_Piece (Pieces.Queen,  White, (D, 1) );
    Create_Piece (Pieces.King,   White, (E, 1) );
    Create_Piece (Pieces.Bishop, White, (F, 1) );
    Create_Piece (Pieces.Knight, White, (G, 1) );
    Create_Piece (Pieces.Rook,   White, (H, 1) );
    For Col in Col_Range loop
      Create_Piece (Pieces.Pawn, White, (Col, 2) );
    end loop;

    Create_Piece (Pieces.Rook,   Black, (A, 8) );
    Create_Piece (Pieces.Knight, Black, (B, 8) );
    Create_Piece (Pieces.Bishop, Black, (C, 8) );
    Create_Piece (Pieces.Queen,  Black, (D, 8) );
    Create_Piece (Pieces.King,   Black, (E, 8) );
    Create_Piece (Pieces.Bishop, Black, (F, 8) );
    Create_Piece (Pieces.Knight, Black, (G, 8) );
    Create_Piece (Pieces.Rook,   Black, (H, 8) );
    For Col in Col_Range loop
      Create_Piece (Pieces.Pawn, Black, (Col, 7) );
    end loop;
  end Init;

  -- Clean-up of board
  procedure Erase is
  begin
    for Row in Row_Range loop
      for Col in Col_Range loop
        if not Is_Empty ((Col, Row)) then
          Delete_Piece ((Col, Row), True);
        end if;
      end loop;
    end loop;
  end Erase;

  -- Null if empty
  function Piece_At (Square : Square_Coordinate) return Pieces.Piece_Access is
  begin
    return The_Board(Square.Col, Square.Row);
  end Piece_At;

  procedure What_Is_At (Square : in Square_Coordinate;
                        Col_Offset, Row_Offset : in Movement_Range;
                        New_Pos : out Movement_Result;
                        Piece   : out Pieces.Piece_Access) is
  begin
    Piece := null;
    New_Pos := Compute_Movement(Square, Col_Offset, Row_Offset);
    if not New_Pos.Valid then
      return;
    else
      Piece := Piece_At(New_Pos.Square);
    end if;
  end What_Is_At;

end Space.Board;

