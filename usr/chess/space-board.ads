with Pieces;

package Space.Board is

  -- Initialization of board
  procedure Init;

  -- Clean-up of board
  procedure Erase;

  -- Create a piece
  procedure Create_Piece (Kind      : in Pieces.Piece_Kind_List;
                          Color     : in Color_List;
                          Square    : in Square_Coordinate;
                          Has_Moved : in Boolean := False);

  -- Delete a piece (If not commited, reverse can be done by Restore_Piece)
  procedure Delete_Piece (Square : in Square_Coordinate;
                          Commit : Boolean);
  procedure Restore_Piece (Piece : in Pieces.Piece_Access);

  -- Move a piece (If not commited, reverse can be done by Undo_Move_Piece)
  procedure Move_Piece (From   : in Square_Coordinate;
                        To     : in Square_Coordinate;
                        Commit : in Boolean);
  procedure Undo_Move_Piece (From   : in Square_Coordinate;
                             To     : in Square_Coordinate);

  -- Null if empty
  function Piece_At (Square : Square_Coordinate) return Pieces.Piece_Access;

  -- Same but returns piece kind and color at normal board initialisation
  type Orig_Piece_Id (Valid : Boolean := False) is record
    case Valid is
      when False => null;
      when True  => Id : Pieces.Piece_Id;
    end case;
  end record;
  function Orig_Piece_Id_At (Square : Square_Coordinate)
           return Orig_Piece_Id;

  -- New_Pos is valid or not (if not, Piece is null)
  -- If Valid, Piece is null if square is empty
  procedure What_Is_At (Square : in Square_Coordinate;
                        Col_Offset, Row_Offset : in Movement_Range;
                        New_Pos : out Movement_Result;
                        Piece   : out Pieces.Piece_Access);
                     
end Space.Board;

