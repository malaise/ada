with Space, Pieces, Game;
package Image is
  -- From the action and the result (Cr being column and row)
  -- Piece character (none for Pawn), or R, N, B, Q, K
  --  got from the To.Dest square of the action (except promotion)
  -- Then
  -- Move      -> Cr-Cr
  -- Take      -> CrXCr
  -- EnPassant -> CrXCrep
  -- Castle    -> o-o
  --           or o-o-o
  -- Promotion -> Cr-Cr=K    (K is one letter: kind of new piece)
  --           or CrXCr=K
  -- Plus extra:
  -- Check        +
  -- Checkmate    ++
  -- Stalemate    ==
  -- -- Which make 10 characters max
  subtype Move_Str is String (1 .. 10);
  function Move_Image (Action : Game.Valid_Action_Rec;
                       Result : Game.Move_Status_List) return Move_Str;

  procedure Move_Value (Str    : in Move_Str;
                        Color  : in Space.Color_List;
                        Action : out Game.Valid_Action_Rec;
                        Result : out Game.Move_Status_List);
  Value_Error : exception;

  -- Piece character (none for Pawn), or R, N, B, Q, K
  function Piece_Image (Piece_Kind : in Pieces.Piece_Kind_List) return String;

  -- Return Pawn if not found
  function Piece_Value (Char : in Character) return Pieces.Piece_Kind_List;

  -- Decode square, may raise Value_Error
  subtype Square_Str is String (1 ..2);
  function Square_Value (S : Square_Str) return Space.Square_Coordinate;

end Image;

