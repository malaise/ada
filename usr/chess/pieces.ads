with Space;
package Pieces is

  type Basic_Piece is tagged private;
  type Piece_Access is access all Basic_Piece'Class;
  -- List of possible actions of a piece
  Procedure Actions_Of (Piece :  Basic_Piece);

  type Piece_Kind_List is (Pawn, Rook, Knight, Bishop, Queen, King);

  type Pawn_Piece   is new Basic_Piece with private;
  procedure Actions_Of (Piece : Pawn_Piece);

  type Rook_Piece   is new Basic_Piece with private;
  procedure Actions_Of (Piece : Rook_Piece);

  type Knight_Piece is new Basic_Piece with private;
  procedure Actions_Of (Piece : Knight_Piece);

  type Bishop_Piece is new Basic_Piece with private;
  procedure Actions_Of (Piece : Bishop_Piece);

  type Queen_Piece  is new Basic_Piece with private;
  procedure Actions_Of (Piece : Queen_Piece);

  type King_Piece   is new Basic_Piece with private;
  procedure Actions_Of (Piece : King_Piece);

  -- Create a new piece
  function Create (Kind   : Piece_Kind_List;
                   Color  : Space.Color_List;
                   Square : Space.Square_Coordinate)
           return Piece_Access;

  procedure Delete (Piece : in out Piece_Access);

  -- Piece exported characteristics
  type Piece_Id is record
    Kind : Piece_Kind_List;
    Color : Space.Color_List;
  end record;
  function Id_Of (Piece : Basic_Piece'Class) return Piece_Id;
  function Pos_Of (Piece : Basic_Piece'Class)
                  return Space.Square_Coordinate;

  -- Move a piece (temporary for thinking, or definitively)
  procedure Move (Piece  : in Piece_Access;
                  To     : in Space.Square_Coordinate;
                  Commit : in Boolean);
  -- Undo a temporary move
  procedure Undo_Move (Piece  : in Piece_Access;
                       To     : in Space.Square_Coordinate);

  -- Influence of a piece:
  -- Can move or take, or covers a piece of same field
  type Action_Kind_List is (Move, Castle, Take, Take_En_Passant,
                            Promote, Take_And_Promote, Cover);
  subtype Promotion_Piece_List is Piece_Kind_List range Rook .. Queen;
  type Action_Rec (Kind : Action_Kind_List := Move) is record
    Dest : Space.Square_Coordinate := Space.Origin;
    case Kind is
      when Move | Take | Cover => null;
      when Take_En_Passant =>
        Taking : Space.Square_Coordinate;
      when Castle =>
        Rook_From, Rook_Dest : Space.Square_Coordinate;
      when Promote | Take_And_Promote =>
        New_Piece : Promotion_Piece_List;
    end case;
  end record;
  type Action_Array is array (Positive range <>) of Action_Rec;
  function Actions_Of (Piece :  Basic_Piece'Class) return Action_Array;

  -- No operation allowed on Basic_Piece
  Basic_Piece_Error : exception;

private

  type Basic_Piece is tagged record
    Square : Space.Square_Coordinate;
    Kind   : Piece_Kind_List;
    Color  : Space.Color_List;
    Has_Moved : Boolean;
  end record;

  type Pawn_Piece   is new Basic_Piece with null record;
  type Rook_Piece   is new Basic_Piece with null record;
  type Knight_Piece is new Basic_Piece with null record;
  type Bishop_Piece is new Basic_Piece with null record;
  type Queen_Piece  is new Basic_Piece with null record;
  type King_Piece   is new Basic_Piece with null record;

end Pieces;

