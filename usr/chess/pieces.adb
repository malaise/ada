with Ada.Tags, Ada.Unchecked_Deallocation;
with Dynamic_List;
with Space.Board;
package body Pieces is

  -- Has a pawn moved by two just before
  Pawn_Moved_Two : array (Space.Color_List) of Piece_Access
                 := (others => null);
  Saved_Pawn_Moved_Two : array (Space.Color_List) of Piece_Access
                       := (others => null);

  -- List of valid moves
  package Action_List_Mng is new Dynamic_List(Action_Rec);
  Action_List : Action_List_Mng.List_Type;
  Not_Empty_List_Error : exception;

  -- Insert a move, a take, a cover
  procedure Insert_Move (To : in Space.Square_Coordinate) is
  begin
    Action_List_Mng.Insert(Action_List, (Kind => Move, Dest => To));
  end Insert_Move;
  procedure Insert_Take (To : in Space.Square_Coordinate) is
  begin
    Action_List_Mng.Insert(Action_List, (Kind => Take, Dest => To) );
  end Insert_Take;
  procedure Insert_Cover (To : in Space.Square_Coordinate) is
  begin
    Action_List_Mng.Insert(Action_List, (Kind => Cover, Dest => To) );
  end Insert_Cover;

  -- Delete current action
  procedure Delete_Action is
    Done : Boolean;
  begin
    Action_List_Mng.Delete(Action_List, Done => Done);
  end Delete_Action;

  -- Create e new piece
  function Create (Kind      : Piece_Kind_List;
                   Color     : Space.Color_List;
                   Square    : Space.Square_Coordinate;
                   Has_Moved : Boolean := False)
           return Piece_Access is
    Pa : Piece_Access;
  begin
    case Kind is
      when Pawn =>
        Pa := new Pawn_Piece;
      when Rook =>
        Pa := new Rook_Piece;
      when Knight =>
        Pa := new Knight_Piece;
      when Bishop=> 
        Pa := new Bishop_Piece;
      when Queen => 
        Pa := new Queen_Piece;
      when King => 
        Pa := new King_Piece;
    end case;
    Pa.Square := Square;
    Pa.Kind := Kind;
    Pa.Color := Color;
    Pa.Has_Moved := Has_Moved;
    return Pa;
  end Create;

  procedure Check_Is_Not_Basic (Piece : in Basic_Piece'Class) is
    use type Ada.Tags.Tag;
  begin
    if Piece'Tag = Basic_Piece'Tag then
      raise Basic_Piece_Error;
    end if;
  end Check_Is_Not_Basic;

  procedure Delete (Piece : in out Piece_Access) is
    procedure Delete_Piece is new Ada.Unchecked_Deallocation(Basic_Piece'Class, Piece_Access);
  begin
    Check_Is_Not_Basic(Piece.all);
    if Pawn_Moved_Two(Piece.Color) = Piece then
      Pawn_Moved_Two(Piece.Color) := null;
    end if;
    Delete_Piece(Piece);
    Piece := null;
  end Delete;

  function Id_Of (Piece : Basic_Piece'Class) return Piece_Id is
  begin
    Check_Is_Not_Basic(Piece);
    return (Piece.Kind, Piece.Color);
  end Id_Of;

  function Pos_Of (Piece : Basic_Piece'Class)
                  return Space.Square_Coordinate is
  begin
    return Piece.Square;
  end Pos_Of;

  -- Move a piece
  function "-" (From, To : Space.Row_Range) return Natural is
  begin
    return abs Integer(Space.Row_Range'Pos(From) - Space.Row_Range'Pos(To) );
  end "-";

  procedure Move (Piece  : in Piece_Access;
                  To     : in Space.Square_Coordinate;
                  Commit : in Boolean) is
  begin
    Check_Is_Not_Basic(Piece.all);
    if Commit then
      Pawn_Moved_Two(Piece.Color) := null;
      if Piece.Kind = Pawn
      and then Piece.Square.Row - To.Row = 2 then
        Pawn_Moved_Two(Piece.Color) := Piece;
      end if;
      Piece.Has_Moved := True;
    else
      Saved_Pawn_Moved_Two(Piece.Color) := Pawn_Moved_Two(Piece.Color);
      Pawn_Moved_Two(Piece.Color) := null;
      if Piece.Kind = Pawn
      and then Piece.Square.Row - To.Row = 2 then
        Pawn_Moved_Two(Piece.Color) := Piece;
      end if;
    end if;
    Piece.Square := To;
  end Move;

  -- Undo a temporary move
  procedure Undo_Move (Piece  : in Piece_Access;
                       To     : in Space.Square_Coordinate) is
  begin
    Check_Is_Not_Basic(Piece.all);
    Pawn_Moved_Two(Piece.Color) := Saved_Pawn_Moved_Two(Piece.Color);
    Piece.Square := To;
  end Undo_Move;

  -- Insert movement if destination square is valid
  -- Return True if Square is valid and empty
  function Add_Action (Piece : Basic_Piece'Class;
                       Row_Offset, Col_Offset : Space.Movement_Range)
                      return Boolean is
    Tmp_Mov : Space.Movement_Result;
    Tmp_Piece : Piece_Access;

    use type Space.Color_List;
  begin
    Check_Is_Not_Basic(Piece);
    -- Get destination content
    Space.Board.What_Is_At (Piece.Square, Col_Offset, Row_Offset,
                            Tmp_Mov, Tmp_Piece);
    if not Tmp_Mov.Valid then
      return False;
    end if;

    if Tmp_Piece = null then
      -- Free
      Insert_Move (Tmp_Mov.Square);
      return True;
    else
      if Tmp_Piece.Color /= Piece.Color then
        -- Take
        Insert_Take (Tmp_Mov.Square);
      else
        -- Cover own piece
        Insert_Cover (Tmp_Mov.Square);
      end if;
      return False;
    end if;
  end Add_Action;

  procedure Actions_Of (Piece : Basic_Piece) is
  begin
    raise Basic_Piece_Error;
  end Actions_Of;

  procedure Actions_Of_Pawn (Piece : Pawn_Piece) is separate;
  procedure Actions_Of (Piece : Pawn_Piece) renames Actions_Of_Pawn;

  procedure Actions_Of_Rook (Piece : Rook_Piece) is separate;
  procedure Actions_Of (Piece : Rook_Piece)  renames Actions_Of_Rook;

  procedure Actions_Of_Knight (Piece : Knight_Piece) is separate;
  procedure Actions_Of (Piece : Knight_Piece) renames Actions_Of_Knight;

  procedure Actions_Of_Bishop (Piece : Bishop_Piece) is separate;
  procedure Actions_Of (Piece : Bishop_Piece) renames Actions_Of_Bishop;

  procedure Actions_Of_Queen (Piece : Queen_Piece) is separate;
  procedure Actions_Of (Piece : Queen_Piece) renames Actions_Of_Queen;

  procedure Actions_Of_King (Piece : King_Piece) is separate;
  procedure Actions_Of (Piece : King_Piece) renames Actions_Of_King;

  -- Build the array of moves from the list
  function Build_Array_From_List return Action_Array is
  begin
    -- Build returned value
    declare
      Result : Action_Array(1 ..  Action_List_Mng.List_Length(Action_List));
    begin
      -- Scan list if not empty
      if not Action_List_Mng.Is_Empty(Action_List) then
        Action_List_Mng.Rewind (Action_List);
        for I in Result'Range loop
           Action_List_Mng.Get (Action_List, Result(I));
        end loop;
      end if;
      return Result;
    end;
  end Build_Array_From_List;

  -- List of possible moves of a piece
  function Actions_Of (Piece : Basic_Piece'Class) return Action_Array is
  begin
    -- List must be empty
    if not Action_List_Mng.Is_Empty (Action_List) then
      raise Not_Empty_List_Error;
    end if;
    -- Dispatch
    Actions_Of (Piece);
    return Build_Array_From_List;
  end Actions_Of;

end Pieces;

