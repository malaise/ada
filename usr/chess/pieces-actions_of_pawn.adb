separate(Pieces)
procedure Actions_Of_Pawn (Piece : in Pawn_Piece) is
  Row_Offset : Space.Movement_Range;
  Id : constant Piece_Id := Id_Of (Piece);
  New_Pos, Tmp_Pos : Space.Movement_Result;
  Dest_Piece : Pieces.Piece_Access;

  use type Space.Color_List, Space.Movement_Range;
  Col_Offsets : constant array (1 .. 2) of Space.Movement_Range
              := (-1, 1);
  Promotion : Boolean;
  Pep_Error : exception;

  subtype Promotion_Kind_List is Action_Kind_List range Promote .. Take_And_Promote;
  procedure Insert_Promotions (Kind : Promotion_Kind_List;
                               Dest : Space.Square_Coordinate) is
    Action : Action_Rec;
  begin
    if Kind = Promote then
      Action := (Promote, Dest, Promotion_Piece_List'First);
    else
      Action := (Take_And_Promote, Dest, Promotion_Piece_List'First);
    end if;
    for Piece_Kind in Promotion_Piece_List loop
      Action.New_Piece := Piece_Kind;
      Action_List.Insert(Action);
    end loop;
  end Insert_Promotions;

begin

  -- In which way are we progressing
  if Id.Color = Space.White then
    Row_Offset := +1;
  else
    Row_Offset := -1;
  end if;

  -- Promotion (by move or take)
  -- End of board?
  Promotion := not Space.Compute_Movement (Piece.Square, 0, Row_Offset *2).Valid;

  -- Progression by one
  Space.Board.What_Is_At (Piece.Square, 0, Row_Offset, New_Pos, Dest_Piece);
  if New_Pos.Valid and then Dest_Piece = null then
    -- Free
    if not Promotion then
      Insert_Move (New_Pos.Square);
    else
      Insert_Promotions (Promote, New_Pos.Square);
    end if;
  end if;

  -- Progression by two
  if not Piece.Has_Moved then
    Space.Board.What_Is_At (Piece.Square, 0, Row_Offset * 2, New_Pos, Dest_Piece);
    if New_Pos.Valid and then Dest_Piece = null then
      -- Free
      Insert_Move (New_Pos.Square);
    end if;
  end if;

  -- Take by one
  for Col of Col_Offsets loop
    Space.Board.What_Is_At (Piece.Square, Col, Row_Offset, New_Pos, Dest_Piece);
    if New_Pos.Valid
    and then Dest_Piece /= null then
      if Dest_Piece.Color /= Piece.Color then
        if not Promotion then
          Insert_Take (New_Pos.Square);
        else
          Insert_Promotions (Take_And_Promote, New_Pos.Square);
        end if;
      else
        Insert_Cover (New_Pos.Square);
      end if;
    end if;
  end loop;

  -- Take "en passant"
  for Col of Col_Offsets loop
    -- Is there a opposing pawn behind me which has just moved by two
    Space.Board.What_Is_At (Piece.Square, Col, 0, Tmp_Pos, Dest_Piece);
    if Tmp_Pos.Valid
    and then Dest_Piece /= null
    and then Dest_Piece.Kind = Pawn
    and then Dest_Piece.Color /= Piece.Color
    and then Pawn_Moved_Two (Dest_Piece.Color) = Dest_Piece then
      -- I move by one row and one col, taking the pawn
      Space.Board.What_Is_At (Piece.Square, Col, Row_Offset, New_Pos,
                              Dest_Piece);
      if Dest_Piece /= null then
        raise Pep_Error;
      end if;
      Action_List.Insert(
            (Kind => Take_En_Passant,
             Dest => New_Pos.Square,
             Taking => Tmp_Pos.Square) );
    end if;
  end loop;

end Actions_Of_Pawn;

