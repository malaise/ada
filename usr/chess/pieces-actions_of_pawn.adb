separate(Pieces)
procedure Actions_Of_Pawn (Piece : in Pawn_Piece) is
  Row_Offset : Space.Movement_Range;
  Id : constant Piece_Id := Id_Of (Piece);
  New_Pos, Tmp_Pos : Space.Movement_Result;
  Dest_Piece : Pieces.Piece_Access;

  use type Space.Color_List, Space.Movement_Range;
  Col_Offsets : constant array (1 .. 2) of Space.Movement_Range
              := (-1, 1);
  Pep_Error : exception;
begin

  -- In which way are we progressing
  if Id.Color = Space.White then
    Row_Offset := +1;
  else
    Row_Offset := -1;
  end if;

  -- Progression by one
  Space.Board.What_Is_At (Piece.Square, 0, Row_Offset, New_Pos, Dest_Piece);
  if New_Pos.Valid and then Dest_Piece = null then
    -- Free
    Insert_Move (New_Pos.Square);
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
  for Col in Col_Offsets'Range loop
    Space.Board.What_Is_At (Piece.Square, Col_Offsets(Col), Row_Offset,
                            New_Pos, Dest_Piece);
    if New_Pos.Valid
    and then Dest_Piece /= null then
      if Dest_Piece.Color /= Piece.Color then
        Insert_Take (New_Pos.Square);
      else
        Insert_Cover (New_Pos.Square);
      end if;
    end if;
  end loop;

  -- Take "en passant"
  for Col in Col_Offsets'Range loop
    -- Is there a opposing pawn behind me which has just moved by two
    Space.Board.What_Is_At (Piece.Square, Col_Offsets(Col), 0,
                            Tmp_Pos, Dest_Piece);
    if Tmp_Pos.Valid
    and then Dest_Piece /= null
    and then Dest_Piece.Kind = Pawn
    and then Dest_Piece.Color /= Piece.Color
    and then Pawn_Moved_Two (Dest_Piece.Color) = Dest_Piece then
      -- I move by one row and one col, taking the pawn
      Space.Board.What_Is_At (Piece.Square, Col_Offsets(Col), Row_Offset,
                              New_Pos, Dest_Piece);
      if Dest_Piece /= null then
        raise Pep_Error;
      end if;
      Action_List_Mng.Insert(Action_List,
            (Kind => Take_En_Passant,
             Dest => New_Pos.Square,
             Taking => Tmp_Pos.Square) );
    end if;
  end loop;

end Actions_Of_Pawn;

