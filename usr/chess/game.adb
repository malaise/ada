with Pieces, Space.Board, Team, Screen;
package body Game is


  type History_Rec is record
    -- The action
    Action : Action_Rec;
    -- The taken piece
    Taken : Pieces.Piece_Access := null;
    -- The promoted piece
    Promoted : Pieces.Piece_Access := null;
  end record;
  History : History_Rec;


  procedure Init is
  begin
    Screen.Display_Board;
    -- Get Movements and attacked squares
    Players.Think (Space.White);
    Players.Think (Space.Black);
  end Init;


  -- Save and Try a move (no commit - must be undone)
  Procedure Try_Move (Color : in Space.Color_List; Action : in Action_Rec) is
    use type Pieces.Action_Kind_List;
    Take_Own : exception;
  begin
    -- Save the action
    History := (Action => Action, Taken  => null, Promoted => null);

    -- Delete taken piece (save taken piece)
    if Action.To.Kind = Pieces.Take
    or else Action.To.Kind = Pieces.Take_And_Promote then
      History.Taken := Space.Board.Piece_At (Action.To.Dest);
      Space.Board.Delete_Piece (Action.To.Dest, False);
    elsif Action.To.Kind = Pieces.Take_En_Passant then
      History.Taken := Space.Board.Piece_At (Action.To.Taking);
      Space.Board.Delete_Piece (Action.To.Taking, False);
    end if;

    -- Move piece
    Space.Board.Move_Piece (Action.From, Action.To.Dest, False);

    -- Promote (save promoting pawn)
    if Action.To.Kind = Pieces.Promote
    or else Action.To.Kind = Pieces.Take_And_Promote then
      History.Promoted := Space.Board.Piece_At (Action.To.Dest);
      Space.Board.Delete_Piece (Action.To.Dest, False);
      Space.Board.Create_Piece (Action.To.New_Piece, Color, Action.To.Dest);
    elsif Action.To.Kind = Pieces.Castle then
      -- Castle: move rook  
      Space.Board.Move_Piece (Action.To.Rook_From, Action.To.Rook_Dest, False);
    end if;

  end Try_Move;


  -- Undo a try
  procedure Undo_Try is
    use type Pieces.Action_Kind_List;
  begin
    -- Undo promotion
    if History.Action.To.Kind = Pieces.Promote
    or else History.Action.To.Kind = Pieces.take_And_Promote then
      Space.Board.Delete_Piece (History.Action.To.Dest, True);
      Space.Board.Restore_Piece (History.Promoted);
    end if;

    -- Move back
    Space.Board.Move_Piece (History.Action.To.Dest, History.Action.From, False);

    -- Recreate taken piece
    if History.Action.To.Kind = Pieces.Take
    or else History.Action.To.Kind = Pieces.Take_En_Passant
    or else History.Action.To.Kind = Pieces.Take_And_Promote then
      Space.Board.Restore_Piece (History.Taken);
    end if;

    -- Undo Rook movement on Caslte
    if History.Action.To.Kind = Pieces.Castle then
      Space.Board.Move_Piece (History.Action.To.Rook_Dest, History.Action.To.Rook_From, False);
    end if;

  end Undo_Try;


  -- Definitively commit a move (in board and screen)
  procedure Commit_Move (Color  : in Space.Color_List;
                         Action : in Action_Rec) is
    use type Pieces.Action_Kind_List;
    Take_Own : exception;
  begin
    -- Delete taken piece
    if Action.To.Kind = Pieces.Take
    or else Action.To.Kind = Pieces.Take_And_Promote then
      Space.Board.Delete_Piece (Action.To.Dest, True);
      Screen.Update_Board ( (1 => Action.To.Dest ) );
    elsif Action.To.Kind = Pieces.Take_En_Passant then
      Space.Board.Delete_Piece (Action.To.Taking, True);
      Screen.Update_Board ( (1 => Action.To.Taking ) );
    end if;

    -- Move piece
    Space.Board.Move_Piece (Action.From, Action.To.Dest, True);
    Screen.Update_Board ( (Action.From, Action.To.Dest) );

    -- Promote
    if Action.To.Kind = Pieces.Promote
    or else Action.To.Kind = Pieces.Take_And_Promote then
      Space.Board.Delete_Piece (Action.To.Dest, True);
      Space.Board.Create_Piece (Action.To.New_Piece, Color, Action.To.Dest);
      Screen.Update_Board ( (1 => Action.To.Dest) );
    elsif Action.To.Kind = Pieces.Castle then
      -- Castle: move rook  
      Space.Board.Move_Piece (Action.To.Rook_From, Action.To.Rook_Dest, True);
    end if;

  end Commit_Move;


   -- See if a king is in check
  function In_Check (Color : Space.Color_List) return Boolean is
    King, Tmp_Piece : Pieces.Piece_Access;
    King_Square : Space.Square_Coordinate;

    Opp_Color : constant Space.Color_List := Space.Opponent (Color);
    Opp_Action : Players.Action_Rec;

    use type Pieces.Piece_Kind_List, Pieces.Piece_Access, Pieces.Action_Kind_List;
    use type Space.Square_Coordinate;

    No_King : exception;
  begin

    -- Find King
    Team.Rewind (Color);
    King := null;
    loop
      Tmp_Piece := Team.Get (Color);
      if Pieces.Id_Of(Tmp_Piece.all).Kind = Pieces.King then
        King := Tmp_Piece;
        exit;
      end if;
    end loop;
    if King = null then
      raise No_King;
    end if;
    King_Square := Pieces.Pos_Of (King.all);

    -- Check King is not check
    players.Rewind_Actions (Opp_Color);
    loop
      Opp_Action := Players.Next_Action (Opp_Color);
      exit when not Opp_Action.Valid;
      if Opp_Action.To.Kind = Pieces.Take
      and then Opp_Action.To.Dest = King_Square then
        -- Opponent would take our king
        return True;
      end if;
    end loop;
    return False;
  end In_Check;


  -- Can a player move (alters opponent thinking)
  function Can_Move (Color : Space.Color_List) return Boolean is
    Action : Players.Action_Rec;
    Opp_Color : constant Space.Color_List := Space.Opponent (Color);
    use type Pieces.Action_Kind_List;
  begin
    Players.Rewind_Actions (Color);
    loop
      Action := Players.Next_Action (Color);
      exit when not Action.Valid;

      if Action.To.Kind /= Pieces.Cover then
        -- Try a movement
        Try_Move (Color, Action);
        Players.Think (Opp_Color);

        if not In_Check (Color) then
          -- This is at least a valid move
          Undo_Try;
          return True;
        end if;

        Undo_Try;
      end if;
    end loop;
    -- No moves found
    return False;
  end Can_Move;



  -- Check a move is valid (no King in chess), commit and refresh screen
  function Do_Move (Action : Action_Rec) return Move_Status_List is
    Color : constant Space.Color_List
          := Pieces.Id_Of (Space.Board.Piece_At (Action.From).all).Color;
    Opp_Color : constant Space.Color_List := Space.Opponent (Color);
    Opp_In_Check, Opp_Can_Move : Boolean;
  begin
    -- Push and verify our king is not in check
    Try_Move (Color, Action);
    Players.Think (Opp_Color);
    if In_Check (Color) then
      Undo_Try;
      return Nok;
    end if;
    Undo_Try;

    -- Comit on screen
    Commit_Move (Color, Action);

    -- Check opponent movements (alters our thinking)
    Opp_Can_Move := Can_Move (Opp_Color);

    -- Compute attacked squares
    Players.Think (Color);

    -- See id opponent King is in Check
    Opp_In_Check := In_Check (Opp_Color);

    -- Global status
    if Opp_Can_Move then
      if Opp_In_Check then
        return Check;
      else
        return Ok;
      end if;
    else 
      -- Cannot move
      if Opp_In_Check then
        return Checkmate;
      else
        return Stalemate;
      end if;
    end if;

  end Do_Move;


end game;

