with Players;
separate(Pieces)
procedure Actions_Of_King (Piece : in King_Piece) is

  use type Space.Movement_Range;

  procedure Check_Castle (Small : in Boolean) is
    -- King Way
    Rook_Col : constant array (Boolean) of Space.Col_Range
             := (True => Space.H, False => Space.A);
    King_Direction : constant array (Boolean) of Space.Movement_Range
                 := (True => 1, False => -1);
    King_Way  : Action_List_Mng.List_Type;
    King_Pos, Rook_Pos : Space.Movement_Result;
    Tmp_Piece : Piece_Access;
    Square_Found : Boolean;
    Castle_Error : exception;

    -- Opponent controls
    Opp_Color : constant Space.Color_List := Space.Opponent (Piece.Color);
    Opp_Action : Players.Action_Rec;

    -- Matching square of king's way
    function Same_Square (King, Ref : Action_Rec) return Boolean is
      use type Space.Square_Coordinate;
    begin
      return King.Dest = Ref.Dest;
    end Same_Square;
    procedure Find_Square is new Action_List_Mng.Search (Same_Square);

    use type Space.Color_List;

  begin
    -- Rook must not have moved
    Tmp_Piece := Space.Board.Piece_At ((Rook_Col(Small), Piece.Square.Row));
    if Tmp_Piece = null
    or else Tmp_Piece.Kind /= Rook
    or else Tmp_Piece.Color /= Piece.Color
    or else Tmp_Piece.Has_Moved then
      return;
    end if;

    -- Check Rook way (up to king, reverse way) is free
    -- Save final rook pos
    declare
      Col : Space.Movement_Range;
      Tmp_Pos : Space.Movement_Result;
      use type Space.Square_Coordinate;
    begin
      Col := 1;
      loop
        Space.Board.What_Is_At (
            (Rook_Col(Small), Piece.Square.Row),
            King_Direction(not Small) * Col, 0,
            Tmp_Pos, Tmp_Piece);
        if not Tmp_Pos.Valid then
          raise Castle_Error;
        end if;
        exit when Tmp_Pos.Square = Piece.Square;
        if Tmp_Piece /= null then
          return;
        end if;
        Rook_Pos := Tmp_Pos;
        Col := Col + 1;
      end loop;
    end;

    -- Build King way
    -- Save final king pos
    for Col in Space.Movement_Range'(1) .. 2 loop
      Space.Board.What_Is_At (
          Piece.Square,
          King_Direction(Small) * Col, 0,
          King_Pos, Tmp_Piece);
      Action_List_Mng.Insert (King_Way, (Kind => Move,
                                         Dest => King_Pos.Square));
    end loop;

    -- King must not be attacked nor on its way
    Action_List_Mng.Insert (King_Way, (Kind => Move, Dest => Piece.Square));
    Players.Rewind_Actions (Opp_Color);
    Opponent_Actions:
    loop
      Opp_Action := Players.Next_Action (Opp_Color);
      exit Opponent_Actions when not Opp_Action.Valid;

      if Opp_Action.To.Kind = Move then
        Find_Square (King_Way, Square_Found, Opp_Action.To,
                     From => Action_List_Mng.Absolute);
        if Square_Found then
          -- We are under attack on our way (or current pos)
          Action_List_Mng.Delete_List (King_Way);
          return;
        end if;
      end if;
    end loop Opponent_Actions;

    -- Ok
    Action_List_Mng.Delete_List (King_Way);
    Action_List_Mng.Insert(Action_List,
            (Kind => Castle,
             Dest => King_Pos.Square,
             Rook_From => (Rook_Col(Small), Piece.Square.Row),
             Rook_Dest => Rook_Pos.Square));


  end Check_Castle;

begin

  -- Insert physical movements
  declare
    Dummy : Boolean;
  begin
    for Col in Space.Movement_Range'(-1) .. 1 loop
      for Row in Space.Movement_Range'(-1) .. 1 loop
        if abs Row + abs Col /= 0 then 
          -- Not myself
          Dummy := Add_Action (Piece, Col, Row);
        end if;
      end loop;
    end loop;
  end;

  -- Check castling
  if not Piece.Has_Moved then
    Check_Castle (True);
    Check_Castle (False);
  end if;

end Actions_Of_King;

