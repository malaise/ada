with Ada.Text_Io;
With Normal, Lower_Str;

with Pieces, Space.Board, Players, Screen, Game;

procedure T1 is

  Action : Players.Action_Rec;
  Color : Space.Color_List;

  procedure Put (Square : in Space.Square_Coordinate) is
  begin
    Ada.Text_Io.Put (Lower_Str(Space.Col_Range'Image(Square.Col))
                   & Normal(Integer(Square.Row), 1) );
  end Put;

  procedure Put (Piece : in Pieces.Basic_Piece'Class) is
    Id : constant Pieces.Piece_Id := Pieces.Id_Of (Piece);
    use Ada.Text_Io;
  begin
    Put (Space.Color_List'Image(Id.Color)
         & " " & Pieces.Piece_Kind_List'Image(Id.Kind));
  end Put;

  procedure Put (Action : in Pieces.Action_Rec) is
    use Ada.Text_Io;
    use type Space.Square_Coordinate, Pieces.Action_Kind_List;
  begin
    case Action.Kind is
      when Pieces.Move =>
        Put ("Move to ");
        Put (Action.Dest);
      when Pieces.Take =>
        Put ("Take at ");
        Put (Action.Dest);
      when Pieces.Cover =>
        Put ("Cover ");
        Put (Action.Dest);
      when Pieces.Take_En_Passant =>
        Put ("Take en passant at ");
        Put (Action.Dest);
      when Pieces.Castle =>
        Put ("Castle with ");
        Put (Action.Rook_From);
      when Pieces.Promote =>
        Put ("Move to ");
        Put (Action.Dest);
        Put (" Promoting to ");
        Put (Pieces.Piece_Kind_List'Image(Action.New_Piece));
      when Pieces.Take_And_Promote =>
        Put ("Take at ");
        Put (Action.Dest);
        Put (" Promoting to ");
        Put (Pieces.Piece_Kind_List'Image(Action.New_Piece));
    end case;
  end Put;

begin
  -- Init game: Set board, think and display
  Space.Board.Init;
  Game.Init;

  Color := Space.White;

  One_Game:
  loop


    -- Dump Actions
    Players.Rewind_Actions (Color);
    loop
      Action := Players.Next_Action (Color);
      exit when not Action.Valid;
      put (Space.Board.Piece_At(Action.From).all);
      Ada.Text_Io.Put (' ');
      put (Action.From);
      Ada.Text_Io.Put (' ');
      put (Action.To);
      Ada.Text_Io.New_Line;
    end loop;
    Ada.Text_Io.New_Line;

    Get_One:
    loop
      declare
        Action : Players.Action_Rec;
        Result : Game.Move_Status_List;
        use type Game.Move_Status_List;
      begin
        Action := Screen.Get(Color);
        -- User exit?
        exit One_Game when not Action.Valid;
        Result := Game.Do_Move (Action);
        case Result is
          when Game.Nok =>
            --  Our king would be in check
            null;
          when Game.Ok =>
            exit Get_One;
          when Game.Check =>
            Ada.Text_Io.Put_Line("Check!");
            exit Get_One;
          when Game.Stalemate =>
            Ada.Text_Io.Put_Line("Stalemate???");
            exit One_Game;
          when Game.Checkmate =>
            Ada.Text_Io.Put_Line("Checkmate!!!");
            exit One_Game;
        end case;
      end;
    end loop Get_One;

    -- New player
    Color := Space.Opponent (Color);
  end loop One_Game;

  Screen.Close;

end T1;

