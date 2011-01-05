with As.B; use As.B;
with Normal, Lower_Str;
package body Image is

  function Piece_Image (Piece_Kind : Pieces.Piece_Kind_List) return String is
    use type Pieces.Piece_Kind_List;
  begin
    if Piece_Kind = Pieces.Knight then
      return "N";
    elsif Piece_Kind = Pieces.Pawn then
      return "";
    else
      return Pieces.Promotion_Piece_List'Image(Piece_Kind)(1 .. 1);
    end if;
  end Piece_Image;

  -- Return Pawn if not found
  function Piece_Value (Char : in Character) return Pieces.Piece_Kind_List is
    use type Pieces.Piece_Kind_List;
  begin
    for Piece in Pieces.Piece_Kind_List loop
      if Piece /= Pieces.Pawn and then Char = Piece_Image (Piece)(1) then
        return Piece;
      end if;
    end loop;
    return Pieces.Pawn;
  end Piece_Value;

  function Move_Image (Action : Game.Valid_Action_Rec;
                       Result : Game.Move_Status_List) return Move_Str is
    -- Result
    Res : Asb_Bs(10);
    Str : Move_Str := (others => ' ');

    function Image (Square : in Space.Square_Coordinate) return String is
    begin
      return Lower_Str (Space.Col_Range'Image(Square.Col))
                     & Normal(Integer(Square.Row), 1);
    end Image;


    use type Space.Col_Range;
    use type Pieces.Action_Kind_List;
  begin
    Res.Set (Piece_Image (Action.Piece));
    case Action.To.Kind is
      when Pieces.Move =>
        Res.Append (Image (Action.From) & "-" & Image (Action.To.Dest) );
      when Pieces.Take =>
        Res.Append (Image (Action.From) & "X" & Image (Action.To.Dest) );
      when Pieces.Take_En_Passant =>
        Res.Append (Image (Action.From) & "X" & Image (Action.To.Dest) & "ep");
      when Pieces.Castle =>
        -- Overwrite piece
        if Action.To.Rook_From.Col = Space.H then
          -- Small castle
          Res.Set ("o-o");
        else
          -- Big castle
           Res.Set ("o-o-o");
        end if;
      when Pieces.Promote =>
        -- Overwrite piece
        Res.Set (Image (Action.From) & "-" & Image (Action.To.Dest)
                               & "=" & Piece_Image (Action.To.New_Piece) );
      when Pieces.Take_And_Promote =>
        -- Overwrite piece
        Res.Set (Image (Action.From) & "X" & Image (Action.To.Dest)
                          & "=" & Piece_Image(Action.To.New_Piece) );
      when Pieces.Cover =>
        Res.Set ("Cover");
    end case;
    -- Check, checkmate, stalemate
    case Result is
      when Game.Nok =>
        Res.Append ("Nk");
      when Game.Ok =>
        null;
      when Game.Check =>
        Res.Append ("+");
      when Game.Stalemate =>
        Res.Append ("==");
      when Game.Checkmate =>
        Res.Append ("+=");
    end case;
    Str (1 .. Res.Length) := Res.Image;
    return Str;
  end Move_Image;

  -- Decode square
  function Square_Value (S : Square_Str) return Space.Square_Coordinate is
  begin
    if S(1) not in 'a' .. 'h' then
      raise Value_Error;
    end if;
    return (Space.Col_Range'Value(S(1..1)), Space.Row_Range'Value(S(2..2)));
  exception
    when others =>
      raise Value_Error;
  end Square_Value;

  procedure Move_Value (Str    : in Move_Str;
                        Color  : in Space.Color_List;
                        Action : out Game.Valid_Action_Rec;
                        Result : out Game.Move_Status_List) is
    -- Next character to parse
    Next : Positive;

    -- Take/Move and associated squares
    Take : Boolean;
    From, Dest : Space.Square_Coordinate;
    Piece_Kind : Pieces.Piece_Kind_List;

    use type Space.Color_List;
    use type Pieces.Piece_Kind_List;
  begin
    -- Spaces (if any) are only at the end
    declare
      Space_Found : Boolean := False;
    begin
      for I in Str'Range loop
        if Space_Found and then Str(I) /= ' ' then
          raise Value_Error;
        elsif not Space_Found and then Str(I) = ' ' then
          Space_Found := True;
        end if;
      end loop;
    end;

    -- Detect castle
    if Str(1 .. 5) = "o-o-o" then
      if Color = Space.White then
        Action := (Valid => True,
                   Piece => Pieces.King,
                   From  => (Space.E, 1),
                   To    => (Kind => Pieces.Castle,
                             Dest => (Space.C, 1),
                             Rook_From => (Space.A, 1),
                             Rook_Dest => (Space.D, 1)));
      else
        Action := (Valid => True,
                   Piece => Pieces.King,
                   From  => (Space.E, 8),
                   To    => (Kind => Pieces.Castle,
                             Dest => (Space.C, 8),
                             Rook_From => (Space.A, 8),
                             Rook_Dest => (Space.D, 8)));

      end if;
      Next := 6;

    elsif  Str(1 .. 3) = "o-o" then
      if Color = Space.White then
        Action := (Valid => True,
                   Piece => Pieces.King,
                   From  => (Space.E, 1),
                   To    => (Kind => Pieces.Castle,
                             Dest => (Space.G, 1),
                             Rook_From => (Space.H, 1),
                             Rook_Dest => (Space.F, 1)));
      else
        Action := (Valid => True,
                   Piece => Pieces.King,
                   From  => (Space.E, 8),
                   To    => (Kind => Pieces.Castle,
                             Dest => (Space.G, 8),
                             Rook_From => (Space.H, 8),
                             Rook_Dest => (Space.F, 8)));
      end if;
      Next := 4;

    else

      -- Skip first letter if piece
      Piece_Kind := Piece_Value (Str(1));
      if Piece_Kind = Pieces.Pawn then
        Next := 1;
      else
        Next := 2;
      end if;

      -- Get both squares and Move/Take action
      From := Square_Value (Str(Next .. Next+1));
      Dest := Square_Value (Str(Next+3 .. Next+4));
      if Str(Next+2) = '-' then
        Take := False;
      elsif  Str(Next+2) = 'X' then
        Take := True;
      else
        raise Value_Error;
      end if;
      Next := Next + 5;

      -- Check "en passant"
      if Str(Next .. Next+1) = "ep" then
        if not Take or else Piece_Kind /= Pieces.Pawn then
          raise Value_Error;
        end if;
        declare
          Taking : Space.Square_Coordinate := Dest;
          use type Space.Row_Range;
        begin
          -- Compute Taking from Dest and Color (the one which takes)
          if Color = Space.White then
            Taking.Row := Taking.Row - 1;
          else
            Taking.Row := Taking.Row + 1;
          end if;
          Action := (Valid => True,
                     Piece => Piece_Kind,
                     From  => From,
                     To    => (Kind => Pieces.Take_En_Passant,
                               Dest => Dest,
                               Taking => Taking));
        end;
        Next := Next + 2;

      -- Promotion
      elsif Str(Next) = '=' then
        declare
          New_Piece : constant Pieces.Piece_Kind_List
                    := Piece_Value (Str(Next+1));
        begin
          if Take then
            Action := (Valid => True,
                       Piece => Piece_Kind,
                       From  => From,
                       To    => (Kind => Pieces.Take_And_Promote,
                                 Dest => Dest,
                                 New_Piece => New_Piece));
          else
            Action := (Valid => True,
                       Piece => Piece_Kind,
                       From  => From,
                       To    => (Kind => Pieces.Promote,
                                 Dest => Dest,
                                 New_Piece => New_Piece));
          end if;
        end;
        Next := Next + 2;

      else
        -- Move/take
        if Take then
          Action := (Valid => True,
                     Piece => Piece_Kind,
                     From  => From,
                     To    => (Kind => Pieces.Take,
                               Dest => Dest));
        else
          Action := (Valid => True,
                     Piece => Piece_Kind,
                     From  => From,
                     To    => (Kind => Pieces.Move,
                               Dest => Dest));
        end if;

      end if;

    end if;

    -- Check, checkmate, stalemate
    declare
      Str2 : constant String (1 .. 2) := Str(Next .. Next+1);
    begin
      if    Str2 = "  " then
        Result := Game.Ok;
      elsif Str2 = "+ " then
        Result := Game.Check;
      elsif Str2 = "==" then
        Result := Game.Stalemate;
      elsif Str2 = "+=" then
        Result := Game.Checkmate;
      else
        raise Value_Error;
      end if;
    end;

  exception
    when others =>
      raise Value_Error;
  end Move_Value;

end Image;

