with Big_Con_Io, Normal, Lower_Str;

package body Screen is

  package Con_Io renames Big_Con_Io;

  Screen_Row_Offset : constant Con_Io.Row_Range := 2;
  Screen_Col_Offset : constant Con_Io.Col_range := 5;
  Back_Black : constant Con_Io.Effective_Basic_Colors := Con_Io.Red;
  Back_White : constant Con_Io.Effective_Basic_Colors := Con_Io.Light_Gray;
  Fore_Black : constant Con_Io.Effective_Colors := Con_Io.Brown;
  Fore_White : constant Con_Io.Effective_Colors := Con_Io.White;

  function To_Con_Io_Square (Square : Space.Square_Coordinate)
                             return Con_Io.Square is
  begin
    return (Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                 - Space.Row_Range'Pos(Square.Row)
                 + Screen_Row_Offset,
            Col => Space.Col_Range'Pos(Square.Col) + Screen_Col_Offset);
  end To_Con_Io_Square;

  procedure Display_Square (Square : in Space.Square_Coordinate) is
    Back : Con_Io.Effective_Basic_Colors;
    Fore : Con_Io.Effective_Colors;
    Id   : Pieces.Piece_Id;
    Chars : Constant array (Pieces.Piece_Kind_List) of Character :=
            (Pieces.Pawn =>   'i',
             Pieces.Rook =>   '+',
             Pieces.Knight => 'L',
             Pieces.Bishop => 'X',
             Pieces.Queen =>  '*',
             Pieces.King =>   'M');

    Char : Character;
    use type Space.Color_List, Pieces.Piece_Access;
  begin
    -- get background
    if Space.Color_Of_Square(Square) = Space.White then
      Back := Back_White;
    else
      Back := Back_Black;
    end if;

    -- Set Foreground and character
    if Space.Board.Piece_At(Square) = null then
      -- Empty square
      Fore := Back;
      Char := ' ';
    else
      -- Set Foreground
      Id := Pieces.Id_Of(Space.Board.Piece_At(Square).all);
      Char := Chars(Id.Kind);

      if Id.Color = Space.White then
        Fore := Fore_White;
      else
        Fore := Fore_Black;
      end if;
    end if;

    -- Move and put 
    Con_Io.Move(To_Con_Io_Square(Square));
    Con_Io.Put(Char,
               Foreground => Fore,
               Blink_Stat => Con_Io.Blink,
               Background => Back,
               Move => False);

  end Display_Square;


  -- Redisplay the board
  procedure Display_Board is
  begin
    Con_Io.Init;
    for Row in Space.Row_Range loop
      for Col in Space.Col_Range loop
        Display_Square( (Col, Row) );
      end loop;
    end loop;
    for Row in Space.Row_Range loop
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       - Space.Row_Range'Pos(Row)
                       + Screen_Row_Offset,
                  Col => Screen_Col_Offset - 2);
      Con_Io.Put(Normal(Integer(Row), 1),
                 Foreground => Con_Io.White,
                 Blink_Stat => Con_Io.Not_Blink,
                 Background => Con_Io.Black,
                 Move => False);
    end loop;
    for Col in Space.Col_Range loop
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       + Screen_Row_Offset + 1,
                  Col => Screen_Col_Offset + Space.Col_Range'Pos(Col));
      Con_Io.Put(Lower_Str(Space.Col_Range'Image(Col)),
                 Foreground => Con_Io.White,
                 Blink_Stat => Con_Io.Not_Blink,
                 Background => Con_Io.Black,
                 Move => False);
    end loop;
    Con_Io.Flush;        
  end Display_Board;


  -- Update some squares of the board
  -- type Update_Array is array (Positive range <> of Space.Square_Coordinate);
  procedure Update_Board (Squares : in Space.Square_Array) is
  begin
    for N in Squares'Range loop
      Display_Square(Squares(N));
    end loop;
    Con_Io.Flush;        
  end Update_Board;

  function Get (Color : Space.Color_List) return Players.Action_Rec is
    Str  : String (1 .. 4);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;
    Fore : Con_Io.Effective_Colors;

    Conv_Ok : Boolean;
    From, To : Space.Square_Coordinate;

    Action : Players.Action_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    if Color = Space.White then
      Fore := Fore_White;
    else
      Fore := Fore_Black;
    end if;
    Str := (others => ' ');
    Ins := False;

    loop
      Con_Io.Move (23, 1);
      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Foreground => Fore,
             Background => Con_Io.Black);
      if Stat = Con_Io.Ret then
        if Lower_Str (Str) = "exit" then
          return (Valid => False);
        end if;
        begin
          From.Col := Space.Col_Range'Value(Str(1..1));
          From.Row := Space.Row_Range'Value(Str(2..2));
          To.Col := Space.Col_Range'Value(Str(3..3));
          To.Row := Space.Row_Range'Value(Str(4..4));
          Conv_Ok := True;
        exception
          when others =>
            Conv_Ok := False;
        end;

        if Conv_Ok then
          Action := Players.Find_Action (Color, From, To);
          if Action.Valid then
            return Action;
          end if;
        end if;
      end if;
    end loop;

  end Get;
    
  procedure Close is
  begin
    Con_Io.Destroy;
  end Close;

end Screen;

