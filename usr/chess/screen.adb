with Big_Con_Io, Normal, Lower_Str, Lower_Char;

with Pieces, Space.Board;

package body Screen is

  package Con_Io renames Big_Con_Io;

  Screen_Row_Offset : constant Con_Io.Row_Range := 3;
  Screen_Col_Offset : constant Con_Io.Col_range := 5;

  Fore_White : constant Con_Io.Effective_Colors := Con_Io.White;
  Fore_Black : constant Con_Io.Effective_Colors := Con_Io.Red;
  Back_White : constant Con_Io.Effective_Basic_Colors := Con_Io.Cyan;
  Back_Black : constant Con_Io.Effective_Basic_Colors := Con_Io.Black;
  Main_Back  : constant Con_Io.Effective_Basic_Colors := Con_Io.Brown;

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
             Pieces.Rook =>   'T',
             Pieces.Knight => 'P',
             Pieces.Bishop => '&',
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
    Con_IO.Set_Background (Main_Back);
    Con_IO.Set_Foreground (Con_Io.White);
    Con_IO.Clear;
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
      Con_Io.Put(Normal(Integer(Row), 1), Move => False);
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       - Space.Row_Range'Pos(Row)
                       + Screen_Row_Offset,
                  Col => Screen_Col_Offset
                       + Space.Col_Range'Pos(Space.Col_Range'Last)
                       + 2);
      Con_Io.Put(Normal(Integer(Row), 1), Move => False);
    end loop;
    for Col in Space.Col_Range loop
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       + Screen_Row_Offset + 1,
                  Col => Screen_Col_Offset + Space.Col_Range'Pos(Col));
      Con_Io.Put(Lower_Str(Space.Col_Range'Image(Col)), Move => False);
      Con_Io.Move(Row => Screen_Row_Offset - 2,
                  Col => Screen_Col_Offset + Space.Col_Range'Pos(Col));
      Con_Io.Put(Lower_Str(Space.Col_Range'Image(Col)), Move => False);
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

  -- Get, Ack
  procedure Erase is
    Erase_Str :  constant String (1 .. 80) := (others => ' ');
  begin
    Con_Io.Move (23, 1);
    Con_Io.Put (Erase_Str, Foreground => Main_Back);
  end Erase;

  function Fore (Color : Space.Color_List) return Con_Io.Effective_Colors is
    use type Space.Color_List;
  begin
    if Color = Space.White then
      return Fore_White;
    else
      return Back_Black;
    end if;
  end Fore;

  function Get (Color : Space.Color_List) return Players.Action_Rec is
    Str  : String (1 .. 5);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Conv_Ok : Boolean;
    From, To : Space.Square_Coordinate;
    Promo : Pieces.Piece_Kind_List;

    Action : Players.Action_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    Str := (others => ' ');
    Ins := False;

    loop
      Con_Io.Move (23, 1);
      Con_Io.Put ("Move:", Foreground => Fore (Color));

      Con_Io.Move (23, 7);
      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Foreground => Fore (Color));
      if Stat = Con_Io.Ret then
        if Str(1) = ' ' then
          Str(1 .. Str'Length-1) := Str(2 .. Str'Length);
          Str(Str'Length) := ' ';
        end if;

        if Lower_Str (Str) = "exit " then
          Erase;
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
          if Str(5) = ' ' then
            Promo := Pieces.Pawn;
            Conv_Ok := True;
          else
            Conv_Ok := False;
            for Piece in Pieces.Promotion_Piece_List loop
              if Lower_Char(Str(5)) = Lower_Char(Pieces.Promotion_Piece_List'Image(Piece)(1)) then
                Promo := Piece;
                Conv_Ok := True;
                exit;
              end if;
            end loop;
          end if;
        end if;

        if Conv_Ok then
          Action := Players.Find_Action (Color, From, To, Promo);
          if Action.Valid then
            Erase;
            return Action;
          end if;
        end if;
      elsif Stat = Con_Io.Refresh then
        Display_Board;
      elsif Stat = Con_Io.Break then
        Erase;
        return (Valid => False);
      end if;
    end loop;

  end Get;

  procedure Put (Color : in Space.Color_List;
                 Message : in String; Ack : in Boolean := False) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timeout : Con_Io.Delay_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    if Ack then
      Timeout := Con_Io.Infinite_Delay;
    else
      Timeout := (Con_Io.Delay_Sec, 1.0);
    end if;
    Ins := False;

    loop
      Con_Io.Move (23, 1);
      Con_Io.Put (Message, Foreground => Fore (Color) );

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => Timeout);
      if Stat = Con_Io.Ret or else Stat = Con_Io.Timeout then
        Erase;
        return;
      elsif Stat = Con_Io.Refresh then
        Display_Board;
      elsif Stat = Con_Io.Break then
        Erase;
        return;
      end if;
    end loop;
  end Put;

    
  procedure Close is
  begin
    Con_Io.Destroy;
  end Close;

end Screen;

