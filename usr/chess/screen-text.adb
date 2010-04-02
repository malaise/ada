separate (Screen)

package body Text is

  Screen_Row_Offset : constant Con_Io.Row_Range := 3;
  Screen_Col_Offset : constant Con_Io.Col_Range := 5;

  Promotion_Row_Offset :  constant Con_Io.Row_Range := 4;
  Promotion_Col : constant Con_Io.Col_Range := 17;


  function To_Con_Io_Square (Color : in Space.Color_List;
                             Square : Space.Square_Coordinate)
                             return Con_Io.Square is
    use type Space.Color_List;
  begin
    if Color = Space.White then
      return (Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                   - Space.Row_Range'Pos(Square.Row)
                   + Screen_Row_Offset,
              Col => Space.Col_Range'Pos(Square.Col) + Screen_Col_Offset);
    else
      return (Row => Space.Row_Range'Pos(Square.Row) + Screen_Row_Offset - 1,
              Col => Space.Col_Range'Pos(Space.Col_Range'Last)
                   - Space.Col_Range'Pos(Square.Col)
                   + Screen_Col_Offset);
    end if;
  end To_Con_Io_Square;

  function To_Space_Square (Color         : Space.Color_List;
                            Con_Io_Square : Con_Io.Square)
           return Square_Result_Rec is
    Result : Square_Result_Rec (True);
    use type Space.Color_List;
  begin
    if      Con_Io_Square.Row < Screen_Row_Offset
    or else Con_Io_Square.Row > Screen_Row_Offset
                              + Space.Row_Range'Pos(Space.Row_Range'Last) - 1
    or else Con_Io_Square.Col < Screen_Col_Offset
    or else Con_Io_Square.Col > Screen_Col_Offset
                              + Space.Col_Range'Pos(Space.Col_Range'Last) then
      return (Valid => False);
    end if;
    if Color = Space.White then
      Result.Square.Row :=
           Space.Row_Range'Val(Space.Row_Range'Pos(Space.Row_Range'Last)
                             - (Con_Io_Square.Row - Screen_Row_Offset));
      Result.Square.Col :=
           Space.Col_Range'Val(Con_Io_Square.Col - Screen_Col_Offset);
    else
      Result.Square.Row :=
           Space.Row_Range'Val(Con_Io_Square.Row - Screen_Row_Offset + 1);
      Result.Square.Col :=
           Space.Col_Range'Val(Space.Col_Range'Pos(Space.Col_Range'Last)
                             - (Con_Io_Square.Col - Screen_Col_Offset));
    end if;
    return Result;
  end To_Space_Square;


  procedure Display_Square (Color : in Space.Color_List;
                            Square : in Space.Square_Coordinate) is
    Back : Con_Io.Effective_Colors;
    Fore : Con_Io.Effective_Colors;
    Id   : Pieces.Piece_Id;

    Char : Character;
    use type Space.Color_List, Pieces.Piece_Access, Pieces.Piece_Kind_List;
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
      if Id.Kind = Pieces.Pawn then
        Char := 'i';
      else
        Char := Image.Piece_Image(Id.Kind)(1);
      end if;

      if Id.Color = Space.White then
        Fore := Fore_White;
      else
        Fore := Fore_Black;
      end if;
    end if;

    -- Move and put
    Con_Io.Move(To_Con_Io_Square(Color, Square));
    Con_Io.Put(Char,
               Foreground => Fore,
               Blink_Stat => Con_Io.Blink,
               Background => Back,
               Move => False);

  end Display_Square;

  procedure Init_Board (Color : in Space.Color_List) is
    use type Space.Color_List;
    Con_Row : Con_Io.Row_Range;
    Con_Col : Con_Io.Col_Range;

    procedure Put (Row : in Space.Row_Range) is
    begin
      Con_Io.Put (Normal(Integer(Row), 1), Move => False);
    end Put;
    procedure Put (Col : in Space.Col_Range) is
    begin
      Con_Io.Put (Lower_Str(Space.Col_Range'Image(Col)), Move => False);
    end Put;

  begin
    for Row in Space.Row_Range loop
      if Color = Space.White then
        Con_Row := Space.Row_Range'Pos(Space.Row_Range'Last)
                 - Space.Row_Range'Pos(Row)
                 + Screen_Row_Offset;
      else
        Con_Row := Space.Row_Range'Pos(Row) + Screen_Row_Offset - 1;
      end if;
      Con_Io.Move(Con_Row, Screen_Col_Offset - 2);
      Put (Row);
      Con_Io.Move(Row => Con_Row,
                  Col => Screen_Col_Offset
                       + Space.Col_Range'Pos(Space.Col_Range'Last)
                       + 2);
      Put (Row);
    end loop;
    for Col in Space.Col_Range loop
      if Color = Space.White then
        Con_Col := Space.Col_Range'Pos(Col) + Screen_Col_Offset;
      else
        Con_Col := Space.Col_Range'Pos(Space.Col_Range'Last)
                 - Space.Col_Range'Pos(Col)
                 + Screen_Col_Offset;
      end if;
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       + Screen_Row_Offset + 1,
                  Col => Con_Col);
      Put (Col);
      Con_Io.Move(Screen_Row_Offset - 2, Con_Col);
      Put (Col);
    end loop;
  end Init_Board;

  procedure Display_Promotion (Move_Color : Space.Color_List) is
    Fore : Con_Io.Effective_Colors;
    use type Space.Color_List;
  begin
    if Move_Color = Space.White then
      Fore := Fore_White;
    else
      Fore := Fore_Black;
    end if;
    for P in Pieces.Promotion_Piece_List loop
      Con_Io.Move (Promotion_Row_Offset + Pieces.Promotion_Piece_List'Pos(P),
                   Promotion_Col);
      if Getting_Promotion then
        Con_Io.Put (Image.Piece_Image(P)(1),
                    Foreground => Fore, Background => Main_Back);
      else
        Con_Io.Put (' ', Foreground => Main_Back, Background => Main_Back);
      end if;
    end loop;
  end Display_Promotion;

  function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec is
    Con_Io_Rec : Con_Io.Mouse_Event_Rec;
    Got_Square : Square_Result_Rec;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List;
  begin
    -- Get and check Con_Io event
    Con_Io.Get_Mouse_Event (Con_Io_Rec, Con_Io.Row_Col);
    if Con_Io_Rec.Button /= Con_Io.Left
    or else Con_Io_Rec.Status = Con_Io.Motion then
      return (Kind => Discard);
    end if;
    if not Con_Io_Rec.Valid then
      return (Kind => Release_Out);
    end if;

    Got_Square := To_Space_Square (Color, (Con_Io_Rec.Row, Con_Io_Rec.Col));
    if not Got_Square.Valid then
      if Con_Io_Rec.Status = Con_Io.Pressed then
        return (Kind => Discard);
      else
        return (Kind => Release_Out);
      end if;
    else
      if Con_Io_Rec.Status = Con_Io.Pressed then
        return (Click, Got_Square.Square);
      else
        return (Release, Got_Square.Square);
      end if;
    end if;
  end Get_Mouse_Event;


  function Get_Promotion (Click : in Boolean) return Pieces.Piece_Kind_List is
    Con_Io_Rec : Con_Io.Mouse_Event_Rec;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List;
  begin
    Con_Io.Get_Mouse_Event (Con_Io_Rec, Con_Io.Row_Col);
    if Con_Io_Rec.Valid
    and then Con_Io_Rec.Button = Con_Io.Left
    and then Con_Io_Rec.Col = Promotion_Col
    and then Con_Io_Rec.Row >= Promotion_Row_Offset
    and then Con_Io_Rec.Row <= Promotion_Row_Offset
                    + Pieces.Promotion_Piece_List'Pos(Pieces.Promotion_Piece_List'Last) then
      if (Click and then Con_Io_Rec.Status = Con_Io.Pressed)
      or else (not Click and then Con_Io_Rec.Status = Con_Io.Released) then
        return Pieces.Promotion_Piece_List'Val(Con_Io_Rec.Row - Promotion_Row_Offset);
      end if;
    end if;
    return Pieces.Pawn;
  end Get_Promotion;

end Text;

