separate (Screen)

package body Graphic is

  type Graph_Square is record
    X : Natural;
    Y : Natural;
  end record;

  Size : Constant Natural := 45;
  Size2 : Constant Natural := Size / 2;
  Len  : Constant Natural := 8 * Size;
  X0 : constant Con_Io.Graphics.X_Range := 60;
  -- Set at init
  Y0 :  Con_Io.Graphics.Y_Range;
  -- For characters
  X_Offset : Natural;
  Y_Offset : Natural;

  function To_Con_Io_Square (Color : in Space.Color_List;
                             Square : Space.Square_Coordinate)
                             return Graph_Square is
    use type Space.Color_List;
  begin            
    if Color = Space.White then 
      return (X => X0 + Space.Col_Range'Pos(Square.Col) * Size,
              Y => Y0 + Space.Row_Range'Pos(Square.Row) * Size);
    else   
      return (X => X0 + (Space.Col_Range'Pos(Space.Col_Range'Last)
                       - Space.Col_Range'Pos(Square.Col)) * Size,
              Y => Y0 + (Space.Row_Range'Pos(Space.Row_Range'Last)
                       - Space.Row_Range'Pos(Square.Row) + 1) * Size);
    end if; 
  end To_Con_Io_Square;

  procedure Display_Square (Color : in Space.Color_List;
                            Square : in Space.Square_Coordinate) is
    Back : Con_Io.Effective_Basic_Colors;
    Fore : Con_Io.Effective_Colors;
    Id   : Pieces.Piece_Id;

    Pos : constant Graph_Square := To_Con_Io_Square (Color, Square);

    Char : Character;
    use type Space.Color_List, Pieces.Piece_Access, Pieces.Piece_Kind_List;
  begin
    -- get background
    if Space.Color_Of_Square(Square) = Space.White then
      Back := Back_White;
    else
      Back := Back_Black;
    end if;

    -- Fill square
    Con_Io.Set_Foreground (Back);
    Con_Io.Set_Background (Back);
--    for X in Pos.X - Size2 .. Pos.X + Size2 loop
--      Con_Io.Graphics.Draw_Line (X, Pos.Y - Size2,
--                                 X, Pos.Y + Size2);
--    end loop;
    Con_Io.Graphics.Fill_Rectangle (Pos.X - Size2, Pos.Y - Size2,
                                    Pos.X + Size2, Pos.Y + Size2);

    -- Set Foreground and character
    if Space.Board.Piece_At(Square) /= null then
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
      Con_Io.Set_Foreground (Fore);
      Con_Io.Graphics.Put(Char, Pos.X - X_Offset, Pos.Y - Y_Offset);
    end if;

  end Display_Square;

  procedure Init_Board (Color : in Space.Color_List) is
    Pos : Graph_Square;
    Offset : Integer;

    procedure Put (Row : in Space.Row_Range; Square : in Graph_Square) is
    begin
      Con_Io.Graphics.Put (Normal(Integer(Row), 1),
           Square.X - X_Offset, Square.Y - Y_Offset);
    end Put;
    procedure Put (Col : in Space.Col_Range; Square : Graph_Square) is
    begin
      Con_Io.Graphics.Put (Lower_Str(Space.Col_Range'Image(Col)),
           Square.X - X_Offset, Square.Y - Y_Offset);
    end Put;

    use type Space.Color_List;
  begin
    -- Compute offsets
    Y0 := Con_Io.Graphics.Y_Max - X0 - Len;
    X_Offset := Con_Io.Graphics.Font_Width  / 2;
    Y_Offset := (Con_Io.Graphics.Font_Height - Con_Io.Graphics.Font_Offset) / 2 + 4;

    -- Print Rows/Cols names
    if Color = Space.White then
      Offset := Size;
    else
      Offset := - Size;
    end if;
    Con_Io.Set_Foreground (Main_Fore);
    Con_Io.Set_Background (Main_Back);
    for R in Space.Row_Range loop
      Pos := To_Con_Io_Square (Color, (Space.A, R));
      Pos.X := Pos.X - Offset;
      Put (R, Pos);
      Pos := To_Con_Io_Square (Color, (Space.H, R));
      Pos.X := Pos.X + Offset;
      Put (R, Pos);
    end loop;
    for C in Space.Col_Range loop
      Pos := To_Con_Io_Square (Color, (C, 1));
      Pos.Y := Pos.Y - Offset;
      Put (C, Pos);
      Pos := To_Con_Io_Square (Color, (C, 8));
      Pos.Y := Pos.Y + Offset;
      Put (C, Pos);
    end loop;
      
  end Init_Board;

  procedure Display_Promotion (Color : in Space.Color_List; Show : in Boolean) is
  begin
    null;
  end Display_Promotion;

  function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec is
  begin
    return (Kind => Discard);
  end Get_Mouse_Event;

  function Get_Promotion (Click : in Boolean) return Pieces.Piece_Kind_List is
  begin
    return Pieces.Pawn;
  end Get_Promotion;
end Graphic;

