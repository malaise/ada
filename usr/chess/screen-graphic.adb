separate (Screen)

package body Graphic is

  type Graph_Square is record
    X : Natural;
    Y : Natural;
  end record;

  Size : constant Natural := 45;
  Size2 : constant Natural := Size / 2;
  Len  : constant Natural := 8 * Size;

  -- Offsets (middle of square)
  X0 : constant Con_Io.Graphics.X_Range := 60;
  -- Set at init
  Y0 :  Con_Io.Graphics.Y_Range;

  Promotion_X : constant Con_Io.Graphics.X_Range := 495;
  Promotion_Y_Offset :  Con_Io.Graphics.Y_Range;

  -- For characters
  X_Offset : Natural;
  Y_Offset : Natural;

  -- Bitmaps
  package Bitmaps is
    Piece_Size : constant := 35;
    Bits_Offset : constant := (Size -  Piece_Size) / 2;
    subtype Piece_Map is Con_Io.Byte_Array (1 .. Piece_Size * Piece_Size);
    type Piece_Map_Access is access all Piece_Map;

    function Get_Bitmap (Kind : in Pieces.Piece_Kind_List) return Piece_Map_Access;

  end Bitmaps;
  package body Bitmaps is separate;


  function To_Con_Io_Square (Color : in Space.Color_List;
                             Square : Space.Square_Coordinate)
                             return Graph_Square is
    use type Space.Color_List;
  begin
    if Color = Space.White then
      return (X => X0 + Space.Col_Range'Pos(Square.Col) * Size,
              Y => Y0 + (Space.Row_Range'Pos(Square.Row) - 1) * Size);
    else
      return (X => X0 + (Space.Col_Range'Pos(Space.Col_Range'Last)
                       - Space.Col_Range'Pos(Square.Col)) * Size,
              Y => Y0 + (Space.Row_Range'Pos(Space.Row_Range'Last)
                       - Space.Row_Range'Pos(Square.Row)) * Size);
    end if;
  end To_Con_Io_Square;

  function To_Space_Square (Color : Space.Color_List;
                            X : Con_Io.Graphics.X_Range;
                            Y : Con_Io.Graphics.Y_Range)
                           return Square_Result_Rec is
    Result : Square_Result_Rec (True);
    use type Space.Color_List;
  begin
    if      X <  X0 - Size2
    or else X >= X0 + Len - Size2
    or else Y <  Y0 - Size2
    or else Y >= Y0 + Len - Size2 then
      return (Valid => False);
    end if;
    if Color = Space.White then
      Result.Square.Col := Space.Col_Range'Val ((X + Size2  - X0) / Size);
      Result.Square.Row := Space.Row_Range'Val ((Y + Size2  - Y0) / Size + 1);
    else
      Result.Square.Col :=
           Space.Col_Range'Val (Space.Col_Range'Pos(Space.Col_Range'Last)
                              - ((X + Size2  - X0) / Size));
      Result.Square.Row :=
           Space.Row_Range'Val (Space.Row_Range'Pos(Space.Row_Range'Last)
                              - (Y + Size2  - Y0) / Size);
    end if;
    return Result;
  end To_Space_Square;


  procedure Display_Square (Color : in Space.Color_List;
                            Square : in Space.Square_Coordinate) is
    Back : Con_Io.Effective_Colors;
    Fore : Con_Io.Effective_Colors;
    Id   : Pieces.Piece_Id;

    Pos : constant Graph_Square := To_Con_Io_Square (Color, Square);

    Bits : Bitmaps.Piece_Map_Access;
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
    Con_Io.Graphics.Fill_Rectangle (Pos.X - Size2, Pos.Y - Size2,
                                    Pos.X + Size2 + 1, Pos.Y + Size2 + 1);

    -- Set Foreground and piece
    if Space.Board.Piece_At(Square) /= null then
      Id := Pieces.Id_Of(Space.Board.Piece_At(Square).all);
      Bits := Bitmaps.Get_Bitmap (Id.Kind);
      if Id.Color = Space.White then
        Fore := Fore_White;
      else
        Fore := Fore_Black;
      end if;
      Con_Io.Set_Foreground (Fore);
      Con_Io.Graphics.Draw_Points(Pos.X - (Size2 - Bitmaps.Bits_Offset),
                                  Pos.Y + (Size2 - Bitmaps.Bits_Offset),
                                  Bitmaps.Piece_Size, Bitmaps.Piece_Size,
                                  Bits.all);
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
    Y0 := Con_Io.Graphics.Y_Max - X0 - Len + Size;
    X_Offset := Con_Io.Graphics.Font_Width  / 2;
    Y_Offset := (Con_Io.Graphics.Font_Height - Con_Io.Graphics.Font_Offset) / 2 + 4;
    Promotion_Y_Offset := Y0 + Size;

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

  procedure Display_Promotion (Move_Color : in Space.Color_List) is
    Fore : Con_Io.Effective_Colors;
    Back : Con_Io.Effective_Colors;
    Bits : Bitmaps.Piece_Map_Access;
    use type Space.Color_List;
    use type Con_Io.Colors;
  begin
    if Getting_Promotion then
      if Move_Color = Space.White then
        Back := Back_White;
        Fore := Fore_White;
      else
        Fore := Fore_Black;
        Back := Back_Black;
      end if;
      for P in Pieces.Promotion_Piece_List loop
        Con_Io.Set_Foreground (Back);
        Con_Io.Set_Background (Back);
        Con_Io.Graphics.Fill_Rectangle (
               Promotion_X - Size2,
               Promotion_Y_Offset
                + Pieces.Promotion_Piece_List'Pos(P) * Size
                - Size2,
               Promotion_X + Size2,
                Promotion_Y_Offset
                + Pieces.Promotion_Piece_List'Pos(P) * Size
                + Size2);

        if Back = Back_White then
          Back := Back_Black;
        else
          Back := Back_White;
        end if;

        Con_Io.Set_Foreground (Fore);
        Bits := Bitmaps.Get_Bitmap (P);
        Con_Io.Graphics.Draw_Points(
            Promotion_X - (Size2 - Bitmaps.Bits_Offset),
            Promotion_Y_Offset
                 + Pieces.Promotion_Piece_List'Pos(P) * Size
                 + (Size2 - Bitmaps.Bits_Offset),
            Bitmaps.Piece_Size,
            Bitmaps.Piece_Size,
            Bits.all);
      end loop;

    else
      Con_Io.Set_Foreground (Main_Back);
      Con_Io.Graphics.Fill_Rectangle (
          Promotion_X - Size2,
          Promotion_Y_Offset
           + Pieces.Promotion_Piece_List'Pos(
               Pieces.Promotion_Piece_List'First) * Size
           - Size2,
          Promotion_X + Size2,
          Promotion_Y_Offset
           + Pieces.Promotion_Piece_List'Pos(
               Pieces.Promotion_Piece_List'Last) * Size
           + Size2);
    end if;

  end Display_Promotion;

  function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec is
    Con_Io_Rec : Con_Io.Mouse_Event_Rec;
    Got_Square : Square_Result_Rec;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List;
  begin
    -- Get and check Con_Io event
    Con_Io.Get_Mouse_Event (Con_Io_Rec, Con_Io.X_Y);
    if Con_Io_Rec.Button /= Con_Io.Left
    or else Con_Io_Rec.Status = Con_Io.Motion then
      return (Kind => Discard);
    end if;
    if not Con_Io_Rec.Valid then
      return (Kind => Release_Out);
    end if;

    Got_Square := To_Space_Square (Color, Con_Io_Rec.X, Con_Io_Rec.Y);
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
    Con_Io.Get_Mouse_Event (Con_Io_Rec, Con_Io.X_Y);
    if Con_Io_Rec.Valid
    and then Con_Io_Rec.Button = Con_Io.Left
    and then Con_Io_Rec.X >= Promotion_X - Size2
    and then Con_Io_Rec.X <  Promotion_X + Size2
    and then Con_Io_Rec.Y >= Promotion_Y_Offset
                             + Pieces.Promotion_Piece_List'Pos(
                                 Pieces.Promotion_Piece_List'First) * Size
                             - Size2
    and then Con_Io_Rec.Y <  Promotion_Y_Offset
                             + Pieces.Promotion_Piece_List'Pos(
                                 Pieces.Promotion_Piece_List'Last) * Size
                             + Size2 then
      if (Click and then Con_Io_Rec.Status = Con_Io.Pressed)
      or else (not Click and then Con_Io_Rec.Status = Con_Io.Released) then
        return Pieces.Promotion_Piece_List'Val(
           (Con_Io_Rec.Y + Size2 - Promotion_Y_Offset) / Size);
      end if;
    end if;
    return Pieces.Pawn;
  end Get_Promotion;

end Graphic;

