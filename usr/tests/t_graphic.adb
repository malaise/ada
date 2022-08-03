with Con_Io, Argument, Images;
procedure T_Graphic is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;
  Colors : Con_Io.Colors_Definition;
  X1, Y1, X2, Y2 : Natural;
  Ic : Con_Io.Effective_Colors;
  Foreground : Con_Io.Effective_Colors;
  Nb_Shapes : Positive;
  Width : Con_Io.X_Range;
  Height : Con_Io.Y_Range;
  use type Con_Io.Colors, Con_Io.Extra_Mvt;
begin

  -- Load default colors
  Colors := Con_Io.Default_Colors;
  -- Store Arguments as colors
  Ic := Con_Io.Effective_Colors'First;
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (Occurence => I) /= "-" then
      Argument.Get_Parameter (Colors(Ic), Occurence => I);
    end if;
    Ic := Con_Io.Effective_Colors'Succ (Ic);
  end loop;
  Con_Io.Set_Colors (Colors);
  Console.Open;
  Screen.Set_To_Screen (Console'Unrestricted_Access);
  Console.Set_Y_Mode (Con_Io.X_Mng_Mode);

  loop
    Screen.Clear;
    Screen.Put ("Font: " & Images.Integer_Image (Console.Font_Width)
       & "x" & Images.Integer_Image (Console.Font_Height));

    -- Chessboard of text and rectangles
    -- Start rows and 21 columns, alternate screen foreground and background
    -- Cells are: space at Row-Col, space at X-Y, rectangle at X-Y,
    -- Must be odd
    Nb_Shapes := 3;
    for Row in 1 .. 3 loop
      for Col in 0 .. 20 loop
        if (Row + Col) rem 2 = 0 then
          Foreground := Console.Foreground;
        else
          Foreground := Console.Background;
        end if;
        if (Row + Col) rem Nb_Shapes = 0 then
          if Foreground = Console.Foreground then
            -- Space at Row-Col
            Screen.Move (Row, Col);
            Screen.Put (' ', Background => Foreground, Move => False);
          end if;
        elsif (Row + Col) rem Nb_Shapes = 1 then
          -- Space at X-Y
          Console.To_Xy ( (Row, Col), X1, Y1);
          Screen.Set_Background (Foreground);
          Console.Put (' ', X1, Y1);
        elsif (Row + Col) rem Nb_Shapes = 2 then
          -- Rectangle at X-Y
          Console.To_Xy ( (Row, Col), X1, Y1);
          Screen.Set_Foreground (Foreground);
          Console.Fill_Rectangle (X1, Y1,
              X1 + Console.Font_Width - 1,
              Y1 + Console.Font_Height - 1);
        elsif (Row + Col) rem Nb_Shapes = 4 then
          -- Nothing
          null;
        else
          -- All values from 0 to Nb_Shape-1 must be covered
          raise Program_Error;
        end if;
      end loop;
    end loop;

    -- 8 black rectangles around a blue ellipse
    for Row in 1 .. 3 loop
      for Col in 30 .. 32 loop
        if Row /= 2 or else Col /= 31 then
          Console.To_Xy ( (Row, Col), X1, Y1);
          Screen.Set_Foreground (Con_Io.Default_Foreground);
          Console.Fill_Rectangle (X1, Y1,
              X1 + Console.Font_Width - 1,
              Y1 + Console.Font_Height - 1);
        end if;
      end loop;
    end loop;
    Screen.Set_Foreground (Con_Io.Color02);
    Screen.Set_Background (Con_Io.Default_Background);
    Console.To_Xy ( (2, 31), X1, Y1);
    Console.Fill_Arc (X1, Y1,
        X1 + Console.Font_Width - 1,
        Y1 + Console.Font_Height - 1,
        0, 360 * 60);

    -- A blue ellipse surrounded by 8 black rectangles
    Console.To_Xy ( (2, 33), X1, Y1);
    Console.Fill_Arc (X1, Y1,
        X1 + Console.Font_Width - 1,
        Y1 + Console.Font_Height - 1,
        0, 360 * 60);
    for Row in 1 .. 3 loop
      for Col in 32 .. 34 loop
        if Row /= 2 or else Col /= 33 then
          Console.To_Xy ( (Row, Col), X1, Y1);
          Screen.Set_Foreground (Con_Io.Default_Foreground);
          Console.Fill_Rectangle (X1, Y1,
              X1 + Console.Font_Width - 1,
              Y1 + Console.Font_Height - 1);
        end if;
      end loop;
    end loop;

    -- A black thick "square" (size of font)
    for Row in 5 .. 24 loop
      for Col in 1 .. 33 loop
        if Row = 5 or else Row = 24 or else Col = 1 or else Col = 33 then
          Console.To_Xy ( (Row, Col), X1, Y1);
          Console.Fill_Rectangle (X1, Y1,
              X1 + Console.Font_Width - 1,
              Y1 + Console.Font_Height - 1);
        end if;
      end loop;
    end loop;
    Width := 31 * Console.Font_Width - 1;
    Height := 18 * Console.Font_Height - 1;
    -- 1 red thin rectangle just inside (2 pixels)
    Screen.Set_Foreground (Con_Io.Color05);
    Console.To_Xy ( (6, 2), X1, Y1);
    Console.Draw_Rectangle (X1, Y1, X1 + Width, Y1 + Height);
    Console.Draw_Rectangle (X1 + 1, Y1 + 1, X1 + Width - 1, Y1 + Height - 1);
    -- A blue "circle" inside
    Screen.Set_Foreground (Con_Io.Color02);
    Console.Draw_Arc (X1 + 2, Y1 + 2, X1 + Width - 2, Y1 + Height - 2,
                      0, 360 * 60);

    -- 1 red thin rectangle at the other side of the squares
    Screen.Set_Foreground (Con_Io.Color05);
    Console.To_Xy ( (7, 3), X1, Y1);
    Console.To_Xy ( (23, 32), X2, Y2);
    Console.Draw_Rectangle (X1 - 1, Y1 - 1, X2, Y2);
    Console.Draw_Rectangle (X1 - 2, Y1 - 2, X2 + 1, Y2 + 1);
    -- A blue full "circle" just inside
    Screen.Set_Foreground (Con_Io.Color02);
    Console.Fill_Arc (X1 - 1, Y1 - 1, X2, Y2, 0, 360 * 60);

    exit when Screen.Get.Mvt = Con_Io.Break;
  end loop;
  Console.Close;
end T_Graphic;

