with Con_Io, Argument, Mixed_Str;
procedure T_Color is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;
  Colors : Con_Io.Colors_Definition;
  Color : Con_Io.Colors;
  X1, Y1, X2, Y2 : Natural;
  Ic : Con_Io.Effective_Colors;
  R : Con_Io.Get_Result;
  use type Con_Io.Curs_Mvt;
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
    Screen.Put ("01234567890123456789012345678901234567890123456789",
                Foreground => Con_Io.Effective_Colors'Last);
    Color := Con_Io.Effective_Colors'Last;
    for I in Con_Io.Effective_Colors loop
      Screen.Move (Con_Io.Colors'Pos(I), 1);
      Screen.Put (Mixed_Str (Con_Io.Color_Name_Of (Color) ),
                  Foreground => Con_Io.Effective_Colors'Last);
      Screen.Move (Con_Io.Colors'Pos(I), 20);
      Screen.Put ("^!@#$%&€*é$ê|", Foreground => Color, Move => False); --## rule line off Char
      -- First target col is 33, Pos of first effective color is 1
      Screen.Set_Foreground (Color);
      Screen.Move (Con_Io.Colors'Pos(I), 32 + Con_Io.Colors'Pos(I));
      Console.To_Xy (Screen.Position, X1, Y1);
      X2 := X1 + Console.Font_Width  - 1;
      Y2 := Y1 + Console.Font_Height - 1;
      case Con_Io.Colors'Pos (I) rem 4 is
        when 1 =>
          Console.Fill_Rectangle (X1, Y1, X2, Y2);
        when 2 =>
          Console.Draw_Arc (X1, Y1, X2, Y2, 0, 60 * 360);
        when 3 =>
          Console.Draw_Rectangle (X1, Y1, X2, Y2);
        when 0 =>
          Console.Fill_Arc (X1, Y1, X2, Y2, 0, 60 * 360);
        when others =>
          null;
      end case;
      Screen.New_Line;
      Color := Con_Io.Colors'Pred(Color);
    end loop;
    Color := Con_Io.Effective_Colors'Last;
    Screen.Set_Foreground (Color);
    Screen.Move (Con_Io.Colors'Pos(Color) + 1, 1);
    Screen.Put ("Black");
    Screen.Move (Con_Io.Colors'Pos(Color) + 1, 20);
    Screen.Put ("^!@#$%&€*é$ê|", Move => False); --## rule line off Char
    Screen.Move (Con_Io.Colors'Pos(Color) + 1, 47);
    Console.To_Xy (Screen.Position, X1, Y1);
    X2 := X1 + Console.Font_Width  - 1;
    Y2 := Y1 + Console.Font_Height - 1;
    Console.Fill_Rectangle (X1, Y1, X2, Y2);
    Screen.New_Line;
    Screen.Put ("01234567890123456789012345678901234567890123456789",
                Foreground => Color);

    Screen.Move;
    R := Screen.Get;
    exit when R.Mvt = Con_Io.Break;
  end loop;
  Console.Close;
end T_Color;

