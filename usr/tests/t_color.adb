with Con_Io, Argument, Mixed_Str;
procedure T_Color is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;
  Colors : Con_Io.Colors_Definition;
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
  Con_Io.Initialise;

  Console.Open;
  Screen.Set_To_Screen (Console'Unrestricted_Access);

  loop
    Screen.Clear;
    for I in Con_Io.Effective_Colors loop
      Screen.Move (Con_Io.Colors'Pos(I), 1);
      Screen.Put (Mixed_Str (Con_Io.Color_Name_Of (I) ));
      Screen.Move (Con_Io.Colors'Pos(I), 20);
      Screen.Put ("^!@#$%&€*é$ê", Foreground => I, Move => False);
      Screen.New_Line;
    end loop;

    Screen.Move;
    R := Screen.Get;
    exit when R.Mvt = Con_Io.Break;
  end loop;
  Console.Close;
end T_Color;

