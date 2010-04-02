with Ada.Text_Io;
with Generic_Con_Io, Con_Io, Argument;
procedure T_Color is
  Colors : Generic_Con_Io.Colors_Definition;
  Ic : Generic_Con_Io.Effective_Colors;
  R : Con_Io.Get_Result;
  use type Con_Io.Curs_Mvt;
begin

  -- Load default colors
  Colors := Generic_Con_Io.Default_Colors;
  -- Store Arguments as colors
  Ic := Generic_Con_Io.Effective_Colors'First;
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (Occurence => I) /= "-" then
      Colors(Ic) := Generic_Con_Io.Asu_Tus (Argument.Get_Parameter (Occurence => I));
    end if;
    Ic := Generic_Con_Io.Effective_Colors'Succ (Ic);
  end loop;
  Generic_Con_Io.Set_Colors (Colors);
  Generic_Con_Io.Initialise;

  Con_Io.Init;

  loop
    Con_Io.Clear;
    for I in Con_Io.Effective_Colors loop
      Con_Io.Move (Con_Io.Colors'Pos(I), 1);
      Con_Io.Put (Con_Io.Color_Name_Of (I) );
      Con_Io.Move (Con_Io.Colors'Pos(I), 20);
      Con_Io.Put ("^!@#$%&€*é$ê", Foreground => I, Move => False);
      Con_Io.New_Line;
    end loop;

    Con_Io.Move;
    R := Con_Io.Get;
    exit when R.Mvt /= Con_Io.Refresh;
  end loop;
  Ada.Text_Io.Put_Line ("Exiting in 3 s");
  delay 3.0;
  Con_Io.Destroy;

end T_Color;

