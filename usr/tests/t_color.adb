with Ada.Text_Io;
with Con_Io;
procedure T_Color is
  R : Con_Io.Get_Result;
  use type Con_Io.Curs_Mvt;
begin
  Con_Io.Init;
  Con_Io.Set_Background (Con_Io.Brown);

  loop
    Con_Io.Clear;
    for I in Con_Io.Effective_Colors loop
      Con_Io.Move (Con_Io.Colors'Pos(I), 1);
      Con_Io.Put (Con_Io.Effective_Colors'Image(I) );
      Con_Io.Move (Con_Io.Colors'Pos(I), 20);
      Con_Io.Put ("^!@#$%&€*é$ê", Foreground => I, Move => False);
      Con_Io.New_Line;
    end loop;

    Con_Io.Move;
    R := Con_Io.Get;
    exit when R.Mvt /= Con_Io.Refresh;
  end loop;
  Ada.Text_Io.Put_Line ("Exiting in 5 s");
  delay 5.0;
  Con_Io.Destroy;

end T_Color;

