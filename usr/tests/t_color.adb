with Con_Io;
procedure T_Color is
  C : Character;
begin
  Con_Io.Init;
  Con_Io.Set_Background (Con_Io.Brown);

  loop
    Con_Io.Clear;
    for I in Con_Io.Effective_Colors loop
      Con_Io.Move (Con_Io.Colors'Pos(I), 1);
      Con_Io.Put (Con_Io.Effective_Colors'Image(I) );
      Con_Io.Move (Con_Io.Colors'Pos(I), 20);
      Con_Io.Put ("!@#$%^&*", Foreground => I, Move => False);
      Con_Io.New_Line;
    end loop;

    Con_Io.Move;
    C := Con_Io.Get (Echo => False);
    exit when C /= Ascii.Nul;
  end loop;
  delay 5.0;
  Con_Io.Destroy;

end T_Color;

