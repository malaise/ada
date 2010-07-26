with Ada.Wide_Text_Io;
with Con_Io, My_Io;
procedure T_Key is

  Got : Con_Io.Get_Result;

  use type Con_Io.Curs_Mvt;
begin

  Con_Io.Init;
  Con_Io.Reset_Term;
  Con_Io.Set_Foreground (Con_Io.Color_Of("Black"));
  Con_Io.Set_Background (Con_Io.Color_Of("Light_Grey"));
  Con_Io.Clear;
  Got := (Mvt => Con_Io.Refresh);
  loop
    if Got.Mvt = Con_Io.Refresh then
      -- Refresh
      Con_Io.Clear;
      Con_Io.Move;
      Con_Io.Put_Line ("Exit with Ctrl C");
    end if;
    Got := Con_Io.Get;

    My_Io.Put (Got.Mvt'Img);

    if Got.Mvt = Con_Io.Full then
      My_Io.Put (Integer'(Wide_Character'Pos (Got.Char)), Base => 16);
      Ada.Wide_Text_Io.Put (" >" & Got.Char & "<");
    elsif Got.Mvt = Con_Io.Break then
      exit;
    end if;
    My_Io.New_Line;
  end loop;
  My_Io.New_Line;
end T_Key;

