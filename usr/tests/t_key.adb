with Ada.Wide_Text_Io;
with Con_Io, My_Io, Language;
procedure T_Key is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  Got : Con_Io.Get_Result;
  Seq : Con_Io.Unicode_Sequence (1 .. 1);

  use type Con_Io.Curs_Mvt;
begin

  Console.Open;
  Console.Clear_Screen (Con_Io.Color_Of("Light_Grey"));
  Screen.Set_To_Screen (Console'Unrestricted_Access);
  Screen.Set_Foreground (Con_Io.Color_Of("Black"));
  Got := (Mvt => Con_Io.Refresh);
  loop
    if Got.Mvt = Con_Io.Refresh then
      -- Refresh
      Screen.Clear;
      Screen.Move;
      Screen.Put_Line ("Exit with Ctrl C");
    end if;
    Got := Screen.Get;

    My_Io.Put (Got.Mvt'Img);

    if Got.Mvt = Con_Io.Full then
      My_Io.Put (Got.Char, Base => 16);
      Ada.Wide_Text_Io.Put ( " " & Language.Unicode_To_Wide (Got.Char));
      Seq(1) := Got.Char;
      My_Io.Put ( " >" & Language.Unicode_To_String (Seq) & "<");
    elsif Got.Mvt = Con_Io.Break then
      exit;
    end if;
    My_Io.New_Line;

  end loop;
  My_Io.New_Line;
end T_Key;

