with Ada.Wide_Text_Io;
with Con_Io, My_Io, Int_Io, Language;
procedure T_Key is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  Got : Con_Io.Get_Result;
  Seq : Con_Io.Unicode_Sequence (1 .. 1);

  Curr_Row : Con_Io.Row_Range := Con_Io.Row_Range_First;
  Clean : constant String (1 .. Con_Io.Def_Col_Last + 1) := (others => ' ');
  Str : String (1 .. 25);
  Start : Positive;

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
      Curr_Row := Con_Io.Row_Range_First;
    end if;
    Got := Screen.Get;

    Screen.Move (Curr_Row, Con_Io.Col_Range_First);
    Screen.Put (Clean);
    Screen.Move (Curr_Row, Con_Io.Col_Range_First);
    Screen.Put (Got.Mvt'Img);

    My_Io.Put (Got.Mvt'Img);

    if Got.Mvt = Con_Io.Full then
      Seq(1) := Got.Char;
      Int_Io.Put (Str, Got.Char, Base => 16);
      for I in reverse Str'Range loop
        if Str(I) = ' ' then
          Start := I;
          exit;
        end if;
      end loop;

      Screen.Put (Str(Start .. Str'Last));
      Screen.Putw (" " & Language.Unicode_To_Wide (Got.Char));
      Screen.Put (" >" & Language.Unicode_To_String (Seq) & "<");

      My_Io.Put (Str(Start .. Str'Last));
      Ada.Wide_Text_Io.Put (" " & Language.Unicode_To_Wide (Got.Char));
      My_Io.Put (" >" & Language.Unicode_To_String (Seq) & "<");
    elsif Got.Mvt = Con_Io.Break then
      Screen.Put (" exiting...");
      Console.Flush;
      My_Io.Put_Line (" exiting...");
      exit;
    end if;
    Curr_Row := Curr_Row + 1;
    if Curr_Row > Screen.Row_Range_Last then
      Screen.Clear;
      Curr_Row := Con_Io.Row_Range_First;
    end if;
    My_Io.New_Line;

  end loop;
  delay 1.0;
  Console.Close;
end T_Key;

