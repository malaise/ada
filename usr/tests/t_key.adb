with Ada.Wide_Text_Io;
with Con_Io, Basic_Proc, Int_Image16, Language;
procedure T_Key is
  Console : aliased Con_Io.Console;
  Screen : Con_Io.Window;

  Got : Con_Io.Get_Result;
  Seq : Con_Io.Unicode_Sequence (1 .. 1);
  function Unicode_Image16 is new Int_Image16 (Con_Io.Unicode_Number);

  Curr_Row : Con_Io.Row_Range := Con_Io.Row_Range_First;
  Next_Row : Con_Io.Row_Range;
  Clean : constant String (1 .. Con_Io.Def_Col_Last + 1) := (others => ' ');

  use type Con_Io.Curs_Mvt;
begin

  Console.Open;
  Console.Clear_Screen (Con_Io.Color_Of("Light_Grey"));
  Screen.Set_To_Screen (Console'Unrestricted_Access);
  Screen.Set_Foreground (Con_Io.Color_Of("Black"));
  Got := (Mvt => Con_Io.Refresh);

  loop
    -- Get input
    Got := Screen.Get;

    -- Clear and reset on refresh
    if Got.Mvt = Con_Io.Refresh then
      -- Refresh
      Screen.Clear;
      Curr_Row := Con_Io.Row_Range_First;
    end if;

    -- Clean next row
    Next_Row := Curr_Row + 1;
    if Next_Row > Screen.Row_Range_Last then
      Next_Row := Con_Io.Row_Range_First;
    end if;
    Screen.Move (Next_Row, Con_Io.Col_Range_First);
    Screen.Put (Clean);

    -- Put input
    Screen.Move (Curr_Row, Con_Io.Col_Range_First);
    Screen.Put (Got.Mvt'Img);

    Basic_Proc.Put_Output (Got.Mvt'Img);

    if Got.Mvt = Con_Io.Full then
      Seq(1) := Got.Char;

      Screen.Put (" " & Unicode_Image16(Got.Char));
      Screen.Putw (" " & Language.Unicode_To_Wide (Got.Char));
      Screen.Put (" >" & Language.Unicode_To_String (Seq) & "<");

      Basic_Proc.Put_Output (" " & Unicode_Image16(Got.Char));
      Ada.Wide_Text_Io.Put (" " & Language.Unicode_To_Wide (Got.Char));
      Basic_Proc.Put_Output (" >" & Language.Unicode_To_String (Seq) & "<");
    elsif Got.Mvt = Con_Io.Break then
      Screen.Put (" exiting...");
      Console.Flush;
      Basic_Proc.Put_Line_Output (" exiting...");
      exit;
    end if;
    Curr_Row := Next_Row;
    Basic_Proc.New_Line_Output;

  end loop;
  delay 1.0;
  Console.Close;
end T_Key;

