with Aski, Con_Io;
with Sok_Input;

procedure T_Input is
  Con : aliased Con_Io.Console;
  Scr : Con_Io.Window;
  Key : Sok_Input.Key_List;
begin

  Con.Open;
  Scr := Con_Io.Get_Screen (Con'Unrestricted_Access);
  Con.Reset_Screen;
  loop
    Key := Sok_Input.Get_Key;
    Scr.Put (" " & Sok_Input.Key_List'Image(Key) & "    " & Aski.Cr);
  end loop;
exception
  when Sok_Input.Break_Requested =>
    Scr.Put ("Break" & "      " & Aski.Cr);
    Sok_Input.End_Of_Program;
end T_Input;
