with Con_Io;
with Sok_Input;

procedure T_Input is
  Key : Sok_Input.Key_List;
begin

  Con_Io.Init;
  Con_Io.Reset_Term;
  loop
    Key := Sok_Input.Get_Key;
    Con_Io.Put (" " & Sok_Input.Key_List'Image(Key) & "    " & Ascii.Cr);
  end loop;
exception
  when Sok_Input.Break_Requested =>
    Con_Io.Put ("Break" & "      " & Ascii.Cr);
    Sok_Input.End_Of_Program;
end T_Input;
