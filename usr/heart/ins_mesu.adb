with My_Io, Argument, Con_Io, Dos;
with Pers_Def, Pers_Fil, Mesu_Nam, Mesu_Edi, Mesu_Fil;

procedure Ins_Mesu is
  File_Name : Mesu_Nam.File_Name_Str;
  Done : Boolean;

  procedure Error (Msg : in String) is
  begin
    My_Io.Put_Line (Msg);
    My_Io.Put_Line ("USAGE : " & Argument.Get_Program_Name
                               & " [ <mesure_file_name> ]");
  end Error;

begin

  if Argument.Get_Nbre_Arg = 1 then

    begin
      File_Name := Argument.Get_Parameter;
    exception
      when others =>
        Error ("Invalid argument.");
        return;
    end;

  elsif Argument.Get_Nbre_Arg = 0 then
    File_Name := (others => ' ');
  else
    Error ("Invalid argument.");
    return;
  end if;


  Pers_Fil.Load;


  Mesu_Edi.Edit (File_Name, Done);

  Con_Io.Reset_Term;

exception
  when Mesu_Fil.File_Not_Found_Error =>
    Con_Io.Reset_Term;
    raise;
  when others =>
    Con_Io.Bell (3);
    delay 5.0;
    Con_Io.Reset_Term;
    raise;
end Ins_Mesu;
