with Ada.Text_Io;
with Con_Io, Argument, X_Mng;
with Pers_Fil, Mesu_Mng, Str_Mng;
procedure Heart is
  Nb_Month : Str_Mng.Offset_Range;

  procedure End_Of_Program is
  begin
    Con_Io.Reset_Term;
  end End_Of_Program;

begin
  begin
    if Argument.Get_Nbre_Arg = 0 then
      Nb_Month := 0;
    elsif Argument.Get_Nbre_Arg = 1 then
      Nb_Month := Str_Mng.Offset_Range'Value (Argument.Get_Parameter);
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Ada.Text_Io.Put_Line ("SYNTAX ERROR. Usage : "
                      & Argument.Get_Program_Name & " [ <nb_month> ]");
      return;
  end;

  Con_Io.Init;

  Pers_Fil.Load;
  Mesu_Mng.List_Mesures (Nb_Month);

  End_Of_Program;

exception
  when others =>
    Con_Io.Bell (3);
    End_Of_Program;
    raise;
end Heart;
