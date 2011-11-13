with Basic_Proc, Argument, Afpx;
with Pers_Fil, Mesu_Nam, Mesu_Edi, Mesu_Fil;

procedure Ins_Mesu is
  File_Name : Mesu_Nam.File_Name_Str;
  Done : Boolean;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Output (Msg);
    Basic_Proc.Put_Line_Output ("USAGE : " & Argument.Get_Program_Name
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

  Afpx.Release_Descriptor;

exception
  when Mesu_Fil.File_Not_Found_Error =>
    Afpx.Release_Descriptor;
    raise;
  when others =>
    Afpx.Bell (3);
    delay 5.0;
    Afpx.Release_Descriptor;
    raise;
end Ins_Mesu;

