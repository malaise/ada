-- Expand full path of a path or of current dir
with Basic_Proc, Argument, Directory;
procedure Full_Path is

begin
  -- Check at most one arg not empty
  if Argument.Get_Nbre_Arg > 1 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                                         & " [ <path> ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  if Argument.Get_Nbre_Arg = 0 then
     Basic_Proc.Put_Line_Output (Directory.Make_Full_Path (""));
  else
    Basic_Proc.Put_Line_Output (Directory.Make_Full_Path (
                  Argument.Get_Parameter (Occurence => 1)));
  end if;
end Full_Path;

