with Ada.Text_Io;
with Text_Handler, Argument;
with Menu1;
procedure Approxp is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage " & Argument.Get_Program_Name
                               & " [ <file_name> ]");
  end;

begin
  if Argument.Get_Nbre_Arg > 1 then
    Usage;
    return;
  elsif Argument.Get_Nbre_Arg = 1 then
-- Verdix bug:
-- Global variable Str in Argument body is set when calling
--    Get_Parameter, but Get_Parameter returns the address
--    (dop vector) of Str that we will pass to Menu1.Main_Screen
--    instead of a copy. THIS IS THE BUG.
-- So, when Con_Io calls Argument.Get_Program_Name, this overwrites
--    this Str variable which changes indirectly the Init_File_Name
--    of Main_Screen
-- The workaround is to make a copy of Argument.Get_Parameter
--
-- Original code => bug:
--    Menu1.Main_Screen (Argument.Get_Parameter);
-- New code:
    declare
      File_Name_Txt : Text_Handler.Text(Argument.Max_Len_Arg);
    begin
      Argument.Get_Parameter(File_Name_Txt);
      Menu1.Main_Screen (Text_Handler.Value(File_Name_Txt));
    end;
-- End
  else
    Menu1.Main_Screen ("");
  end if;
end Approxp;

