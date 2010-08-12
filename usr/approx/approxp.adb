with Ada.Text_Io;
with Argument;
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
    Menu1.Main_Screen (Argument.Get_Parameter);
  else
    Menu1.Main_Screen ("");
  end if;
end Approxp;

