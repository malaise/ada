with My_Io;
with Argument;

with Common;
with Action;


procedure Mmind is
begin
  declare
    Level : Common.Last_Level_Range;
  begin
    if Argument.Get_Nbre_Arg > 1 then
      raise Constraint_Error;
    end if;
    Level := Common.Last_Level_Range'Value (Argument.Get_Parameter);
    Common.Store_Level (Level);
    Common.Set_Level_To_Stored;
  exception
    when Argument.Argument_Not_Found =>
      Level := Common.Last_Level_Range'First;
      Common.Store_Level (Level);
      Common.Set_Level_To_Stored;
    when Constraint_Error =>
      My_Io.Put_Line (
       "Syntax ERROR. Usage is ""MMIND [ <level> ]"" (level from 3 to 5).");
      return;
  end;

  Action.Init;

  loop
    exit when not Action.Play;
  end loop;

exception
  when Action.No_Mouse =>
    My_Io.Put_Line (
     "Sorry, MOUSE not found.");
    return;

end Mmind;
