-- executes some commands in all the sub-dir in the tree of sub-dir
--  under the current
-- Syntax: pexec command [ { ;command } ]
--  each command can contain spaces
with Directory;
with My_Io, Text_Handler, Recurs, Sys_Calls;
with Command;
procedure Pexec is

  No_Action : Boolean;
  No_Name_Of_Dir : Boolean;
  Not_In_Current : Boolean;
  First_Level_Only : Boolean;
  Leaves_Only : Boolean;
  No_Stop_On_Error : Boolean;
  Follow_Links : Boolean;

  Initial_Dir : Text_Handler.Text (Directory.Max_Dir_Name_Len);

  procedure Restore is
  begin
    Directory.Change_Current (Text_Handler.Value (Initial_Dir));
  exception
    when Directory.Name_Error =>
      My_Io.Put_Line ("Error going back to original directory.");
  end Restore;


  function Execute return Boolean is
    Exec_Return : Integer;
    To_Call : Text_Handler.Text (1024);
  begin
    for I in 1 .. Command.Nbre_Commands loop
      Text_Handler.Set (To_Call, Command.Nth_Command (I));
      if not No_Action then
        My_Io.Put_Line("--> " & Text_Handler.Value (To_Call));
      end if;

      Exec_Return := Sys_Calls.Call_System (Text_Handler.Value (To_Call));

      if Exec_Return /= 0 then
        My_Io.Put_Line ("Error executing command "
                      & Text_Handler.Value (To_Call) );
        return False;
      end if;

    end loop;
    return True;
  end Execute;

  procedure My_Recurs is new Recurs (Do_In_Dir => Execute);

begin
  Directory.Get_Current (Initial_Dir);

  Command.Parse (No_Action, No_Name_Of_Dir, Not_In_Current, First_Level_Only,
                 Leaves_Only, No_Stop_On_Error, Follow_Links);

  My_Recurs (Name_Of_Dir      => not No_Name_Of_Dir,
             In_Current       => not Not_In_Current,
             First_Level_Only => First_Level_Only,
             Leaves_Only      => Leaves_Only,
             Stop_On_Error    => not No_Stop_On_Error,
             Follow_Links     => Follow_Links);

  Restore;
exception
  when Command.No_Command =>
    My_Io.New_Line;
    My_Io.Put_Line ("No command line to pexec.");
    Command.Print_Usage;
    Restore;
  when others =>
    My_Io.New_Line;
    My_Io.Put_Line ("Unexpected error.");
    Command.Print_Usage;
    Restore;
    raise;
end Pexec;

