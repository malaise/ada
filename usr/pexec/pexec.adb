-- executes some commands in all the sub-dir in the tree of sub-dir
--  under the current
-- Syntax: pexec command [ { ;command } ]
--  each command can contain spaces
with Ada.Exceptions;
with Directory;
with As.U; use As.U;
with Recurs, Sys_Calls, Basic_Proc;
with Command;
procedure Pexec is

  No_Action : Boolean;
  No_Name_Of_Dir : Boolean;
  Not_In_Current : Boolean;
  First_Level_Only : Boolean;
  Leaves_Only : Boolean;
  No_Stop_On_Error : Boolean;
  Follow_Links : Boolean;

  Initial_Dir : Asu_Us;

  -- Restore saved current dir
  procedure Restore is
  begin
    Directory.Change_Current (Asu_Ts(Initial_Dir));
  exception
    when Directory.Name_Error =>
      Basic_Proc.Put_Line_Error ("Error going back to original directory.");
  end Restore;

  -- What to do in each dir
  function Execute return Boolean is
    Exec_Return : Integer;
  begin
    for I in 1 .. Command.Nbre_Commands loop
      if not No_Action then
        Basic_Proc.Put_Line_Output ("--> " & Command.Nth_Command (I));
      end if;

      Exec_Return := Sys_Calls.Call_System (Command.Nth_Command (I));

      if Exec_Return /= 0 then
        Basic_Proc.Put_Line_Error ("Error executing command "
                      & Command.Nth_Command (I));
        return False;
      end if;

    end loop;
    return True;
  end Execute;

  procedure My_Recurs is new Recurs (Do_In_Dir => Execute);

begin

  -- Parse command line
  Command.Parse (No_Action, No_Name_Of_Dir, Not_In_Current, First_Level_Only,
                 Leaves_Only, No_Stop_On_Error, Follow_Links);

  -- Save current dir, Recurs, restore current dir
  Initial_Dir := Asu_Tus (Directory.Get_Current);
  My_Recurs (Name_Of_Dir      => not No_Name_Of_Dir,
             In_Current       => not Not_In_Current,
             First_Level_Only => First_Level_Only,
             Leaves_Only      => Leaves_Only,
             Stop_On_Error    => not No_Stop_On_Error,
             Follow_Links     => Follow_Links);
  Restore;

exception
  when Command.No_Command =>
    Basic_Proc.New_Line_Error;
    Basic_Proc.Put_Line_Error ("No command line to pexec.");
    Command.Print_Usage;
    Restore;
    Basic_Proc.Set_Error_Exit_Code;
  when Command.Too_Many_Commands =>
    Basic_Proc.New_Line_Error;
    Basic_Proc.Put_Line_Error ("Too many commands to pexec.");
    Command.Print_Usage;
    Restore;
    Basic_Proc.Set_Error_Exit_Code;
  when Command.Help =>
    Command.Print_Usage;
    Restore;
  when Err:others =>
    Basic_Proc.New_Line_Error;
    Basic_Proc.Put_Line_Error ("Unexpected error "
                             & Ada.Exceptions.Exception_Name (Err) & ".");
    Command.Print_Usage;
    Restore;
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Pexec;

