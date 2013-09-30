-- Executes some commands in all the sub-dir in the tree of sub-dir
--  under the current
-- Syntax: pexec [ <options> ] <command> [ { ; <command> } ]
--  each command can contain spaces
with Ada.Exceptions;
with As.U, Directory, Recurs, Sys_Calls, Basic_Proc;
with Cmd;
procedure Pexec is

  No_Action : Boolean;
  No_Name_Of_Dir : Boolean;
  Not_In_Current : Boolean;
  First_Level_Only : Boolean;
  Leaves_Only : Boolean;
  No_Stop_On_Error : Boolean;
  Follow_Links : Boolean;

  Initial_Dir : As.U.Asu_Us;

  -- Restore saved current dir
  procedure Restore is
  begin
    Directory.Change_Current (Initial_Dir.Image);
  exception
    when Directory.Name_Error =>
      Basic_Proc.Put_Line_Error ("Error going back to original directory.");
  end Restore;

  -- What to do in each dir
  function Execute return Boolean is
    Exec_Return : Integer;
  begin
    for I in 1 .. Cmd.Nbre_Commands loop
      if not No_Action then
        Basic_Proc.Put_Line_Output ("--> " & Cmd.Nth_Command (I));
      end if;

      Exec_Return := Sys_Calls.Call_System (Cmd.Nth_Command (I));

      if Exec_Return /= 0 then
        Basic_Proc.Put_Line_Error ("Error executing command "
                      & Cmd.Nth_Command (I));
        return False;
      end if;

    end loop;
    return True;
  end Execute;

begin

  -- Parse command line
  Cmd.Parse (No_Action, No_Name_Of_Dir, Not_In_Current, First_Level_Only,
                 Leaves_Only, No_Stop_On_Error, Follow_Links);

  -- Save current dir, Recurs, restore current dir
  Initial_Dir := As.U.Tus (Directory.Get_Current);
  Recurs (Do_In_Dir        => Execute'Access,
          Name_Of_Dir      => not No_Name_Of_Dir,
          In_Current       => not Not_In_Current,
          First_Level_Only => First_Level_Only,
          Leaves_Only      => Leaves_Only,
          Stop_On_Error    => not No_Stop_On_Error,
          Follow_Links     => Follow_Links);
  Restore;

exception
  when Cmd.No_Command =>
    Basic_Proc.New_Line_Error;
    Basic_Proc.Put_Line_Error ("No command line to pexec.");
    Cmd.Print_Usage;
    Restore;
    Basic_Proc.Set_Error_Exit_Code;
  when Cmd.Help =>
    Cmd.Print_Usage;
    Restore;
  when Err:others =>
    Basic_Proc.New_Line_Error;
    Basic_Proc.Put_Line_Error ("Unexpected error "
                             & Ada.Exceptions.Exception_Name (Err) & ".");
    Cmd.Print_Usage;
    Restore;
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Pexec;

