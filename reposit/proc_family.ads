with Many_Strings, Sys_Calls;
package Proc_Family is


  -- Callback for death report
  subtype Death_Cause_List is Sys_Calls.Death_Cause_List
          range Sys_Calls.Exited .. Sys_Calls.Signaled;
  type Death_Rec (Cause : Death_Cause_List := Sys_Calls.Exited) is record
    case Cause is
      when Sys_Calls.Exited =>
        Exited_Pid : Sys_Calls.Pid;
        Exit_Code : Integer;
      when Sys_Calls.Signaled =>
        Signaled_Pid : Sys_Calls.Pid;
        Signal : Positive;
    end case;
  end record;
  type Death_Callback_Access is access procedure (Death_Report : in Death_Rec);

  -- Result of Spawn
  -- Pipes to communicate between father and son
  type Spawn_Result_Rec (Ok : Boolean := True;
                         Open : Boolean := True) is record
    case Ok is
      when True =>
        Child_Pid : Sys_Calls.Pid;
        case Open is
          when True =>
            Fd_In  : Sys_Calls.File_Desc;
            Fd_Out : Sys_Calls.File_Desc;
          when False =>
            null;
        end case;
      when False =>
        null;
    end case;
  end record;
 
  -- Spawn a process (with mutation if mutation /= "")
  --  opening com channel if Communication
  -- If Death_Callback is set, it will be called on child's death
  function Spawn (Mutation      : String := "";
                  Communication : Boolean := True;
                  Death_Report  : Death_Callback_Access := null)
           return Spawn_Result_Rec;

  -- After a Spawn with Mutation and Communication, the child can
  -- retreive fds
  -- No_Fd is raised if they cannot be retreived
  procedure Child_Get_Fds (Fd_In, Fd_Out : out Sys_Calls.File_Desc);
  No_Fd : exception;

end Proc_Family;

