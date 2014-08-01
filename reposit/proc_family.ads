-- Spawn (fork) a child and mutate (exec) a new program
with Many_Strings, Sys_Calls;
package Proc_Family is

  -- Callback for death report
  subtype Death_Cause_List is Sys_Calls.Death_Cause_List;
  type Death_Rec (Cause : Death_Cause_List := Sys_Calls.Exited) is record
    case Cause is
      when Sys_Calls.Exited =>
        Exited_Pid : Sys_Calls.Pid;
        Exit_Code : Integer;
      when Sys_Calls.Signaled =>
        Signaled_Pid : Sys_Calls.Pid;
        Signal : Sys_Calls.Signal_Range;
    end case;
  end record;
  type Death_Callback_Access is access procedure (Death_Report : in Death_Rec);

  -- Result of Spawn
  -- Pipes to communicate between father and child
  -- These are Fds, in the father, to write to the child Stdin
  --  or to listen from the child Stdout or Stderr
  type Spawn_Result_Rec (Ok : Boolean := True;
                         Open : Boolean := True) is record
    case Ok is
      when True =>
        Child_Pid : Sys_Calls.Pid;
        case Open is
          when True =>
            Fd_In  : Sys_Calls.File_Desc;
            Fd_Out : Sys_Calls.File_Desc;
            Fd_Err : Sys_Calls.File_Desc;
          when False =>
            null;
        end case;
      when False =>
        null;
    end case;
  end record;

  -- Spawn a process (with mutation if mutation /= "")
  --  - if set, Mutation contains the program name and arguments packed
  --    in a Many_Strings.Many_String,
  --  - redirecting standard in/out/err flows if Std_Fds,
  --  - opening communication channel if New_Fds.
  -- If Death_Report is set, it will be called on child's death.
  -- WARNING: Spawn internally sets its own Event_Mng Sig_Child_Callback, so
  --  programs using it shall not set their own Sig_Child callback,
  --  or they shall save it before Spawn and restore it after Death_Report
  --  has been called.
  type Comm_Kind_List is (None, Std_Fds, New_Fds);
  function Spawn (Mutation     : Many_Strings.Many_String
                               := Many_Strings.Empty_String;
                  Comm         : Comm_Kind_List := None;
                  Death_Report : Death_Callback_Access := null)
           return Spawn_Result_Rec;

  -- After a Spawn with Mutation and Comm=New_Fds, the child can
  -- retreive the fds
  -- No_Fd is raised if they cannot be retreived
  procedure Child_Get_Fds (Fd_In, Fd_Out, Fd_Err : out Sys_Calls.File_Desc);
  No_Fd : exception;

end Proc_Family;

