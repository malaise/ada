with Dynamic_List, Event_Mng, Environ, Trace.Queue;
package body Proc_Family is

  Sig_Child : constant Sys_Calls.Signal_Range := 17;
  No_Dead : constant Sys_Calls.Death_Rec (Sys_Calls.No_Dead)
          := (others => <>);

  -- Debug option
  Logger : Trace.Queue.Queue_Logger;

  -- List of running children
  type Child_Rec is record
    Child_Pid : Sys_Calls.Pid;
    Open : Boolean;
    Fd_In, Fd_Out, Fd_Err : Sys_Calls.File_Desc;
    Child_Cb  : Death_Callback_Access;
  end record;
  package Child_Dyn_List_Mng is new Dynamic_List (Child_Rec);
  package Child_List_Mng renames Child_Dyn_List_Mng.Dyn_List;
  Child_List : Child_List_Mng.List_Type;

  -- Find a child by Pid
  function Same_Pid (Curr, Crit : Child_Rec) return Boolean is
    use type Sys_Calls.Pid;
  begin
    return Curr.Child_Pid = Crit.Child_Pid;
  end Same_Pid;
  function Search_Pid is new Child_List_Mng.Search (Same_Pid);

  -- List of pending death events
  --   when signchild is called before father has registered the child
  package Death_Dyn_List_Mng is new Dynamic_List (Sys_Calls.Death_Rec);
  package Death_List_Mng renames Death_Dyn_List_Mng.Dyn_List;
  Death_List : Death_List_Mng.List_Type;

  -- Find a death by pid
  -- Crit must have cause Exited
  function Same_Pid (Curr, Crit : Sys_Calls.Death_Rec) return Boolean is
    use type Sys_Calls.Death_Info_List, Sys_Calls.Pid;
  begin
    return (case Curr.Cause is
      when Sys_Calls.No_Dead => False,
      when Sys_Calls.Exited => Curr.Exited_Pid = Crit.Exited_Pid,
      when Sys_Calls.Signaled => Curr.Signaled_Pid = Crit.Exited_Pid,
      when Sys_Calls.Stopped => False);
  end Same_Pid;
  function Search_Pid is new Death_List_Mng.Search (Same_Pid);


  -- Close a Fd, no exception
  procedure Close (Fd : in Sys_Calls.File_Desc) is
  begin
    Sys_Calls.Close (Fd);
  exception
    when Sys_Calls.System_Error =>
      null;
  end Close;

  -- Handle the death of a child. Returns False when no more child
  -- If In_Death is no set to No_Dead, then use it, othervise use the one of
  --  Sys_Calls.Next_Dead
  function Handle_Death (In_Death : Sys_Calls.Death_Rec) return Boolean is
    Death_Dscr : Sys_Calls.Death_Rec;
    Child  : Child_Rec;
    Report : Death_Rec;
    Moved  : Boolean;
    use type Sys_Calls.Death_Rec, Sys_Calls.Death_Info_List;
  begin
    -- Get death description
    if In_Death /= No_Dead then
      Logger.Log_Debug ("Handle pending death");
      Death_Dscr := In_Death;
    else
      Logger.Log_Debug ("Handle check new death");
      Death_Dscr := Sys_Calls.Next_Dead;
    end if;

    -- Done when No_Dead, ignore Stopped
    case Death_Dscr.Cause is
      when Sys_Calls.No_Dead =>
        -- Done
        Logger.Log_Debug ("No more death");
        return False;
      when Sys_Calls.Exited =>
        -- Pid to search
        Child.Child_Pid := Death_Dscr.Exited_Pid;
      when Sys_Calls.Signaled =>
        -- Pid to search
        Child.Child_Pid := Death_Dscr.Signaled_Pid;
      when Sys_Calls.Stopped =>
        -- Skip
        Logger.Log_Debug ("Stopped");
        return True;
    end case;

    -- Find child in list
    if not Search_Pid (Child_List, Child,
                       From => Child_List_Mng.Current_Absolute) then
      -- We are called (by signal) before the child insertion
      -- Insert Dead in pending list and skip
      Logger.Log_Debug ("Child not found");
      Death_List.Insert (Death_Dscr);
      return True;
    end if;

    Logger.Log_Debug ("Child found");
    Child_List.Get (Child, Moved => Moved);

    -- Call Cb if set
    if Child.Child_Cb /= null then
      if Death_Dscr.Cause = Sys_Calls.Exited then
        Report := (Sys_Calls.Exited,
                   Death_Dscr.Exited_Pid,
                   Death_Dscr.Exit_Code);
      else
        Report := (Sys_Calls.Signaled,
                   Death_Dscr.Signaled_Pid,
                   Death_Dscr.Signal);
      end if;
      Child.Child_Cb (Report);
    end if;

    -- Done
    Logger.Log_Debug ("Death handled");
    return True;
  end Handle_Death;


  -- The SigChild callback
  procedure Sig_Child_Cb is
  begin
    Logger.Log_Debug ("Sig child");
    loop --## rule line off Loop_While
      -- Handle successive Sys_Calls.Next_dead
      exit when not Handle_Death (No_Dead);
    end loop;
    Logger.Log_Debug ("Sig no more child");
  end Sig_Child_Cb;

  -- Image of a fd, of a pid
  function Image (I : Natural) return String is
    Str : constant String := I'Img;
  begin
    return Str (2 .. Str'Last);
  end Image;

  -- Variable name for a fd
  type Fd_Kind_List is (In_Fd, Out_Fd, Err_Fd);
  function Var_Name (Proc_Id : Sys_Calls.Pid; Fd_Kind : Fd_Kind_List)
           return String is
    Prefix : constant String := "PFamily_";
  begin
    return Prefix & Image (Natural(Proc_Id))
         & (case Fd_Kind is
              when In_Fd  => "_In",
              when Out_Fd => "_Out",
              when Err_Fd => "_Err");
  end Var_Name;

  -- Put env Fd in and out variables
  procedure Putenv (Proc_Id : in Sys_Calls.Pid;
                    Fd_In, Fd_Out, Fd_Err : in Sys_Calls.File_Desc) is
  begin
    Sys_Calls.Setenv (Var_Name(Proc_Id, In_Fd),  Image (Natural(Fd_In)));
    Sys_Calls.Setenv (Var_Name(Proc_Id, Out_Fd), Image (Natural(Fd_Out)));
    Sys_Calls.Setenv (Var_Name(Proc_Id, Err_Fd), Image (Natural(Fd_Err)));
  end Putenv;

  -- Spawn a process (with mutation if mutation /= "")
  --  redirecting standard in/out/err flows if Std_Fds
  --  opening com channel if New_Fds
  -- If Death_Callback is set, it will be called on child's death
  function Spawn (Mutation      : Many_Strings.Many_String
                               := Many_Strings.Empty_String;
                  Comm          : Comm_Kind_List := None;
                  Death_Report  : Death_Callback_Access := null)
           return Spawn_Result_Rec is
    Child : Child_Rec;
    Death : Sys_Calls.Death_Rec;
    Dummy : Boolean;
    Result : Spawn_Result_Rec;
    Failure : constant Spawn_Result_Rec := (Ok => False, Open => False);

    I_Am_Child : Boolean;
  begin
    Logger.Init ("Proc_Family");
    Logger.Log_Debug ("Spawning");
    -- Init if needed
    Event_Mng.Set_Sig_Child_Callback (Sig_Child_Cb'Access);
    -- Init Child rec
    Child.Open := Comm /= None;
    Child.Child_Cb := Death_Report;

    -- Open Communication if needed
    if Comm = None then
      Result := (Ok => True, Open => False, Child_Pid => 1);
    else
      begin
        Sys_Calls.Pipe (Child.Fd_In, Result.Fd_In);
        Sys_Calls.Set_Cloexec (Child.Fd_In, False);
        Sys_Calls.Set_Cloexec (Result.Fd_In, False);
      exception
        when Sys_Calls.System_Error =>
          return Failure;
      end;
      begin
        Sys_Calls.Pipe (Result.Fd_Out, Child.Fd_Out);
        Sys_Calls.Set_Cloexec (Child.Fd_Out, False);
        Sys_Calls.Set_Cloexec (Result.Fd_Out, False);
      exception
        when Sys_Calls.System_Error =>
          Close (Child.Fd_In);
          Close (Result.Fd_In);
          return Failure;
      end;
      begin
        Sys_Calls.Pipe (Result.Fd_Err, Child.Fd_Err);
        Sys_Calls.Set_Cloexec (Child.Fd_Err, False);
        Sys_Calls.Set_Cloexec (Result.Fd_Err, False);
      exception
        when Sys_Calls.System_Error =>
          Close (Child.Fd_In);
          Close (Result.Fd_In);
          Close (Child.Fd_Out);
          Close (Result.Fd_Out);
          return Failure;
      end;
    end if;

    -- Procreate
    begin
      Sys_Calls.Procreate (I_Am_Child, Result.Child_Pid);
    exception
      when Sys_Calls.System_Error =>
        if Comm /= None then
          Close (Child.Fd_In);
          Close (Result.Fd_In);
          Close (Child.Fd_Out);
          Close (Result.Fd_Out);
          Close (Child.Fd_Err);
          Close (Result.Fd_Err);
        end if;
        return Failure;
    end;

    -- Father or child?
    if not I_Am_Child then

      -- Father
      ---------
      if Comm /= None then
        -- Close child Fds, Store Fds to child
        Close (Child.Fd_In);
        Close (Child.Fd_Out);
        Close (Child.Fd_Err);
        Child.Fd_In  := Result.Fd_In;
        Child.Fd_Out := Result.Fd_Out;
        Child.Fd_Err := Result.Fd_Err;
      end if;
      -- Block sigchild while inserting child
      Sys_Calls.Allow_Signal (Sig_Child, False);
      Child.Child_Pid := Result.Child_Pid;
      Child_List.Insert (Child);
      Logger.Log_Debug ("Child inserted");
      Sys_Calls.Allow_Signal (Sig_Child, True);
      -- Check if pending sigchild (occured between Procreate and Block)
      -- Find child by pid in list of pending
      Death := (Cause => Sys_Calls.Exited,
                Exited_Pid => Result.Child_Pid,
                Exit_Code => 0);
      if Search_Pid (Death_List, Death,
                     From => Death_List_Mng.Current_Absolute) then
        -- Handle this pending dead
        Logger.Log_Debug ("Found pending dead");
        Death_List.Get (Death, Moved => Dummy);
        Dummy := Handle_Death (Death);
      else
        -- In case sigchild has been lost
        Logger.Log_Debug ("Check lost child");
        Dummy := Handle_Death (No_Dead);
      end if;

      -- Success
      Logger.Log_Debug ("Spawned");
      return Result;
    end if;

    -- Child
    --------
    -- Close father fds
    if Comm /= None then
      -- Close father fds
      Close (Result.Fd_In);
      Close (Result.Fd_Out);
      Close (Result.Fd_Err);
    end if;

    if not Mutation.Is_Empty then
      begin
        -- Reroute standard Fds is needed: Close and dup2
        if Comm = Std_Fds then
          Child.Fd_In  := Sys_Calls.Dup2 (Child.Fd_In,  Sys_Calls.Stdin);
          Sys_Calls.Set_Cloexec (Child.Fd_In, False);
          Child.Fd_Out := Sys_Calls.Dup2 (Child.Fd_Out, Sys_Calls.Stdout);
          Sys_Calls.Set_Cloexec (Child.Fd_Out, False);
          Child.Fd_Err := Sys_Calls.Dup2 (Child.Fd_Err, Sys_Calls.Stderr);
          Sys_Calls.Set_Cloexec (Child.Fd_Err, False);
        end if;
        -- Export Fds and Mutate
        Putenv (Result.Child_Pid, Child.Fd_In, Child.Fd_Out, Child.Fd_Err);
        Sys_Calls.Mutate (Mutation);
        -- Should not be reached
        raise Constraint_Error;
      exception
        when others =>
          if Comm /= None then
            Close (Child.Fd_In);
            Close (Child.Fd_Out);
            Close (Child.Fd_Err);
          end if;
          -- Suicide
          Sys_Calls.Suicide;
      end;
    end if;

    -- No mutation
    if Comm /= None then
      -- Return child fds
      Result.Fd_In  := Child.Fd_In;
      Result.Fd_Out := Child.Fd_Out;
      Result.Fd_Err := Child.Fd_Err;
    end if;

    return Result;
  end Spawn;

  -- After a Spawn with Mutation and Communication, the child can
  -- retreive fds
  -- No_Fd is raised if they cannot be retreived
  procedure Child_Get_Fds (Fd_In, Fd_Out, Fd_Err : out Sys_Calls.File_Desc) is
    My_Pid : constant Sys_Calls.Pid := Sys_Calls.Get_Pid;
    Int_Val   : Integer;
  begin
    -- Get Fd_In
    Int_Val := Environ.Get_Int(Var_Name (My_Pid, In_Fd), -1);
    if Int_Val = -1 then
      raise No_Fd;
    end if;
    Fd_In := Sys_Calls.File_Desc(Int_Val);

    -- Get Fd_Out
    Int_Val := Environ.Get_Int(Var_Name (My_Pid, Out_Fd), -1);
    if Int_Val = -1 then
      raise No_Fd;
    end if;
    Fd_Out := Sys_Calls.File_Desc(Int_Val);

    -- Get Fd_Err
    Int_Val := Environ.Get_Int(Var_Name (My_Pid, Err_Fd), -1);
    if Int_Val = -1 then
      raise No_Fd;
    end if;
    Fd_Err := Sys_Calls.File_Desc(Int_Val);
  exception
    when others =>
      raise No_Fd;
  end Child_Get_Fds;

end Proc_Family;

