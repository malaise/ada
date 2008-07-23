with Dynamic_List, Event_Mng, Environ;
package body Proc_Family is

  Init_Done : Boolean := False;

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
  function Same_Pid (E1, E2 : Child_Rec) return Boolean is
    use type Sys_Calls.Pid;
  begin
    return E1.Child_Pid = E2.Child_Pid;
  end Same_Pid;
  procedure Search_Pid is new Child_List_Mng.Search (Same_Pid);

  -- Close a Fd, no exception
  procedure Close (Fd : in Sys_Calls.File_Desc) is
  begin
    Sys_Calls.Close (Fd);
  exception
    when Sys_Calls.System_Error =>
      null;
  end Close;

  -- Handle the death of a child. Returns False when no more child
  function Handle_Death return Boolean is
    Death_Dscr : Sys_Calls.Death_Rec;
    Child : Child_Rec;
    Report : Death_Rec;
    Found : Boolean;
    Done  : Boolean;
    use type Sys_Calls.Death_Cause_List;
  begin
    -- Get death description
    Death_Dscr := Sys_Calls.Next_Dead;

    -- Done when No_Dead, ignore Stopped
    case Death_Dscr.Cause is
      when Sys_Calls.No_Dead =>
        -- Done
        return False;
      when Sys_Calls.Exited =>
        -- Pid to search
        Child.Child_Pid := Death_Dscr.Exited_Pid;
      when Sys_Calls.Signaled =>
        -- Pid to search
        Child.Child_Pid := Death_Dscr.Signaled_Pid;
      when Sys_Calls.Stopped =>
        -- Skip
        return True;
    end case;

    -- Find child in list
    Search_Pid (Child_List, Found, Child, From => Child_List_Mng.Absolute);
    if not Found then
      -- Skip
      return True;
    end if;

    Child_List_Mng.Get (Child_List, Child, Done => Done);

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
    return True;
  end Handle_Death;


  -- The SigChild callback
  procedure Sig_Child_Cb is
  begin
    loop
      exit when not Handle_Death;
    end loop;
  end Sig_Child_Cb;

  -- Initialisation
  procedure Init is
  begin
    if Init_Done then
      return;
    end if;
    Event_Mng.Set_Sig_Child_Callback (Sig_Child_Cb'Access);
    Event_Mng.Wait (0);
    Init_Done := True;
  end Init;

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
    case Fd_Kind is
      when In_Fd =>
        return Prefix & Image (Natural(Proc_Id)) & "_In";
      when Out_Fd =>
        return Prefix & Image (Natural(Proc_Id)) & "_Out";
      when Err_Fd =>
        return Prefix & Image (Natural(Proc_Id)) & "_Err";
    end case;
  end Var_Name;

  -- Put env Fd in and out variables
  procedure Putenv (Proc_Id : in Sys_Calls.Pid;
                    Fd_In, Fd_Out, Fd_Err : in Sys_Calls.File_Desc) is
  begin
    Sys_Calls.Putenv (Var_Name(Proc_Id, In_Fd),  Image (Natural(Fd_In)));
    Sys_Calls.Putenv (Var_Name(Proc_Id, Out_Fd), Image (Natural(Fd_Out)));
    Sys_Calls.Putenv (Var_Name(Proc_Id, Err_Fd), Image (Natural(Fd_Err)));
  end Putenv;

  -- Spawn a process (with mutation if mutation /= "")
  --  redirecting standard in/out/err flows if Std_Fds
  --  opening com channel if New_Fds
  -- If Death_Callback is set, it will be called on child's death
  function Spawn (Mutation      : String := "";
                  Comm          : Comm_Kind_List := None;
                  Death_Report  : Death_Callback_Access := null)
           return Spawn_Result_Rec is
    Child : Child_Rec;
    Result : Spawn_Result_Rec;
    Failure : constant Spawn_Result_Rec := (Ok => False, Open => False);

    I_Am_Child : Boolean;
  begin
    -- Init if needed
    Init;
    -- Init Child rec
    Child.Open := Comm /= None;
    Child.Child_Cb := Death_Report;

    -- Open Communication if needed
    if Comm = None then
      Result := (Ok => True, Open => False, Child_Pid => 1);
    else
      begin
        Sys_Calls.Pipe (Child.Fd_In, Result.Fd_In);
      exception
        when Sys_Calls.System_Error =>
          return Failure;
      end;
      begin
        Sys_Calls.Pipe (Result.Fd_Out, Child.Fd_Out);
      exception
        when Sys_Calls.System_Error =>
          Close (Child.Fd_In);
          Close (Result.Fd_In);
          return Failure;
      end;
      begin
        Sys_Calls.Pipe (Result.Fd_Err, Child.Fd_Err);
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
      Child.Child_Pid := Result.Child_Pid;
      Child_List_Mng.Insert (Child_List, Child);
      -- Success
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

    if Mutation /= "" then
      begin
        -- Reroute standard Fds is needed: Close and dup2
        if Comm = Std_Fds then
          Child.Fd_In  := Sys_Calls.Dup2 (Child.Fd_In,  Sys_Calls.Stdin);
          Child.Fd_Out := Sys_Calls.Dup2 (Child.Fd_Out, Sys_Calls.Stdout);
          Child.Fd_Err := Sys_Calls.Dup2 (Child.Fd_Err, Sys_Calls.Stderr);
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
          return Failure;
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

