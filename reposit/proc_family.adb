with Dynamic_List, Event_Mng;
package body Proc_Family is

  Init_Done : Boolean := False;

  -- List of running children
  type Child_Rec is record
    Child_Pid : Sys_Calls.Pid;
    Open : Boolean;
    Fd_In, Fd_Out : Sys_Calls.File_Desc;
    Child_Cb  : Death_Callback_Access;
  end record;
  package Child_List_Mng is new Dynamic_List (Child_Rec);
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
    begin
      Search_Pid (Child_List, Child, From_Current => False);
    exception
      when Child_List_Mng.Not_In_List =>
        -- Skip
        return True;
    end;

    -- Get child
    if Child_List_Mng.Get_Position (Child_List) = 1 then
      Child_List_Mng.Get (Child_List, Child, Child_List_Mng.Next);
    else
      Child_List_Mng.Get (Child_List, Child, Child_List_Mng.Prev);
    end if;

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

    -- Close channels if open
    if Child.Open then
      Close (Child.Fd_In);
      Close (Child.Fd_Out);
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
  function Var_Name (Proc_Id : Sys_Calls.Pid; Fd_In : Boolean)
           return String is
    Prefix : constant String := "PFamily_";
  begin
    if Fd_In then
      return Prefix & Image (Natural(Proc_Id)) & "_In";
    else
      return Prefix & Image (Natural(Proc_Id)) & "_Out";
    end if;
  end Var_Name;

  -- Put env Fd in and out variables
  procedure Putenv (Proc_Id : in Sys_Calls.Pid;
                    Fd_In, Fd_Out : in Sys_Calls.File_Desc) is
  begin
    Sys_Calls.Putenv (Var_Name(Proc_Id, True),  Image (Natural(Fd_In)));
    Sys_Calls.Putenv (Var_Name(Proc_Id, False), Image (Natural(Fd_Out)));
  end Putenv;

  -- Spawn a process (with mutation if mutation /= "")
  --  opening com channel if Communication
  -- If Death_Callback is set, it will be called on child's death
  function Spawn (Mutation      : String := "";
                  Communication : Boolean := True;
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
    Child.Open := Communication;
    Child.Child_Cb := Death_Report;

    -- Open Communication if needed
    if not Communication then
      Result := (Ok => True, Open => False, Child_Pid => 1);
    else
      begin
        Sys_Calls.Pipe (Child.Fd_In, Result.Fd_Out);
      exception
        when Sys_Calls.System_Error =>
          return Failure;
      end;
      begin
        Sys_Calls.Pipe (Result.Fd_In, Child.Fd_Out);
      exception
        when Sys_Calls.System_Error =>
          Close (Child.Fd_In);
          Close (Result.Fd_Out);
          return Failure;
      end;
    end if;

    -- Procreate
    begin
      Sys_Calls.Procreate (I_Am_Child, Result.Child_Pid);
    exception
      when Sys_Calls.System_Error =>
        if Communication then
          Close (Child.Fd_In);
          Close (Result.Fd_Out);
          Close (Result.Fd_In);
          Close (Child.Fd_Out);
        end if;
        return Failure;
    end;

    -- Father or child?
    if not I_Am_Child then

      -- Father
      ---------
      if Communication then
        -- Close child Fds, Store Fds to child
        Close (Child.Fd_In);
        Close (Child.Fd_Out);
        Child.Fd_In  := Result.Fd_In;
        Child.Fd_Out := Result.Fd_Out;
      end if;
      Child.Child_Pid := Result.Child_Pid;
      Child_List_Mng.Insert (Child_List, Child);
      -- Success
      return Result;
    end if;

    -- Child 
    --------
    -- Close father fds
    if Communication then
      -- Close father fds
      Close (Result.Fd_In);
      Close (Result.Fd_Out);
    end if;

    if Mutation /= "" then
      -- Export Fds and Mutate
      begin
        Putenv (Result.Child_Pid, Child.Fd_In, Child.Fd_Out);
        Sys_Calls.Mutate (Mutation);
        -- Should not be reached
        raise Constraint_Error;
      exception
        when others =>
          if Communication then
            Close (Child.Fd_In);
            Close (Child.Fd_Out);
          end if;
          return Failure;
      end;
    end if;

    -- No mutation
    if Communication then
      -- Return child fds
      Result.Fd_In := Child.Fd_In;
      Result.Fd_Out := Child.Fd_Out;
    end if;
    
    return Result;
  end Spawn;

  -- After a Spawn with Mutation and Communication, the child can
  -- retreive fds
  -- No_Fd is raised if they cannot be retreived
  procedure Child_Get_Fds (Fd_In, Fd_Out : out Sys_Calls.File_Desc) is
    My_Pid : constant Sys_Calls.Pid := Sys_Calls.Get_Pid;
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Value : String (1 .. Integer'Width);
    Env_Len   : Natural;
  begin
    -- Get Fd_In
    Sys_Calls.Getenv (Var_Name (My_Pid, True), Env_Set, Env_Trunc,
                      Env_Value, Env_Len);
    if not Env_Set or else Env_Trunc or else Env_Len = 0 then
      raise No_Fd;
    end if;
    Fd_In := Sys_Calls.File_Desc'Value (Env_Value(1 .. Env_Len));

    -- Get Fd_Out
    Sys_Calls.Getenv (Var_Name (My_Pid, False), Env_Set, Env_Trunc,
                      Env_Value, Env_Len);
    if not Env_Set or else Env_Trunc or else Env_Len = 0 then
      raise No_Fd;
    end if;
    Fd_Out := Sys_Calls.File_Desc'Value (Env_Value(1 .. Env_Len));
  exception
    when others =>
      raise No_Fd;
  end Child_Get_Fds;

end Proc_Family;

