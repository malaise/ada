with Sys_Calls, Proc_Family, Event_Mng, Text_Line, Mutex_Manager, Trace.Loggers,
     Input_Buffer;
package body Command is

  -- The Mutex of exclusive execution
  Mut : Mutex_Manager.Mutex (Mutex_Manager.Simple, True);

  -- Debug option
  Logger : Trace.Loggers.Logger;

  -- Output fd (to distinguish Error flow) and policy
  Mix_Policy : Flow_Mixing_Policies;
  Output_Fd : Sys_Calls.File_Desc;

  -- Result, Text files and Buffers of stdout/stderr to insert complete lines
  type Dscr_Rec is record
    Flow : Flow_Access;
    Done : Boolean;
    File : Text_Line.File_Type;
    Buff : Input_Buffer.Buffer;
  end record;
  Dscrs : array (Boolean) of Dscr_Rec;

  -- The result child execution
  Child_Done : Boolean;
  Child_Result : Proc_Family.Death_Rec;

  -- Aborted by sigterm
  Aborted : Boolean;

  -- Current_Pid
  Current_Pid : Sys_Calls.Pid;

  -- Termination callback
  Prev_Term_Cb : aliased Event_Mng.Sig_Callback;
  procedure Term_Cb is
    use type Event_Mng.Sig_Callback;
  begin
    Logger.Log_Debug ("Sigterm received");
    Aborted := True;
    if Prev_Term_Cb /= null then
      Prev_Term_Cb.all;
    end if;
  end Term_Cb;

  -- The callback for death of child
  procedure Death_Cb (Death_Report : in Proc_Family.Death_Rec) is
    use type Sys_Calls.Death_Cause_List, Sys_Calls.Pid;
  begin
    case Death_Report.Cause is
      when Sys_Calls.Exited =>
        if Death_Report.Exited_Pid /= Current_Pid then
          Logger.Log_Debug ("Death Cb bad exit pid");
          return;
        end if;
      when Sys_Calls.Signaled  =>
        if Death_Report.Signaled_Pid /= Current_Pid then
          Logger.Log_Debug ("Death Cb bad signal pid");
          return;
        end if;
    end case;
    if Logger.Debug_On then
      Logger.Log_Debug ("Death Cb "
                      & Death_Report.Cause'Img);
      if Death_Report.Cause = Sys_Calls.Exited then
        Logger.Log_Debug ("Exit code "
                            & Death_Report.Exit_Code'Img);
      end if;
    end if;
    Child_Result := Death_Report;
    Child_Done := True;
  end Death_Cb;

  -- Add a string to a list flow
  procedure Insert_Line (Is_Output : in Boolean; Line : in String) is
    Loc_Line : As.U.Asu_Us := As.U.Tus (Line);
    Len : constant Natural := Loc_Line.Length;
  begin
    -- Remove trailing Line_Feed
    if Len /= 0
    and then Loc_Line.Element (Len) = Text_Line.Line_Feed_Char then
      Loc_Line.Delete (Len, Len);
    end if;
    Dscrs(Is_Output).Flow.List.Insert (Loc_Line);
  end Insert_Line;
  procedure Insert_Out (Line : in String) is
  begin
    Insert_Line (True, Line);
  end Insert_Out;
  procedure Insert_Err (Line : in String) is
  begin
    Insert_Line (False, Line);
  end Insert_Err;

  -- Reset a flow
  procedure Reset_Flow (Flow : in out Flow_Rec) is
  begin
    if Flow.Kind = Str then
      Flow.Str.Set_Null;
    else
      Flow.List.Delete_List;
    end if;
  end Reset_Flow;

  -- The callback for reading out/err output of child
  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Is_Output : Boolean;
    Line : As.U.Asu_Us;
    Got : Boolean;
    List : Boolean;
    use type Sys_Calls.File_Desc;
  begin
    Logger.Log_Debug ("Fd Cb "
        & (if Fd = Output_Fd then "output" else "error")
        & " flow");
    -- Init Text_Line flow
    Is_Output := Fd = Output_Fd;
    if not Dscrs(Is_Output).File.Is_Open then
      Dscrs(Is_Output).File.Open (Text_Line.In_File, Fd);
    end if;
    Got := False;
    -- Read lines and store in Output/Error_Result
    loop
      Line := Dscrs(Is_Output).File.Get;
      exit when Line.Is_Null;
      -- Got at least an event
      Logger.Log_Debug ("Fd Cb got >" & Line.Image & "<");
      Got := True;
      List := False;
      -- Apply policy to flow
      if Is_Output and then Mix_Policy = None then
        -- Stdout -> Stdout
        Sys_Calls.Put_Output (Line.Image);
      elsif not Is_Output and then Mix_Policy /= Both then
        -- Stderr -> Stderr
        Sys_Calls.Put_Error (Line.Image);
      elsif Dscrs(Is_Output).Flow.Kind = Str then
        -- String
        Dscrs(Is_Output).Flow.Str.Append (Line);
      else
        -- List
        Dscrs(True).Buff.Push (Line.Image);
        List := True;
      end if;
    end loop;
    -- End of this Cb. End of flow?
    if not Got then
      -- We were awaken but nothing to read -> End of flow
      Logger.Log_Debug ("Fd Cb end of flow");
      if List then
        -- Insert Tail
        Line := As.U.Tus (Dscrs(Is_Output).Buff.Tail);
        Dscrs(Is_Output).Buff.Clean;
        if not Line.Is_Null then
          Dscrs(Is_Output).Flow.List.Insert (Line);
        end if;
      end if;
      Dscrs(Is_Output).Done := True;
      Dscrs(Is_Output).File.Close;
      Event_Mng.Del_Fd_Callback (Fd, True);
      Sys_Calls.Close (Fd);
    end if;
    Logger.Log_Debug ("Fd Cb Done");
    return True;
  end Fd_Cb;

  -- Execute Cmd
  procedure Execute (Cmd : in Many_Strings.Many_String;
                     Use_Shell : in Boolean;
                     Mix_Policy : in Flow_Mixing_Policies;
                     Out_Flow : in Flow_Access;
                     Err_Flow : in Flow_Access;
                     Exit_Code : out Exit_Code_Range;
                     Shell : in String := Default_Shell) is
    Cmd_Line : Many_Strings.Many_String;
    Str : As.U.Asu_Us;
    Nb_Substr : Positive;
    Spawn_Result : Proc_Family.Spawn_Result_Rec;
    Prev_Child_Cb : aliased Event_Mng.Sig_Callback;
    Signals_Handled : Boolean;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
    use type Sys_Calls.Death_Cause_List;
  begin
    Mut.Get;
    Logger.Init ("Command");

    -- Init results and 'global' exchange variables
    if Mix_Policy /= None then
      Reset_Flow (Out_Flow.all);
    end if;
    if Mix_Policy = Both then
      Reset_Flow (Err_Flow.all);
    end if;
    Dscrs(True).Done := False;
    Dscrs(True).Flow := Out_Flow;
    Dscrs(True).Buff.Set (Insert_Out'Access);
    Dscrs(False).Done := False;
    Dscrs(False).Flow := Err_Flow;
    Dscrs(False).Buff.Set (Insert_Err'Access);
    Exit_Code := Error;

    -- Ready for sigterm and sigchild
    Aborted := False;
    Signals_Handled := Event_Mng.Are_Signals_Handled;
    Prev_Term_Cb := Event_Mng.Get_Sig_Term_Callback;
    Event_Mng.Set_Sig_Term_Callback (Term_Cb'Access);
    Prev_Child_Cb := Event_Mng.Get_Sig_Child_Callback;

    -- Build command line
    if Use_Shell then
      Cmd_Line.Set (Shell);
      Cmd_Line.Cat ("-c");
      -- Extract substrings and concatenante with spaces
      Nb_Substr := Many_Strings.Nb (Cmd);
      for I in 1 .. Nb_Substr loop
        Str.Append (As.U.Asu_Us'(Many_Strings.Nth (Cmd, I)));
        if I /= Nb_Substr then
          Str.Append (" ");
        end if;
      end loop;
      Cmd_Line.Cat (Str);
    else
      Cmd_Line := Cmd;
    end if;
    -- Spawn
    Logger.Log_Debug ("Spawning >" & Cmd_Line.Image('#') & "<");
    Spawn_Result := Proc_Family.Spawn (Cmd_Line,
                                       Proc_Family.Std_Fds,
                                       Death_Cb'Access);
    if not Spawn_Result.Ok or else not Spawn_Result.Open then
      Logger.Log_Debug ("Spawn error: "
                      & Spawn_Result.Ok'Img
                      & " " & Spawn_Result.Open'Img);
      Mut.Release;
      raise Spawn_Error;
    end if;

    -- Init Cb for out/err flow
    Current_Pid := Spawn_Result.Child_Pid;
    Output_Fd := Spawn_Result.Fd_Out;
    Command.Mix_Policy := Mix_Policy;
    Dummy := Sys_Calls.Set_Blocking (Spawn_Result.Fd_Out, False);
    Dummy := Sys_Calls.Set_Blocking (Spawn_Result.Fd_Err, False);
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Out, True, Fd_Cb'Access);
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Err, True, Fd_Cb'Access);
    Logger.Log_Debug ("Fds are: "
               & Spawn_Result.Fd_Out'Img
               & " and " & Spawn_Result.Fd_Err'Img);

    -- Wait until child ends and no more out/err data
    --  or aborted by sigterm
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when Child_Done and then Dscrs(True).Done and then Dscrs(False).Done;
      if Aborted then
        Mut.Release;
        raise Terminate_Request;
      end if;
    end loop;

    -- Unset Cbs and close
    Logger.Log_Debug ("Cleaning");
    Event_Mng.Set_Sig_Term_Callback (Prev_Term_Cb);
    Event_Mng.Set_Sig_Child_Callback (Prev_Child_Cb);
    -- Restore default signal policy if this was the case initially
    if not Signals_Handled and then Event_Mng.Reset_Default_Signals_Policy then
      -- A signal was received and not handled
      raise Terminate_Request;
    end if;
    Sys_Calls.Close (Spawn_Result.Fd_In);

    -- Set "out" values
    Dscrs(True).Flow := null;
    Dscrs(False).Flow := null;
    Exit_Code := (if Child_Result.Cause = Sys_Calls.Exited then
                    Child_Result.Exit_Code
                  else Error);

    Mut.Release;
  exception
    when others =>
      if Mut.Is_Owner then
        Mut.Release;
      end if;
      raise;

  end Execute;

end Command;

