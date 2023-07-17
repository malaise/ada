-- Test Proc_Family communication (through pipes)
-- This is the father of t_proc_child
with Sys_Calls, Proc_Family, Many_Strings, Argument, Event_Mng;
-- Note on lsof result
-- Father: 0r, 1w, 2w: stdin, stdout and stderr
--         3r, 4r: Fd1 and Fd2 on Program_Name
--         5r, 6w: Event_Mng pipe for dummy signal (see wait_evt.c)
--         8w, 9r, 11r: the pipes to/from in, out err of child
--                      (7r, 10w and 12w are closed after procreation)
-- Child: 0r, 1w, 2w: stdin, stdout and stderr, dups of 7r, 10w and 12w
--        3r, 5w: Event_Mng pipe (Fd1=3r of father has been closed on exec,
--                 so it is reused)
--        4r: Fd2 of father that has close on exec
--        7r, 10w, 12w: the pipes from/to father (8w, 9r, 11r are closed
--                      after procreation)

procedure T_Proc_Father is

  Str : Many_Strings.Many_String;
  Spawn_Result : Proc_Family.Spawn_Result_Rec;

  Child_Dead : Boolean := False;
  procedure Death_Cb (Death_Report : in Proc_Family.Death_Rec) is
    use type Sys_Calls.Death_Cause_List;
  begin
    if Death_Report.Cause = Sys_Calls.Exited then
      Sys_Calls.Put_Line_Output (
          "Father: child pid " & Death_Report.Exited_Pid'Img
        & " has exited with code " & Death_Report.Exit_Code'Img);
    else
      Sys_Calls.Put_Line_Output (
          "Father: child pid " & Death_Report.Signaled_Pid'Img
        & " has exited on signal " & Death_Report.Signal'Img);
    end if;
    Child_Dead := True;
  end Death_Cb;

  Result : Integer := 1;
  Done : Boolean := False;
  procedure Term_Cb is
  begin
    Sys_Calls.Put_Line_Output ("Father: aborted by user");
    Done := True;
  end Term_Cb;

  Child_Disconnected : Boolean := False;
  Nb_Failures : Natural := 0;
  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Unused_Read : in Boolean) return Boolean is
    Buf : String (1 .. 1024);
    Res : Natural;
    Reply : constant String := "F2C";
    use type Sys_Calls.File_Desc;
  begin
    if Fd /= Spawn_Result.Fd_Out then
      Sys_Calls.Put_Line_Output ("Father: Fd Cb on invalid Fd");
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Buf'Address, Buf'Length);
    if Res = 0 then
      Sys_Calls.Put_Line_Output ("Father: Read 0");
      Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Out, True);
      Child_Disconnected := True;
      return True;
    end if;
    Sys_Calls.Put_Line_Output ("Father: Read >" & Buf(1 .. Res) & "<");
    Buf(1 .. Reply'Length) := Reply;
    begin
      -- Reply "F2C"
      Res := Sys_Calls.Write (Spawn_Result.Fd_In, Buf'Address, Reply'Length);
      if Res /= Reply'Length then
        Sys_Calls.Put_Line_Output ("Father: Can write only "
                              & Res'Img & " bytes on In fd");
        Result := 1;
      else
        Result := 0;
      end if;
    exception
      when Sys_Calls.System_Error =>
        -- Maybe child has died, allow one failure
        Sys_Calls.Put_Line_Output ("Father: Failed to write on In fd");
        Result := Nb_Failures;
        Nb_Failures := Nb_Failures + 1;
    end;
    return False;
  end Fd_Cb;

  I_Am_Father : Boolean;

  Fd1, Fd2 : Sys_Calls.File_Desc;

  Event : Boolean;

begin
  -- Fd1 should be closed on child but not fd2
  Fd1 := Sys_Calls.Open (Argument.Get_Program_Name, Sys_Calls.In_File);
  Fd2 := Sys_Calls.Open (Argument.Get_Program_Name, Sys_Calls.In_File);
  Sys_Calls.Set_Cloexec (Fd2, False);
  Sys_Calls.Put_Line_Output ("Father: " & Fd1'Img
       & " should be closed in child but not " & Fd2'Img & ".");

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Str.Cat (Argument.Get_Parameter (Occurence => I));
  end loop;


  Spawn_Result := Proc_Family.Spawn (Str,
                                     Proc_Family.Std_Fds,
                                     Death_Cb'Unrestricted_Access);
  if not Spawn_Result.Ok then
    Sys_Calls.Put_Line_Output ("Father: Spawn result NOT OK");
    return;
  end if;

  declare
    use type Sys_Calls.Pid;
  begin
    I_Am_Father := Sys_Calls.Get_Pid /= Spawn_Result.Child_Pid;
    if I_Am_Father then
      Sys_Calls.Put_Line_Output ("Father: I am father of "
                          & Spawn_Result.Child_Pid'Img);
    else
      Sys_Calls.Put_Line_Output ("Father: I am child "
                           & Spawn_Result.Child_Pid'Img);
    end if;
  end;

  if not Spawn_Result.Open then
    Sys_Calls.Put_Line_Output ("Father: Spawn result NOT OPEN");
    return;
  end if;

  Sys_Calls.Put_Line_Output ("Father: Fds are " & Spawn_Result.Fd_In'Img
                     & ", " & Spawn_Result.Fd_Out'Img
                     & " and " & Spawn_Result.Fd_Err'Img);

  if not I_Am_Father then
    Sys_Calls.Put_Line_Output ("Father: Child exiting");
    return;
  end if;

  Event_Mng.Set_Sig_Term_Callback (Term_Cb'Unrestricted_Access);
  Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Out, True,
                             Fd_Cb'Unrestricted_Access);

  loop
    Event := Event_Mng.Wait (3_000);
    -- Sigterm or child completed (dead detected or timeout)
    exit when Done
              or else ( (Child_Dead or else not Event)
                         and then Child_Disconnected);
  end loop;

  Sys_Calls.Set_Exit_Code (Result);
end T_Proc_Father;

