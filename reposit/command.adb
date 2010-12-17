with Ada.Text_Io;
with Sys_Calls, Environ, Proc_Family, Event_Mng, Text_Line;
package body Command is

  -- Debug option
  Debug_Init : Boolean := False;
  Command_Debug_Name : constant String := "COMMAND_DEBUG";
  Debug : Boolean := False;

  -- Output fd (to distinguish Error flow) and policy
  Mix_Policy : Flow_Mixing_Policies;
  Output_Fd : Sys_Calls.File_Desc;

  -- The result of out/err flow
  Output_Done : Boolean;
  Output_Result : Flow_Access;
  Error_Done : Boolean;
  Error_Result : Flow_Access;

  -- The result child execution
  Child_Done : Boolean;
  Child_Result : Proc_Family.Death_Rec;

  -- Aborted by sigterm
  Aborted : Boolean;

  -- Current_Pid
  Current_Pid : Sys_Calls.Pid;

  -- Termination callback
  procedure Term_Cb is
  begin
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Sigterm received");
    end if;
    Aborted := True;
  end Term_Cb;

  -- The callback for death of child
  procedure Death_Cb (Death_Report : in Proc_Family.Death_Rec) is
    use type Sys_Calls.Death_Cause_List, Sys_Calls.Pid;
  begin
    case Death_Report.Cause is
      when Sys_Calls.Exited =>
        if Death_Report.Exited_Pid /= Current_Pid then
          if Debug then
            Ada.Text_Io.Put_Line ("Command: Death Cb bad exit pid");
          end if;
          return;
        end if;
      when Sys_Calls.Signaled  =>
        if Death_Report.Signaled_Pid /= Current_Pid then
          if Debug then
            Ada.Text_Io.Put_Line ("Command: Death Cb bad signal pid");
          end if;
          return;
        end if;
    end case;
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Death Cb " & Death_Report.Cause'Img);
      if Death_Report.Cause = Sys_Calls.Exited then
        Ada.Text_Io.Put_Line ("Command: Exit code "
                            & Death_Report.Exit_Code'Img);
      end if;
    end if;
    Child_Result := Death_Report;
    Child_Done := True;
  end Death_Cb;

  -- Add a string of flow to a out flow
  procedure Add_Flow (Flow : in out Flow_Rec; Line : in Asu_Us) is
    Loc_Line : Asu_Us := Line;
    Len : Natural;
  begin
    if Flow.Kind = Str then
      Asu.Append (Flow.Str, Loc_Line);
    else
      Len := Asu.Length (Loc_Line);
      -- Remove trailing Line_Feed
      if Len /= 0
      and then Asu.Element (Loc_Line, Len) = Text_Line.Line_Feed_Char then
        Asu.Delete (Loc_Line, Len, Len);
      end if;
      Flow.List.Insert (Loc_Line);
    end if;
  end Add_Flow;

  -- Reset a flow
  procedure Reset_Flow (Flow : in out Flow_Rec) is
  begin
    if Flow.Kind = Str then
      Flow.Str := Asu_Null;
    else
      Flow.List.Delete_List;
    end if;
  end Reset_Flow;

  -- The callback for reading out/err output of child
  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Flow : Text_Line.File_Type;
    Line : Asu_Us;
    Got : Boolean;
    use type Sys_Calls.File_Desc;
  begin
    if Debug then
      if Fd = Output_Fd then
        if Debug then
          Ada.Text_Io.Put_Line ("Command: Fd Cb output flow");
        end if;
      else
        if Debug then
          Ada.Text_Io.Put_Line ("Command: Fd Cb error flow");
        end if;
      end if;
    end if;
    -- Init Text_Line flow
    Flow.Open (Text_Line.In_File, Fd);
    Got := False;
    -- Read lines and store in Output/Error_Result
    loop
      Line := Flow.Get;
      exit when Asu_Is_Null (Line);
      -- Got at least an event
      Got := True;
      -- Apply policy to flow
      if Fd = Output_Fd then
        if Mix_Policy = None then
          -- Stdout -> Stdout
          Sys_Calls.Put_Output (Asu_Ts (Line));
        else
          -- Stdout -> Output
          Add_Flow (Output_Result.all, Line);
        end if;
      else
        if Mix_Policy /= Both then
          -- Stderr -> Stderr
          Sys_Calls.Put_Error (Asu_Ts (Line));
        else
          -- Stderr -> Error
          Add_Flow (Error_Result.all, Line);
        end if;
      end if;
      if Debug then
        Ada.Text_Io.Put_Line ("Command: Fd Cb got >" & Asu_Ts (Line) & "<");
      end if;
    end loop;
    Flow.Close;
    if not Got then
      if Debug then
        Ada.Text_Io.Put_Line ("Command: Fd Cb end of flow");
      end if;
      -- We were awaken but nothing to read -> End of flow
      if Fd = Output_Fd then
        Output_Done := True;
      else
        Error_Done := True;
      end if;
      Event_Mng.Del_Fd_Callback (Fd, True);
      Sys_Calls.Close (Fd);
    end if;
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Fd Cb Done");
    end if;
    return True;
  end Fd_Cb;

  -- Execute Cmd
  procedure Execute (Cmd : in Many_Strings.Many_String;
                     Use_Sh : in Boolean;
                     Mix_Policy : in Flow_Mixing_Policies;
                     Out_Flow : in Flow_Access;
                     Err_Flow : in Flow_Access;
                     Exit_Code : out Exit_Code_Range) is
    Cmd_Line : Many_Strings.Many_String;
    Str : Asu_Us;
    Nb_Substr : Positive;
    Spawn_Result : Proc_Family.Spawn_Result_Rec;
    Prev_Term_Cb : aliased Event_Mng.Sig_Callback;
    use type Sys_Calls.Death_Cause_List;
  begin
    -- Init Debug
    if not Debug_Init then
      Debug := Environ.Is_Yes (Command_Debug_Name);
      Debug_Init := True;
    end if;

    -- Init results and 'global' exchange variables
    if Mix_Policy /= None then
      Reset_Flow (Out_Flow.all);
    end if;
    if Mix_Policy = Both then
      Reset_Flow (Err_Flow.all);
    end if;
    Output_Done := False;
    Output_Result := Out_Flow;
    Error_Done := False;
    Error_Result := Err_Flow;
    Child_Done := False;
    Exit_Code := Error;

    -- Ready for sigterm
    Aborted := False;
    Prev_Term_Cb := Event_Mng.Get_Sig_Term_Callback;
    Event_Mng.Set_Sig_Term_Callback (Term_Cb'Access);

    -- Build command line
    if Use_Sh then
      Cmd_Line.Set ("/bin/sh");
      Cmd_Line.Cat ("-c");
      -- Extract substrings and concatenante with spaces
      Nb_Substr := Many_Strings.Nb (Cmd);
      for I in 1 .. Nb_Substr loop
        Asu.Append (Str, Asu_Us'(Many_Strings.Nth (Cmd, I)));
        if I /= Nb_Substr then
          Asu.Append (Str, " ");
        end if;
      end loop;
      Cmd_Line.Cat (Str);
    else
      Cmd_Line := Cmd;
    end if;
    -- Spawn
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Spwaning >" & Cmd_Line.Image & "<");
    end if;
    Spawn_Result := Proc_Family.Spawn (Cmd_Line.Image,
                                       Proc_Family.Std_Fds,
                                       Death_Cb'Access);
    if not Spawn_Result.Ok or else not Spawn_Result.Open then
      if Debug then
        Ada.Text_Io.Put_Line ("Command: Spawn error: " & Spawn_Result.Ok'Img
                             & " " & Spawn_Result.Open'Img);
      end if;
      raise Spawn_Error;
    end if;

    -- Init Cb for out/err flow
    Current_Pid := Spawn_Result.Child_Pid;
    Output_Fd := Spawn_Result.Fd_Out;
    Command.Mix_Policy := Mix_Policy;
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Out, True, Fd_Cb'Access);
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Err, True, Fd_Cb'Access);
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Fds are: " & Spawn_Result.Fd_Out'Img
                           & " and " & Spawn_Result.Fd_Err'Img);
    end if;

    -- Wait until child ends and no more out/err data
    --  or aborted by sigterm
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when Child_Done and then Output_Done and then Error_Done;
      if Aborted then
        raise Terminate_Request;
      end if;
    end loop;

    -- Unset Cbs and close
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Cleaning");
    end if;
    Event_Mng.Set_Sig_Term_Callback (Prev_Term_Cb);
    Sys_Calls.Close (Spawn_Result.Fd_In);

    -- Set "out" values
    Output_Result := null;
    Error_Result := null;
    if Child_Result.Cause = Sys_Calls.Exited then
      Exit_Code := Child_Result.Exit_Code;
    else
      Exit_Code := Error;
    end if;

  end Execute;

end Command;

