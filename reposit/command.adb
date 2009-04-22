with Ada.Text_Io;
with Sys_Calls, Environ, Proc_Family, Event_Mng, Many_Strings, Text_Line,
     Null_Procedure;
package body Command is

  -- Asu stuff
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  -- Debug option
  Debug_Init : Boolean := False;
  Command_Debug_Name : constant String := "COMMAND_DEBUG";
  Debug : Boolean := False;

  -- Output fd (to distinguish Error flow) and policy
  Output_Fd : Sys_Calls.File_Desc;
  Mix_Error : Boolean;

  -- The result of out/err flow
  Output_Done : Boolean;
  Output_Result : Ada.Strings.Unbounded.Unbounded_String;

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
          return;
        end if;
      when Sys_Calls.Signaled  =>
        if Death_Report.Signaled_Pid /= Current_Pid then
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

  -- The callback for reading out/err output of child
  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    Flow : Text_Line.File_Type;
    Line : Asu_Us;
    use type Asu_Us, Sys_Calls.File_Desc;
  begin
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Fd Cb");
    end if;
    -- Init Text_Line flow
    Flow.Open (Text_Line.In_File, Fd);
    -- Read lines and store in Output_Result
    loop
      Line := Flow.Get;
      exit when Line = Asu_Null;
      -- Store always output flow, mix error flow if requested
      if Fd = Output_Fd or else Mix_Error then
        Ada.Strings.Unbounded.Append (Output_Result, Line);
      else
        -- Error flow on our error flow
        Sys_Calls.Put_Error (Asu_Ts (Line));
      end if;
      if Debug then
        Ada.Text_Io.Put_Line ("Command: Fd Cb got >" & Asu_Ts (Line) & "<");
      end if;
    end loop;
    Flow.Close;
    Output_Done := True;
    return True;
  end Fd_Cb;

  -- Issue '/bin/sh "Cmd"' and set resulting execution flow
  procedure Shell (Cmd : in String;
                   Mix_Error_Flow : in Boolean;
                   Exit_Code : out Integer;
                   Out_Flow : out Ada.Strings.Unbounded.Unbounded_String) is
    Spawn_Result : Proc_Family.Spawn_Result_Rec;
    Prev_Term_Cb : aliased Event_Mng.Sig_Callback;
    use type Sys_Calls.Death_Cause_List;
  begin
    -- Init path to Words if first call and ENV set
    if not Debug_Init then
      Debug := Environ.Is_Yes (Command_Debug_Name);
      Debug_Init := True;
    end if;

    -- Init results
    Output_Done := False;
    Output_Result := Asu_Null;
    Child_Done := False;
    Exit_Code := -1;
    Out_Flow := Asu_Null;

    -- Ready for sigterm
    Aborted := False;
    Prev_Term_Cb := Event_Mng.Get_Sig_Term_Callback;
    Event_Mng.Set_Sig_Term_Callback (Term_Cb'Access);

    -- Spawn
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Spwaning >" & Cmd & "<");
    end if;
    Spawn_Result := Proc_Family.Spawn (
      Many_Strings.Cat (Many_Strings.Cat ("/bin/sh", "-c"),
                        """" & Cmd & """"),
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
    Mix_Error := Mix_Error_Flow;
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Out, True, Fd_Cb'Access);
    Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Err, True, Fd_Cb'Access);

    -- Wait until child end and no more out/err data
    --  or aborted by sigterm
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when Child_Done and then Output_Done;
      if Aborted then
        raise Terminate_Request;
      end if;
    end loop;

    -- Unset Cb of out/err flow and close
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Cleaning Fd Cbs");
    end if;
    Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Out, True);
    Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Err, True);
    Event_Mng.Set_Sig_Term_Callback (Prev_Term_Cb);
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Closing Fds");
    end if;
    Sys_Calls.Close (Spawn_Result.Fd_In);
    Sys_Calls.Close (Spawn_Result.Fd_Out);
    Sys_Calls.Close (Spawn_Result.Fd_Err);

    -- Set "out" values
    if Debug then
      Ada.Text_Io.Put_Line ("Command: Copying result");
    end if;
    Out_Flow := Output_Result;
    Output_Result := Asu_Null;
    if Child_Result.Cause = Sys_Calls.Exited then
      Exit_Code := Child_Result.Exit_Code;
    else
      Exit_Code := -1;
    end if;

  end Shell;

end Command;

