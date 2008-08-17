with Ada.Text_Io;
with Sys_Calls, Proc_Family, Event_Mng, Environ, Many_Strings, Text_Line;
package body Command is

  -- Path to Words
  Words_Path_Env_Name : constant String := "WORDS_PATH";
  Words_Path_Init : Boolean := False;
  Words_Path : Asu_Us;
  -- Debug option
  Xwords_Debug_Name : constant String := "XWORDS_DEBUG";
  Debug : Boolean := False;

  -- The result of out/err flow
  Output_Done : Boolean;
  Output_Result : Res_List;

  -- The result child execution
  Child_Done : Boolean;
  Child_Result : Proc_Family.Death_Rec;

  -- Aborted by sigterm
  Aborted : Boolean;

  -- Termination callback
    procedure Term_Cb is
  begin
    if Debug then
      Ada.Text_Io.Put_Line ("Sigterm received");
    end if;
    Aborted := True;
  end Term_Cb;

  -- The callback for death of child
  procedure Death_Cb (Death_Report : in Proc_Family.Death_Rec) is
    use type Sys_Calls.Death_Cause_List;
  begin
    if Debug then
      Ada.Text_Io.Put_Line ("Death Cb " & Death_Report.Cause'Img);
      if Death_Report.Cause = Sys_Calls.Exited then
        Ada.Text_Io.Put_Line ("Exit code " & Death_Report.Exit_Code'Img);
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
    use type Asu_Us;
  begin
    if Debug then
      Ada.Text_Io.Put_Line ("Fd Cb");
    end if;
    -- Init Text_Line flow
    Flow.Open (Text_Line.In_File, Fd);
    -- Read lines and store in Output_Result
    loop
      Line := Flow.Get;
      exit when Line = Asu_Null;
      -- Remove trailing line feed
      if Asu.Element (Line, Asu.Length (Line)) = Text_Line.Line_Feed_Char then
        Asu.Delete (Line, Asu.Length (Line), Asu.Length (Line));
      end if;
      Output_Result.Insert (Line);
      if Debug then
        Ada.Text_Io.Put_Line ("Fd Cb got >" & Asu_Ts (Line) & "<");
      end if;
    end loop;
    Flow.Close;
    Output_Done := True;
    return True;
  end Fd_Cb;

  -- subtype Line_Type is Asu_Us;
  -- package Res_Mng is newDynamic_List (Line_Type);
  -- subtype Res_List is Res_Mng.Dyn_List.List_Type;
  procedure Exec (Command : in String;
                  Argument : in String;
                  Ok : out Boolean;
                  Result : in out Res_List) is
    Spawn_Result : Proc_Family.Spawn_Result_Rec;
    use type Sys_Calls.Death_Cause_List;
  begin
    -- Init path to Words if first call and ENV set
    if not Words_Path_Init then
      if Environ.Is_Set (Words_Path_Env_Name) then
        Words_Path := Asu_Tus (Environ.Getenv (Words_Path_Env_Name) & "/");
      end if;
      Debug := Environ.Is_Yes (Xwords_Debug_Name);
      Words_Path_Init := True;
    end if;

    -- Init results
    Output_Done := False;
    Output_Result.Delete_List (Deallocate => False);
    Child_Done := False;
    Result.Delete_List (Deallocate => False);
    -- Ready for sigterm
    Event_Mng.Set_Sig_Term_Callback (Term_Cb'Access);

    -- Spawn
    if Debug then
      Ada.Text_Io.Put_Line ("Spwaning >" & Asu_Ts (Words_Path) & Command
                                         & "<>" & Argument & "<");
    end if;
    Spawn_Result := Proc_Family.Spawn (
        Many_Strings.Cat (Asu_Ts (Words_Path) & Command, Argument),
        Proc_Family.Std_Fds,
        Death_Cb'Access);
    if not Spawn_Result.Ok or else not Spawn_Result.Open then
      if Debug then
        Ada.Text_Io.Put_Line ("Spawn error: " & Spawn_Result.Ok'Img
                             & " " & Spawn_Result.Open'Img);
      end if;
      Result.Insert (Asu_Tus ("Spawn error"));
      Ok := False;
      return;
    end if;

    -- Init Cb for out/err flow
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
      Ada.Text_Io.Put_Line ("Cleaning Fd Cbs");
    end if;
    Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Out, True);
    Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Err, True);
    if Debug then
      Ada.Text_Io.Put_Line ("Closing Fds");
    end if;
    Sys_Calls.Close (Spawn_Result.Fd_In);
    Sys_Calls.Close (Spawn_Result.Fd_Out);
    Sys_Calls.Close (Spawn_Result.Fd_Err);

    -- Set "out" values
    if Debug then
      Ada.Text_Io.Put_Line ("Copying result");
    end if;
    Result.Insert_Copy (Output_Result);
    Ok := Child_Result.Cause = Sys_Calls.Exited
    and then Child_Result.Exit_Code = 0;
  end Exec;

end Command;

