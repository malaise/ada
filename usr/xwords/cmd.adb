with As.U, Aski, Environ, Many_Strings, Str_Util;
package body Cmd is

  -- Path to Words
  Words_Path_Env_Name : constant String := "WORDS_PATH";
  Words_Path_Init : Boolean := False;
  Words_Path : As.U.Asu_Us;

  -- Outputs of execution
  Output_Flow : aliased Command.Flow_Rec (Command.List);
  Error_Flow : aliased Command.Flow_Rec (Command.Str);
  Exit_Code : Command.Exit_Code_Range;

  -- Words exits with 0 if found, 1 i not found and 2 on error
  Words_Error : constant Command.Exit_Code_Range := 2;

  -- Replace all LineFeeds by spaces
  procedure Normalize (List : in out Res_List) is
    Line : As.U.Asu_Us;
    Moved : Boolean;
  begin
    if List.Is_Empty then
      return;
    end if;
    List.Rewind;
    loop
      -- Replace Lf by space for each Rec
      List.Read (Line, Res_Mng.Dyn_List.Current);
      Line := As.U.Tus (Str_Util.Substit (Line.Image, Aski.Lf_S, " "));
      -- Remove trailing spaces
      Line := As.U.Tus (Str_Util.Strip (Line.Image));
      List.Modify (Line, Moved => Moved);
      exit when not Moved;
    end loop;
    -- Rewind to end
    List.Rewind (True, Res_Mng.Dyn_List.Prev);
  end Normalize;

  -- subtype Line_Type is Asu_Us;
  -- package Res_Mng is newDynamic_List (Line_Type);
  -- subtype Res_List is Res_Mng.Dyn_List.List_Type;
  procedure Exec (Com : in String;
                  Arg : in String;
                  Ok : out Boolean;
                  Res : in out Res_List) is
    Cmd : Many_Strings.Many_String;
    use type As.U.Asu_Us;
  begin
    -- Init path to Words if first call and ENV set
    if not Words_Path_Init then
      if Environ.Is_Set (Words_Path_Env_Name) then
        Words_Path := As.U.Tus (Environ.Getenv (Words_Path_Env_Name) & "/");
      end if;
      Words_Path_Init := True;
    end if;

    -- Spawn
    Logger.Log_Debug ("Executing >" & Words_Path.Image & Com & "<>" & Arg & "<");
    Cmd.Set (Words_Path & Com);
    Cmd.Cat (Arg);
    -- Words needs bash
    Command.Execute (Cmd, True, Command.Both,
        Output_Flow'Access, Error_Flow'Access, Exit_Code, "/bin/bash");

    if Exit_Code = Words_Error then
      Logger.Log_Debug ("Words error");
      if Error_Flow.Str.Is_Null then
        Res.Insert (As.U.Tus ("Words error"));
      else
        Res.Insert (Error_Flow.Str);
      end if;
      Normalize (Res);
      Ok := False;
      return;
    end if;

    -- Set "out" values: Copy list of output lines and append
    --  error string
    Logger.Log_Debug ("Copying result");
    Res.Insert_Copy (Output_Flow.List);
    if not Error_Flow.Str.Is_Null then
      Res.Insert (Error_Flow.Str);
    end if;
    Normalize (Res);
    Ok := True;

  exception
    when Command.Spawn_Error =>
      Logger.Log_Error ("Spwan error");
      Res.Insert (As.U.Tus ("Spawn error"));
      Normalize (Res);
      Ok := False;
  end Exec;

end Cmd;

