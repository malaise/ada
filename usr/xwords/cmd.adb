with Ada.Text_Io, Ada.Characters.Latin_1;
with Environ, Many_Strings, String_Mng;
with As.U; use As.U;
package body Cmd is

  -- Path to Words
  Words_Path_Env_Name : constant String := "WORDS_PATH";
  Words_Path_Init : Boolean := False;
  Words_Path : Asu_Us;
  -- Debug option
  Xwords_Debug_Name : constant String := "XWORDS_DEBUG";
  Debug : Boolean := False;

  -- Outputs of execution
  Output_Flow : aliased Command.Flow_Rec (Command.List);
  Error_Flow : aliased Command.Flow_Rec (Command.Str);
  Exit_Code : Command.Exit_Code_Range;

  -- Replace all LineFeeds by spaces
  procedure Normalize (List : in out Res_List) is
    Line : Asu_Us;
    Moved : Boolean;
  begin
    if List.Is_Empty then
      return;
    end if;
    List.Rewind;
    loop
      -- Replace for est Rec
      List.Read (Line, Res_Mng.Dyn_List.Current);
      Line := Asu_Tus (String_Mng.Replace (
                Asu_Ts (Line), Ada.Characters.Latin_1.Lf & "", " "));
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
    use type Asu_Us;
  begin
    -- Init path to Words if first call and ENV set
    if not Words_Path_Init then
      if Environ.Is_Set (Words_Path_Env_Name) then
        Words_Path := Asu_Tus (Environ.Getenv (Words_Path_Env_Name) & "/");
      end if;
      Debug := Environ.Is_Yes (Xwords_Debug_Name);
      Words_Path_Init := True;
    end if;

    -- Spawn
    if Debug then
      Ada.Text_Io.Put_Line ("Executing >" & Asu_Ts (Words_Path) & Com
                                         & "<>" & Arg & "<");
    end if;
    Command.Execute (
        Many_Strings.Cat (Asu_Ts (Words_Path) & Com, Arg),
        False, Command.Both,
        Output_Flow'Access, Error_Flow'Access, Exit_Code);

    if Exit_Code = Command.Error then
      if Debug then
        Ada.Text_Io.Put_Line ("Execute error");
      end if;
      Res.Insert (Asu_Tus ("Spawn error"));
      Ok := False;
      return;
    end if;

    -- Set "out" values: Copy list of output lines and append
    --  error string
    if Debug then
      Ada.Text_Io.Put_Line ("Copying result");
    end if;
    Res.Insert_Copy (Output_Flow.List);
    if Error_Flow.Str /= Asu_Null then
      Res.Insert (Error_Flow.Str);
    end if;
    Normalize (Res);
    Ok := Exit_Code = 0;

    -- Exec failed leads to Exit code 1 with no output
    if Exit_Code = 1 and then Res.Is_Empty then
      if Debug then
        Ada.Text_Io.Put_Line ("Adding spwan error");
      end if;
      Res.Insert (Asu_Tus ("Spawn error"));
    end if;
  end Exec;

end Cmd;

