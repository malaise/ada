with Ada.Exceptions, Ada.Characters.Latin_1;
with Event_Mng, Async_Stdin, Rnd, Argument, Basic_Proc, Parser, Pattern;
with Dictio_Lib;
procedure T_Dictio is

  Init : Boolean := False;
  Verbose : Boolean := False;
  Saved_State : Dictio_Lib.Dictio_State_List := Dictio_Lib.Unavailable;

  -- Signal received (while loading)
  Sig : Boolean := False;
  procedure Sig_Cb is
  begin
    Async_Stdin.Put_Line_Out ("CLIENT: Aborted");
    Sig := True;
  end Sig_Cb;

  -- Parsing
  procedure Put_Help is
    procedure P (Str : in String) renames Async_Stdin.Put_Line_Out;
  begin
    P ("Comands are:");
    P (" set    <name>    [ <data> ]        alias  <name>    [ <of_name> ]");
    P (" get    [ alias ] [ <name> ]");
    P (" notify [ alias ] [ <pattern> ]     cancel [ alias ] [ <pattern> ]");
    P (" add    <host>                      del    <host>");
    P (" help                               status");
    P (" quit | exit | q");
  end Put_Help;

  Rule : Pattern.Rule_No;

  Id_Set    : constant Pattern.Pattern_Id := 010;
  Id_Alias  : constant Pattern.Pattern_Id := 020;
  Id_Get    : constant Pattern.Pattern_Id := 100;
  Id_Notify : constant Pattern.Pattern_Id := 110;
  Id_Cancel : constant Pattern.Pattern_Id := 120;
  Id_Add    : constant Pattern.Pattern_Id := 200;
  Id_Del    : constant Pattern.Pattern_Id := 210;
  Id_Help   : constant Pattern.Pattern_Id := 300;
  Id_Status : constant Pattern.Pattern_Id := 310;
  Id_Exit   : constant Pattern.Pattern_Id := 320;
  Id_Quit   : constant Pattern.Pattern_Id := 321;
  Id_Q      : constant Pattern.Pattern_Id := 322;
  Id_Error  : constant Pattern.Pattern_Id := 999;

  function Com_Fix_Opt (Rule : in Pattern.Rule_No;
                        Id   : in Pattern.Pattern_Id;
                        Nb_Match : in Natural;
                        Iter : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Rule, Nb_Match);
    Arg1 : constant String := Parser.Current_Word (Iter);
    Arg2 : constant String := Parser.Next_Word (Iter);
    Arg3 : constant String := Parser.Next_Word (Iter);
    use type Pattern.Pattern_Id;
  begin
    -- Set/Alias <name> [ <data/of_name> ]
    if Arg1 = "" or else Arg3 /= "" then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
      return False;
    end if;
    if Id = Id_Set then
      Dictio_Lib.Set (Arg1, Arg2);
    elsif Id = Id_Alias then
      Dictio_Lib.Set_Alias (Arg1, Arg2);
    else
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
    end if;
    return False;
  end Com_Fix_Opt;

  function Com_Opt_Opt (Rule : in Pattern.Rule_No;
                        Id   : in Pattern.Pattern_Id;
                        Nb_Match : in Natural;
                        Iter : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Rule);
    Arg1 : constant String := Parser.Current_Word (Iter);
    Arg2 : constant String := Parser.Next_Word (Iter);
    Alias_Name : constant String := "alias";
    Alias : Boolean := Nb_Match = 2;
    Name_Is_Alias : Boolean := False;

    function Get_Name return String is
    begin
      if Alias then
        return Arg1;
      elsif Name_Is_Alias then
        return Alias_Name;
      else
        return Arg1;
      end if;
    end Get_Name;

    use type Pattern.Pattern_Id;
  begin
    -- Get/Notify/Cancel [ alias ] [ <name> ]
    -- If <name> is not set then name is "alias"
    if (not Alias and then Arg1 = "") or else Arg2 /= "" then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
      return False;
    end if;

    -- Find our way in optionnal args:
    -- alias      -> name is "alias"
    -- alias name -> alias name
    -- name       -> name
    if Alias and then Arg1 = "" then
      -- <command> alias
      Alias := False;
      Name_Is_Alias := True;
    else
      -- <command> alias <name> or <command> <name> <data>
      Name_Is_Alias := False;
    end if;

    if Id = Id_Get then
      if not Alias then
        Async_Stdin.Put_Line_Out ("CLIENT: got >"
                            & Dictio_Lib.Get (Get_Name) & "<");
      else
        Async_Stdin.Put_Line_Out ("CLIENT: got alias >"
                            & Dictio_Lib.Get_Alias (Get_Name) & "<");
      end if;
    elsif Id = Id_Notify then
      Dictio_Lib.Notify (Get_Name, not Alias, True);
    elsif Id = Id_Cancel then
      Dictio_Lib.Notify (Get_Name, not Alias, False);
    else
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
    end if;
    return False;
  end Com_Opt_Opt;

  function Com_Fix (Rule : in Pattern.Rule_No;
                    Id   : in Pattern.Pattern_Id;
                    Nb_Match : in Natural;
                    Iter : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Rule, Nb_Match);
    Arg1 : constant String := Parser.Current_Word (Iter);
    Arg2 : constant String := Parser.Next_Word (Iter);
    use type Pattern.Pattern_Id;
  begin
    -- Add/Del <host>
    if Arg1 = "" or else Arg2 /= "" then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
      return False;
    end if;
    if Id = Id_Add then
      Dictio_Lib.Add_Host (Arg1);
    elsif Id = Id_Del then
      Dictio_Lib.Del_Host (Arg1);
    else
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
    end if;
    return False;
  end Com_Fix;

  function Com (Rule : in Pattern.Rule_No;
                Id   : in Pattern.Pattern_Id;
                Nb_Match : in Natural;
                Iter : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Rule, Nb_Match);
    Arg1 : constant String := Parser.Current_Word (Iter);
    use type Pattern.Pattern_Id;
  begin
    -- Help/Status/Exit/Quit or error
    if Arg1 /= "" then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
      return False;
    end if;
    if Id = Id_Help then
      Put_Help;
    elsif Id = Id_Status then
      Async_Stdin.Put_Line_Out("CLIENT: Dictio status is " & Saved_State'Img);
    elsif Id = Id_Quit then
      Async_Stdin.Put_Line_Out ("CLIENT: Exiting");
      Event_Mng.Send_Dummy_Signal;
      return True;
    elsif Id = Id_Error then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
    else
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
    end if;
    return False;
  end Com;

  procedure Init_Parsing is
  begin
    Rule := Pattern.Get_Free_Rule;
    Pattern.Set (Rule, Id_Set, "set",
                 Com_Fix_Opt'Unrestricted_Access);
    Pattern.Set (Rule, Id_Alias, "alias",
                 Com_Fix_Opt'Unrestricted_Access);
    Pattern.Set (Rule, Id_Get, "get [ alias ]",
                 Com_Opt_Opt'Unrestricted_Access);
    Pattern.Set (Rule, Id_Notify, "notify [ alias ]",
                 Com_Opt_Opt'Unrestricted_Access);
    Pattern.Set (Rule, Id_Cancel, "cancel [ alias ]",
                 Com_Opt_Opt'Unrestricted_Access);
    Pattern.Set (Rule, Id_Add, "add",
                 Com_Fix'Unrestricted_Access);
    Pattern.Set (Rule, Id_Del, "del",
                 Com_Fix'Unrestricted_Access);
    Pattern.Set (Rule, Id_Help, "help",
                 Com'Unrestricted_Access);
    Pattern.Set (Rule, Id_Status, "status",
                 Com'Unrestricted_Access);
    Pattern.Set (Rule, Id_Quit, "quit",
                 Com'Unrestricted_Access);
    Pattern.Set (Rule, Id_Exit, "exit",
                 Com'Unrestricted_Access, Id_Quit);
    Pattern.Set (Rule, Id_Q, "q",
                 Com'Unrestricted_Access, Id_Quit);
    Pattern.Set (Rule, Id_Error, "",
                 Com'Unrestricted_Access);
  end Init_Parsing;

  -- String from stdin

  function Stdin_Cb (Str : in String) return Boolean is
  begin
    -- Sanity checks and specific codes
    if Str'Length = 0 then
      return True;
    end if;
    if Str(Str'Last) = Ada.Characters.Latin_1.Eot then
      Async_Stdin.Put_Line_Out ("CLIENT: Terminated");
      Event_Mng.Send_Dummy_Signal;
      return True;
    end if;
    if Str'Length <= 1
    or else Str(Str'Last) /= Ada.Characters.Latin_1.Lf then
      Async_Stdin.Put_Line_Out ("CLIENT: Discarded");
      return False;
    end if;

    return Pattern.Check (Rule, Str(Str'First .. Str'Last - 1));

  exception
    when Dictio_Lib.No_Dictio =>
      Async_Stdin.Put_Line_Out ("CLIENT: No Dictio");
      return False;
    when Dictio_Lib.No_Item =>
      Async_Stdin.Put_Line_Out ("CLIENT: No Item");
      return False;
    when Dictio_Lib.Name_Too_Long =>
      Async_Stdin.Put_Line_Out ("CLIENT: Name Too Long");
      return False;
    when Dictio_Lib.Data_Too_Long =>
      Async_Stdin.Put_Line_Out ("CLIENT: Data Too Long");
      return False;
    when Error:others =>
      Async_Stdin.Put_Line_Out ("CLIENT: Exception: "
             & Ada.Exceptions.Exception_Name (Error));
      return False;
  end Stdin_Cb;

  -- Dictio lib callbacks

  procedure Load;

  procedure Dictio_State_Cb (State : in Dictio_Lib.Dictio_State_List) is
    use type Dictio_Lib.Dictio_State_List;
  begin
    Saved_State := State;
    if not Init and then not Sig then
      Async_Stdin.Put_Line_Out("CLIENT: Dictio state is " & State'Img);
    end if;
    if State /= Dictio_Lib.Unavailable and then Init then
      Load;
    end if;
  end Dictio_State_Cb;

  procedure Dictio_Notify_Cb (Name : in String;
                              Item : in Boolean;
                              Data : in String) is
  begin
    Async_Stdin.Put_Out ("CLIENT: Notified on ");
    if Item then
      Async_Stdin.Put_Out ("item");
    else
      Async_Stdin.Put_Out ("alias");
    end if;
    Async_Stdin.Put_Line_Out (" >" & Name & "< - >" & Data & "<");
  end Dictio_Notify_Cb;

  -- Load dictio

  procedure Load is
    Name : String (1 .. 10);
    N : Positive;
  begin
    for I in 1 .. 100 loop
      for J in 1 .. 100 loop
        N := Rnd.Gen.Int_Random (1, Name'Last);
        for K in 1 .. N loop
          Name(K) := Character'Val(Rnd.Gen.Int_Random (
                Character'Pos('a'), Character'Pos('z')));
        end loop;
        if N > 3 and then Rnd.Gen.Int_Random = 1 then
          Name(2) := '.';
        end if;
        if Verbose then
          Async_Stdin.Put_Line_Out ("CLIENT: Initializing " & Name(1..N));
        end if;
        Dictio_Lib.Set (Name(1..N), "Init_" & Name(1..N));
        exit when Sig;
      end loop;
      Event_Mng.Pause (100);
      exit when Sig;
    end loop;
    Sig := True;
    Event_Mng.Send_Dummy_Signal;
  exception
    when Dictio_Lib.No_Dictio =>
      Async_Stdin.Put_Line_Out ("CLIENT.LOAD: No Dictio");
      Sig := True;
      Event_Mng.Send_Dummy_Signal;
      Basic_Proc.Set_Error_Exit_Code;
  end Load;

begin
  begin
    if Argument.Get_Parameter (1, "i") = "" then
      Init := True;
    else
      Init := False;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Init := False;
  end;

  begin
    if Argument.Get_Parameter (1, "v") = "" then
      Verbose := True;
    else
      Verbose := False;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Verbose := False;
  end;

  -- Init parsing
  Init_Parsing;

  -- Set async stdin
  if not Init then
    begin
      Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access,
                             Async_Stdin.Max_Chars_Range'Last);
    exception
      when Async_Stdin.Error =>
        Async_Stdin.Put_Line_Out("CLIENT: Cannot set stdin async");
        return;
    end;
  end if;

  Event_Mng.Set_Sig_Term_Callback (Sig_Cb'Unrestricted_Access);

  Rnd.Gen.Randomize;

  Dictio_Lib.Dictio_State_Cb := Dictio_State_Cb'Unrestricted_Access;
  Dictio_Lib.Notify_Cb := Dictio_Notify_Cb'Unrestricted_Access;
  Dictio_Lib.Init;

  if not Init and then not Sig then
    Put_Help;
  end if;
  if not Sig then
    Event_Mng.Pause (Event_Mng.Infinite_Ms);
  end if;

  Async_Stdin.Set_Async;
exception
  when others =>
    Async_Stdin.Set_Async;
    raise;
end T_Dictio;

