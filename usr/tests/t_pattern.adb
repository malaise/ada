with Basic_Proc, As.U, Parser, Pattern, Lower_Str;
procedure T_Pattern is

  Done : Boolean := False;

  Mr, Rule : Pattern.Rule_No;
  High_Id : Pattern.Pattern_Id := Pattern.Pattern_Id'First;

  type Cb_Data_Rec is record
    Ru : Pattern.Rule_No;
    Pa : Pattern.Pattern_Id;
    Nb : Natural;
    Tail : As.U.Asu_Us;
  end record;
  Last_Data : Cb_Data_Rec;

  function Check_Data (Ru : Pattern.Rule_No;
                       Pa : Pattern.Pattern_Id;
                       Nb :  Natural;
                       Tail : String) return Boolean is
    use type Pattern.Rule_No, Pattern.Pattern_Id;
  begin
    if Ru /= Last_Data.Ru then
      Basic_Proc.Put_Line_Output (
                       "Error: Cb Ru " & Pattern.Image (Last_Data.Ru)
                     & " differs from expected " & Pattern.Image (Ru));
      Done := True;
      return False;
    end if;
    if Pa /= Last_Data.Pa then
      Basic_Proc.Put_Line_Output (
                       "Error: Cb Pa " & Last_Data.Pa'Img
                     & " differs from expected " & Pa'Img);
      Done := True;
      return False;
    end if;
    if Nb /= Last_Data.Nb then
      Basic_Proc.Put_Line_Output (
                       "Error: Cb Nb " & Last_Data.Nb'Img
                     & " differs from expected " & Nb'Img);
      Done := True;
      return False;
    end if;
    if Tail /= Last_Data.Tail.Image then
      Basic_Proc.Put_Line_Output (
                       "Error: Cb Tail " & Last_Data.Tail.Image
                     & " differs from expected " & Tail);
      Done := True;
      return False;
    end if;
    Basic_Proc.Put_Line_Output ("OK");
    return True;
  end Check_Data;

  function Cli (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
  begin
    Basic_Proc.Put_Output ("Called Cb (" & Pattern.Image (Ru) & ","
                                   & Pa'Img & ","
                                   & Nb'Img & ", tail: ");
    Last_Data.Ru := Ru;
    Last_Data.Pa := Pa;
    Last_Data.Nb := Nb;
    Last_Data.Tail.Set_Null;
    while Parser.Current_Word (It) /= "" loop
      Basic_Proc.Put_Output (">" & Parser.Current_Word (It) & "<");
      if not Last_Data.Tail.Is_Null then
        Last_Data.Tail.Append (" ");
      end if;
      Last_Data.Tail.Append (Parser.Current_Word (It));
      Parser.Next_Word (It);
    end loop;
    Basic_Proc.Put_Line_Output (").");
    return Nb /= 0;
  end Cli;

  -- Skip heading separators and return a String(1 .. N)
  function Parse (Str : String) return String is
    First : Natural;
  begin
    First := 0;
    for I in Str'Range loop
      if not Pattern.Is_Sep (Str(I)) then
        First := I;
        exit;
      end if;
    end loop;
    if First = 0 then
      return "";
    else
      declare
        Ret : constant String (1 .. Str'Last-First+1)
            := Str (First .. Str'Last);
      begin
        return Ret;
      end;
    end if;
  end Parse;

  function Set (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Ru, Pa, Nb);
    New_Pa : Pattern.Pattern_Id;

    Str : constant String := Parser.Image (It);
    Last : constant Natural := Parser.Last_Index (It);
  begin
    -- First word is pattern id
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Current_Word(It));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Invalid pattern id "
                                  & Parser.Current_Word(It));
        return False;
    end;
    declare
      use type Pattern.Pattern_Id;
    begin
      if New_Pa > High_Id then
        High_Id := New_Pa;
      end if;
    end;

    if Last = 0 then
      Pattern.Set (Rule, New_Pa, "", Cli'Unrestricted_Access);
    else
      Pattern.Set (Rule,
                   New_Pa,
                   Parse (Str(Last + 1 .. Str'Last)),
                   Cli'Unrestricted_Access);
    end if;
    return False;
  end Set;

  function Del (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Ru, Pa, Nb);
    New_Pa : Pattern.Pattern_Id;

  begin
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Current_Word(It));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Invalid pattern id"
                                  & Parser.Current_Word(It));
        return False;
    end;
    if Parser.Next_Word(It) /= "" then
      Basic_Proc.Put_Line_Output ("Invalid extra argument "
                          & Parser.Current_Word(It));
      return False;
    end if;
    Pattern.Del (Rule, New_Pa);
    return False;
  end Del;

  function Che (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Ru, Pa, Nb);
    Str : constant String := Parser.Image (It);
    First : constant Natural := Parser.First_Index (It);
    Res : Boolean;
  begin
    -- Check remaining of string
    if Parser.Last_Index (It) = 0 then
      Res := Pattern.Check (Rule, "");
    else
      Res := Pattern.Check (Rule, Parse (Str(First .. Str'Last)));
    end if;
    if Res then
      Basic_Proc.Put_Line_Output ("Check ok");
    else
      Basic_Proc.Put_Line_Output ("Check nok");
    end if;
    return False;
  end Che;

  function Put (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Ru, Pa, Nb);
    Str : constant String := Parser.Current_Word (It);
    Str1 : constant String := Parser.Next_Word (It);
    Pat2Put : Pattern.Pattern_Id;
    procedure Put_Pat (Id : Pattern.Pattern_Id) is
    begin
      Basic_Proc.Put_Line_Output (Id'Img & " -> "
          & Pattern.Pattern_Id'Image(Pattern.Get_Id4Cb (Rule, Id))
          & ":" & Pattern.Image (Rule, Id));
    end Put_Pat;
  begin
    if Str /= "" then
      if Str1 /= "" then
        Basic_Proc.Put_Line_Output ("Invalid pattern id " & Str & " " & Str1);
        return False;
      end if;
      -- First word is pattern id
      begin
        Pat2Put := Pattern.Pattern_Id'Value (Str);
      exception
        when Constraint_Error =>
          Basic_Proc.Put_Line_Output ("Invalid pattern id " & Str);
          return False;
      end;
      Put_Pat (Pat2Put);
      return False;
    end if;

    for I in 1 .. High_Id loop
      begin
        Put_Pat (I);
      exception
        when Pattern.Invalid_Pattern =>
          null;
      end;
    end loop;
    return False;

  end Put;

  function Hel (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
    pragma Unreferenced (Ru, Pa, Nb, It);
  begin
    Basic_Proc.Put_Line_Output ("The following commands are supported:");
    Basic_Proc.Put_Line_Output ("  set <id> <pattern>");
    Basic_Proc.Put_Line_Output ("  del <id>");
    Basic_Proc.Put_Line_Output ("  put [ <id> ]");
    Basic_Proc.Put_Line_Output ("  check <string>");
    Basic_Proc.Put_Line_Output ("  auto");
    Basic_Proc.Put_Line_Output ("  exit, quit or q");
    return False;
  end Hel;

  function Def (Ru : in Pattern.Rule_No;
                 Pa : in Pattern.Pattern_Id;
                 Nb : in Natural;
                 It : in Parser.Iterator) return Boolean is

  begin
    if Parser.Current_Word (It) /= "" then
      Basic_Proc.Put_Line_Output ("Invalid command: " & Parser.Image(It) & ".");
      return Hel (Ru, Pa, Nb, It);
    end if;
    return False;
  end Def;

  function Exi (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id;
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
  begin
    if Parser.Current_Word (It) = "" then
      Basic_Proc.Put_Line_Output ("Exiting");
      Done := True;
    else
      Parser.Reset (It);
      Parser.Next_Word (It);
      return Def (Ru, Pa, Nb, It);
    end if;
    return False;
  end Exi;

  function Auto (Ru : in Pattern.Rule_No;
                 Pa : in Pattern.Pattern_Id;
                 Nb : in Natural;
                 It : in Parser.Iterator) return Boolean is
    R : Pattern.Rule_No;
  begin
    if Parser.Current_Word (It) /= "" then
      Parser.Reset (It);
      Parser.Next_Word (It);
      return Def (Ru, Pa, Nb, It);
    end if;
    R := Pattern.Get_Free_Rule;
    R.Set (10, "get alias", Cli'Unrestricted_Access);
    R.Set (20, "get", Cli'Unrestricted_Access);
    R.Set (30, "notify", Cli'Unrestricted_Access);
    R.Set (40, "quit", Cli'Unrestricted_Access);
    R.Set (41, "exit", Cli'Unrestricted_Access, 40);
    R.Set (100, "", Cli'Unrestricted_Access);
    R.Check ("get foo");
    if not Check_Data (R, 20, 1, "foo") then return False; end if;
    R.Check ("get alias foo");
    if not Check_Data (R, 10, 2, "foo") then return False; end if;
    R.Check ("notify alias");
    if not Check_Data (R, 30, 1, "alias") then return False; end if;
    R.Check ("notif alias");
    if not Check_Data (R, 100, 0, "notif alias") then return False; end if;
    R.Check ("quit");
    if not Check_Data (R, 40, 1, "") then return False; end if;
    R.Check ("exit");
    if not Check_Data (R, 40, 1, "") then return False; end if;

    R.Del (10);
    R.Set (10, "get [ alias ]", Cli'Unrestricted_Access);
    R.Del (20);
    R.Del (30);
    R.Check ("get foo  ");
    if not Check_Data (R, 10, 1, "foo") then return False; end if;
    R.Check ("get   alias foo");
    if not Check_Data (R, 10, 2, "foo") then return False; end if;
    Basic_Proc.Put_Line_Output ("Auto test OK.");
    return True;
  end Auto;

begin

  -- Hook parser (rule 1)
  Mr := Pattern.Get_Free_Rule;
  Pattern.Set (Mr, 10, "set",   T_Pattern.Set'Unrestricted_Access);
  Pattern.Set (Mr, 20, "del",   Del'Unrestricted_Access);
  Pattern.Set (Mr, 30, "check", Che'Unrestricted_Access);
  Pattern.Set (Mr, 40, "help",  Hel'Unrestricted_Access);
  Pattern.Set (Mr, 50, "exit",  Exi'Unrestricted_Access, 50);
  Pattern.Set (Mr, 99, "",      Def'Unrestricted_Access);
  Pattern.Set (Mr, 51, "quit",  Exi'Unrestricted_Access, 50);
  Pattern.Set (Mr, 52, "q",     Exi'Unrestricted_Access, 50);
  Pattern.Set (Mr, 70, "put",   Put'Unrestricted_Access);
  Pattern.Set (Mr, 60, "auto",  Auto'Unrestricted_Access);

  -- Set rule
  Rule := Pattern.Get_Free_Rule;

  loop
    begin
      Pattern.Check (Mr, Lower_Str(Basic_Proc.Get_Line));
      pragma Warnings (Off, "variable ""*"" is not modified in loop body");
      exit when Done;
      pragma Warnings (On, "variable ""*"" is not modified in loop body");
    exception
      when Pattern.Invalid_Pattern =>
        Basic_Proc.Put_Line_Output ("EXCEPTION: Invalid_Pattern");
    end;
  end loop;
  Pattern.Del_Rule (Mr);
  Pattern.Del_Rule (Rule);

end T_Pattern;

