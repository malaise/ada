with Ada.Text_Io;
with Parser, Pattern, Lower_Str;
procedure T_Pattern is

  Done : Boolean := False;

  Mr, Rule : Pattern.Rule_No;

  function Cli (Ru : in Pattern.Rule_No; 
                Pa : in Pattern.Pattern_Id; 
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
  begin
    Ada.Text_Io.Put ("Called Cb (Rule=" & Pattern.Image (Ru)
                   & ", Id=" & Pa'Img
                   & ", Matches=" & Nb'Img
                   & ", tail: ");
    while Parser.Current_Word (It) /= "" loop
      Ada.Text_Io.Put (">" & Parser.Current_Word (It) & "<");
      Parser.Next_Word (It);
    end loop;
    Ada.Text_Io.Put_Line (").");
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
    New_Pa : Pattern.Pattern_Id;

    Str : constant String := Parser.Image (It);
    Last : constant Natural := Parser.Last_Index (It);
  begin
    -- First word is pattern id
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Current_Word(It));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Invalid pattern id " & Parser.Current_Word(It));
        return False;
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
    New_Pa : Pattern.Pattern_Id;

  begin
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Current_Word(It));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Invalid pattern id" & Parser.Current_Word(It));
        return False;
    end;
    if Parser.Next_Word(It) /= "" then
      Ada.Text_Io.Put_Line ("Invalid extra argument "
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
      Ada.Text_Io.Put_Line ("Check ok");
    else
      Ada.Text_Io.Put_Line ("Check nok");
    end if;
    return False;
  end Che;

  function Hel (Ru : in Pattern.Rule_No;
                Pa : in Pattern.Pattern_Id; 
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
  begin
    Ada.Text_Io.Put_Line ("The following commands are supported:");
    Ada.Text_Io.Put_Line ("  set <id> <pattern>");
    Ada.Text_Io.Put_Line ("  del <id>");
    Ada.Text_Io.Put_Line ("  check <string>");
    Ada.Text_Io.Put_Line ("  exit, quit or q");
    return False;
  end Hel;

  function Def (Ru : in Pattern.Rule_No;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 It : in Parser.Iterator) return Boolean is

  begin
    if Parser.Current_Word (It) /= "" then
      Ada.Text_Io.Put_Line ("Invalid command: " & Parser.Image(It) & ".");
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
      Ada.Text_Io.Put_Line ("Exiting");
      Done := True;
    else
      Parser.Reset (It);
      Parser.Next_Word (It);
      return Def (Ru, Pa, Nb, It);
    end if;
    return False;
  end Exi;


  Buf : String (1 .. 1024);
  Len : Natural;

begin

  -- Hook parser (rule 1)
  Mr := Pattern.Get_Free_Rule;
  Pattern.Set (Mr, 10, "set",   Set'Unrestricted_Access);
  Pattern.Set (Mr, 20, "del",   Del'Unrestricted_Access);
  Pattern.Set (Mr, 30, "check", Che'Unrestricted_Access);
  Pattern.Set (Mr, 40, "help",  Hel'Unrestricted_Access);
  Pattern.Set (Mr, 50, "exit",  Exi'Unrestricted_Access, 50);
  Pattern.Set (Mr, 60, "",      Def'Unrestricted_Access);
  Pattern.Set (Mr, 51, "quit",  Exi'Unrestricted_Access, 50);
  Pattern.Set (Mr, 52, "q",     Exi'Unrestricted_Access, 50);

  -- Set rule
  Rule := Pattern.Get_Free_Rule;

  loop
    Ada.Text_Io.Put ("> ");
    Ada.Text_Io.Get_Line (Buf, Len);
    begin
      Pattern.Check (Mr, Lower_Str(Buf(1 .. Len)));
      exit when Done;
    exception
      when Pattern.Invalid_Pattern =>
        Ada.Text_Io.Put_Line ("EXCEPTION: Invalid_Pattern");
    end;
  end loop;
  Pattern.Del_Rule (Mr);
  Pattern.Del_Rule (Rule);

end T_Pattern;

