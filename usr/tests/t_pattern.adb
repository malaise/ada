with Ada.Text_Io;
with Parser, Pattern, Lower_Str;
procedure T_Pattern is

  Done : Boolean := False;

  Rule : Pattern.Rule_No;

  function Cli (Ru : in Pattern.Rule_No; 
                Pa : in Pattern.Pattern_Id; 
                Nb : in Natural;
                It : in Parser.Iterator) return Boolean is
  begin
    Ada.Text_Io.Put ("Called Cb (" & Ru'Img & "," & Pa'Img & ","
                                   & Nb'Img & ", tail: ");
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

    Pattern.Set (Rule,
                 New_Pa,
                 Parse (Str(Last + 1 .. Str'Last)),
                 Cli'Unrestricted_Access);
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
  begin
    -- Check remaining of string
    if Pattern.Check (Rule, Parse (Str(First .. Str'Last))) then
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
  Pattern.Set (1, 10, "set",   Set'Unrestricted_Access);
  Pattern.Set (1, 20, "del",   Del'Unrestricted_Access);
  Pattern.Set (1, 30, "check", Che'Unrestricted_Access);
  Pattern.Set (1, 40, "help",  Hel'Unrestricted_Access);
  Pattern.Set (1, 50, "exit",  Exi'Unrestricted_Access);
  Pattern.Set (1, 60, "",      Def'Unrestricted_Access);
  Pattern.Set (1, 51, "quit",  Exi'Unrestricted_Access);
  Pattern.Set (1, 52, "q",     Exi'Unrestricted_Access);

  -- Set rule
  Rule := Pattern.Get_Free_Rule;

  loop
    Ada.Text_Io.Put ("> ");
    Ada.Text_Io.Get_Line (Buf, Len);
    Pattern.Check (1, Lower_Str(Buf(1 .. Len)));
    exit when Done;
  end loop;

end T_Pattern;

