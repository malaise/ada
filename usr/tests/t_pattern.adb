with Ada.text_Io;
with Parser, Pattern, Lower_Str;
procedure T_Pattern is

  Done : Boolean := False;

  procedure Cli (Ru : in Pattern.Rule_No; 
                 St : in String;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 Id : in Natural) is
  begin
    Ada.Text_Io.Put_Line ("Called Cb (" & Ru'Img & ", "
                        & St & "," & Pa'Img & "," & Nb'Img & ","
                        & Id'Img & ").");
  end Cli;

  function Tail (Str : String) return String is
  begin
    for I in Str'Range loop
      if not Pattern.Is_Sep (Str(I)) then
        return Str (I .. Str'Last);
      end if;
    end loop;
    return "";
  end Tail;

  procedure Set (Ru : in Pattern.Rule_No;
                 St : in String;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 Id : in Natural) is
    It : Parser.Iterator;
    New_Pa : Pattern.Pattern_Id;

  begin
    Parser.Create (St(Id+1 .. St'Last), Pattern.Is_Sep'Access, It);
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Next_Word(It));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Invalid pattern id " & Parser.Current_Word(It));
        Parser.Delete (It);
        return;
    end;

    Pattern.Set (2, New_Pa,
                 Tail(St(Parser.Last_Index(It) + 1 .. St'Last)),
                 Cli'Unrestricted_Access);
    Parser.Delete (It);
  end Set;

  procedure Del (Ru : in Pattern.Rule_No;
                 St : in String;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 Id : in Natural) is
    It : Parser.Iterator;
    New_Pa : Pattern.Pattern_Id;

  begin
    Parser.Create (St(Id+1 .. St'Last), Pattern.Is_Sep'Access, It);
    begin
      New_Pa := Pattern.Pattern_Id'Value (Parser.Next_Word(It));
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("Invalid pattern id" & Parser.Current_Word(It));
        Parser.Delete (It);
        return;
    end;
    if Parser.Next_Word(It) /= "" then
      Ada.Text_Io.Put_Line ("Invalid extra argument "
                          & Parser.Current_Word(It));
      Parser.Delete (It);
      return;
    end if;

    Pattern.Del (2, New_Pa);
  end Del;

  procedure Che (Ru : in Pattern.Rule_No;
                 St : in String;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 Id : in Natural) is
  begin
    if Pattern.Check (2, St (Id+1 .. St'Last)) then
      Ada.Text_Io.Put_Line ("Check ok");
    else
      Ada.Text_Io.Put_Line ("No match");
    end if;
  end Che;

  procedure Hel (Ru : in Pattern.Rule_No;
                  St : in String;
                  Pa : in Pattern.Pattern_Id; 
                  Nb : in Natural;
                  Id : in Natural) is
  begin
    Ada.Text_Io.Put_Line ("The following commands are supported:");
    Ada.Text_Io.Put_Line ("  set <id> <pattern>");
    Ada.Text_Io.Put_Line ("  del <id>");
    Ada.Text_Io.Put_Line ("  check <string>");
    Ada.Text_Io.Put_Line ("  exit or quit");
  end Hel;

  procedure Def (Ru : in Pattern.Rule_No;
                 St : in String;
                 Pa : in Pattern.Pattern_Id; 
                 Nb : in Natural;
                 Id : in Natural) is
    It : Parser.Iterator;

  begin
    Parser.Create (St, Pattern.Is_Sep'Access, It);
    if Parser.Next_Word (It) /= "" then
      Ada.Text_Io.Put_Line ("Invalid command: " & St & ".");
      Hel (Ru, St, Pa, Nb, Id);
    end if;
  end Def;

  procedure Exi (Ru : in Pattern.Rule_No;
                 St : in String;
                 Pa : in Pattern.Pattern_Id;
                 Nb : in Natural;
                 Id : in Natural) is
  begin
    if St = "exit" or else St = "quit" then
      Ada.Text_Io.Put_Line ("Exiting");
      Done := True;
    else
      Def (Ru, St, Pa, Nb, Id);
    end if;
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

  loop
    Ada.Text_Io.Put ("> ");
    Ada.Text_Io.Get_Line (Buf, Len);
    Pattern.Check (1, Lower_Str(Buf(1 .. Len)));
    exit when Done;
  end loop;
end T_Pattern;

