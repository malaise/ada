with As.U; use As.U;
with Argument, Basic_Proc, Parser, String_Mng;
package body Command is

  -- Pexec options definitions
  type Opt_Key_Array is array (Positive range <>) of Character;
  Opt_Key : constant Opt_Key_Array := ('a', 'd', 'c', 'f', 'l', 'i', 's');
  Nbre_Max_Opt : constant Positive := Opt_Key'Length;
  subtype Index_Opt is Positive range Opt_Key'Range;

  -- Parse already called...
  Parsed : Boolean := False;

  -- Commands
  Nb_Commands : Command_Nb_Range;
  Commands : array (Command_No_Range) of Asu_Us;

  procedure Print_Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage : " & Argument.Get_Program_Name
                               & " [options] command [ { ; command } ]");
    Basic_Proc.Put_Line_Output (" options : -[a][d][c][f][l][i][s]");
    Basic_Proc.Put_Line_Output ("  a for do not print actions.");
    Basic_Proc.Put_Line_Output ("  d for do not print name of each dir.");
    Basic_Proc.Put_Line_Output ("  c for don't exec in current dir.");
    Basic_Proc.Put_Line_Output ("  f for stop after 1st level of sub dir.");
    Basic_Proc.Put_Line_Output ("  l for exec in leaves only (dirs with no subdir).");
    Basic_Proc.Put_Line_Output ("  i for ignore command errors.");
    Basic_Proc.Put_Line_Output ("  s for follow symbolic links.");
  end Print_Usage;

  -- For parsing commands
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = ';';
  end Is_Sep;

  procedure Parse (No_Action,      No_Name_Of_Dir,
                   Not_In_Current, First_Level_Only, Leaves_Only,
                   No_Stop_On_Error,
                   Follow_Links : out Boolean) is

    -- Is 1st argument a pexec option
    Pexec_Options : Boolean;

    -- String copy of Argument
    Str : Asu_Us;

    -- Local copies of out parameters
    La, Ld, Lc, Lf, Ll, Li, Ls : Boolean;

  begin
    -- Set out default values
    La := False;
    Ld := False;
    Lc := False;
    Lf := False;
    Ll := False;
    Li := False;
    Ls := False;
    No_Action        := False;
    No_Name_Of_Dir   := False;
    Not_In_Current   := False;
    First_Level_Only := False;
    Leaves_Only      := False;
    No_Stop_On_Error := False;
    Follow_Links     := False;

    -- Check that not already parsed
    if Parsed then
      raise Already_Parsed;
    end if;

    -- Help mode
    if Argument.Get_Nbre_Arg = 1
    and then  Argument.Get_Parameter (1) = "-h" then
      raise Help;
    end if;

    -- Search and check wether first argument is a pexec option
    --  sets the out parameters and Pexec_Options
    declare
      -- If any error is detected in pexec option
      Wrong_Pexec_Opt : exception;
      Char : Character;
    begin

      begin
        -- Search a char key
        Str := Asu_Tus (Argument.Get_Parameter (Occurence => 1));
        if Asu.Length (Str) < 2
        or else Asu.Element (Str, 1) /= '-'
        or else Asu.Length (Str) > Nbre_Max_Opt  + 1 then
          raise Wrong_Pexec_Opt;
        end if;
      exception
        when Argument.Argument_Not_Found =>
          raise Wrong_Pexec_Opt;
      end;

      -- Each option must appear once or not.
      -- check that any letter is a pexec option
      for I in 2 .. Asu.Length (Str) loop
        -- Check a letter
        declare
          Ok_So_Far : Boolean := False;
        begin
          -- Check that Str(I) is a pexec option
          Char := Asu.Element (Str, I);
          for J in Index_Opt loop
            if Char = Opt_Key(J) then
              -- Character is found within pexec options
              Ok_So_Far := True;
              exit;
            end if;
          end loop;
          if not Ok_So_Far then raise Wrong_Pexec_Opt; end if;
          -- Current letter is an option: it must appear once.
          if I /= Asu.Length (Str) then
            for J in I + 1 .. Asu.Length (Str) loop
              if Asu.Element (Str, J) = Char then
                -- Character appears twice
                Ok_So_Far := False;
                exit;
              end if;
            end loop;
          end if;
          if not Ok_So_Far then raise Wrong_Pexec_Opt; end if;
        end;

        -- Set out params
        if    Char = Opt_Key(1) then La := True;
        elsif Char = Opt_Key(2) then Ld := True;
        elsif Char = Opt_Key(3) then Lc := True;
        elsif Char = Opt_Key(4) then Lf := True;
        elsif Char = Opt_Key(5) then Ll := True;
        elsif Char = Opt_Key(6) then Li := True;
        elsif Char = Opt_Key(7) then Ls := True;
        end if;

      end loop;

      Pexec_Options := True;

    exception
      when Wrong_Pexec_Opt =>
        Pexec_Options := False;
        -- Not a pexec option
    end;

    -- Check that there is at least one argument remaining
    if Pexec_Options then
      if Argument.Get_Nbre_Arg < 2 then raise No_Command; end if;
    else
      if Argument.Get_Nbre_Arg < 1 then raise No_Command; end if;
    end if;


    -- Build all the command line : Concatenate all commands in str
    declare
      -- Fist no-pexec_option argument
      First_Com : Positive range 1 .. 2;
    begin
      -- Set Fist no-pexec_option argument
      if Pexec_Options then
        First_Com := 2;
      else
        First_Com := 1;
      end if;

      -- Concatenate all the command line in Str
      Str := Asu_Null;
      for I in First_Com .. Argument.Get_Nbre_Arg loop
        Asu.Append (Str, " " & Argument.Get_Parameter (Occurence => I));
      end loop;
      Asu.Delete (Str, 1, 1);

      -- Remove first and last "
      if Asu.Element (Str, 1) = '"'
      and then Asu.Element (Str, Asu.Length (Str)) = '"' then
        Asu.Delete (Str, Asu.Length (Str), Asu.Length (Str));
        Asu.Delete (Str, 1, 1);
      end if;
    end;

    -- Parse (split) the commands in Str
    declare
      -- Parser iterator
      Iter : Parser.Iterator;
      Tmp : Asu_Us;
      Start, Stop : Natural;
      use type Asu_Us;
    begin
      Iter.Set (Asu_Ts (Str), Is_Sep'Access);
      Nb_Commands := 0;
      loop
        Tmp := Asu_Tus (Iter.Next_Word);
        exit when Tmp = Asu_Null;
          -- Skip leading and tailing spaces
          Start := String_Mng.Parse_Spaces (Asu_Ts (Tmp), True);
          Stop  := String_Mng.Parse_Spaces (Asu_Ts (Tmp), False);
          if Start /= 0 then
            -- Not Full of spaces => Store
            if Nb_Commands = Command_Nb_Range'Last then
              raise Too_Many_Commands;
            end if;
            Nb_Commands := Nb_Commands + 1;
            Commands (Nb_Commands) := Asu.Unbounded_Slice (Tmp, Start, Stop);
            end if;
      end loop;

    end;

    -- set out values
    No_Action        := La;
    No_Name_Of_Dir   := Ld;
    Not_In_Current   := Lc;
    First_Level_Only := Lf;
    Leaves_Only      := Ll;
    No_Stop_On_Error := Li;
    Follow_Links     := Ls;
    Parsed := True;

  end Parse;


  function Nbre_Commands return Command_Nb_Range is
  begin
    if not Parsed then
      raise Not_Parsed;
    end if;
    return Nb_Commands;
  end Nbre_Commands;

  function Nth_Command (N : Command_No_Range) return String is
  begin
    if not Parsed then
      raise Not_Parsed;
    end if;
    if N <= Nbre_Commands then
      return Asu_Ts (Commands(N));
    else
      return "";
    end if;
  end Nth_Command;

end Command;

