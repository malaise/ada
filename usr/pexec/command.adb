with My_Io, Text_Handler, Argument;
package body Command is

  -- Pexec options definitions
  type Opt_Key_Array is array (Positive range <>) of Character;
  Opt_Key : constant Opt_Key_Array := ('a', 'd', 'c', 'f', 'l', 'i', 's');
  Nbre_Max_Opt : constant := Opt_Key'Length;
  subtype Index_Opt is Positive range Opt_Key'Range;

  -- Parse already called...
  Parsed : Boolean := False;

  -- Result of parsing : set Nbre_Param and Param array
  Max_Nbre_Param : constant := 16;
  Max_Len_Param : constant := 132;

  subtype T_Param is Text_Handler.Text (Max_Len_Param);

  Nbre_Param : Natural;
  Param : array (1 .. Max_Nbre_Param) of T_Param;

  procedure Print_Usage is
  begin
    My_Io.Put_Line ("Usage : pexec [options] command [{;command}]");
    My_Io.Put_Line (" options : -[a][d][c][f][l][i][s]");
    My_Io.Put_Line ("  a for do not print actions.");
    My_Io.Put_Line ("  d for do not print name of each dir.");
    My_Io.Put_Line ("  c for don't do in current dir.");
    My_Io.Put_Line ("  f for stop after 1st level of sub dir.");
    My_Io.Put_Line ("  l for do in leaves only (dirs with no subdir).");
    My_Io.Put_Line ("  i for ignore command errors.");
    My_Io.Put_Line ("  s for follow symbolic links.");
  end Print_Usage;

  procedure Parse (
   No_Action,      No_Name_Of_Dir,
   Not_In_Current, First_Level_Only, Leaves_Only,
   No_Stop_On_Error,
   Follow_Links : out Boolean) is

    -- Is 1st argument a pexec option
    Pexec_Options : Boolean;

    -- To get arguments
    First_Pos : Natural;
    Arg : Text_Handler.Text (Max_Len_Param);
    -- String copy of Arg
    Str : String (1 .. Max_Len_Param);
    Len : Positive;

    -- Pos is index of first available position in Str
    --  string concatenation of pexec arguments which are not pexec option
    Pos : Positive;

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
    No_Action        := La;
    No_Name_Of_Dir   := Ld;
    Not_In_Current   := Lc;
    First_Level_Only := Lf;
    Leaves_Only      := Lf;
    No_Stop_On_Error := Li;
    Follow_Links     := Ls;

    -- Check that not already parsed
    if Parsed then
      raise Already_Parsed;
    end if;

    -- Search and check wether first argument is a pexec option
    --  sets the out parameters and Pexec_Options
    declare
      -- If any error is detected in pexec option
      Wrong_Pexec_Opt : exception;
    begin

      begin
        -- Search a key
        Argument.Get_Param_And_Pos (Parameter => Arg, Position => First_Pos,
         Occurence => 1, Param_Key => "");
      exception
        when Argument.Argument_Not_Found =>
          raise Wrong_Pexec_Opt;
      end;

      -- Key must be first, not empty and not more than Nbre_Max_Opt
      if First_Pos /= 1 or else
       Text_Handler.Length (Arg) = 0 or else
       Text_Handler.Length (Arg) > Nbre_Max_Opt then
        raise Wrong_Pexec_Opt;
      end if;

      -- A key at first. May be an option. Store as string.
      Len := Text_Handler.Length (Arg);
      Str (1 .. Len) := Text_Handler.Value (Arg);

      -- Each option must appear once or not.
      -- check that any letter is a pexec option
      for I in 1 .. Len loop
        -- Check a letter
        declare
          Ok_So_Far : Boolean := False;
        begin
          -- Check that Str(I) is a pexec option
          for J in Index_Opt loop
            if Str(I) = Opt_Key(J) then
              -- Character is found within pexec options
              Ok_So_Far := True;
              exit;
            end if;
          end loop;
          if not Ok_So_Far then raise Wrong_Pexec_Opt; end if;
          -- Current letter is an option. Must appear once.
          if I /= Len then
            for J in I+1 .. Len loop
              if Str(J) = Str(I) then
                -- Character appears twice
                Ok_So_Far := False;
                exit;
              end if;
            end loop;
          end if;
          if not Ok_So_Far then raise Wrong_Pexec_Opt; end if;
        end;

        -- set out params
        if    Str(I) = Opt_Key(1) then La := True;
        elsif Str(I) = Opt_Key(2) then Ld := True;
        elsif Str(I) = Opt_Key(3) then Lc := True;
        elsif Str(I) = Opt_Key(4) then Lf := True;
        elsif Str(I) = Opt_Key(5) then Ll := True;
        elsif Str(I) = Opt_Key(6) then Li := True;
        elsif Str(I) = Opt_Key(7) then Ls := True;
        end if;

      end loop;

      Pexec_Options := True;

    exception
      when Wrong_Pexec_Opt =>
        Pexec_Options := False;
        -- not a pexec option
    end;

    -- check that there is at least one argument remaining
    if Pexec_Options then
      if Argument.Get_Nbre_Arg < 2 then raise No_Command; end if;
    else
      if Argument.Get_Nbre_Arg < 1 then raise No_Command; end if;
    end if;


    -- build all the command line : Concatenate all commands in str
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
      Pos := 1;
      for I in First_Com .. Argument.Get_Nbre_Arg loop
        Argument.Get_Parameter (Parameter => Arg, Occurence => I);
        Len := Text_Handler.Length (Arg);
        Str (Pos .. Pos + Len) := Text_Handler.Value (Arg) & ' ';
        Pos := Pos + Len + 1;
      end loop;
      -- Pos is index of first available position
      -- It becomes index of last character
      Pos := Pos - 2;
      -- remove first and last "
      if Str(1) = '"' and then Str(Pos) = '"' then
        Str (1 .. Pos - 2) := Str (2 .. Pos - 1);
        Pos := Pos - 2;
      end if;
    end;

    -- Parse (split) the command line in Str
    declare
      -- begin of current command (first char or first after ;)
      First : Positive := 1;
      -- end   of current command
      Last  : Natural := 0;
       -- No of current command
      Index : Natural := 0;
    begin
      for I in 1 .. Pos loop

        -- search separator
        if Str(I) = ';' then

          -- new argument
          if I = First then
            -- first char is ';', or ";;" in string
            null;
          elsif I = First + 1 and then Str (I - 1) = ' ' then
            -- "; ;"
            null;
          else
            -- at least one siginificant char in First .. I-1
            Last := I - 1;

            -- Skip leading and tailing spaces
            if Str (First) = ' ' then
              First := First + 1;
            end if;
            if Str (Last) = ' ' then
              Last := Last - 1;
            end if;

            -- store this
            Index := Index + 1;
            Text_Handler.Set (Param (Index),
             Text_Handler.To_Text (Str (First .. Last)) );

            -- set first
            First := I + 1;
          end if;

        elsif I = Pos then

          -- end of Str
          Last := I;

          -- Skip leading spaces
          if Str (First) = ' ' then
            First := First + 1;
          end if;

          -- store this
          Index := Index + 1;
          Text_Handler.Set (Param (Index),
           Text_Handler.To_Text (Str (First .. Last)));

        end if;

      end loop;

      -- done
      Nbre_Param := Index;
      Parsed := True;

    end;

    -- set out values
    No_Action        := La;
    No_Name_Of_Dir   := Ld;
    Not_In_Current   := Lc;
    First_Level_Only := Lf;
    Leaves_Only      := Ll;
    No_Stop_On_Error := Li;
    Follow_Links     := Ls;

  end Parse;



  function Nbre_Commands return Natural is
  begin
    if not Parsed then
      raise Not_Parsed;
    end if;
    return Nbre_Param;
  end Nbre_Commands;

  function Nth_Command (N : Positive) return String is
  begin
    if not Parsed then
      raise Not_Parsed;
    end if;
    if N <= Nbre_Commands then
      return Text_Handler.Value (Param (N));
    else
      return "";
    end if;
  end Nth_Command;

end Command;

