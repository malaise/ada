-- More powerfull search and substitution in strings,
--  based on regex.
with Hexa_Utils, Upper_Str, Lower_Str, Mixed_Str;
package body Str_Util.Regex is

  -- Internal: compile regex
  procedure Compile (Compiled : in out Regular_Expressions.Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Options : in Options_Rec) is
  begin
    Compiled.Compile (Ok, Criteria,
                      Options.Case_Sensitive,
                      Options.Multi_Line,
                      Options.Dot_All);
  end Compile;


  -- Internal Locate with Regex compiled
  function Locate (Within     : String;
                   Compiled   : Regular_Expressions.Compiled_Pattern;
                   From_Index : Natural;
                   To_Index   : Natural;
                   Forward    : Boolean;
                   Occurence  : Positive)
           return Regular_Expressions.Match_Cell is
    I1, I2 : Natural;
    Occ : Natural;
    Info, Prev : Regular_Expressions.Match_Cell;
    use type Regular_Expressions.Match_Cell;
  begin
    -- Empty Within => No_match
    if Within'Length = 0 then
      return Regular_Expressions.No_Match;
    end if;
    -- Initialise indexes
    I1 := (if From_Index = 0 then Within'First else From_Index);
    I2 := (if To_Index   = 0 then Within'Last  else To_Index);
    if I1 not in Within'Range
    or else I2 not in Within'Range then
      raise Invalid_Index;
    end if;

    Occ := 0;
    Prev := Regular_Expressions.Match_Cell(No_Match);
    if Forward then
      -- Loop as long as matches and until Occ different matching
      for I in I1 .. I2 loop
        Info := Compiled.Match (Within(I .. I2));
        if Info = Regular_Expressions.No_Match then
          return Regular_Expressions.No_Match;
        elsif Info /= Prev then
          -- One new matching occurence
          Occ := Occ + 1;
          if Occ = Occurence then
            -- Correct occurence number
            return Info;
          end if;
          Prev := Info;
        end if;
      end loop;
    else
      -- Loop until Occ different matching
      for I in reverse I1 .. I2 loop
        Info := Compiled.Match (Within(I .. I2));
        if Info /= Prev then
          -- One new matching occurence
          Occ := Occ + 1;
          if Occ = Occurence then
            -- Correct occurence number
            return Info;
          end if;
          Prev := Info;
        end if;
      end loop;
      return Regular_Expressions.No_Match;
    end if;
    -- Just to be safe
    return Regular_Expressions.No_Match;
  end Locate;


  -- Locate a fragment of Within string matching the regexp Criteria.
  -- Search is performed between the given From_Index (first if 0) and
  --  the To_Index (last if 0) indexes of Within string.
  -- Search is performed forward or backward, and returns the
  --  Nth matching Occurence.
  -- Returns Start and Stop indexes in Within, of chars matching the criteria.
  -- Returns Start=1 and Stop=0 (No_Match) if no corresponding match found.
  function Locate (Within     : String;
                   Criteria   : String;
                   From_Index : Natural := 0;
                   To_Index   : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1;
                   Options    : Options_Rec := Default_Options)
           return Search_Result is
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
  begin
    -- Compile regex
    Compile (Compiled, Ok, Criteria, Options);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;
    return Search_Result (Locate (Within, Compiled, From_Index, To_Index,
                          Forward, Occurence));
  end Locate;

  -- Internal
  -- Replace Working(Info(1).First_Offset .. Info(1).End_Offset)
  -- by By.
  -- In By, \i (0 <= i <= 9) is replaced by
  --  Working (Info(i-1).First_Offset .. Info(i-1).End_Offset)
  -- Start is set to the first char after new string (maybe above Working)
  procedure Substit (Working : in out As.U.Asu_Us;
                     N_Match : in Positive;
                     Info    : in Regular_Expressions.Match_Array;
                     By      : in String;
                     Start   : out Natural) is
    -- New By after substitution of \0, \1...
    Newby : As.U.Asu_Us;
    -- For build Newby
    Esc : Natural;
    From : Positive;
    Char : Character;
    Info_Index : Positive;
    Linfo : constant Regular_Expressions.Match_Array := Info;
    Case_Start : Positive;
    Case_Char : Character;
  begin
    -- Compute Newby, insert matching substrings and hexa byte
    Newby := As.U.Tus (By);
    From := 1;
    loop
      Esc := Locate_Escape (Newby.Image, From, "\\0123456789x");
      exit when Esc = 0;
      Char := Newby.Element (Esc);
      if Char = '\' then
        -- "\\" -> "\"
        Newby.Delete (Esc, Esc);
        From := Esc;
      elsif Char >= '0' and then Char <= '9' then
        -- "\i" -> matching slice of Working:
        -- i is Index in Info - 1: \0 -> Info(1)...
        Info_Index := Character'Pos (Char) - Character'Pos ('0') + 1;
        if Info_Index > N_Match
        or else (Linfo(Info_Index).First_Offset = 1
                 and then Linfo(Info_Index).Last_Offset_Stop = 0) then
          -- "\i" -> "" if no such matching info
          Newby.Delete (Esc - 1, Esc);
        else
          Newby.Replace (Esc - 1, Esc,
                     Working.Slice (Linfo(Info_Index).First_Offset,
                                    Linfo(Info_Index).Last_Offset_Stop));
          -- Move forward by the length of new string.
          -- One back to \ then length forward
          From := Esc + Linfo(Info_Index).Last_Offset_Stop
                      - Linfo(Info_Index).First_Offset;
        end if;
      elsif Char = 'x' and then Esc <= Newby.Length - 2 then
        declare
          Byte : Integer;
        begin
          Byte := 16#10# * Hexa_Utils.Char_To_Hexa (Newby.Element (Esc + 1))
                        +  Hexa_Utils.Char_To_Hexa (Newby.Element (Esc + 2));
          -- Replace "\xIJ" by the code
          Newby.Replace (Esc - 1, Esc + 2, Character'Val (Byte) & "");
          From := Esc;
        exception
          when others =>
            -- Conversion failed
            From := Esc;
        end;
      else
        -- "\*" unchanged
        From := Esc;
      end if;
      exit when From > Newby.Length;
    end loop;

    -- Apply case switches to replacement string
    Case_Start := 1;
    Case_Char := 'c';
    From := 1;
    loop
      Esc := Locate_Escape (Newby.Image, From, "\umlc");
      if Esc = 0 then
        -- No more escape
        if Case_Char = 'c' then
          -- No current conversion: Done
          exit;
        else
          -- Apply last conversion: simulate an appended "\c"
          Char := 'c';
          Esc := Newby.Length + 2;
        end if;
      else
        -- Store the new char and delete this sequence
        Char := Newby.Element (Esc);
        Newby.Delete (Esc - 1, Esc);
        From := Esc - 1;
      end if;
      if Char = Case_Char then
        -- No change
        null;
      elsif Case_Char = 'c' then
        -- No conversion to apply
        Case_Start := From;
        Case_Char := Char;
      elsif Case_Char = 'u' then
        -- Replace from Case_Start to \u by UPPERCASE
        --  of str from Case_Start to before \
        Newby.Replace (Case_Start, Esc - 2,
            Upper_Str (Newby.Slice (Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      elsif Case_Char = 'm' then
        -- Replace from Case_Start to \u by Mixed
        --  of str from Case_Start to before \
        Newby.Replace (Case_Start, Esc - 2,
            Mixed_Str (Newby.Slice (Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      elsif Case_Char = 'l' then
        -- Replace from Case_Start to \u by lowercase
        --  of str from Case_Start to before \
        Newby.Replace (Case_Start, Esc - 2,
            Lower_Str (Newby.Slice (Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      -- else Impossible, bug in Locate_Escape
      end if;
      exit when From > Newby.Length;
    end loop;

    -- Replace the matching slice of Working by the Newby
    Working.Replace (Linfo(1).First_Offset, Linfo(1).Last_Offset_Stop,
                           Newby.Image);
    -- Update Start to the first character after the matching slice
    Start := Linfo(1).First_Offset + Newby.Length;
  end Substit;

  -- Replace in Within all occurences of Criteria by By.
  -- Search and replace is performed between the given From_Index (first if 0)
  --  and the given To_Index (last if 0) indexes of Within string.
  -- Criteria can be a regular expression and By may reference partial
  --  matching substrings (\0, \1, \2, ... \9).
  -- Once a substitution has occured, the search continues from the
  --  second replaced character (thus avoiding loops), up to To_Index.
  -- This cycle may be iterated several times, Nb_Cycles, up to no substitution
  --  if Nb_Cycles is 0 (tbeware that this may lead to infinite loop),
  function Substit (Within     : String;
                    Criteria   : String;
                    By         : String;
                    From_Index : Natural := 0;
                    To_Index   : Natural := 0;
                    Options    : Options_Rec := Default_Options;
                    Nb_Cycles  : Natural := 1)
           return String is
    -- Working string
    I1, I2 : Natural;
    Working : As.U.Asu_Us;
    -- Regex compilation
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    -- Nb of cycles
    Cycle : Positive;
    -- Match info: allow expansion of \0, \1 to 9
    N_Match : Natural;
    Info : Regular_Expressions.Match_Array(1 .. 10);
    Match_Found : Boolean;
    -- Current startup point and Last char of Working
    Start : Positive;
    Last : Natural;
  begin
    -- Empty Within => empty result
    if Within'Length = 0 then
      return "";
    end if;
    -- Initialise indexes
    I1 := (if From_Index = 0 then Within'First else From_Index);
    I2 := (if To_Index   = 0 then Within'Last  else To_Index);
    if I1 not in Within'Range
    or else I2 not in Within'Range then
      raise Invalid_Index;
    end if;
    -- Extract working string
    Working := As.U.Tus (Within(I1 .. I2));

    -- Compile regex
    Compile (Compiled, Ok, Criteria, Options);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;

    -- Loop on scanning the string
    Cycle := 1;
    Cycles:
    loop
      -- Scan the string from start to end
      Start := 1;
      Last := Working.Length;
      -- Substitutions may lead to empty string
      exit Cycles when Last = 0;
      Match_Found := False;
      Pass:
      loop
        Compiled.Exec (Working.Image(Start .. Last), N_Match, Info);
        -- No (more) match?
        exit Pass when N_Match = 0;
        -- A match, substitute and recompute Last
        Substit (Working, N_Match, Info, By, Start);
        Match_Found := True;
        Last := Working.Length;
        -- Done with this pass
        exit Pass when Start > Last;
      end loop Pass;
      -- Done if no match occured during last pass
      exit Cycles when not Match_Found;
      -- Done when number of cycles reached
      exit Cycles when Cycle = Nb_Cycles;
    end loop Cycles;
    -- Build and return result
    return Normalize (Within (Within'First .. I1 - 1)
                    & Working.Image
                    & Within (I2 + 1 .. Within'Last));
  end Substit;


  -- Split Str into several substrings that match the substrings "(...)"
  --  of the criteria.
  -- Returns the array of slices (empty array if Str does not match).
  function Split (Str        : String;
                  Criteria   : String;
                  Max_Slices : Positive;
                  Options    : Options_Rec := Default_Options)
           return As.U.Utils.Asu_Array is
    -- Regex compilation
    Ok : Boolean;
    Compiled : Regular_Expressions.Compiled_Pattern;
    -- One extra cell (the first) to store global indexes
    Cells : Regular_Expressions.Match_Array (1 .. Max_Slices + 1);
    N_Matched : Natural;
  begin
    -- Compile regex
    Compile (Compiled, Ok, Criteria, Options);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;
    -- Execute regex
    Compiled.Exec (Str, N_Matched, Cells);
    -- Empty slice if no strict match
    if not Regular_Expressions.Strict_Match (Str, Cells) then
      declare
        Result : constant As.U.Utils.Asu_Array(1 .. 0) := (others => As.U.Asu_Null);
      begin
        return Result;
      end;
    end if;
    -- Build result: Copy
    declare
      Result : As.U.Utils.Asu_Array(1 .. N_Matched - 1);
    begin
      for I in Result'Range loop
        Result(I) := As.U.Tus (
         Str(Cells(I + 1).First_Offset .. Cells(I + 1).Last_Offset_Stop));
      end loop;
      return Result;
    end;
  end Split;

  -- Split Str into several substrings separated by strings matching the
  --  separator.
  -- Returns the array of slices (Str if no match).
  function Split_Sep (Str       : String;
                      Separator : String;
                      Options   : Options_Rec := Default_Options)
           return As.U.Utils.Asu_Array is
    -- Regex compilation
    Ok : Boolean;
    Compiled : Regular_Expressions.Compiled_Pattern;
    -- The match results
    Cell : Regular_Expressions.Match_Cell;
    -- Result
    Result : As.U.Utils.Asu_Ua.Unb_Array;
    -- For Locate
    From_Index : Natural;
    use type  Regular_Expressions.Match_Cell;
  begin
    -- Compile regex
    Compile (Compiled, Ok, Separator, Options);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;

    -- Prepare for successive searches
    From_Index := 1;

    -- Find successive occurences of Separator
    loop
      -- First occurent in tail
      Cell := Locate (Str, Compiled, From_Index, 0, True, 1);
      if Cell /= Regular_Expressions.No_Match then
          -- A match
        if Cell.Last_Offset_Start = Str'First then
          -- Str starts by a match, insert Asu_Null
          Result.Append (As.U.Asu_Null);
          From_Index := Cell.Last_Offset_Stop + 1;
        else
          -- A real match
          if Cell.Last_Offset_Stop = Str'Last then
            -- Str ends by a match, append this slice and Asu_Null and exit
            Result.Append (As.U.Tus (Str(From_Index .. Cell.First_Offset - 1)));
            Result.Append (As.U.Asu_Null);
            exit;
          else
            -- Append this slice
            Result.Append (As.U.Tus (Str(From_Index .. Cell.First_Offset - 1)));
            -- Tail starts after this separator
            From_Index := Cell.Last_Offset_Stop + 1;
          end if;
        end if;
      elsif From_Index = Str'First then
        -- No match at all
        Result := As.U.Utils.Asu_Ua.To_Unb_Array (As.U.Tus (Str));
        exit;
      else
        -- No more match: Append tail
        Result.Append(As.U.Tus (Str(From_Index .. Str'Last)));
        exit;
      end if;
      exit when From_Index > Str'Last;
    end loop;
    return Result.To_Array;
  end Split_Sep;

end Str_Util.Regex;

