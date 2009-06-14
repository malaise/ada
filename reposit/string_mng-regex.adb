-- More powerfull search and substitution in strings,
--  based on regex.
with Char_To_Hexa, Upper_Str, Lower_Str, Mixed_Str;
package body String_Mng.Regex is


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
                   Occurence  : Positive := 1)
           return Search_Result is
    I1, I2 : Natural;
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    Occ : Natural;
    N_Match : Natural;
    Info, Prev : Regular_Expressions.One_Match_Array;
    use type Regular_Expressions.One_Match_Array;
  begin
    -- Empty Within => No_match
    if Within'Length = 0 then
      return No_Match;
    end if;
    -- Initialise indexes
    if From_Index = 0 then
      I1 := Within'First;
    else
      I1 := From_Index;
    end if;
    if To_Index = 0 then
      I2 := Within'Last;
    else
      I2 := To_Index;
    end if;
    if I1 not in Within'Range
    or else I2 not in Within'Range then
      raise Invalid_Index;
    end if;

    -- Compile regex
    Regular_Expressions.Compile (Compiled, Ok, Criteria);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;

    Occ := 0;
    Prev(1) := Regular_Expressions.Match_Cell(No_Match);
    if Forward then
      -- Loop as long as matches and until Occ different matching
      for I in I1 .. I2 loop
        Regular_Expressions.Exec (Compiled, Within(I .. I2), N_Match, Info);
        if N_Match = 0 then
          return No_Match;
        elsif Info /= Prev then
          -- One new matching occurence
          Occ := Occ + 1;
          if Occ = Occurence then
            -- Correct occurence number
            return Search_Result(Info(1));
          end if;
          Prev := Info;
        end if;
      end loop;
    else
      -- Loop until Occ different matching
      for I in reverse I1 .. I2 loop
        Regular_Expressions.Exec (Compiled, Within(I .. I2), N_Match, Info);
        if Info /= Prev then
          -- One new matching occurence
          Occ := Occ + 1;
          if Occ = Occurence then
            -- Correct occurence number
            return Search_Result(Info(1));
          end if;
          Prev := Info;
        end if;
      end loop;
      return No_Match;
    end if;
    -- Just to be safe
    return No_Match;
  end Locate;

  -- Tools for replace
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Us : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;

  -- Replace Working(Info(1).First_Offset .. Info(1).End_Offset)
  -- by By.
  -- In By, \i (0 <= i <= 9) is replaced by
  --  Working (Info(i-1).First_Offset .. Info(i-1).End_Offset)
  -- Start is set to the first char after new string (maybe above Working)
  procedure Substit (Working : in out Asu_Us;
                     N_Match : in Positive;
                     Info    : in Regular_Expressions.Match_Array;
                     By      : in String;
                     Start   : out Natural) is
    -- New By after substitution of \0, \1...
    Newby : Asu_Us;
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
    Newby := Asu_Tus (By);
    From := 1;
    loop
      Esc := Locate_Escape (Asu_Ts (Newby), From, "\\0123456789x");
      exit when Esc = 0;
      Char := Asu.Element (Newby, Esc);
      if Char = '\' then
        -- "\\" -> "\"
        Newby := Asu.Delete (Newby, Esc, Esc);
        From := Esc;
      elsif Char >= '0' and then Char <= '9' then
        -- "\i" -> matching slice of Working:
        -- i is Index in Info - 1: \0 -> Info(1)...
        Info_Index := Character'Pos (Char) - Character'Pos ('0') + 1;
        if Info_Index > N_Match
        or else (Linfo(Info_Index).First_Offset = 1
                 and then Linfo(Info_Index).Last_Offset_Stop = 0) then
          -- "\i" -> "" if no such matching info
          Newby := Asu.Delete (Newby, Esc - 1, Esc);
        else
          Newby := Asu.Replace_Slice (Newby, Esc - 1, Esc,
                     Asu.Slice (Working, Linfo(Info_Index).First_Offset,
                                         Linfo(Info_Index).Last_Offset_Stop));
          -- Move forward by the length of new string.
          -- One back to \ then length forward
          From := Esc + Linfo(Info_Index).Last_Offset_Stop
                      - Linfo(Info_Index).First_Offset;
        end if;
      elsif Char = 'x' and then Esc <= Asu.Length (Newby) - 2 then
        declare
          Byte : Integer;
        begin
          Byte := 16#10# * Char_To_Hexa (Asu.Element (Newby, Esc + 1))
                        +  Char_To_Hexa (Asu.Element (Newby, Esc + 2));
          -- Replace "\xIJ" by the code
          Newby := Asu.Replace_Slice (Newby, Esc - 1, Esc + 2,
                        Character'Val (Byte) & "");
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
      exit when From > Asu.Length (Newby);
    end loop;

    -- Apply case switches to replacement string
    Case_Start := 1;
    Case_Char := 'c';
    From := 1;
    loop
      Esc := Locate_Escape (Asu_Ts (Newby), From, "\umlc");
      if Esc = 0 then
        -- No more escape
        if Case_Char = 'c' then
          -- No current conversion: Done
          exit;
        else
          -- Apply last conversion: simulate an appended "\c"
          Char := 'c';
          Esc := Asu.Length (Newby) + 2;
        end if;
      else
        -- Store the new char and delete this sequence
        Char := Asu.Element (Newby, Esc);
        Newby := Asu.Delete (Newby, Esc - 1, Esc);
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
        Newby := Asu.Replace_Slice (Newby, Case_Start, Esc - 2,
          Upper_Str (Asu.Slice (Newby, Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      elsif Case_Char = 'm' then
        -- Replace from Case_Start to \u by Mixed
        --  of str from Case_Start to before \
        Newby := Asu.Replace_Slice (Newby, Case_Start, Esc - 2,
          Mixed_Str (Asu.Slice (Newby, Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      elsif Case_Char = 'l' then
        -- Replace from Case_Start to \u by lowercase
        --  of str from Case_Start to before \
        Newby := Asu.Replace_Slice (Newby, Case_Start, Esc - 2,
          Lower_Str (Asu.Slice (Newby, Case_Start, Esc - 2)));
        Case_Start := Esc - 1;
        Case_Char := Char;
      else
         -- Impossible, bug in Locate_Escape
         null;
      end if;
      exit when From > Asu.Length (Newby);
    end loop;

    -- Replace the matching slice of Working by the Newby
    Asu.Replace_Slice (Working, Linfo(1).First_Offset, Linfo(1).Last_Offset_Stop,
                       Asu_Ts (Newby));
    -- Update Start to the first character after the matching slice
    Start := Linfo(1).First_Offset + Asu.Length (Newby);
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
  function Replace (Within     : String;
                    Criteria   : String;
                    By         : String;
                    From_Index : Natural := 0;
                    To_Index   : Natural := 0;
                    Nb_Cycles  : Natural := 1)
           return String is
    -- Working string
    I1, I2 : Natural;
    Working : Asu_Us;
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
    if From_Index = 0 then
      I1 := Within'First;
    else
      I1 := From_Index;
    end if;
    if To_Index = 0 then
      I2 := Within'Last;
    else
      I2 := To_Index;
    end if;
    if I1 not in Within'Range
    or else I2 not in Within'Range then
      raise Invalid_Index;
    end if;
    -- Extract working string
    Working := Asu_Tus (Within(I1 .. I2));

    -- Compile regex
    Regular_Expressions.Compile (Compiled, Ok, Criteria);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;

    -- Loop on scanning the string
    Cycle := 1;
    Cycles:
    loop
      -- Scan the string from start to end
      Start := 1;
      Last := Asu.Length (Working);
      -- Substitutions may lead to empty string
      exit Cycles when Last = 0;
      Match_Found := False;
      Pass:
      loop
        Regular_Expressions.Exec (Compiled, Asu_Ts (Working) (Start .. Last),
          N_Match, Info);
        -- No (more) match?
        exit Pass when N_Match = 0;
        -- A match, substitute and recompute Last
        Substit (Working, N_Match, Info, By, Start);
        Match_Found := True;
        Last := Asu.Length (Working);
        -- Done with this pass
        exit Pass when Start > Last;
      end loop Pass;
      -- Done if no match occured during last pass
      exit when not Match_Found;
      -- Done when number of cycles reached
      exit when Cycle = Nb_Cycles;
    end loop Cycles;
    -- Build and return result
    return Within (Within'First .. I1 - 1)
         & Asu_Ts (Working)
         & Within (I2 + 1 .. Within'Last);
  end Replace;


  -- Split Str into several substrings that match the substrings "(...)"
  --  of the criteria.
  -- Returns the array of slices (empty array if Str does not match).
  function Split (Str : String;
                  Criteria : String;
                  Max_Slices : Positive) return String_Slice is
    -- Regex compilation
    Ok : Boolean;
    Compiled : Regular_Expressions.Compiled_Pattern;
    -- One extra cell (the first) to store global indexes
    Cells : Regular_Expressions.Match_Array (1 .. Max_Slices + 1);
    N_Matched : Natural;
  begin
    -- Compile regex
    Regular_Expressions.Compile (Compiled, Ok, Criteria);
    if not Ok then
      raise Invalid_Regular_Expression;
    end if;
    -- Execute regex
    Regular_Expressions.Exec (Compiled, Str, N_Matched, Cells);
    -- Empty slice if no strict match
    if N_Matched <= 1
    or else  Cells(1).First_Offset /= Str'First
    or else  Cells(1).Last_Offset_Stop /= Str'Last then
      declare
        Result : constant String_Slice (1 .. 0)
               := (others => Ada.Strings.Unbounded.Null_Unbounded_String);
      begin
        return Result;
      end;
    end if;
    -- Build result: Copy
    declare
      Result : String_Slice (1 .. N_Matched - 1);
    begin
      for I in Result'Range loop
        Result(I) := Ada.Strings.Unbounded.To_Unbounded_String (
         Str(Cells(I + 1).First_Offset .. Cells(I + 1).Last_Offset_Stop));
      end loop;
      return Result;
    end;
  end Split;

end String_Mng.Regex;


