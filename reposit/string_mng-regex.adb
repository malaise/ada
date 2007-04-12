-- More powerfull search and substitution in strings,
--  based on regex.
with Ada.Strings.Unbounded;
with Utf_8;
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

  -- Check if Char is the startup of a valid UTF-8 sequence
  --  and increment Offset accordingly
  procedure Apply_Utf8 (Char : in Character;
                        Offset : in out Regular_Expressions.Offset_Range) is
    Len : Utf_8.Len_Range;
  begin
    -- Get lenght of sequence
    Len := Utf_8.Nb_Chars (Char);
    -- Apply offset
    Offset := Offset + Len - 1;
  exception
    when Utf_8.Invalid_Sequence =>
      -- Leave Offset unchanged
      null;
  end Apply_Utf8;

  -- Replace Working(Info(1).Start_Offset .. Info(1).End_Offset)
  -- by By.
  -- In By, \i (0 <= i <= 9) is replaced by
  --  Working (Info(i-1).Start_Offset .. Info(i-1).End_Offset)
  -- Start is set to the first char after new string (maybe above Working)
  procedure Substit (Working : in out Asu_Us;
                     N_Match : in Positive;
                     Info    : in Regular_Expressions.Match_Array;
                     By      : in String;
                     Utf_8   : in Boolean;
                     Start   : out Natural) is
    -- New By after substitution of \0, \1...
    Newby : Asu_Us;
    -- For build Newby
    Esc : Natural;
    From : Positive;
    Char : Character;
    Info_Index : Positive;
    Linfo : Regular_Expressions.Match_Array := Info;
  begin
    -- Compute Newby
    Newby := Asu_Tus (By);
    From := 1;
    loop
      Esc := Locate_Escape (Asu_Ts (Newby), From, "\\0123456789");
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
        or else (Linfo(Info_Index).Start_Offset = 1
                 and then Linfo(Info_Index).End_Offset = 0) then
          -- "\i" -> "" if no such matching info
          Newby := Asu.Delete (Newby, Esc - 1, Esc);
        else
          if Utf_8 then
            -- Apply UTF-8 correction if End_Offset is the first char of a UTF8
            -- sequence
            Apply_Utf8 (Asu.Element (Working, Linfo(Info_Index).End_Offset),
                        Linfo(Info_Index).End_Offset);
          end if;
          Newby := Asu.Replace_Slice (Newby, Esc - 1, Esc,
                     Asu.Slice (Working, Linfo(Info_Index).Start_Offset,
                                         Linfo(Info_Index).End_Offset));
          -- Move forward by the length of new string.
          -- One back to \ then length forward
          From := Esc + Linfo(Info_Index).End_Offset
                      - Linfo(Info_Index).Start_Offset;
        end if;
      else
        -- "\x" unchanged
        From := Esc;
      end if;
      exit when From > Asu.Length (Newby);
    end loop;

    if Utf_8 then
      -- Apply UTF-8 correction if End_Offset is the first char of a UTF8
      -- sequence
      Apply_Utf8 (Asu.Element (Working, Linfo(1).End_Offset),
                        Linfo(1).End_Offset);
    end if;
    -- Replace the matching slice of Working by the Newby
    Asu.Replace_Slice (Working, Linfo(1).Start_Offset, Linfo(1).End_Offset,
                       Asu_Ts (Newby));
    -- Update Start to the first character after the matching slice
    Start := Linfo(1).Start_Offset + Asu.Length (Newby);
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
                    Nb_Cycles  : Natural := 1;
                    Utf_8      : Boolean := True)
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
        Substit (Working, N_Match, Info, By, Utf_8, Start);
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


end String_Mng.Regex;


