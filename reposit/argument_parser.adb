with Ada.Unchecked_Deallocation, Ada.Characters.Latin_1;
with String_Mng, Int_Image;
package body Argument_Parser is

  -- Image without leading space
  function Image is new Int_Image (Natural);

  function Asu_Tus (Source : in String) return Asu.Unbounded_String
           renames Asu.To_Unbounded_String;

  -- The result of parsing of an arg
  -- If Index is not 0, then it is a single (char or string) key
  --  and Set and Options are set to the option if any
  -- Elsif Option is not empty, then it is the list of chars of
  --  a multiple chars key
  -- Else, it is "--"
  type Arg_Dscr is record
    Index : Natural;
    Char : Boolean;
    Set : Boolean;
    Option : Asu_Us;
  end record;

  -- Parse an argument
  -- P_Dscr.OK is set to false if not a valid key
  -- P_Dscr.Error is not set if not a key at all
  procedure Parse_Arg (The_Keys : in The_Keys_Type;
                       Arg_No : in Positive;
                       P_Dscr : in out Parsed_Dscr;
                       A_Dscr : out Arg_Dscr) is
    Str : constant String := Argument.Get_Parameter (Arg_No);
    Len : Natural;
    Crit : Asu_Us;
    Found : Boolean;
    use type Asu_Us;
  begin
    P_Dscr.Ok := False;
    A_Dscr.Index := 0;
    A_Dscr.Set := False;
    A_Dscr.Option := Asu_Null;
    if Str'Length >= 1 and then Str(1) = '-' then
      if Str = "-" then
        -- Just a "-": normal word
        return;
      end if;

      if Str'Length >= 2 and then Str(2) = '-' then
        if Str = "--" then
          P_Dscr.Ok := True;
          return;
        end if;
        -- Full key, look for option
        Len := String_Mng.Locate (Str, "=", 3);
        if Len = 3 then
          -- "--="
          P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
             & Image(Arg_No) & " is not valid");
          return;
        end if;
        -- Set Len to last of key string
        if Len = 0 then
          Crit := Asu_Tus (Str(3 .. Str'Last));
        else
          -- An option: save it
          A_Dscr.Set := True;
          A_Dscr.Option := Asu_Tus (Str(Len + 1 .. Str'Last));
          Crit := Asu_Tus (Str(3 .. Len - 1));
        end if;
        -- Find matching key
        for I in The_Keys'Range loop
          if The_Keys(I).Key_String = Crit then
            -- Found
            -- If it has an option and if it can
            if Len /= 0
            and then not The_Keys(I).Key_Can_Option then
              P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
                 & Image(Arg_No) & " cannot have option");
              return;
            end if;
            A_Dscr.Index := I;
            A_Dscr.Char := False;
            P_Dscr.Ok := True;
            return;
          end if;
        end loop;
        -- Not found
        P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
           & Image(Arg_No) & " is not expected");
        return;
      end if;

      -- Char key, no minus allowed
      if String_Mng.Locate (Str, "-", 2) /= 0 then
        P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
           & Image(Arg_No) & " contains minus");
        return;
      end if;
      -- Char key
      if Str'Length = 2 then
        -- A single char key, locate it
        for I in The_Keys'Range loop
          if The_Keys(I).Key_Char = Str(2) then
            -- Found
            A_Dscr.Index := I;
            A_Dscr.Char := True;
          end if;
        end loop;
        if A_Dscr.Index = 0 then
          -- Not found
          P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
             & Image(Arg_No) & " is not expected");
          return;
        end if;
        -- Found, see if it can have and has an option
        if The_Keys(A_Dscr.Index).Key_Can_Option
        and then Arg_No /= Argument.Get_Nbre_Arg then
          declare
            Option : constant String
                   := Argument.Get_Parameter (Arg_No + 1);
          begin
            if (Option'Length >= 1 and then Option(1) /= '-')
            or else Option = "-"
            or else Option = "" then
              A_Dscr.Set := True;
              A_Dscr.Option := Asu_Tus (Option);
            end if;
          end;
        end if;
        P_Dscr.Ok := True;
        return;
      else
        -- Multiple char key, check all are valid
        for I in 2 .. Str'Last loop
          Found := False;
          for J in The_Keys'Range loop
            if Str(I) = The_Keys(J).Key_Char then
              Found := True;
              exit;
            end if;
          end loop;
          if not Found then
            P_Dscr.Error := Asu_Tus ("Argument " & Str & " at pos "
               & Image(Arg_No) & " has not expected key " & Str(I));
            return;
          end if;
        end loop;
        A_Dscr.Index := 0;
        A_Dscr.Char := True;
        A_Dscr.Option := Asu_Tus (Str(2 .. Str'Last));
        P_Dscr.Ok := True;
        return;
      end if;
    end if;
  end Parse_Arg;


  -- Constructor
  -- One key has both Char_Key and String_Key unset
  -- No_Key : exception;
  -- Two keys have same Char or String key
  -- Dup_Key : exception;
  function Parse (The_Keys : The_Keys_Type) return Parsed_Dscr is
    -- The result
    Dscr : Parsed_Dscr;
    -- A parsed argument
    Arg : Arg_Dscr;
    -- An indicator that next/current argument is an option
    Is_Option : Boolean;
    use type Asu_Us;
  begin
    -- First check the keys
    for I in The_Keys'Range loop
      -- No key must have both unset
      if The_Keys(I).Key_Char = No_Key_Char
      and then The_Keys(I).Key_String = No_Key_String then
        raise No_Key;
      end if;
      -- Key char and string must be unique
      for J in I + 1 .. The_Keys'Last loop
        if The_Keys(I).Key_Char = The_Keys(J).Key_Char
        and then The_Keys(I).Key_Char /= No_Key_Char then
          raise Dup_Key;
        end if;
        if The_Keys(I).Key_String = The_Keys(J).Key_String
        and then The_Keys(I).Key_String /= No_Key_String then
          raise Dup_Key;
        end if;
      end loop;
      -- Short key must not be an unprintable character
      if The_Keys(I).Key_Char < ' ' or else  The_Keys(I).Key_Char > '~' then
        raise Unprintable_Key;
      end if;
      -- Long key must not contain space or unprintable character
      declare
        Str : constant String := Asu.To_String (The_Keys(I).Key_String);
      begin
        for J in Str'Range loop
          if Str(J) <= ' ' or else Str(J) > '~' then
            raise Unprintable_Key;
          end if;
        end loop;
      end;
    end loop;

    -- Parse all the arguments
    Dscr.Ok := False;
    Is_Option := False;
    for I in 1 .. Argument.Get_Nbre_Arg loop
      -- Detect Argument_Too_Long
      begin
        if Argument.Get_Parameter (I) = "" then
          null;
        end if;
      exception
        when Argument.Argument_Too_Long =>
          Dscr.Error := Asu_Tus ("Argument at pos "
             & Image(I) & " is too long");
          return Dscr;
      end;
      -- Parse this argument
      Parse_Arg (The_Keys, I, Dscr, Arg);
      if not Dscr.Ok then
        if Dscr.Error /= Asu_Null then
          -- Error detected
          return Dscr;
        end if;
        -- Else, not key argument
      end if;
      -- Check this arg for Key_Can_Multiple, set terminator indexes
      if Arg.Index /= 0 then
        -- A valid single Char or String key
        if Dscr.First_Occurence(Arg.Index) = 0 then
          Dscr.First_Occurence(Arg.Index) := I;
        elsif not The_Keys(Arg.Index).Key_Can_Multiple then
          Dscr.Ok := False;
          Dscr.Error := Asu_Tus ("Argument "
             & Argument.Get_Parameter (Occurence => I)
             & " at pos "
             & Image(I) & " appears several times");
          return Dscr;
        end if;
        Dscr.Nb_Occurences(Arg.Index) := Dscr.Nb_Occurences(Arg.Index) + 1;
        Dscr.Last_Pos_Key := I;
        Dscr.Nb_Embedded := Dscr.Nb_Occurences(No_Key_Index);
        if Arg.Char and then Arg.Set then
          -- A Char key with option
          Dscr.First_Pos_After_Keys := I + 2;
          Is_Option := True;
        else
          -- Nex arg is not key
          Dscr.First_Pos_After_Keys := I + 1;
          Is_Option := False;
        end if;
      elsif Arg.Option /= Asu_Null then
        -- A valid group of Char keys, check each char
        for J in 1 .. Asu.Length (Arg.Option) loop
          -- Locate the corresponding key (existence has already been checked)
          for K in The_Keys'Range loop
            if Asu.Element (Arg.Option, J) = The_Keys(K).Key_Char then
              if Dscr.First_Occurence(K) = 0 then
                Dscr.First_Occurence(K) := I;
              elsif not The_Keys(K).Key_Can_Multiple then
                Dscr.Ok := False;
                Dscr.Error := Asu_Tus ("Argument "
                   & Argument.Get_Parameter (Occurence => I)
                   & " at pos "
                   & Image(I) & " makes key " & The_Keys(K).Key_Char
                   & " appear several times");
                return Dscr;
              end if;
              Dscr.Nb_Occurences(K) := Dscr.Nb_Occurences(K) + 1;
              exit;
            end if;
          end loop;
        end loop;
        Dscr.Last_Pos_Key := I;
        Dscr.Nb_Embedded := Dscr.Nb_Occurences(No_Key_Index);
        -- Nex arg is not key
        Dscr.First_Pos_After_Keys := I + 1;
        Is_Option := False;
      else
        -- Not key or "--"
        if Argument.Get_Parameter (I) = "--" then
          -- Done if "--", all remaining are arguments
          Dscr.First_Pos_After_Keys := I + 1;
          Dscr.Nb_Occurences(No_Key_Index) := Dscr.Nb_Occurences(No_Key_Index)
                                 + Argument.Get_Nbre_Arg - I;
          if Dscr.Nb_Occurences(No_Key_Index) /= 0
          and then Dscr.First_Occurence(No_Key_Index) = 0 then
            Dscr.First_Occurence(No_Key_Index) := I + 1;
          end if;
          exit;
        else
          -- Check that this is not the option of a previous simple char key
          if not Is_Option then
            if Dscr.First_Occurence(No_Key_Index) = 0 then
              Dscr.First_Occurence(No_Key_Index) := I;
            end if;
            Dscr.Nb_Occurences(No_Key_Index) :=
                 Dscr.Nb_Occurences(No_Key_Index) + 1;
          end if;
        end if;
        Is_Option := False;
      end if;
    end loop;

    -- Adjust First not key when none are keys
    if Dscr.First_Pos_After_Keys = 0 then
      Dscr.First_Pos_After_Keys := 1;
    end if;
    -- Adjust First not key when all are keys or embedded arguments
    if Dscr.First_Pos_After_Keys > Argument.Get_Nbre_Arg then
      Dscr.First_Pos_After_Keys := 0;
    end if;
    -- Store the keys
    Dscr.Ok := True;
    Dscr.The_Keys := new The_Keys_Type'(The_Keys);
    return Dscr;
  end Parse;

  -- Free the keys
  -- Clean memory allocated during parsing
  procedure Deallocate is new Ada.Unchecked_Deallocation (
      The_Keys_Type, Keys_Access);
  procedure Reset (Dscr : in out Parsed_Dscr) is
  begin
    if Dscr.Ok and then Dscr.The_Keys /= null then
      Deallocate (Dscr.The_Keys);
      Dscr.Ok := False;
      Dscr.The_Keys := null;
      Dscr.Error := Asu_Null;
    else
      raise Parsing_Error;
    end if;
  end Reset;

  -- Was parsing OK
  function Is_Ok (Dscr : Parsed_Dscr) return Boolean is
  begin
    return Dscr.Ok;
  end Is_Ok;

  -- Error string
  -- Possible returned strings:
  --  "OK."
  --  "Argument <arg> at pos <i> is not expected."
  --  "Argument at pos <i> is too long."
  --  "Argument <arg> at pos <i> is too long."
  --  "Argument <arg> at pos <i> appears several times."
  --  "Argument <arg> at pos <i> appears shall not have option."
  function Get_Error (Dscr : Parsed_Dscr) return String is
  begin
    if Dscr.Ok then
      return "OK.";
    else
      return Asu.To_String (Dscr.Error);
    end if;
  end Get_Error;


  -- All the following operations may raise
  -- Parsing_Error : exception;
  --  if called on a Dscr that is not Parsed_Is_Ok.
  function Get_Number_Keys (Dscr : Parsed_Dscr) return Natural is
    Total : Natural := 0;
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    for I in Dscr.The_Keys.all'Range loop
      Total := Total + Dscr.Nb_Occurences(I);
    end loop;
    return Total;
  end Get_Number_Keys;

  -- Return the position of the last argument related to keys (including
  --  the possible option of a char key)
  function Get_Last_Pos_Of_Keys (Dscr : Parsed_Dscr) return Natural is
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    return Dscr.Last_Pos_Key;
  end Get_Last_Pos_Of_Keys;

  -- Return position of first argument not related to keys (taking into account
  --  the possible option of a char key) and skipping "--" if any
  function Get_First_Pos_After_Keys (Dscr : Parsed_Dscr) return Natural is
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    return Dscr.First_Pos_After_Keys;
  end Get_First_Pos_After_Keys;

  -- Return the number of embedded arguents, arguments that are not a key nor
  --  an option but are followed by a key.
  function Get_Nb_Embedded_Arguments (Dscr : Parsed_Dscr) return Natural is
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    return Dscr.Nb_Embedded;
  end Get_Nb_Embedded_Arguments;

  -- The following operations alow retreiving info per key
  -- Index is relative to the array provided as input

  -- Nb of occurences of the key, possibly 0
  function Get_Nb_Occurences (Dscr  : Parsed_Dscr;
                              Index : The_Keys_Index) return Natural is
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    if Index > Dscr.The_Keys'Last then
      raise Invalid_Index;
    end if;
    return Dscr.Nb_Occurences(Index);
  end Get_Nb_Occurences;

  function Is_Set (Dscr  : Parsed_Dscr;
                   Index : The_Keys_Index) return Boolean is
  begin
    return Get_Nb_Occurences (Dscr, Index) /= 0;
  end Is_Set;

  -- Does an argument match the key, returns the option or No_Match if not match
  No_Match : constant String := "" & Ada.Characters.Latin_1.Nul;
  function Match (Arg_No : Positive; Key : A_Key_Type) return String is
    Str : constant String := Argument.Get_Parameter (Arg_No);
    Len : Natural;
  begin
    if Str'Length < 2 or else Str(1) /= '-' or else Str = "--" then
      return No_Match;
    end if;
    if Str(2) = '-' then
      -- Full key, look for option
      Len := String_Mng.Locate (Str, "=", 3);
      -- Set Len to last of key string
      if Len = 0 then
        Len := Str'Last;
      else
        Len := Len - 1;
      end if;
      if Str(3 .. Len) /= Asu.To_String (Key.Key_String) then
        return No_Match;
      else
        -- Return the option if any
        return Str (Len + 2 .. Str'Last);
      end if;
    end if;
    -- A char key
    if Str'Length > 2 then
      -- Multiple char keys
      if String_Mng.Locate (Str, "" & Key.Key_Char, 2) = 0 then
        return No_Match;
      else
        -- No option possible
        return "";
      end if;
    end if;
    -- Single char key
    if Str(2) /= Key.Key_Char then
      return No_Match;
    end if;
    -- Match, check option
    if not Key.Key_Can_Option
    or else Arg_No = Argument.Get_Nbre_Arg then
      -- Cannot have option
      return "";
    end if;
    -- Look at next arg
    declare
      Option : constant String
             := Argument.Get_Parameter (Arg_No + 1);
    begin
      if Option'Length >= 1 and then
         (Option(1) /= '-' or else Option = "-") then
        return Option;
      else
        return "";
      end if;
    end;
  end Match;

  -- Raised anonymous exception when a key/option... shall be found but is
  --  not.
  Internal_Error : exception;

  -- Is a string a key or an option
  function Is_Key (Arg_No : Positive; The_Keys : The_Keys_Type)
                  return Boolean is
    Str : constant String := Argument.Get_Parameter (Arg_No);
    Char : Character;
  begin
    -- First solve the easiest cases
    if Str /= "" and then Str(1) = '-' and then Str /= "-" then
      -- This is a key
      return True;
    elsif Arg_No = 1 then
      -- First arg cannot be an option nor a not_key
      return False;
    end if;
    -- Now we have a non-key arg, see if previous arg is a single char key
    declare
      Prev : constant String := Argument.Get_Parameter (Arg_No - 1);
    begin
      if Prev'Length = 2 and then Prev(1) = '-' and then Prev(2) /= '-' then
        -- Prev is a single char key
        Char := Prev(2);
      else
        return False;
      end if;
    end;
    -- Now we need to know if this key allows options
    for I in The_Keys'Range loop
      if The_Keys(I).Key_Char = Char then
        return The_Keys(I).Key_Can_Option;
      end if;
    end loop;
    -- Normaly we should have found the key
    raise Internal_Error;
  end Is_Key;

  -- Option of a key, possibly empty
  function Get_Option (Dscr      : Parsed_Dscr;
                       Index     : The_Keys_Index;
                       Occurence : Positive := 1) return String is
    Loc : Positive;
  begin
    Loc := Get_Position (Dscr, Index, Occurence);

    -- Handle No_Key_Index
    if Index = No_Key_Index then
      return Argument.Get_Parameter (Loc);
    else
      return Match (Loc, Dscr.The_Keys(Index));
    end if;
  end Get_Option;

  -- Absolute position of an occurence
  function Get_Position (Dscr      : Parsed_Dscr;
                         Index     : The_Keys_Index;
                         Occurence : Positive := 1) return Positive is
    Loc : Natural;
    No_More_Keys : Boolean;
  begin
    if not Dscr.Ok then
      raise Parsing_Error;
    end if;
    if Index > Dscr.The_Keys'Last then
      raise Invalid_Index;
    end if;

    -- Handle No_Key_Index
    if Index = No_Key_Index then
      if Occurence > Dscr.Nb_Occurences(Index) then
        raise Invalid_Occurence;
      elsif Occurence = 1 then
        return Dscr.First_Occurence(No_Key_Index);
      end if;
      -- Iterate from first occurence to last argument
      Loc := 0;
      No_More_Keys := False;
      for No in 1 .. Argument.Get_Nbre_Arg loop
        if Argument.Get_Parameter (No) = "--"
        and then not No_More_Keys then
          -- First '--', end of options
          No_More_Keys := True;
        elsif No_More_Keys then
          Loc := Loc + 1;
        elsif not Is_Key (No, Dscr.The_Keys.all) then
          Loc := Loc + 1;
        end if;
        if Loc = Occurence then
          -- Got the expected occurence
          return No;
        end if;
      end loop;
      -- Normally not reached
      raise Internal_Error;
    end if;

    -- Normal key
    if Occurence > Dscr.Nb_Occurences(Index) then
      raise Invalid_Occurence;
    elsif Occurence = 1 then
      return Dscr.First_Occurence(Index);
    else
      -- Iterate from first occurence to Last_Pos_Key
      Loc := 1;
      for Arg in Dscr.First_Occurence(Index) + 1 .. Dscr.Last_Pos_Key loop
        declare
          Opt : constant String := Match (Arg, Dscr.The_Keys(Index));
        begin
          if Opt /= No_Match then
            -- This argument does match
            Loc := Loc + 1;
            if Loc = Occurence then
              -- Got the expected occurence
              return Arg;
            end if;
          end if;
        end;
      end loop;
      -- Normally not reached
      raise Internal_Error;
    end if;
  end Get_Position;

  overriding procedure Finalize (Dscr : in out Parsed_Dscr) is
  begin
    if Dscr.The_Keys /= null then
      Deallocate (Dscr.The_Keys);
    end if;
    Dscr.Ok := False;
    Dscr.Error := Asu_Null;
    Dscr.The_Keys := null;
  end Finalize;

  overriding procedure Adjust (Dscr : in out Parsed_Dscr) is
    Keys : Keys_Access;
  begin
    if Dscr.The_Keys = null then
      return;
    end if;
    -- Allocate and copy keys
    Keys := new The_Keys_Type'(Dscr.The_Keys.all);
    Dscr.The_Keys := Keys;
  end Adjust;

end Argument_Parser;

