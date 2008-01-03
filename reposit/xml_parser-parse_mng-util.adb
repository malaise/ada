with String_Mng;
separate (Xml_Parser.Parse_Mng)
package body Util is

  -- Current line of input
  function Get_Line_No (Flow : Flow_Type) return Natural is
  begin
    case Flow.Kind is
      when Xml_File | Xml_String =>
        return Flow.Xml_Line;
      when Dtd_File =>
        return Flow.Dtd_Line;
    end case;
  end Get_Line_No;

  ------------------
  -- Syntax check --
  ------------------
  function Is_Letter (Char : Character) return Boolean is
  begin
    return (Char >= 'a' and then Char <= 'z')
    or else (Char >= 'A' and then Char <= 'Z');
  end Is_Letter;
  function Is_Digit (Char : Character) return Boolean is
  begin
    return Char >= '0' and then Char <= '9';
  end Is_Digit;
  function Is_Valid_In_Name (Char : Character) return Boolean is
  begin
    return Is_Letter (Char)
           or else Is_Digit (Char)
           or else Char = '_'
           or else Char = ':'
           or else Char = '-'
           or else Char = '.';
  end Is_Valid_In_Name;

  -- Check that a Name is correct
  function Name_Ok (Name : Asu_Us;
                    Allow_Token : Boolean := False) return Boolean is
    Char : Character;
  begin
    -- Must not be empty
    if Asu.Length (Name) = 0 then
      return False;
    else
      -- For true name (not token) first char must be letter or '_' or ':'
      Char := Asu.Element (Name, 1);
      if not Allow_Token
      and then (Is_Digit (Char) or else Char = '-' or else Char = '.') then
        return False;
      end if;
      -- Other chars must be letter, digit, or '_' or ':' or '-'
      for I in 1 .. Asu.Length (Name) loop
        Char := Asu.Element (Name, I);
        if not Is_Valid_In_Name (Char) then
          return False;
        end if;
      end loop;
      return True;
    end if;
  end Name_Ok;

  -- Check that Str defines valid names seprated by Sep
  function Names_Ok (Str : Asu_Us;
                     Seps : String;
                     Allow_Token : Boolean := False) return Boolean is
    S : String(1 .. Asu.Length (Str)) := Asu_Ts (Str);
    I1, I2 : Natural;
    function Is_Sep (C : Character) return Boolean is
    begin
      for I in Seps'Range loop
        if C = Seps(I) then
          return True;
        end if;
      end loop;
      return False;
    end Is_Sep;
  begin
    -- Must not be empty
    if S = "" then
      return False;
    end if;
    -- Identify words
    I1 := S'First;
    Word:loop
      -- Look for start of word
      loop
        exit Word when I1 > S'Last;
        exit when not Is_Sep (S(I1));
        I1 := I1 + 1;
      end loop;
      -- Look for end of word
      I2 := I1;
      loop
        I2 := I2 + 1;
        exit when I2 > S'Last or else Is_Sep (S(I2));
      end loop;
      -- Check word
      if not Name_Ok (Asu_Tus (S(I1 .. I2 - 1)), Allow_Token) then
        return False;
      end if;
      -- Done
      exit Word when I2 > S'Last;
      -- Ready for next word
      I1 := I2;
    end loop Word;
    -- All names were OK
    return True;
  end Names_Ok;


  ------------------
  -- Getting char --
  ------------------
  -- Circular buffer of read characters
  Max_Len : constant := 10;

  -- Separator for current line of input
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  procedure Error (Flow : in out Flow_Type;
                   Msg : in String; Line_No : in Natural := 0) is
    Err_Msg : Asu_Us;
  begin
    Err_Msg := Asu_Tus ("Xml_Parse error at line");
    if Line_No = 0 then
      Asu.Append (Err_Msg, Natural'Image(Get_Line_No(Flow)));
    else
      Asu.Append (Err_Msg, Line_No'Img);
    end if;
    if Flow.Kind = Dtd_File then
      Asu.Append (Err_Msg, " of dtd");
    end if;
    Asu.Append (Err_Msg, ": " & Msg & ".");
    -- The error message is attached to the exception
    -- Xml_parser will copy it in the Flow.
    Exception_Messenger.Raise_Exception (Parse_Error'Identity,
                                         Asu_Ts (Err_Msg));
  end Error;

  -- Get character and store in queue
  procedure Get (Flow : in out Flow_Type; Char : out Character) is
  begin
    case Flow.Kind is
      when Xml_File =>
        Char := Text_Char.Get (Flow.Xml_File);
      when Xml_String =>
        if Flow.In_Stri = Asu.Length (Flow.In_Str) then
          raise End_Error;
        end if;
        Flow.In_Stri := Flow.In_Stri + 1;
        Char := Asu.Element (Flow.In_Str, Flow.In_Stri);
      when Dtd_File =>
        Char := Text_Char.Get (Flow.Dtd_File);
    end case;
    My_Circ.Push (Flow.Circ, Char);
    if Char = Lf then
      if Flow.Kind /= Dtd_File then
        Flow.Xml_Line := Flow.Xml_Line + 1;
      else
        Flow.Dtd_Line := Flow.Dtd_Line + 1;
      end if;
    end if;
  exception
    when Text_Char.End_Error =>
      raise End_Error;
  end Get;

  -- Get a string
  procedure Get (Flow : in out Flow_Type; Str : out String) is
  begin
    Flow.Nb_Got := 0;
    for I in Str'Range loop
      Get (Flow, Str(I));
      Flow.Nb_Got := Flow.Nb_Got + 1;
    end loop;
  end Get;

  -- Get N characters
  procedure Get (Flow : in out Flow_Type; N : in Positive; Str : out String) is
  begin
    Get (Flow, Str(Str'First .. Str'First + N - 1));
  end Get;

  -- Undo some gets (default 1)
  procedure Unget (Flow : in out Flow_Type; N : Positive := 1) is
    Char : Character;
  begin
    for I in 1 .. N loop
      My_Circ.Look_Last (Flow.Circ, Char);
      My_Circ.Discard_Last (Flow.Circ);
      case Flow.Kind is
        when Xml_File =>
          Text_Char.Unget (Flow.Xml_File, Char);
        when Xml_String =>
          Flow.In_Stri := Flow.In_Stri - 1;
        when Dtd_File =>
          Text_Char.Unget (Flow.Dtd_File, Char);
      end case;
      if Char = Lf then
        if Flow.Kind /= Dtd_File then
          Flow.Xml_Line := Flow.Xml_Line - 1;
        else
          Flow.Dtd_Line := Flow.Dtd_Line - 1;
        end if;
      end if;
    end loop;
  end Unget;

  -- Read last char got
  procedure Read (Flow : in out Flow_Type; Char : out Character) is
  begin
    My_Circ.Look_Last (Flow.Circ, Char);
  end Read;

  -- Read Str'Length chars got
  procedure Read (Flow : in out Flow_Type; Str : out String) is
    L : constant Integer := Str'Last;
  begin
    for I in 1 .. Str'Length loop
      -- I=1 => last pushed => Str'Last
      My_Circ.Look_Last (Flow.Circ, Str(L - I + 1), I);
    end loop;
  end Read;

  -------------
  -- Parsing --
  -------------
  -- Detect separator
  function Is_Separator (Char : Character) return Boolean is
  begin
    return Char = Space
    or else Char = Ada.Characters.Latin_1.Lf
    or else Char = Ada.Characters.Latin_1.Cr
    or else Char = Ada.Characters.Latin_1.Ht;
  end Is_Separator;

  -- Skip separators until a significant char (not separator) is got
  procedure Skip_Separators (Flow : in out Flow_Type) is
    Char : Character;
  begin
    loop
      Get (Flow, Char);
      exit when not Is_Separator (Char);
    end loop;
    Unget (Flow);
  end Skip_Separators;

  -- Skip separators, return the skipped separators
  procedure Get_Separators (Flow : in out Flow_Type;
                              Seps : out Asu_Us) is
    Char : Character;
    use type Asu_Us;
  begin
    Seps := Asu_Null;
    loop
      Get (Flow, Char);
      exit when not Is_Separator (Char);
      Seps := Seps & Char;
    end loop;
    Unget (Flow);
  end Get_Separators;

  function Get_Curr_Str (Flow : Flow_Type) return Asu_Us is
  begin
    return Flow.Curr_Str;
  end Get_Curr_Str;

  procedure Reset_Curr_Str (Flow : in out Flow_Type) is
  begin
    Flow.Curr_Str := Asu_Null;
  end Reset_Curr_Str;

  -- Replace all separators by spaces
  procedure Fix_Spaces (Str : in out String) is
  begin
    for I in Str'Range loop
      if Is_Separator (Str(I)) then
        Str(I) := Space;
      end if;
    end loop;
  end Fix_Spaces;

  -- Parse until Criteria is found or until a separator if Criteria = ""
  procedure Parse_Until_Str (Flow : in out Flow_Type; Criteria : in String) is
    Str : String (Criteria'Range);
    Char : Character;
    use type Asu_Us;
  begin
    if Criteria'Length > Max_Len then
      raise Constraint_Error;
    end if;
    loop
      Get (Flow, Char);
      if Criteria = "" then
        exit when Is_Separator (Char);
        Flow.Curr_Str := Flow.Curr_Str & Char;
      else
        Read (Flow, Str);
        -- Space in Str matches any separator
        Fix_Spaces (Str);
        Flow.Curr_Str := Flow.Curr_Str & Char;
        exit when Str = Criteria;
      end if;
    end loop;
  end Parse_Until_Str;

  -- Parse until one of the chars is found (any separator if space)
  procedure Parse_Until_Char (Flow : in out Flow_Type; Criteria : in String) is
    Char : Character;
    use type Asu_Us;
  begin
    if Criteria'Length = 0 then
      raise Constraint_Error;
    end if;
    This_Char:
    loop
      Get (Flow, Char);
      -- Compare to each char of the criteria
      for I in Criteria'Range loop
        if Criteria(I) = Space then
          exit This_Char when Is_Separator (Char);
        else
          exit This_Char when Char = Criteria(I);
        end if;
      end loop;
      Flow.Curr_Str := Flow.Curr_Str & Char;
    end loop This_Char;
  end Parse_Until_Char;

  procedure Parse_Until_Stop (Flow : in out Flow_Type) is
  begin
    Parse_Until_Char (Flow, "" & Stop);
  end Parse_Until_Stop;

  -- Parse until a ')' closes the already got '('
  -- Sets Curr_Str
  procedure Parse_Until_Close (Flow : in out Flow_Type) is
    Char : Character;
    Nb : Natural;
    use type Asu_Us;
  begin
    -- One '(' already got
    Nb := 1;
    loop
      Get (Flow, Char);
      -- Count opening and closing parenthesis
      if Char = '(' then
        Nb := Nb + 1;
      elsif Char = ')' then
        Nb := Nb - 1;
        exit when Nb = 0;
      end if;
      Flow.Curr_Str := Flow.Curr_Str & Char;
    end loop;
  end Parse_Until_Close;

  -- Parse until end of name, resets Curr_Str
  procedure Parse_Name (Flow : in out Flow_Type; Name : out Asu_Us) is
    Char : Character;
    use type Asu_Us;
  begin
    Name := Asu_Null;
    loop
      Get (Flow, Char);
      -- Loop while valid in name
      exit when not Is_Valid_In_Name (Char);
      Name := Name & Char;
    end loop;
    Unget (Flow);
  end Parse_Name;

  -- Try to parse a keyword, rollback if not
  procedure Try (Flow : in out Flow_Type; Str : in String; Ok : out Boolean) is
    Got_Str : String (1 .. Str'Length);
  begin
    Get (Flow, Got_Str);
    -- Space in Str matches any separator
    Fix_Spaces (Got_Str);
    -- Check if match
    if Got_Str = Str then
      -- Got it
      Ok := True;
    else
      -- Got enough chars but not those expected
      Unget (Flow, Str'Length);
      Ok := False;
    end if;
  exception
    when End_Error =>
      -- Not enough chars
      Unget (Flow, Flow.Nb_Got);
      Ok := False;
  end Try;

  -- Expand %Var; and &#xx; if in dtd
  -- or Expand &Var; if not in dtd, both recursively
  procedure Expand_Vars (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out Asu_Us;
                         In_Dtd : in Boolean) is
    Result : Asu_Us;
    -- Number of ";" to skip (because within "&var;")
    Nb2Skip : Natural;
    -- Indexes of start and stop of variable name
    Istart, Istop : Natural;
    -- Kind of starter
    type Starter_Kind is (Param_Ref, Char_Ref, None);
    Starter : Starter_Kind;
    -- Last valid index in string
    Last : Natural;
    -- Current character
    Char : Character;
    -- Entity name and value
    Name, Val : Asu_Us;
    use type Asu_Us;
    -- Entity found
    Found : Boolean;

    -- Variable resolver, when not in dtd
    function Variable_Of (Name : String) return String is
      Got : Asu_Us;
    begin
      Entity_Mng.Get (Dtd.Entity_List, Asu_Tus (Name), False, Got);
      return Asu_Ts (Got);
    exception
      when Entity_Mng.Entity_Not_Found =>
        Error (Ctx.Flow, "Unknown entity " & Name);
        -- Useless because Error already raises it
        --  but gnat complains :-(
        raise Parse_Error;
    end Variable_Of;
  begin
    if not In_Dtd then
      Text := Asu_Tus (String_Mng.Eval_Variables (
                         Str => Asu_Ts (Text),
                         Start_Delimiter => "&",
                         Stop_Delimiter  => ";",
                         Resolv => Variable_Of'Access));
      return;
    end if;
    -- Expand variables when in dtd
    -- Loop as long as an expansion occured
    Result := Text;
    Istart := 0;
    Last := Asu.Length (Result);
    loop
      -- Scan all the string
      Istart := Istart + 1;
      exit when Istart = Last;

      -- Locate start of var name '%' or "&#"
      if Asu.Element (Result, Istart) = '%' then
        Starter := Param_Ref;
      elsif Asu.Element (Result, Istart) = '&'
      and then Asu.Element (Result, Istart + 1) = '#' then
        Starter := Char_Ref;
      else
        Starter := None;
      end if;

      if Starter /= None then
        -- Locate stop of var name ';',
        -- skipping intermediate &var; sequences
        Nb2Skip := 0;
        Istop := 0;
        for I in Istart + 1 .. Last loop
          Char := Asu.Element (Result, I);
          if Char = ';' then
            if Nb2Skip = 0 then
              -- Current ';' matches '%'
              Istop := I;
              exit;
            else
              -- Current ';' matches a '&'
              Nb2Skip := Nb2Skip - 1;
            end if;
          elsif Char = '&' then
            if I /= Last and then Asu.Element (Result, I + 1) = '#' then
              -- A character reference within a %var;
              -- Restart substitution from current
              Istart := I - 1;
              exit;
            else
              -- A &var; to skip
              Nb2Skip := Nb2Skip + 1;
            end if;
          elsif Char = '%' then
            -- A %var; within a %var;
            -- Restart substitution from current
            Istart := I - 1;
            exit;
          end if;
        end loop;

        -- Check that a stop was found
        if Istop = 0 then
          Error (Ctx.Flow, "Unterminated entity reference " & Asu_Ts (Text));
        end if;
        -- Check that a stop is big enough
        if Istop = Istart + 1 then
          -- "%;"
          Error (Ctx.Flow, "Emtpy entity reference " & Asu_Ts (Text));
        end if;
        -- Got an entity name: get value if it exists
        Name := Asu_Tus (Asu.Slice (Result, Istart + 1, Istop - 1));
        Entity_Mng.Exists (Dtd.Entity_List,
                           Name, Starter = Param_Ref, Found);
        if not Found then
          if Starter = Param_Ref then
            Error (Ctx.Flow, "Unknown entity %" & Asu_Ts (Name));
          else
            Error (Ctx.Flow, "Unknown entity " & Asu_Ts (Name));
          end if;
        end if;
        Entity_Mng.Get (Dtd.Entity_List, Name, Starter = Param_Ref, Val);

        -- Substitute from start to stop
        Asu.Replace_Slice (Result, Istart, Istop, Asu_Ts (Val));
        -- Istart is now the first replaced character. OK.
        -- Update Last
        if Starter = Param_Ref then
          -- "%Name;" has been replaced by "Val"
          Last := Last - Asu.Length (Name) - 2 + Asu.Length (Val);
        else
          -- "&#Name;" has been replaced by "Val"
          Last := Last - Asu.Length (Name) - 3 + Asu.Length (Val);
        end if;
      end if;

    end loop;

    Text := Result;
  exception
    when Entity_Mng.Entity_Not_Found =>
      Error (Ctx.Flow, "Unknown entity " & Asu_Ts (Name));
      raise Parse_Error;
  end Expand_Vars;


  -- Fix text: expand variables and remove repetition of separators
  procedure Fix_Text (Ctx : in out Ctx_Type;
                      Dtd : in out Dtd_Type;
                      Text : in out Asu_Us;
                      In_Dtd : in Boolean;
                      Preserve_Spaces : in Boolean) is
    Char : Character;
    Found : Boolean;
    Name, S1, S2 : Asu_Us;
    use type Asu_Us;
  begin
    if Text = Asu_Null then
      return;
    end if;

    -- Expand entities values
    S1 := Text;
    Expand_Vars (Ctx, Dtd, S1, In_Dtd);

    -- Skip Cr
    for I in 1 .. Asu.Length (S1) loop
      Char := Asu.Element (S1, I);
      if Char /= Ada.Characters.Latin_1.Cr then
        Asu.Append (S2, Char);
      end if;
    end loop;

    if not In_Dtd and then not Preserve_Spaces then
      -- Replace "{ Lf | Tab | Space }" by a space
      S1 := S2;
      S2 := Asu_Null;
      Found := False;
      for I in 1 .. Asu.Length (S1) loop
        Char := Asu.Element (S1, I);
        if not Found then
          -- Not skipping yet, replace any separator by sapce
          if Is_Separator (Char) then
            Asu.Append (S2, Space);
            Found := True;
          else
            Asu.Append (S2, Char);
          end if;
        else
          -- Skipping: skip all separators
          if not Is_Separator (Char) then
            Asu.Append (S2, Char);
            Found := False;
          end if;
        end if;
      end loop;
      -- Remove heading space and trailing space if any
      if Asu.Element (S2, 1) = ' ' then
        Asu.Delete (S2, 1, 1);
      end if;
      if Asu.Element (S2, Asu.Length (S2)) = ' ' then
        Asu.Delete (S2, Asu.Length (S2), Asu.Length (S2));
      end if;
    end if;
    -- Done
    Text := S2;
  exception
    when String_Mng.Delimiter_Mismatch =>
      Error (Ctx.Flow, "Invalid entity reference in text " & Asu_Ts (Text));
      -- Useless because Error already raises it
      --  but gnat complains :-(
      raise Parse_Error;
  end Fix_Text;

  -- Remove sepators from text
  function Remove_Separators (Text : Asu_Us) return Asu_Us is
    Res : String (1 .. Asu.Length (Text)) := Asu_Ts (Text);
  begin
    -- Replace any separator by a space
    for I in Res'Range loop
      if Util.Is_Separator (Res(I)) then
        Res(I) := Util.Space;
      end if;
    end loop;
    -- Replace any space by nothing
    return Asu_Tus (String_Mng.Replace (Res, Space & "", ""));
  end Remove_Separators;

end Util;

