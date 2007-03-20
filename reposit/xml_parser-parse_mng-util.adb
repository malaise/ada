separate (Xml_Parser.Parse_Mng)

package body Util is
  -- Current line of input
  Current_Line : Natural := 0;
  function Get_Line_No return Natural is
  begin
    return Current_Line;
  end Get_Line_No;

  procedure Init is
  begin
    Current_Line := 1;
  end Init;
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

  -- Check that a Name is correct
  function Name_Ok (Name : Asu_Us) return Boolean is
    Char : Character;
  begin
    -- Must not be empty
    if Asu.Length (Name) = 0 then
      return False;
    elsif Asu.Length (Name) = 1 then
      -- One char name must be a letter
      return Is_Letter (Asu.Element (Name, 1));
    else
      -- First char must be letter or '_' or ':'
      Char := Asu.Element (Name, 1);
      if not Is_Letter (Char)
      and then Char /= '_'
      and then Char /= ':' then
        return False;
      end if;
      for I in 2 .. Asu.Length (Name) loop
        -- Other chars must be letter, digit, or '_' or ':' or '-'
        Char := Asu.Element (Name, 1);
        if not Is_Letter (Char)
        and then not Is_Digit (Char)
        and then Char /= '_'
        and then Char /= ':'
        and then Char /= '-' then
          return False;
        end if;
      end loop;
      return True;
    end if;
  end Name_Ok;

  ------------------
  -- Getting char --
  ------------------
  -- Circular buffer of read characters
  Max_Len : constant := 10;
  package My_Circ is new Queues.Circ (Max_Len, Character);

  -- Separator for current line of input
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  -- Error message
  Err_Msg : Asu_Us;
  procedure Error (Msg : in String) is
  begin
    Err_Msg := Asu.To_Unbounded_String (
                   "Xml_Parse error at line" & Current_Line'Img & ": " & Msg & ".");
    raise Parse_Error;
  end Error;
  function Get_Error_Message return Asu_Us is
  begin
    return Err_Msg;
  end Get_Error_Message;

  -- Get character and store in queue
  function Get return Character is
    Char : Character;
  begin
    Char := Text_Char.Get (File_Mng.File);
    My_Circ.Push (Char);
    if Char = Lf then
      Current_Line := Current_Line + 1;
    end if;
    return Char;
  exception
    when Text_Char.End_Error =>
      raise End_Error;
  end Get;

  -- To know how many where got before End_Error
  Nb_Got : Natural;
  -- Get a string
  procedure Get (Str : out String) is
  begin
    Nb_Got := 0;
    for I in Str'Range loop
      Str(I) := Get;
      Nb_Got := Nb_Got + 1;
    end loop;
  end Get;

  -- Get N characters
  function Get (N : Positive) return String is
    Str : String (1 .. N);
  begin
    Get (Str);
    return Str;
  end Get;

  -- Undo some gets (default 1)
  procedure Unget (N : Positive := 1) is
    Char : Character;
  begin
    for I in 1 .. N loop
      My_Circ.Look_Last (Char);
      My_Circ.Discard_Last;
      Text_Char.Unget (File_Mng.File, Char);
      if Char = Lf then
        Current_Line := Current_Line - 1;
      end if;
    end loop;
  end Unget;

  -- Read last char got
  function Read return Character is
    Char : Character;
  begin
    My_Circ.Look_Last (Char);
    return Char;
  end Read;

  -- Read Str'Length chars got
  procedure Read (Str : out String) is
    L : constant Integer := Str'Last;
  begin
    for I in 1 .. Str'Length loop
      -- I=1 => last pushed => Str'Last
      My_Circ.Look_Last (Str(L - I + 1), I);
    end loop;
  end Read;

  -------------
  -- Parsing --
  -------------
  -- Detect separator
  function Is_Separator (Char : Character) return Boolean is
  begin
    return Char = ' '
    or else Char = Ada.Characters.Latin_1.Lf
    or else Char = Ada.Characters.Latin_1.Cr
    or else Char = Ada.Characters.Latin_1.Ht;
  end Is_Separator;

  -- Skip separators until a significant char (not separator) is got
  procedure Skip_Separators is
    Char : Character;
  begin
    loop
      Char := Get;
      exit when not Is_Separator (Char);
    end loop;
  end Skip_Separators;

  -- Current significant string, loaded by Parse_Until_xxx
  Curr_Str : Asu_Us;
  function Get_Curr_Str return Asu_Us is
  begin
    return Curr_Str;
  end Get_Curr_Str;

  procedure Reset_Curr_Str is
  begin
    Curr_Str := Asu_Null;
  end Reset_Curr_Str;

  -- Parse until Criteria is found or until a separator if Criteria = ""
  procedure Parse_Until_Str (Criteria : in String) is
    Str : String (Criteria'Range);
    Char : Character;
    use type Asu_Us;
  begin
    if Criteria'Length > Max_Len then
      raise Constraint_Error;
    end if;
    loop
      Char := Get;
      if Criteria = "" then
        exit when Is_Separator (Char);
        Curr_Str := Curr_Str & Char;
      else
        Read (Str);
        Curr_Str := Curr_Str & Char;
        exit when Str = Criteria;
      end if;
    end loop;
  end Parse_Until_Str;

  -- Parse until one of the chars is found (any separator if space)
  procedure Parse_Until_Char (Criteria : in String) is
    Char : Character;
    use type Asu_Us;
  begin
    if Criteria'Length = 0 then
      raise Constraint_Error;
    end if;
    This_Char:
    loop
      Char := Get;
      -- Compare to each char of the criteria
      for I in Criteria'Range loop
        if Criteria(I) = ' ' then
          exit This_Char when Is_Separator (Char);
        else
          exit This_Char when Char = Criteria(I);
        end if;
      end loop;
      Curr_Str := Curr_Str & Char;
    end loop This_Char;
  end Parse_Until_Char;

  procedure Parse_Until_Stop is
  begin
    Parse_Until_Char ("" & Stop);
  end Parse_Until_Stop;

  -- Try to parse a keyword, rollback if not
  function Try (Str : String) return Boolean is
  begin
    if Get (Str'Length) = Str then
      -- Got it
      return True;
    else
      -- Got enough chars but not those expected
      Unget (Str'Length);
      return False;
    end if;
  exception
    when End_Error =>
      -- Not enough chars
      Unget (Nb_Got);
      return False;
  end Try;

  -- Fix text: expand variables and remove repetition of separators
  function Fix_Text (Text : Asu_Us) return Asu_Us is
    Index, Jndex : Natural;
    Char : Character;
    Found : Boolean;
    Name, S1, S2 : Asu_Us;
    use type Asu_Us;
  begin
    if Text = Asu_Null then
      return Text;
    end if;

    -- Expand variables
    Index := 1;
    loop
      Char := Asu.Element (Text, Index);
      if Char = '&' then
        -- Look for end of variable name
        Found := False;
        if Index /= Asu.Length (Text) then
          Jndex := Index + 1;
          loop
            if Asu.Element (Text, Jndex) = ';' then
              Found := True;
              exit;
            elsif Asu.Element (Text, Jndex) = '&'
            or else Asu.Element (Text, Jndex) = '%' then
              Found := False;
              exit;
            end if;
            exit when Jndex = Asu.Length(Text);
            Jndex := Jndex + 1;
          end loop;
        end if;
        -- Check ';' has been found and name is not empty
        if not Found then
          Error ("Unterminated entity name "
               & Asu.Slice (Text, Index, Asu.Length(Text)));
        elsif Jndex = Index + 1 then
          Error ("Empty entity name &;");
        end if;
        -- Check variable exists
        Name := Asu.To_Unbounded_String (
                Asu.Slice (Text, Index + 1, Jndex - 1));
        -- Append entity value
        begin
          Asu.Append (S1, Entity_Mng.Get (Name));
        exception
          when Entity_Mng.Entity_Not_Found =>
            Error ("Unknown entity " & Asu.To_String (Name));
        end;
        -- Jump to ';'
        Index := Jndex;
      else
        -- Not a variable, append char
        Asu.Append (S1, Char);
      end if;
      -- Next
      exit when Index = Asu.Length (Text);
      Index := Index + 1;
    end loop;

    -- Skip Cr, replace Tab by space
    --  and replace "Lf [ { sep } ]" by a space
    Found := False;
    for I in 1 .. Asu.Length (S1) loop
      Char := Asu.Element (S1, I);
      if Char /= Ada.Characters.Latin_1.Cr then
        if not Found then
          -- Not skipping
          if Char = Lf then
            Asu.Append (S2, ' ');
            Found := True;
          elsif Char = Ada.Characters.Latin_1.Ht then
            Asu.Append (S2, ' ');
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
      end if;
    end loop;
    -- Done
    return S2;
  end Fix_Text;

end Util;

