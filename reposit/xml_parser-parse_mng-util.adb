with String_Mng;
separate (Xml_Parser.Parse_Mng)
package body Util is

  -- Saved line of input (when switching to dtd file)
  Saved_Line : Natural := 0;
  -- Current line of input
  Current_Line : Natural := 0;
  function Get_Line_No return Natural is
  begin
    return Current_Line;
  end Get_Line_No;

  -- Saved file of input (when switching to dtd file)
  Saved_File : Text_Char.File_Type;
  -- Current file
  File : Text_Char.File_Type;

  -- Switch to a new file or switch back
  procedure Init (Back : in Boolean;
                  To_File : in Text_Char.File_Type) is
    New_File : Text_Char.File_Type;
  begin
    if not Back then
      -- Save current info
      Saved_Line := Current_Line;
      Saved_File := File;
      -- Switch to new
      File := To_File;
      Current_Line := 1;
    else
      -- Switch back
      Current_Line := Saved_Line;
      File := Saved_File;
      -- Reset saved info
      Saved_Line := 0;
      Saved_File := New_File;
    end if;
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
  function Name_Ok (Name : Asu_Us) return Boolean is
    Char : Character;
  begin
    -- Must not be empty
    if Asu.Length (Name) = 0 then
      return False;
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
        Char := Asu.Element (Name, I);
        if not Is_Valid_In_Name (Char) then
          return False;
        end if;
      end loop;
      return True;
    end if;
  end Name_Ok;

  -- Check that Str defines valid names seprated by Sep
  function Names_Ok (Str : Asu_Us; Seps : String) return Boolean is
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
      if not Name_Ok (Asu_Tus (S(I1 .. I2 - 1))) then
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
  package My_Circ is new Queues.Circ (Max_Len, Character);

  -- Separator for current line of input
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  -- Error message
  Err_Msg : Asu_Us;
  procedure Error (Msg : in String; Line_No : in Natural := 0) is
  begin
    if Line_No = 0 then
      Err_Msg := Asu.To_Unbounded_String (
                     "Xml_Parse error at line" & Current_Line'Img
                   & ": " & Msg & ".");
    else
      Err_Msg := Asu.To_Unbounded_String (
                     "Xml_Parse error at line" & Line_No'Img
                   & ": " & Msg & ".");
    end if;
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
    Char := Text_Char.Get (File);
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
      Text_Char.Unget (File, Char);
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
    return Char = Space
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
    Unget;
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
        -- Space in Str matches any separator
        Fix_Spaces (Str);
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
        if Criteria(I) = Space then
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

  -- Parse until end of name, resets Curr_Str
  function Parse_Name return Asu_Us is
    Res : Asu_Us;
    Char : Character;
    use type Asu_Us;
  begin
    loop
      Char := Get;
      -- Loop while valid in name
      exit when not Is_Valid_In_Name(Char);
      Res := Res & Char;
    end loop;
    Unget;
    return Res;
  end Parse_Name;

  -- Try to parse a keyword, rollback if not
  function Try (Str : String) return Boolean is
    Got_Str : String (1 .. Str'Length);
  begin
    Get (Got_Str);
    -- Space in Str matches any separator
    Fix_Spaces (Got_Str);
    -- Check if match
    if Got_Str = Str then
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
  function Fix_Text (Text : Asu_Us;
                     Preserve_Spaces : Boolean := False) return Asu_Us is
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
          Asu.Append (S1, Entity_Mng.Get (Name, False));
        exception
          when Entity_Mng.Entity_Not_Found =>
            Error ("Unknown entity " & Asu_Ts (Name));
        end;
        -- Jump to ';'
        Index := Jndex;
      elsif Char = '%' then
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
          Asu.Append (S1, Entity_Mng.Get (Name, True));
        exception
          when Entity_Mng.Entity_Not_Found =>
            Error ("Unknown entity " & Asu_Ts (Name));
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

    -- Skip Cr
    for I in 1 .. Asu.Length (S1) loop
      Char := Asu.Element (S1, I);
      if Char /= Ada.Characters.Latin_1.Cr then
        Asu.Append (S2, Char);
      end if;
    end loop;

    if not Preserve_Spaces then
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
      -- Remove heading sapce and trailing space if any
      if Asu.Element (S2, 1) = ' ' then
        Asu.Delete (S2, 1, 1);
      end if;
      if Asu.Element (S2, Asu.Length (S2)) = ' ' then
        Asu.Delete (S2, Asu.Length (S2), Asu.Length (S2));
      end if;
    end if;
    -- Done
    return S2;
  end Fix_Text;

  -- Remove sepators from text
  function Remove_Separators (Text : Asu_Us) return Asu_Us is
    Res : String (1 .. Asu.Length (Text)) := Asu_Ts (Text);
  begin
    for I in Res'Range loop
      if Util.Is_Separator (Res(I)) then
        Res(I) := Util.Space;
      end if;
    end loop;
    return Asu_Tus (String_Mng.Replace (Space & "", "", Res));
  end Remove_Separators;

end Util;

