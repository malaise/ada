with Utf_8, Utf_16;
separate (Xml_Parser.Parse_Mng)
package body Util is
  -- Autodetect ancoding family
  type Encoding_List is (Ucs4_Be, Ucs4_Le, Ucs4_Unusual,
                         Utf16_Be, Utf16_Le, Utf8,
                         Ebcdic, Other);
  procedure Guess_Encoding (Flow : in out Flow_Type) is
    Str : String (1 .. 4);
    C : array (1 .. Str'Length) of Integer;
    Encoding : Encoding_List;
    Encod : Encod_List;
  begin
    -- Read 4 bytes
    Get (Flow, Str);
    for I in Str'Range loop
      C(I) := Character'Pos (Str(I));
    end loop;
    -- If there is a Byte Order Mark then skip it
    Encoding := Other;
    if C = (0, 0, 16#FE#, 16#FF#) then
      Encoding := Ucs4_Be;
    elsif C = (16#FF#, 16#FE#, 0, 0) then
      Encoding := Ucs4_Le;
    elsif C = (0, 0, 16#FF#, 16#FE#) or else C = (16#FE#, 16#FF#, 0, 0) then
      Encoding := Ucs4_Unusual;
    elsif C(1) = 16#FE# and then C(2) = 16#FF#
    and then (C(3) /= 0 or else C(4) /= 0) then
      -- Only 2 bytes of BOM => restore 2 bytes
      Unget (Flow, 2);
      Encoding := Utf16_Be;
    elsif C(1) = 16#FF# and then C(2) = 16#FE#
    and then (C(3) /= 0 or else C(4) /= 0) then
      -- Only 2 bytes of BOM => restore 2 bytes
      Unget (Flow, 2);
      Encoding := Utf16_Le;
    elsif C (1 .. 3) = (16#EF#, 16#BB#, 16#BF#) then
      -- Only 3 bytes of BOM => restore 1 byte
      Unget (Flow, 1);
      Encoding := Utf8;
    end if;

    if Encoding /= Other then
      Trace ("Got a Byte Order Marker");
    else
      -- Else restore "<?xml" (4 first bytes of it)
      Unget (Flow, Str'Length);
      if  C = (0, 0, 0, 16#3C#) then
        Encoding := Ucs4_Be;
      elsif C = (16#3C#, 0, 0, 0) then
        Encoding := Ucs4_Le;
      elsif C = (0, 0, 16#3C#, 0) or else C = (0, 16#3C#, 0, 0) then
        Encoding := Ucs4_Unusual;
      elsif C = (0, 16#3C#, 0, 16#3F#) then
        Encoding := Utf16_Be;
      elsif C = (16#3C#, 0, 16#3F#, 0) then
        Encoding := Utf16_Le;
      elsif C = (16#3C#, 16#3F#, 16#78#, 16#6D#) then
        Encoding := Utf8;
      elsif C = (16#4C#, 16#6F#, 16#A7#, 16#94#) then
        Encoding := Ebcdic;
      else
        Encoding := Other;
      end if;
    end if;

    -- Check validity of detected encoding and save it
    if Encoding = Utf16_Be then
      Encod := Utf16_Be;
    elsif Encoding = Utf16_Le then
      Encod := Utf16_Le;
    elsif Encoding = Utf8 or else Encoding = Other then
      Encod := Utf8;
    else
      Error (Flow, "Unsupported encoding " & Encoding'Img);
    end if;

    -- Beware: Ungot characters are "raw" but unget supposes that characters
    --  are already interpreted
    -- So they must be forced to be interpreted at next read
    -- So the Nb_Bytes_xxx must be Adjusted. Reset is acceptable
    --  because this is for sure the beginning of the flow.
    -- Store for flow
    Flow.Curr_Flow.Encod := Encod;
    Flow.Curr_Flow.Nb_Bytes := 0;
  end Guess_Encoding;

  -- Current line of input
  function Get_Line_No (Flow : Flow_Type) return Natural is
  begin
    return Flow.Curr_Flow.Line;
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
           or else Char = '.'
           -- UTF-8
           or else Char > Ada.Characters.Latin_1.Del;
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
    S : constant String(1 .. Asu.Length (Str)) := Asu_Ts (Str);
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
  Cdata_Directive : constant String := Start & Directive & Cdata;

  -- Separator for current line of input
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  procedure Error (Flow : in out Flow_Type;
                   Msg : in String; Line_No : in Natural := 0) is
    Err_Msg : Asu_Us;
    Put_Line_No : Natural := 0;
    use type Asu_Us;
  begin
    if Line_No = 0 then
      Put_Line_No := Get_Line_No(Flow);
    else
      Put_Line_No := Line_No;
    end if;
    Err_Msg := Asu_Tus ("Xml_Parse error");
    if Put_Line_No /= 0 then
      Asu.Append (Err_Msg, " at line" & Put_Line_No'Img);
    end if;
    if Flow.Curr_Flow.Kind /= Xml_Flow then
      -- Dtd or external entity
      if Put_Line_No /= 0 then
        Asu.Append (Err_Msg, " of");
      else
        Asu.Append (Err_Msg, " in");
      end if;
      if Flow.Curr_Flow.Kind = Dtd_Flow then
        Asu.Append (Err_Msg, " dtd");
      else
        Asu.Append (Err_Msg, " external entity");
      end if;
      if Flow.Curr_Flow.Name /= Asu_Null then
        Asu.Append (Err_Msg, " " & Flow.Curr_Flow.Name);
      end if;
    end if;
    Asu.Append (Err_Msg, ": " & Msg & ".");
    -- The error message is attached to the exception
    -- Xml_parser will copy it in the Flow.
    Trace ("Raising Parse_Error with " & Asu_Ts (Err_Msg));
    Exception_Messenger.Raise_Exception (Parse_Error'Identity,
                                         Asu_Ts (Err_Msg));
  end Error;

  -- Report Unsupported Cdata
 procedure Cdata_Error (Flow : in out Flow_Type) is
 begin
    Error (Flow, "CDATA directive detected in an unsupported context");
 end Cdata_Error;

  -- Start recording
  procedure Start_Recording (Flow : in out Flow_Type) is
  begin
    Flow.Recorded := Asu_Null;
    Flow.Recording := True;
  end Start_Recording;

  -- Stop recoding and retrieve recorded data
  procedure Stop_Recording (Flow : in out Flow_Type; Recorded : out Asu_Us) is
  begin
    Flow.Recording := False;
    Recorded := Flow.Recorded;
    Flow.Recorded := Asu_Null;
  end Stop_Recording;

  -- Internal: Get one char on current flow - Raw
  procedure Get_One_Char (Flow : in out Flow_Type; Char : out Character) is
  begin
    if Flow.Curr_Flow.Is_File then
      Char := Flow.Curr_Flow.File.Get;
    else
      if Flow.Curr_Flow.In_Stri = Asu.Length (Flow.Curr_Flow.In_Str) then
        raise End_Error;
      end if;
      Flow.Curr_Flow.In_Stri := Flow.Curr_Flow.In_Stri + 1;
      Char := Asu.Element (Flow.Curr_Flow.In_Str, Flow.Curr_Flow.In_Stri);
    end if;
  end;

  -- Internal: Get one char on current flow - handle encoding
  Decoding_Error : exception;
  procedure Get_Char (Flow : in out Flow_Type; Char : out Character) is
    Str2 : Utf_8.Sequence(1 .. 2);
    Seq16 : Utf_16.Sequence(1 .. Utf_16.Max_Chars);
    Seq8 : Asu_Us;
    Unicode : Utf_8.Unicode_Number;
  begin
    if Flow.Curr_Flow.Encod = Utf8 then
      -- Utf8 => get char
      Get_One_Char (Flow, Char);
      return;
    elsif Flow.Curr_Flow.Nb_Bytes /= 0 then
      -- Utf16 but some chars in buffer => get char
      Get_One_Char (Flow, Char);
      Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes - 1;
      return;
    else
      -- Utf16 => read first word
      Get_One_Char (Flow, Str2(1));
      Get_One_Char (Flow, Str2(2));
    end if;

    -- Decoding of UTF-16, common to all flows, get a Unicode
    Seq16(1 .. 1) := Utf_16.Merge (Str2);
    -- Convert to UTF-16BE
    if Flow.Curr_Flow.Encod = Utf16_Le then
      Utf_16.Swap (Seq16(1));
    end if;
    if Utf_16.Nb_Chars (Seq16(1)) = 1 then
      Unicode := Utf_16.Decode (Seq16(1 .. 1));
    else
      -- Need to read second word
      begin
        Get_One_Char (Flow, Str2(1));
        Get_One_Char (Flow, Str2(2));
      exception
        when End_Error =>
          raise Decoding_Error;
      end;
      Seq16(2 .. 2) := Utf_16.Merge (Str2);
      if Flow.Curr_Flow.Encod = Utf16_Le then
        Utf_16.Swap (Seq16(1));
      end if;
      Unicode := Utf_16.Decode (Seq16);
    end if;

    -- Get a Utf8 sequence
    Seq8 := Asu_Tus (Utf_8.Encode (Unicode));

    -- Re-insert in flow all but first character
    if Flow.Curr_Flow.Is_File then
      for I in reverse 2 .. Asu.Length (Seq8) loop
        Flow.Curr_Flow.File.Unget (Asu.Element (Seq8, I));
      end loop;
      Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes
                               + Asu.Length (Seq8) - 1;
    else
      -- Insert Seq8 (2 .. Last) at current index of In_String
      Asu.Insert (Flow.Curr_Flow.In_Str, Flow.Curr_Flow.In_Stri,
                  Asu.Slice (Seq8, 2, Asu.Length (Seq8)) );
      Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes
                               + Asu.Length (Seq8) - 1;
    end if;

    -- Return First Char
    Char := Asu.Element (Seq8, 1);
  exception
    when End_Error =>
      raise;
    when Text_Char.End_Error =>
      raise End_Error;
    when Error:others =>
      Trace ("Decoding error " & Ada.Exceptions.Exception_Name (Error));
      raise Decoding_Error;
  end Get_Char;

  -- Get character and store in queue
  procedure Get (Flow : in out Flow_Type; Char : out Character) is
  begin
    Get_Char (Flow, Char);
    My_Circ.Push (Flow.Circ, Char);
    if Flow.Recording then
      if Flow.Skip_Recording <= 0 then
        -- 0: all Get to skip have been skipped (but an unget
        --  still shall be skipped). -1: No more skip
        -- Record this get
        Asu.Append (Flow.Recorded, Char);
      end if;
      if Flow.Skip_Recording /= No_Skip_Rec then
        -- Skip recording this get
        Flow.Skip_Recording := Flow.Skip_Recording - 1;
      end if;
    end if;
    if Char = Lf then
      Flow.Curr_Flow.Line := Flow.Curr_Flow.Line + 1;
    end if;
  exception
    when Text_Char.End_Error =>
      raise End_Error;
    when Text_Char.Io_Error =>
      raise File_Error;
    when Decoding_Error =>
      Error (Flow, "Error while decoding character");
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

  -- Undo some gets (default 1)
  procedure Unget (Flow : in out Flow_Type; N : Natural := 1) is
    Char : Character;
    Len : Natural;
  begin
    for I in 1 .. N loop
      My_Circ.Look_Last (Flow.Circ, Char);
      My_Circ.Discard_Last (Flow.Circ);
      if Flow.Curr_Flow.Is_File then
        Flow.Curr_Flow.File.Unget (Char);
        Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes + 1;
      else
        Flow.Curr_Flow.In_Stri := Flow.Curr_Flow.In_Stri - 1;
        Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes + 1;
      end if;
      if Flow.Recording then
        if Flow.Skip_Recording = No_Skip_Rec then
          -- Record this unget (remove the recorded get)
          Len := Asu.Length (Flow.Recorded);
          if Len /= 0 then
            Asu.Delete (Flow.Recorded, Len, Len);
          end if;
        else
          -- Skip next Get of this char
          Flow.Skip_Recording := Flow.Skip_Recording + 1;
        end if;
      end if;
      if Char = Lf then
        Flow.Curr_Flow.Line := Flow.Curr_Flow.Line - 1;
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

  -- Injects Str in flow so that it will be got
  procedure Insert (Flow : in out Flow_Type;  Str : in String) is
  begin
    if Flow.Curr_Flow.Is_File then
      for I in reverse Str'Range loop
        Flow.Curr_Flow.File.Unget (Str(I));
      end loop;
    else
      -- Insert Str after current pos (last read)
      Asu.Insert (Flow.Curr_Flow.In_Str, Flow.Curr_Flow.In_Stri + 1, Str);
    end if;
    -- The inserted characters shall not be recorded (when got/ungot)
    if Flow.Recording then
      if Flow.Skip_Recording = No_Skip_Rec then
        Flow.Skip_Recording := Str'Length;
      else
        Flow.Skip_Recording := Flow.Skip_Recording + Str'Length;
      end if;
    end if;
  end Insert;

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
  exception
    when End_Error => null;
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
  exception
    when End_Error => null;
  end Get_Separators;

  function Get_Curr_Str (Flow : Flow_Type;
                         Check_Cdata : Boolean := True) return Asu_Us is
  begin
    if Check_Cdata then
      Util.Check_Cdata (Flow.Curr_Str);
    end if;
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
  -- Sets Curr_Str, consumes all separators if this is the criteria
  procedure Parse_Until_Str (Flow : in out Flow_Type; Criteria : in String) is
    Str : String (Criteria'Range);
    Char : Character;
    use type Asu_Us;
  begin
    if Criteria'Length > Max_Buf_Len then
      Trace ("Parsing until Str with a too long criteria");
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
    if Is_Separator (Char) then
      Skip_Separators (Flow);
    end if;
  end Parse_Until_Str;

  -- Parse until one of the chars is found (any separator if space)
  -- Sets Curr_Str, consumes all separators if this is the criteria
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
    if Is_Separator (Char) then
      Skip_Separators (Flow);
    end if;
  end Parse_Until_Char;

  procedure Parse_Until_Stop (Flow : in out Flow_Type) is
  begin
    Parse_Until_Char (Flow, "" & Stop);
  end Parse_Until_Stop;

  -- Parse until end of flow
  -- Sets Curr_Str
  procedure Parse_Until_End (Flow : in out Flow_Type) is
    Char : Character;
    use type Asu_Us;
  begin
    loop
      Get (Flow, Char);
      Flow.Curr_Str := Flow.Curr_Str & Char;
    end loop;
  exception
    when End_Error => null;
  end Parse_Until_End;

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

  -- Parse until end of name
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
    Check_Cdata (Name);
  end Parse_Name;

  -- Try to parse a keyword, rollback if not
  procedure Try (Flow : in out Flow_Type; Str : in String; Ok : out Boolean;
                 Consume : in Boolean := True) is
    Got_Str : String (1 .. Str'Length);
  begin
    -- Get same amount of chars as Str
    Get (Flow, Got_Str);
    -- Space in Str matches any separator
    Fix_Spaces (Got_Str);
    -- Check if match
    if Got_Str = Str then
      -- Got it
      Ok := True;
    else
      -- Got enough chars but not those expected
      Ok := False;
    end if;
    if not Ok or else not Consume then
      -- No match or explicit arg to not consume
      Unget (Flow, Str'Length);
    elsif Is_Separator (Str(Str'Last)) then
      -- Consume any separator following Str last separator
      Skip_Separators (Flow);
    end if;
    Check_Cdata (Asu_Tus(Got_Str));
  exception
    when End_Error =>
      -- Not enough chars
      Ok := False;
      Unget (Flow, Flow.Nb_Got);
      Check_Cdata (Asu_Tus(Got_Str(1 ..  Flow.Nb_Got)));
  end Try;

  -- Expand %Var; and &#xx; and &Var; if in dtd
  -- or Expand &Var; and &#xx; if not in dtd, both recursively
  procedure Expand_Vars (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out Asu_Us;
                         Context : in Context_List) is
    Result : Asu_Us;
    -- Indexes of start of search
    Sstart : Positive;
    -- Indexes of start and stop of variable name
    Istart, Istop : Natural;
    Starter : Character;
    -- Last valid index in string
    Last : Natural;
    -- Current character
    Char : Character;
    -- Entity name and value
    Name, Val : Asu_Us;
    use type Asu_Us;
    -- Entity found
    Found : Boolean;
    -- Do we need to restart
    Restart : Boolean;

    -- Variable resolver, when not in dtd
    function Variable_Of (Name : String) return String is
      Got : Asu_Us;
    begin
      Entity_Mng.Get (Ctx, Dtd, Ctx.Flow.Curr_Flow.Encod,
                      Context, Asu_Tus (Name), False, Got);
      return Asu_Ts (Got);
    exception
      when Entity_Mng.Entity_Not_Found =>
        Error (Ctx.Flow, "Unknown entity " & Name);
        -- Useless because Error already raises it
        --  but gnat complains :-(
        raise Parse_Error;
      when Entity_Mng.Invalid_Char_Code =>
        Error (Ctx.Flow, "Invalid Char code " & Name);
        raise Parse_Error;
    end Variable_Of;

  begin
    Check_Cdata (Text);
    if not In_Dtd (Context) then
      -- In xml, only expand general entities
      if Ctx.Expand then
        -- If Parse was called with Expand_Entities = True
        --  but allow ';' within text
        Text := Asu_Tus (String_Mng.Eval_Variables (
                           Str => Asu_Ts (Text),
                           Start_Delimiter => "&",
                           Stop_Delimiter  => ";",
                           Resolv => Variable_Of'Access,
                           Muliple_Passes => False,
                           No_Check_Stop => True));
      end if;
      return;
    end if;

    -- Expand when in dtd: parameter entities and character references
    -- Restart at beginning as long as an expansion occured
    Result := Text;
    Restart := False;
    Istart := 1;
    loop
      -- Start searching new reference from Istart to Last
      Sstart := Istart;
      Last := Asu.Length (Result);

      -- Default: not found
      Istart := 0;
      Istop := 0;

      -- Locate first deepest starter and corresponding stop
      -- Will need to restart if more that one starter
      for I in Sstart .. Last loop
        -- Locate start of var name '%' or "&#"
        Char := Asu.Element (Result, I);
        if Char = '%' then
          Restart := Istart /= 0;
          Istart := I;
          Starter := Char;
        elsif Char = '&'
        and then I /= Last
        and then Asu.Element (Result, I+1) = '#' then
          Restart := Istart /= 0;
          Istart := I;
          Starter := Char;
        elsif Char = ';' then
          if Istart /= 0 then
            -- The end of a reference, otherwise ignore
            Istop := I;
            exit;
          end if;
        end if;
      end loop;

      -- End of this scanning
      if Istart = 0 then
        -- Done when no reference
        exit when not Restart;
        -- Restart scanning from 1 to Last
        Restart := False;
        Istart := 1;
      end if;

      if Istop = 0 then
        -- A start with no stop => Error
        Error (Ctx.Flow, "Unterminated entity reference " & Asu_Ts (Text));
      end if;

      -- Check that a stop is big enough
      if Istop = Istart + 1 then
        -- "%;"
        Error (Ctx.Flow, "Emtpy entity reference " & Asu_Ts (Text));
      end if;

      -- Got an entity name: get value if it exists (skip % & ;)
      Name := Asu_Tus (Asu.Slice (Result, Istart + 1, Istop - 1));
      Entity_Mng.Exists (Dtd.Entity_List,
                           Name, Starter = '%', Found);
      if not Found then
        if Starter = '%' then
          Error (Ctx.Flow, "Unknown entity %" & Asu_Ts (Name));
        else
          Error (Ctx.Flow, "Unknown entity " & Asu_Ts (Name));
        end if;
      end if;
      Entity_Mng.Get (Ctx, Dtd, Ctx.Flow.Curr_Flow.Encod,
                      Context, Name, Starter = '%', Val);

      -- Substitute from start to stop
      Asu.Replace_Slice (Result, Istart, Istop, Asu_Ts (Val));

      -- Non parameter entity bypassed => &name; -> &name;
      --  restart from after length of Val
      if Starter = '&' then
        Istart := Istart + Asu.Length (Val);
      end if;

    end loop;

    Text := Result;
  exception
    when Entity_Mng.Entity_Not_Found =>
      Error (Ctx.Flow, "Unknown entity " & Asu_Ts (Name));
    when Entity_Mng.Invalid_Char_Code =>
      Error (Ctx.Flow, "Invalid Char code " & Asu_Ts (Name));
  end Expand_Vars;


  -- Expand a name if it is a (parameter) entity reference
  -- Error if Text contains % or & but not at beginning
  -- Error if Text contains ; but not at end
  -- Does nothing if not an entity reference
  procedure Expand_Name (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out Asu_Us;
                         Context : in Context_List) is
    Str : constant String := Asu_Ts (Text);
    Len : constant Natural := Str'Length;
    Ne, Np, Ns: Natural;

    procedure Entity_Error is
    begin
      Error (Ctx.Flow, "Invalid (parameter) entity reference " & Str);
    end Entity_Error;

  begin
    -- Check presence of "&" and "%" and ";"
    Ne := String_Mng.Locate (Str, "&");
    Np := String_Mng.Locate (Str, "%");
    if Ne + Np = 0 then
      return;
    end if;
    Ns := String_Mng.Locate (Str, ";");
    -- Must have one start at beginning and one stop at end
    if Ne + Np /= 1        -- both or not at beginning
    or else Ns /= Len      -- not at end
    or else String_Mng.Locate (Str, "&", Occurence => 2) /= 0
    or else String_Mng.Locate (Str, "%", Occurence => 2) /= 0
    or else String_Mng.Locate (Str, ";", Occurence => 2) /= 0 then
      Entity_Error;
    end if;
    Expand_Vars (Ctx, Dtd, Text, Context);
  end Expand_Name;

  -- Fix text: expand variables and remove repetition of separators
  procedure Fix_Text (Ctx : in out Ctx_Type;
                      Dtd : in out Dtd_Type;
                      Text : in out Asu_Us;
                      Context : in Context_List;
                      Preserve_Spaces : in Boolean) is
    Char : Character;
    Found : Boolean;
    S1, S2 : Asu_Us;
    use type Asu_Us;
  begin
    if Text = Asu_Null then
      return;
    end if;

    -- Expand entities values
    S1 := Text;
    Expand_Vars (Ctx, Dtd, S1, Context);

    -- Skip Cr
    for I in 1 .. Asu.Length (S1) loop
      Char := Asu.Element (S1, I);
      if Char /= Ada.Characters.Latin_1.Cr then
        Asu.Append (S2, Char);
      end if;
    end loop;

    if not In_Dtd(Context) and then not Preserve_Spaces then
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
      if Asu.Length (S2) /= 0
      and then Asu.Element (S2, Asu.Length (S2)) = ' ' then
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
      if Is_Separator (Res(I)) then
        Res(I) := Util.Space;
      end if;
    end loop;
    -- Replace any space by nothing
    return Asu_Tus (String_Mng.Replace (Res, Space & "", ""));
  end Remove_Separators;

  -- Replace any sequence of separators by a space
  function Normalize_Separators (Text : Asu_Us) return Asu_Us is
    Res : Asu_Us;
    Char : Character;
    -- Will skip leading separators
    Prev_Is_Sep : Boolean := True;
  begin
    for I in 1 .. Asu.Length (Text) loop
      Char := Asu.Element (Text, I);
      if Util.Is_Separator (Char) then
        if not Prev_Is_Sep then
          Asu.Append (Res, Util.Space);
          Prev_Is_Sep := True;
        end if;
      else
        Asu.Append (Res, Char);
        Prev_Is_Sep := False;
      end if;
    end loop;
    -- Remove trailing space
    if Asu.Length (Res) > 1
    and then Asu.Element (Res, Asu.Length (Res)) = Util.Space then
      Asu.Delete (Res, Asu.Length (Res), Asu.Length (Res));
    end if;
    return Res;
  end Normalize_Separators;

  -- Skip any Cdata section (<![CDATA[xxx]]>)
  -- If Full_Markup then check for <![CDATA[, else check for [CDATA[
  -- Ok set to True if a CDATA was found
  -- Resets Curr_Str!!!
  procedure Skip_Cdata (Flow : in out Flow_Type;
                        Full_Markup : in Boolean;
                        Ok : out Boolean) is
  begin
    begin
      if Full_Markup then
        Try (Flow, Cdata_Directive, Ok);
      else
        Try (Flow, Cdata, Ok);
      end if;
    exception
      when Cdata_Detected =>
        Ok := True;
      when End_Error =>
        Ok := False;
    end;
    if Ok then
      -- "<![CDATA[", a CDATA block, skip until "]]>"
      Parse_Until_Str (Flow, "]]" & Stop);
      Trace ("Skipped " & Cdata_Directive & Asu_Ts (Get_Curr_Str (Flow)));
      Reset_Curr_Str (Flow);
      return;
    end if;
  end Skip_Cdata;

  -- Check for <![CDATA[ in text, raises Cdata_Detected
  procedure Check_Cdata (Str : in Asu_Us) is
  begin
    if String_Mng.Locate (Asu_Ts(Str), Cdata_Directive) /= 0 then
      Trace ("Detected Cdata in " & Asu_Ts(Str));
      raise Cdata_Detected;
    end if;
  end Check_Cdata;

  -- Check for <![CDATA[ in the next N characters if Flow, raises Cdata_Detected
  procedure Check_Cdata (Flow : in out Flow_Type; N : Natural := 0) is
    Got_Str : String (1 .. N + Cdata_Directive'Length);
  begin
    -- Ensure no buffer overflow
    if Got_Str'Length > Max_Buf_Len then
      Trace ("Checking Cdata within a too long string");
      raise Constraint_Error;
    end if;
    -- Check Cdata in next chars of flow
    Get (Flow, Got_Str);
    Check_Cdata (Asu_Tus(Got_Str));
    Unget (Flow, Got_Str'Length);
  exception
    when End_Error =>
      -- Not enough chars
      Check_Cdata (Asu_Tus(Got_Str(1 ..  Flow.Nb_Got)));
      Unget (Flow, Flow.Nb_Got);
  end Check_Cdata;


end Util;

