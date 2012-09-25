with Utf_8, Utf_16, Sys_Calls, Str_Util.Regex;
separate (Xml_Parser.Parse_Mng)
package body Util is
  -- Autodetect encoding family
  type Encoding_List is (Ucs4_Be, Ucs4_Le, Ucs4_Unusual,
                         Utf16_Be, Utf16_Le, Utf8,
                         Ebcdic, Other);
  procedure Guess_Encoding (Flow : in out Flow_Type) is
    Str : String (1 .. 4);
    C : array (1 .. Str'Length) of Integer;
    Encoding : Encoding_List;
    Encod : Detected_Encod_List;
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
    Trace ("Guessed encoding " & Encod'Img);
    Flow.Curr_Flow.Encod := Encod;
    Flow.Curr_Flow.Nb_Bytes := 0;
  end Guess_Encoding;

  -- Load a character encoding map
  procedure Load_Map (Flow : in out Flow_Type;
                      Name : in String) is
    Var_Name : constant String := "XML_PARSER_MAP_DIR";

  begin
    declare
      File_Name : constant String
                := Directory.Build_File_Name (Environ.Getenv (Var_Name),
                                              Name, "xml");
    begin
      if not Environ.Is_Set (Var_Name)
      or else not Sys_Calls.File_Found (File_Name) then
        Error (Flow, "Unsupported encoding (only UTF-8, UTF-16 "
                          & "and ISO-8859-1 are natively supported)");
      end if;
      Trace ("Loading map " & Name & " from file " & File_Name);
      Flow.Curr_Flow.Map.Load (File_Name);
      Flow.Curr_Flow.Encod := Other;
    exception
      when Byte_To_Unicode.File_Error =>
        Error (Flow, "Error accessing encoding file " & File_Name);
      when Except:Byte_To_Unicode.Parse_Error =>
        declare
          Except_Access : Ada.Exceptions.Exception_Occurrence_Access
                        := Ada.Exceptions.Save_Occurrence (Except);
          Msg : constant String
              := Exception_Messenger.Exception_Message (Except_Access);
        begin
          Exception_Messenger.Deallocate (Except_Access);
          if Msg = "" then
            Error (Flow, "Error while parsing encoding file " & File_Name);
          else
            -- Append message raised by the map parsing (without last '.')
            Error (Flow, "Error while parsing encoding file " & File_Name
             & ": " & Str_Util.Cut (Msg, 1, Head => False));
          end if;
        end;
    end;
  end Load_Map;

  -- Current line of input
  function Get_Line_No (Flow : Flow_Type) return Natural is
  begin
    return Flow.Curr_Flow.Line;
  end Get_Line_No;

  ------------------
  -- Syntax check --
  ------------------
 -- Encoding name check
  function Is_Letter (Char : Character) return Boolean is
  begin
    return (Char >= 'a' and then Char <= 'z')
    or else (Char >= 'A' and then Char <= 'Z');
  end Is_Letter;

  function Is_Valid_Encoding (Name : As.U.Asu_Us) return Boolean is
    Char : Character;
  begin
    if Name.Length = 0 then
      return False;
    end if;
    Char := Name.Element (1);
    if not Is_Letter (Char) then
      return False;
    end if;
    for I in 2 .. Name.Length loop
      Char := Name.Element (I);
      if not Is_Letter (Char)
      and then (Char < '0' or else '9' < Char)
      and then Char /= '.'
      and then Char /= '_'
      and then Char /= '-' then
        return False;
      end if;
    end loop;
    return True;
  end Is_Valid_Encoding;

  function Is_Valid_Start (U : Utf_8.Unicode_Number) return Boolean is
  begin
    return  (Character'Pos('a') <= U and then U <= Character'Pos('z'))
    or else (Character'Pos('A') <= U and then U <= Character'Pos('Z'))
    or else U = Character'Pos (':')
    or else U = Character'Pos ('_')
    or else (16#C0# <= U and then U <= 16#2FF#
             and then U /= 16#D7#  and then U /= 16#F7#)
    or else (16#370# <= U and then U <= 16#1FFF# and then U /= 16#37E#)
    or else U = 16#200C#
    or else U = 16#200D#
    or else (16#2070# <= U and then U <= 16#218F#)
    or else (16#2C00# <= U and then U <= 16#2FEF#)
    or else (16#3001# <= U and then U <= 16#D7FF#)
    or else (16#F900# <= U and then U <= 16#FDCF#);
  end Is_Valid_Start;
  function Is_Valid_In_Name (U : Utf_8.Unicode_Number) return Boolean is
  begin
    return  U = Character'Pos ('-')
    or else U = Character'Pos ('.')
    or else (Character'Pos('0') <= U and then U <= Character'Pos('9'))
    or else Is_Valid_Start (U)
    or else U= 16#B7#
    or else (16#300# <= U and then U <= 16#36F#)
    or else U= 16#203F#
    or else U= 16#2040#;
  end Is_Valid_In_Name;

  -- Check that a Name is correct
  function Name_Ok (Name : As.U.Asu_Us;
                    Allow_Token : Boolean := False) return Boolean is
  begin
    -- Must not be empty
    if Name.Length = 0 then
      return False;
    end if;
    declare
      Unicodes : constant Utf_8.Unicode_Sequence
               := Utf_8.Decode (Name.Image);
    begin
      -- For true name (not token) first char must be Valid_Start
      if not Allow_Token
      and then not Is_Valid_Start (Unicodes(1)) then
        return False;
      end if;
      -- Other chars must be Is_Valid_In_Name
      for I in Unicodes'Range loop
        if not Is_Valid_In_Name (Unicodes(I)) then
          return False;
        end if;
      end loop;
      return True;
    end;
  exception
    when Utf_8.Invalid_Sequence =>
      return False;
  end Name_Ok;

  -- Check that Str defines valid names seprated by Sep
  function Names_Ok (Str : As.U.Asu_Us;
                     Seps : String;
                     Allow_Token : Boolean := False) return Boolean is
    S : constant String(1 .. Str.Length) := Str.Image;
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
      if not Name_Ok (As.U.Tus (S(I1 .. I2 - 1)), Allow_Token) then
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

  -- Separator for current line of input
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  function Build_Error (Flow : in Flow_Type;
                        Is_Error : in Boolean;
                        Msg : in String;
                        Line_No : in Natural) return String is
    Err_Msg : As.U.Asu_Us;
    Put_Line_No : Natural := 0;
    use type As.U.Asu_Us;
  begin
    if Line_No = 0 then
      Put_Line_No := Get_Line_No(Flow);
    else
      Put_Line_No := Line_No;
    end if;
    Err_Msg := As.U.Tus ("Xml_Parser");
    if Is_Error then
      Err_Msg.Append (" error");
    else
      Err_Msg.Append (" warning");
    end if;
    if Put_Line_No /= 0 then
      Err_Msg.Append (" at line" & Put_Line_No'Img);
    end if;
    -- Xml, Dtd or external entity
    if Put_Line_No /= 0 then
      Err_Msg.Append (" of");
    else
      Err_Msg.Append (" in");
    end if;
    case Flow.Curr_Flow.Kind is
      when Xml_Flow | Int_Dtd_Flow =>
        Err_Msg.Append (" xml");
      when Dtd_Flow =>
        Err_Msg.Append (" dtd");
      when Ext_Flow =>
        Err_Msg.Append (" external entity");
    end case;
    if Flow.Curr_Flow.Kind /= Xml_Flow
    and then not Flow.Curr_Flow.Name.Is_Null then
      Err_Msg.Append (" " & Flow.Curr_Flow.Name);
    end if;
    Err_Msg.Append (": " & Msg & ".");
    return Err_Msg.Image;
  end Build_Error;

  procedure Error (Flow : in out Flow_Type;
                   Msg : in String;
                   Line_No : in Natural := 0) is
    Err_Msg : constant String := Build_Error (Flow, True, Msg, Line_No);
  begin
    -- The error message is attached to the exception
    -- Xml_parser will copy it in the Flow.
    Trace ("Raising Parse_Error with " & Err_Msg);
    Exception_Messenger.Raise_Exception (Parse_Error'Identity, Err_Msg);
  end Error;
  procedure Warning (Ctx     : in out Ctx_Type;
                     Msg     : in String;
                     Line_No : in Natural := 0) is
    Err_Msg : constant String := Build_Error (Ctx.Flow, False, Msg, Line_No);
  begin
    if Ctx.Warnings = null then
      return;
    end if;
    Trace ("Calling Warning with " & Err_Msg);
    Ctx.Warnings (Ctx, Err_Msg);
  end Warning;

  -- Start recording
  procedure Start_Recording (Flow : in out Flow_Type) is
  begin
    Flow.Recorded.Set_Null;
    Flow.Recording := True;
  end Start_Recording;

  -- Stop recoding and retrieve recorded data
  procedure Stop_Recording (Flow : in out Flow_Type; Recorded : out As.U.Asu_Us) is
  begin
    Flow.Recording := False;
    Recorded := Flow.Recorded;
    Flow.Recorded.Set_Null;
  end Stop_Recording;

  -- Internal: Get one char on current flow - Raw
  procedure Get_One_Char (Flow : in out Flow_Type; Char : out Character) is
  begin
    if Flow.Curr_Flow.Is_File then
      Char := Flow.Curr_Flow.File.Get;
    else
      if Flow.Curr_Flow.In_Stri = Flow.Curr_Flow.In_Str.Length then
        raise End_Error;
      end if;
      Flow.Curr_Flow.In_Stri := Flow.Curr_Flow.In_Stri + 1;
      Char := Flow.Curr_Flow.In_Str.Element (Flow.Curr_Flow.In_Stri);
    end if;
  end;

  -- Internal: Get one char on current flow - handle encoding
  Decoding_Error : exception;
  procedure Get_Char (Flow : in out Flow_Type; Char : out Character) is
    Str2 : Utf_8.Sequence(1 .. 2);
    Seq16 : Utf_16.Sequence(1 .. Utf_16.Max_Chars);
    Seq8 : As.U.Asu_Us;
    Unicode : Utf_8.Unicode_Number;
  begin
    if Flow.Curr_Flow.Encod = Utf8 then
      -- Utf8 => get char
      Get_One_Char (Flow, Char);
      return;
    elsif Flow.Curr_Flow.Nb_Bytes /= 0 then
      -- Utf16 or latin1 but some chars in buffer => get char
      Get_One_Char (Flow, Char);
      Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes - 1;
      return;
    elsif Flow.Curr_Flow.Encod = Latin1 then
      -- Latin 1 <=> Unicode.
      Get_One_Char (Flow, Char);
      Unicode := Character'Pos(Char);
    elsif Flow.Curr_Flow.Encod = Other then
      -- Other 1 -> Unicode: Use map
      Get_One_Char (Flow, Char);
      Unicode := Flow.Curr_Flow.Map.Convert(Character'Pos(Char));
    else
      -- Utf16 => read first word
      Get_One_Char (Flow, Str2(1));
      Get_One_Char (Flow, Str2(2));

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
    end if;

    -- Get a Utf8 sequence
    Seq8 := As.U.Tus (Utf_8.Encode (Unicode));

    if Seq8.Length /= 1 then
      -- Re-insert in flow all but first character
      if Flow.Curr_Flow.Is_File then
        for I in reverse 2 .. Seq8.Length loop
          Flow.Curr_Flow.File.Unget (Seq8.Element (I));
        end loop;
        Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes
                                 + Seq8.Length - 1;
      else
        -- Insert Seq8 (2 .. Last) at current index of In_String
        Flow.Curr_Flow.In_Str.Insert (Flow.Curr_Flow.In_Stri,
                    Seq8.Slice (2, Seq8.Length) );
        Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes
                                 + Seq8.Length - 1;
      end if;
    end if;

    -- Return First Char
    Char := Seq8.Element (1);
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
    -- Skip CRs: Replace CrLf by Lf, or else Cr by Lf
    if Char = Ada.Characters.Latin_1.Lf
    and then Flow.Curr_Flow.Prev_Char_Was_Cr then
      -- Prev Cr already gave a Lf, skip this one
      Get_Char (Flow, Char);
    end if;
    if Char = Ada.Characters.Latin_1.Cr then
      Char := Ada.Characters.Latin_1.Lf;
      Flow.Curr_Flow.Prev_Char_Was_Cr := True;
    else
      Flow.Curr_Flow.Prev_Char_Was_Cr := False;
    end if;

    My_Circ.Push (Flow.Circ, Char);
    if Flow.Recording then
      if Flow.Skip_Recording <= 0 then
        -- 0: all Get to skip have been skipped (but an unget
        --  still shall be skipped). -1: No more skip
        -- Record this get
        Flow.Recorded.Append (Char);
      end if;
      if Flow.Skip_Recording /= No_Skip_Rec then
        -- Skip recording this get
        Flow.Skip_Recording := Flow.Skip_Recording - 1;
      end if;
    end if;
    if Char = Lf and then not Flow.Curr_Flow.Same_Line then
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
      else
        Flow.Curr_Flow.In_Stri := Flow.Curr_Flow.In_Stri - 1;
      end if;
      Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes + 1;
      if Flow.Recording then
        if Flow.Skip_Recording = No_Skip_Rec then
          -- Record this unget (remove the recorded get)
          Len := Flow.Recorded.Length;
          if Len /= 0 then
            Flow.Recorded.Delete (Len, Len);
          end if;
        else
          -- Skip next Get of this char
          Flow.Skip_Recording := Flow.Skip_Recording + 1;
        end if;
      end if;
      if Char = Lf and then not Flow.Curr_Flow.Same_Line then
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
      Flow.Curr_Flow.In_Str.Insert (Flow.Curr_Flow.In_Stri + 1, Str);
    end if;
    Flow.Curr_Flow.Nb_Bytes := Flow.Curr_Flow.Nb_Bytes + Str'Length;
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

  function Is_Separators (Str : As.U.Asu_Us) return Boolean is
  begin
    for I in 1 .. Str.Length loop
      if not Is_Separator (Str.Element (I) ) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Separators;

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

  procedure Get_Curr_Str (Flow : in out Flow_Type;
                          Str : out As.U.Asu_Us;
                          Reset : in Boolean := True) is

  begin
    Str := Flow.Curr_Str;
    if Reset then
      Flow.Curr_Str.Set_Null;
    end if;
  end Get_Curr_Str;

  procedure Reset_Curr_Str (Flow : in out Flow_Type) is
  begin
    Flow.Curr_Str.Set_Null;
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
  begin
    if Criteria'Length > Max_Buf_Len then
      Trace ("Parsing until Str with a too long criteria");
      raise Constraint_Error;
    end if;
    loop
      Get (Flow, Char);
      if Criteria = "" then
        exit when Is_Separator (Char);
        Flow.Curr_Str.Append (Char);
      else
        Read (Flow, Str);
        -- Space in Str matches any separator
        Fix_Spaces (Str);
        Flow.Curr_Str.Append (Char);
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
      Flow.Curr_Str.Append (Char);
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
  begin
    loop
      Get (Flow, Char);
      Flow.Curr_Str.Append (Char);
    end loop;
  exception
    when End_Error => null;
  end Parse_Until_End;

  -- Parse until a ')' closes the already got '('
  -- Sets Curr_Str
  procedure Parse_Until_Close (Flow : in out Flow_Type) is
    Char : Character;
    Nb : Natural;
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
      Flow.Curr_Str.Append (Char);
    end loop;
  end Parse_Until_Close;

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
  exception
    when End_Error =>
      -- Not enough chars
      Ok := False;
      Unget (Flow, Flow.Nb_Got);
  end Try;

  -- List of names of entities expanding to each other, to detect recursion
  package Name_Dyn_List_Mng is new Dynamic_List (As.U.Asu_Us);
  package Name_List_Mng renames Name_Dyn_List_Mng.Dyn_List;
  procedure Search_Name is new Name_List_Mng.Search (As.U."=");

  -- INTERNAL: Verify propoer nesting of parenths
  function Check_Nesting (Str : String) return Boolean is
    Level : Natural := 0;
    Char : Character;
  begin
    for I in Str'Range loop
      Char := Str(I);
      if Char = '(' then
        Level := Level + 1;
      elsif Char = ')' then
        if Level = 0 then
          -- More closing than opening
          return False;
        end if;
        Level := Level - 1;
      end if;
    end loop;
    -- As many closing as opening?
    return Level = 0;
  end Check_Nesting;

  -- INTERNAL: Expand text (expand vars) returns the index of localized '<'
  --  if any
  procedure Expand_Internal (Ctx : in out Ctx_Type;
                             Dtd : in out Dtd_Type;
                             Text : in out As.U.Asu_Us;
                             Context : in Context_List;
                             Start_Index : out Natural;
                             Name_Stack : in out Name_List_Mng.List_Type) is
    Result : As.U.Asu_Us;
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
    Name, Val : As.U.Asu_Us;
    -- Entity found
    Found : Boolean;
    -- Entity list is empty
    Stack_Empty : Boolean;

    use type As.U.Asu_Us;
  begin
    Start_Index := 0;
    if not In_Dtd (Context) and then not Ctx.Expand then
      -- Do not expand in Xml
      return;
    end if;

    -- Expand when in dtd: parameter entities
    -- Expand when in Xml: general entities
    -- Expand recursively but, in Xml, stop when generating a '<'

    -- Start searching new reference from Istart to Last
    Sstart := 1;
    Result := Text;
    -- Normalize separators if attribute
    if Context = Ref_Attribute then
      declare
        Str : String := Result.Image;
      begin
        Fix_Spaces (Str);
        Result := As.U.Tus (Str);
      end;
    end if;

    loop
      Last := Result.Length;

      -- Default: not found
      Istart := 0;
      Istop := 0;

      -- Locate first deepest starter and corresponding stop
      -- Will need to restart if more that one starter
      for I in Sstart .. Last loop
        -- Locate start of var name '%' or "&#"
        Char := Result.Element (I);
        if Char = Ent_Param and then In_Dtd (Context) then
          -- Parameter entity
          Istart := I;
          Starter := Ent_Param;
        elsif Char = Ent_Other then
          if not In_Dtd (Context)
             and then (I = Last
               or else Result.Element (I + 1) /= Ent_Char) then
            -- General entity in Xml
            Istart := I;
            Starter := Ent_Other;
          elsif I /= Last
          and then Result.Element (I + 1) = Ent_Char then
            -- Character entity everywhere
            Istart := I;
            Starter := Ent_Char;
          end if;
        elsif Char = Start then
          if Context = Ref_Xml or else Context = Ref_Dtd then
            -- A '<' in expanded content
            Start_Index := I;
            return;
          elsif Context = Ref_Attribute then
            -- '<' forbidden in expansion in attribute value
            if not Name_Stack.Is_Empty then
              Error (Ctx.Flow, "Forbidden character '<' in replacement text"
                             & " of attribute value");
            end if;
          end if;
        elsif Char = Ent_End then
          if Istart /= 0 then
            -- The end of a reference, otherwise ignore
            Istop := I;
            exit;
          end if;
        end if;
      end loop;


      -- End of scanning if no more reference
      exit when Istart = 0;

      if Istop = 0 then
        -- A start with no stop => Error
        Error (Ctx.Flow, "Unterminated entity reference " & Text.Image);
      end if;

      -- Check that a stop is big enough
      if Istop = Istart + 1 then
        -- "%;" or "&;"
        Error (Ctx.Flow, "Emtpy entity reference " & Text.Image);
      end if;

      -- Got an entity name: get value if it exists (skip % & ;)
      Name := Result.Uslice (Istart + 1, Istop - 1);

      Entity_Mng.Exists (Dtd.Entity_List,
                           Name, Starter = Ent_Param, Found);
      if not Found then
        if Starter = Ent_Param then
          Error (Ctx.Flow, "Unknown entity " & Ent_Param & " " & Name.Image);
        else
          Error (Ctx.Flow, "Unknown entity " & Name.Image);
        end if;
      end if;

      -- Verify that this entity is not already in the stack
      Search_Name (Name_Stack, Found, (Starter & Name),
                     From => Name_List_Mng.Absolute);
      if Found then
        Error (Ctx.Flow, "Recursive reference to entity "
                       & Starter & Name.Image);
      end if;

      Entity_Mng.Get (Ctx, Dtd, Context, Name, Starter = Ent_Param, Val);

      -- Check and expand this entity replacement (recursively)
      -- Skip when this is a character entity or
      --  in Dtd and too short to get an expansion (< 3 chars)
      if Starter /= Ent_Char
      and then (Context /= Ref_Entity or else Val.Length >= 3) then
        Stack_Empty := Name_Stack.Is_Empty;
        if not Stack_Empty then
          -- Push this entity name in the stack
          Name_Stack.Rewind;
        end if;
        Name_Stack.Insert ( (Starter & Name), Name_List_Mng.Prev);
        Trace ("Expanding >" & Val.Image & "<");
        Expand_Internal (Ctx, Dtd, Val, Context, Start_Index, Name_Stack);
        Trace ("Expanded >" & Name.Image & "< as >" & Val.Image & "<");
        -- Pop this entity name from the stack
        Name_Stack.Rewind;
        Name_Stack.Delete;
      end if;
      -- Verify nesting of parentheses if within content (children) description
      if Context = Ref_Dtd_Content
      and then not Check_Nesting (Val.Image) then
        Error (Ctx.Flow, "Incorrect nesting of parentheses in entity "
                       & Val.Image);
      end if;

      -- Substitute from start to stop
      Result.Replace (Istart, Istop, Val.Image);

      -- Is a Start localized?
      if Start_Index /= 0 then
        -- Update it to be the index in new text
        Start_Index := Istart + Start_Index - 1;
        -- Done with expansion
        exit;
      end if;

      -- Go on parsing after expansion
      Sstart := Istart + Val.Length;

    end loop;

    -- Done
    Text := Result;

  exception
    when Entity_Mng.Entity_Not_Found =>
      Error (Ctx.Flow, "Unknown entity " & Name.Image);
    when Entity_Mng.Invalid_Char_Code =>
      Error (Ctx.Flow, "Invalid Char code " & Name.Image);
    when Entity_Mng.Entity_Standalone =>
      Error (Ctx.Flow, "Invalid reference in standalone document to entity "
           & Name.Image & " defined in external markup declaration");
  end Expand_Internal;

  -- Expand text (expand vars) returns the index of localized '<'
  --  if any
  procedure Expand_Text (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out As.U.Asu_Us;
                         Context : in Context_List;
                         Start_Index : out Natural) is

    List : Name_List_Mng.List_Type;
  begin
    Expand_Internal (Ctx, Dtd, Text, Context, Start_Index, List);
  end Expand_Text;

  -- Expand entities: %Var; and &#xx; if in dtd
  --                  &Var; and &#xx; if in xml
  -- Stop at '<' when in Xml content
  procedure Expand_Vars (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out As.U.Asu_Us;
                         Context : in Context_List) is
    Start_Index : Natural;
  begin
    Expand_Text (Ctx, Dtd, Text, Context, Start_Index);
  end Expand_Vars;

  -- Expand a name if it is a (parameter) entity reference
  -- Error if Text contains % or & but not at beginning
  -- Error if Text contains ; but not at end
  -- Does nothing if not an entity reference
  procedure Expand_Name (Ctx : in out Ctx_Type;
                         Dtd : in out Dtd_Type;
                         Text : in out As.U.Asu_Us;
                         Context : in Context_List) is
    Str : constant String := Text.Image;
    Len : constant Natural := Str'Length;
    Ne, Np, Ns: Natural;

    procedure Entity_Error is
    begin
      Error (Ctx.Flow, "Invalid (parameter) entity reference " & Str);
    end Entity_Error;

  begin
    -- Check presence of "&" and "%" and ";"
    Ne := Str_Util.Locate (Str, Ent_Other & "");
    Np := Str_Util.Locate (Str, Ent_Param & "");
    if Ne + Np = 0 then
      return;
    end if;
    Ns := Str_Util.Locate (Str, ";");
    -- Must have one start at beginning and one stop at end
    if Ne + Np /= 1        -- both or not at beginning
    or else Ns /= Len      -- not at end
    or else Str_Util.Locate (Str, Ent_Other & "", Occurence => 2) /= 0
    or else Str_Util.Locate (Str, Ent_Param & "", Occurence => 2) /= 0
    or else Str_Util.Locate (Str, Ent_End   & "", Occurence => 2) /= 0 then
      Entity_Error;
    end if;
    Expand_Vars (Ctx, Dtd, Text, Context);
  end Expand_Name;

  -- Fix text: replace any separator by a space
  procedure Normalize (Text : in out As.U.Asu_Us) is
    Res : String(1 .. Text.Length) := Text.Image;
  begin
    -- Replace "{ Lf | Tab | Space }" by a space
    Fix_Spaces (Res);
    Text := As.U.Tus (Res);
  end Normalize;

  -- Replace any sequence of spaces by a space
  -- Remove Leading and trailing spaces
  procedure Normalize_Spaces (Text : in out As.U.Asu_Us) is
    Res : As.U.Asu_Us;
    Char : Character;
    -- Will skip leading spaces
    Prev_Is_Space : Boolean := True;
  begin
    for I in 1 .. Text.Length loop
      Char := Text.Element (I);
      if Char = Space then
        if not Prev_Is_Space then
          Res.Append (Util.Space);
          Prev_Is_Space := True;
        end if;
      else
        Res.Append (Char);
        Prev_Is_Space := False;
      end if;
    end loop;
    -- Remove trailing space
    if Res.Length > 1
    and then Res.Element (Res.Length) = Util.Space then
      Res.Delete (Res.Length, Res.Length);
    end if;
    Text := Res;
  end Normalize_Spaces;

  -- Remove separators from text
  procedure Remove_Separators (Text : in out As.U.Asu_Us; Seps : in String) is
    Lseps : As.U.Asu_Us;
    Index : Natural;
    use type As.U.Asu_Us;
  begin
    -- No char of Seps can be separator
    -- No dup
    for I in Seps'Range loop
      if Is_Separator (Seps(I))  then
        raise Constraint_Error;
      end if;
      for J in I + 1 .. Seps'Last loop
        if Seps(J) = Seps(I) then
          raise Constraint_Error;
        end if;
      end loop;
    end loop;

    -- Replace any separators by one space
    Normalize (Text);
    Normalize_Spaces (Text);

    -- Build the "find" pattern
    if Seps = "" then
      Text := As.U.Tus (Str_Util.Substit (Text.Image, " ", ""));
    else
      -- One char: no problem
      Lseps := As.U.Tus (Seps);
      if Seps'Length /= 1 then
        -- Will use "[Seps]",
        -- Avoid "^x" => move '^' at the end
        if Lseps.Element (1) = '^' then
          Lseps.Delete (1, 1);
          Lseps.Append ('^');
        end if;
        -- Move "-" at the beginning
        Index := Str_Util.Locate (Lseps.Image, "-");
        if Index /= 0 then
          Lseps.Delete (Index, 1);
          Lseps := "-" & Lseps;
        end if;
        Lseps := '[' & Lseps & ']';
      end if;
    end if;

    -- Replace " ?([seps]) ?" by \1
    Text := As.U.Tus (Str_Util.Regex.Substit (Text.Image,
        " ?(" & Lseps.Image & ") ?", "\1"));

  end Remove_Separators;

  -- Remove (no expanded) entities from text
  procedure Remove_Entities (Text : in out As.U.Asu_Us) is
    Result : As.U.Asu_Us;
    In_Entity : Boolean;
    Char : Character;
  begin
    In_Entity := False;
    for I in 1 .. Text.Length loop
      Char := Text.Element (I);
      if not In_Entity then
        if Char = Ent_Param or else Char = Ent_Other then
          -- Beginning of entity reference
          In_Entity := True;
        else
          -- Out of entity
          Result.Append (Char);
        end if;
      else
        if Char = Ent_End then
          -- End of entity reference
          In_Entity := False;
        end if;
      end if;
    end loop;
    Text := Result;
  end Remove_Entities;

  -- Push current flow
  procedure Push_Flow (Flow : in out Flow_Type) is
  begin
    -- Push current flow
    Flow.Flows.Push (Flow.Curr_Flow);
  end Push_Flow;

  -- Pop and restore current flow
  procedure Pop_Flow (Flow : in out Flow_Type) is
  begin
    Flow.Flows.Pop (Flow.Curr_Flow);
  end Pop_Flow;

end Util;

