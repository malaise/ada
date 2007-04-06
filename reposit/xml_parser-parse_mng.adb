with Ada.Characters.Latin_1;
with Queues, Lower_Str;
separate (Xml_Parser)

package body Parse_Mng  is

  package Util is

    -- Init to a new file, or back to a previous file
    --  (To_File not significant then)
    -- Specify (for Error) if it is a dtd file
    procedure Init (Back : in Boolean;
                    To_File : in Text_Char.File_Type;
                    Is_Dtd : in Boolean);
    ------------------
    -- Syntax check --
    ------------------
    function Is_Letter (Char : Character) return Boolean;
    -- Check that Name is valid
    function Name_Ok (Name : Asu_Us;
                      Allow_Token : Boolean := False) return Boolean;
    -- Check that Str defines valid names separated by Seps
    function Names_Ok (Str : Asu_Us;
                       Seps : String;
                       Allow_Token : Boolean := False) return Boolean;
    -- Report an error, raises Parsing_Error.
    procedure Error (Msg : in String; Line_No : in Natural := 0);
    -- Retrieve error message
    function Get_Error_Message return Asu_Us;
    -- Get current line number
    function Get_Line_No return Natural;

    -- Get character and store in queue
    End_Error : exception;
    function Get return Character;

    -- Get a string
    procedure Get (Str : out String);
    -- Get N characters
    function Get (N : Positive) return String;
    -- Undo some gets (default 1)
    procedure Unget (N : Positive := 1);
    -- Read last char got
    function Read return Character;
    -- Read Str'Length chars got
    procedure Read (Str : out String);

    -------------
    -- Parsing --
    -------------
    Start : constant Character := '<';
    Stop : constant Character := '>';
    Instruction : constant Character := '?';
    Directive : constant Character := '!';
    Slash : constant Character := '/';
    Equal : constant Character := '=';
    Space : constant Character := ' ';
    -- Detect separator
    function Is_Separator (Char : Character) return Boolean;
    -- Skip separators until a significant char (not separator); got
    procedure Skip_Separators;
    -- Skip separators, return the skipped separators
    function Get_Separators return Asu_Us;
    -- Current significant string, loaded by Parse_Until_xxx
    function Get_Curr_Str return Asu_Us;
    -- Reset current string
    procedure Reset_Curr_Str;
    -- Parse until Criteria found, or until a separator if Criteria = ""
    -- Sets Curr_Str
    procedure Parse_Until_Str (Criteria : in String);
    -- Parse until one of the chars; found (any separator if space)
    -- Sets Curr_Str
    procedure Parse_Until_Char (Criteria : in String);
    -- Parse until stop character
    -- Sets Curr_Str
    procedure Parse_Until_Stop;
    -- Parse until a ')' closes the already got '('
    -- Sets Curr_Str
    procedure Parse_Until_Close;
    -- Parse while name looks valid
    function Parse_Name return Asu_Us;
    -- Try to parse a keyword, rollback if not
    function Try (Str : String) return Boolean;
    -- Fix text: expand entities and remove repetition of separators
    function Fix_Text (Text : Asu_Us;
                       In_Dtd : Boolean;
                       Preserve_Spaces : Boolean) return Asu_Us;
    -- Remove sepators from text
    function Remove_Separators (Text : Asu_Us) return Asu_Us;
  end Util;
  package body Util is separate;

  -- Parse a directive <! >
  -- Dtd uses Parse_Directive for comment and CDATA
  procedure Parse_Directive (Only_Skip : in Boolean);

  -- Parse instruction <? >
  -- Dtd adds its own instructions (except xml)
  procedure Parse_Instruction;

  -- Parse a value "Value" or 'Value' (of an entity or, attribute default...)
  function Parse_Value (In_Dtd : in Boolean) return Asu_Us is
    Value : Asu_Us;
    use type Asu_Us;
  begin
    if Util.Get = ''' then
      Util.Parse_Until_Char ("'");
    elsif Util.Read = '"' then
      Util.Parse_Until_Char ("""");
    else
      Util.Error ("Unexpected value delimiter " & Util.Read);
    end if;
    -- Save parsed text
    Value := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    -- Fix separators and expand entities
    return Util.Fix_Text (Value, In_Dtd, False);
  end Parse_Value;

  -- Dtd management, uses util and the tree
  package Dtd is
    -- Init (clear) Dtd data
    procedure Init;
    -- Parse a dtd (either a external file or internal if name is empty)
    procedure Parse (File_Name : in String);
    -- Check Current element of the tree
    procedure Check_Element;
    -- Perform final checks: that IDREF(s) appear as ID
    procedure Final_Check;
  end Dtd;
  package body Dtd is separate;

  -- Parse attributes of an element Name='Value' or Name="Value"
  -- Either of xml prologue directive or on current element
  procedure Parse_Attributes (Of_Xml : in Boolean) is
    Attribute_Name, Attribute_Value : Asu_Us;
    Attribute_Index : Natural;
    Char : Character;
    Line_No : Natural;
  begin
    -- Loop on several attributes
    loop
      -- Parse name
      Line_No := Util.Get_Line_No;
      Util.Parse_Until_Char (Util.Equal & "");
      Attribute_Name := Util.Get_Curr_Str;
      if not Util.Name_Ok (Attribute_Name) then
        Util.Error ("invalid attribute name "
                  & Asu_Ts (Attribute_Name));
      end if;
      Util.Reset_Curr_Str;
      -- Attribute name must be unique
      if Of_Xml then
        Tree_Mng.Find_Xml_Attribute (Attribute_Name,
                  Attribute_Index, Attribute_Value);
        if Attribute_Index /= 0 then
          Util.Error ("Attribute " & Asu_Ts (Attribute_Name)
                    & " already defined for xml");
        end if;
      else
        if Tree_Mng.Attribute_Exists (Attribute_Name) then
          Util.Error ("Attribute " & Asu_Ts (Attribute_Name)
                    & " already defined for this element");
        end if;
      end if;
      -- Parse value
      Attribute_Value := Parse_Value (False);
      if Of_Xml then
        Tree_Mng.Add_Xml_Attribute (Attribute_Name, Attribute_Value, Line_No);
      else
        Tree_Mng.Add_Attribute (Attribute_Name, Attribute_Value, Line_No);
      end if;
      Trace ("Parsed attribute " & Asu_Ts (Attribute_Name)
           & ", " & Asu_Ts (Attribute_Value));
      -- Skip to new attribute if not end of element start
      Util.Skip_Separators;
      Char := Util.Get;
      -- Stop when ? in directive, or when /, or > in element
      if Of_Xml then
        if Char = Util.Instruction then
          if Util.Get = Util.Stop then
            -- ?> OK
            exit;
          else
            Util.Error ("Unexpected character for terminating instruction");
          end if;
        end if;
      else
        exit when Char = Util.Slash or else Char = Util.Stop;
      end if;
      Util.Unget;
    end loop;
  end Parse_Attributes;

  -- Check Xml version is correctly defined
  procedure Check_Xml_Version is
    Attribute_Value : Asu_Us;
    Attribute_Index : Natural;
  begin
    -- Version must be set, and at first position
    Tree_Mng.Find_Xml_Attribute (Asu.To_Unbounded_String ("version"),
                                 Attribute_Index, Attribute_Value);
    if Attribute_Index /= 1 then
      Util.Error ("Expected version info as first xml attribute");
    end if;
    declare
      Vers : constant String := Asu_Ts (Attribute_Value);
    begin
      if Vers /= "1.0"
      and then Vers /= "1.1" then
        Util.Error ("Unexpected xml version " & Vers);
      end if;
    end;
  end Check_Xml_Version;

  -- Parse an instruction (<?xxx?>)
  procedure Parse_Instruction is
    Char : Character;
    Name : Asu_Us;
  begin
    -- See if this is the xml directive
    if Util.Try ("xml") then
      if Util.Is_Separator (Util.Get) then
        -- Only one xml declaration allowd
        if Tree_Mng.Xml_Existst then
          Util.Error ("Second declaration of xml");
        end if;
        Trace ("Parsing xml declaration");
        Tree_Mng.Set_Xml (Util.Get_Line_No);
        -- Parse xml attributes
        Util.Skip_Separators;
        Char := Util.Get;
        Util.Unget;
        if Char /= Util.Instruction then
          Parse_Attributes (Of_Xml => True);
        end if;
        Check_Xml_Version;
        Trace ("Parsed xml declaration");
        return;
      else
        Util.Error ("Invalid processing instruction");
      end if;
    elsif Lower_Str (Util.Get(3)) = "xml" then
      Util.Error ("Invalid processing instruction");
    else
      -- OK, go on
      Util.Unget (3);
    end if;

    -- Parse instruction until ? or separator
    if not Tree_Mng.Xml_Existst then
      Util.Error ("Invalid processing instruction without xml declaration");
    end if;
    Util.Parse_Until_Char (Util.Instruction & Util.Space);
    Name := Util.Get_Curr_Str;
    if not Util.Name_Ok (Name) then
      Util.Error ("Unvalid processing instruction name"
               & Asu_Ts (Name));
    end if;
    Util.Reset_Curr_Str;
    if Util.Read /= Util.Instruction then
      -- Some text after the name
      Util.Skip_Separators;
      Util.Parse_Until_Char (Util.Instruction & "");
    end if;
    -- Store PI and text if any
    Tree_Mng.Add_Pi (Name, Util.Get_Curr_Str, Util.Get_Line_No);
    Util.Reset_Curr_Str;
    -- Skip to the end
    if Util.Get /= Util.Stop then
      Util.Error ("Unvalid processing instruction termination");
    end if;
  exception
    when Util.End_Error =>
      Util.Error (
        "Unexpected end of file while parsing processing instruction");
  end Parse_Instruction;

  -- Parse "<!DOCTYPE" <Spc> <Name> [ <Spc> "SYSTEM" <Spc> <File> ]
  --  [ <Spc> ] [ "[" <IntSubset> "]" [ <Spc> ] ] "!>"
  -- Reject PUBLIC directive
  procedure Parse_Doctype is
    Doctype_Name, Doctype_File : Asu_Us;
  begin
    -- Parse and check name
    Doctype_Name := Util.Parse_Name;
    if not Util.Name_Ok (Doctype_Name) then
      Util.Error ("Invalid DOCTYPE name " & Asu_Ts (Doctype_Name));
    end if;
    -- What's next
    Util.Skip_Separators;
    if Util.Try ("PUBLIC ") then
      Util.Error ("Unsuported PUBLIC DOCTYPE external ID definition");
    elsif Util.Try ("SYSTEM ") then
      -- A dtd file spec, file name expected
      Util.Skip_Separators;
      if Util.Get = ''' then
        Util.Parse_Until_Char ("'");
      elsif Util.Read = '"' then
        Util.Parse_Until_Char ("""");
      else
        Util.Error ("Unexpected delimiter of DOCTYPE external ID");
      end if;
      Doctype_File := Util.Get_Curr_Str;
      Util.Reset_Curr_Str;
      Dtd.Parse ( Asu.To_String (Doctype_File));
    end if;
    -- Now see if there is an internal definition section
    Util.Skip_Separators;
    if Util.Get = '[' then
      Dtd.Parse ("");
    else
      Util.Unget;
    end if;
    -- Now this should be the end
    Util.Skip_Separators;
    if Util.Get /= Util.Stop then
      Util.Error ("Unexpected character " & Util.Read & " in DOCTYPE");
    end if;
    Trace ("Parsed <!DOCTYPE ... >");
  end Parse_Doctype;

  -- Parse a directive (<!xxx>)
  -- If Only_Skip, allow comments and CDATA only
  -- Otherwise, also allow DOCTYPE
  procedure Parse_Directive (Only_Skip : in Boolean) is
    Index : Natural;
  begin
    -- Got <!, what's next?
    -- Comment, CDATA or DOCTYPE
    if Util.Try ("--") then
      -- "<!--", a comment, skip util "-->"
      Util.Parse_Until_Str ("--" & Util.Stop);
      -- Check that no "--" within comment
      Index := Asu.Index (Util.Get_Curr_Str, "--");
      if Index /= 0 and then Index < Asu.Length(Util.Get_Curr_Str) - 2 then
        Util.Error ("Invalid ""--"" in comment");
      end if;
      Trace ("Skipped <!--" & Asu_Ts (Util.Get_Curr_Str));
      Util.Reset_Curr_Str;
    elsif Util.Try ("[CDATA[") then
      -- "<![CDATA[", a CDATA block, skip until "]]>"
      Util.Parse_Until_Str ("]]" & Util.Stop);
      Trace ("Skipped <![CDATA[" & Asu_Ts (Util.Get_Curr_Str));
      Util.Reset_Curr_Str;
    elsif not Only_Skip and then Util.Try ("DOCTYPE ") then
      Parse_Doctype;
    else
      -- Reject directive
      Util.Parse_Until_Stop;
      Trace ("Invalid directive <!"
           & Asu_Ts (Util.Get_Curr_Str) & Util.Stop);
      Util.Reset_Curr_Str;
    end if;
  exception
    when Util.End_Error =>
      Util.Error ("Unexpected end of file while parsing directive");
  end Parse_Directive;

  -- Parse the prologue
  procedure Parse_Prologue is
    C1, C2 : Character;
  begin
    -- Loop until end of prologue (<name>)
    loop
      -- Get until a significant character, if any
      begin
        Util.Skip_Separators;
      exception
        when Util.End_Error =>
          exit;
      end;
      C1 := Util.Get;
      -- Shall be '<'
      if C1 /= Util.Start then
        Util.Error ("Unexpected character " & C1 & " while expecting "
                  & Util.Start & " in prologue");
      end if;
      C2 := Util.Get;
      case C2 is
        when Util.Instruction =>
          Parse_Instruction;
        when Util.Directive =>
          -- Directive or comment or CDATA
          Parse_Directive (Only_Skip => False);
        when others =>
          -- A name go back to before '<'
          Util.Unget;
          Util.Unget;
          exit;
      end case;
    end loop;
  exception
    when Util.End_Error =>
      Util.Error ("Unexpected end of file");
  end Parse_Prologue;

  -- Parse an element
  procedure Parse_Element (Root : in Boolean);

  -- Parse text or sub-elements of an element (until </)
  procedure Parse_Children is
    Text : Asu_Us;
    Char : Character;
    Line_No : Natural;
    Preserve : Boolean;
    use type Asu_Us;
  begin
    Line_No := Util.Get_Line_No;
    Text := Util.Get_Separators;
    loop
      if Util.Get = Util.Start then
        Char := Util.Get;
        if Char = Util.Slash then
          -- "</" end of this element
          return;
        elsif Char = Util.Directive then
          -- Must be a comment or CDATA
          Parse_Directive (Only_Skip => True);
          Line_No := Util.Get_Line_No;
          Text := Util.Get_Separators;
        else
          -- A new sub-element
          Util.Unget;
          Parse_Element (False);
          Line_No := Util.Get_Line_No;
          Text := Util.Get_Separators;
        end if;
      else
        -- A text, will stop with a new sub-element or
        --  with stop of current element
        Util.Unget;
        Util.Parse_Until_Char (Util.Start & "");
        Util.Unget;
        -- Fix this text. Try to preserve spaces if
        -- current element has attribute xml:space set to preserve
        Preserve := Tree_Mng.Get_Attribute (Asu_Tus ("xml:space"))
                    = Asu_Tus ("preserve");
        if Preserve then
          Trace ("Preserving spaces of the following text");
        end if;
        -- Add previous separators and this text
        Text := Util.Fix_Text (Text & Util.Get_Curr_Str, False, Preserve);
        Tree_Mng.Add_Text (Text, Line_No);
        Trace ("Parsed Text " & Asu_Ts (Text));
        Util.Reset_Curr_Str;
      end if;
    end loop;
  end Parse_Children;

  -- Parse an element (<Name...>)
  procedure Parse_Element (Root : in Boolean) is
    Element_Name, End_Name : Asu_Us;
    Char : Character;
    Line_No : Natural;
    use type Asu_Us;
  begin
    Line_No := Util.Get_Line_No;
    -- Parse name until /, > or a separator
    Util.Parse_Until_Char ("/> ");
    -- Check and store name
    if not Util.Name_Ok (Util.Get_Curr_Str) then
      Util.Error ("Invalid element name " & Asu_Ts (Util.Get_Curr_Str));
    end if;
    Element_Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    -- Add new element and move to it
    Tree_Mng.Add_Element (Element_Name, Line_No);
    Trace ("Parsing element " & Asu_Ts (Element_Name));
    -- See next significant character
    if Util.Is_Separator (Util.Read) then
      -- Skip separators
      Util.Skip_Separators;
      Char := Util.Get;
    else
      Char := Util.Read;
    end if;
    -- If not / nor >, then parse_attributes
    if Char /= Util.Slash and then Char /= Util.Stop then
      Util.Unget;
      Parse_Attributes (Of_Xml => False);
      Char := Util.Read;
    end if;
    -- If /, then must be followed by >, return
    if Char = Util.Slash then
      -- <Name [ attributes ]/>
      if Util.Get /= Util.Stop then
        Util.Error ("Unexpected char " & Util.Read & " after " & Util.Slash);
      end if;
      -- End of this empty element
      Dtd.Check_Element;
      Trace ("Parsed element " & Asu_Ts (Element_Name));
      if not Root then
        Tree_Mng.Move_Up;
      end if;
      return;
    elsif Char = Util.Stop then
      -- >: parse text and children elements until </
      Parse_Children;
      -- Check Name matches
      Util.Parse_Until_Char (Util.Stop & "");
      End_Name := Util.Get_Curr_Str;
      Util.Reset_Curr_Str;
      if End_Name /= Element_Name then
        Util.Error ("Element name mismatch, expected "
                  & Asu_Ts (Element_Name)
                  & ", got " & Asu_Ts (End_Name));
      end if;
      -- End of this non empty element
      Dtd.Check_Element;
      Trace ("Parsed element " & Asu_Ts (Element_Name));
      if not Root then
        Tree_Mng.Move_Up;
      end if;
      return;
    else
      Util.Error ("Unexpected character " & Util.Read
                & " while parsing element");
    end if;
  end Parse_Element;

  -- Parse the root element and until end of file
  procedure Parse_Root_To_End is
    Root_Found : Boolean;
    C1, C2 : Character;
  begin
    -- Loop until end of file
    Root_Found := False;
    loop
      -- Get until a significant character, if any
      begin
        Util.Skip_Separators;
      exception
        when Util.End_Error =>
          exit;
      end;
      C1 := Util.Get;
      -- Shall be '<'
      if C1 /= Util.Start then
        Util.Error ("Unexpected character " & C1 & " while expecting "
                  & Util.Start & " for root");
      end if;
      C2 := Util.Get;
      case C2 is
        when Util.Instruction =>
          Util.Error ("Unexpected processing instruction");
        when Util.Directive =>
          -- Directive : only comment
          Parse_Directive (False);
        when others =>
          Util.Unget;
          if Root_Found then
            Util.Error ("More that one root element found");
          end if;
          Parse_Element (True);
          Root_Found := True;
      end case;
    end loop;
    -- One (and only one) root must have been found
    if not Root_Found then
      Util.Error ("No root element found");
    end if;
  exception
    when Util.End_Error =>
      Util.Error ("Unexpected end of file");
  end Parse_Root_To_End;

  -- Main parser (entry point)
  procedure Parse (File : in out Text_Char.File_Type) is
  begin
    -- Init util to the xml file
    Util.Init (False, File, False);
    -- Init Prologue with an empty root
    Tree_Mng.Init_Prologue;
    -- Reset Dtd
    Dtd.Init;
    -- Parse prologue then root element
    Parse_Prologue;
    Parse_Root_To_End;
    -- Perform final checks versus dtd
    Dtd.Final_Check;
    -- Clean Dtd memory
    Dtd.Init;
  end Parse;

  -- Get parse error message
  function Get_Error_Message return Asu_Us renames Util.Get_Error_Message;

end Parse_Mng;

