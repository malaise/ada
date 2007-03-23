with Ada.Characters.Latin_1;
with Queues, Lower_Str;
separate (Xml_Parser)

package body Parse_Mng  is

  package Util is
    procedure Init;
    ------------------
    -- Syntax check --
    ------------------
    function Is_Letter (Char : Character) return Boolean;
    -- Check Name; valid
    function Name_Ok (Name : Asu_Us) return Boolean;
    -- Report an error, raises Parsing_Error.
    procedure Error (Msg : in String);
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
    -- Detect separator
    function Is_Separator (Char : Character) return Boolean;
    -- Skip separators until a significant char (not separator); got
    procedure Skip_Separators;
    -- Current significant string, loaded by Parse_Until_xxx
    function Get_Curr_Str return Asu_Us;
    -- Reset current string
    procedure Reset_Curr_Str;
    -- Parse until Criteria; found or until a separator if Criteria = ""
    procedure Parse_Until_Str (Criteria : in String);
    -- Parse until one of the chars; found (any separator if space)
    procedure Parse_Until_Char (Criteria : in String);
    -- Parse until stop character
    procedure Parse_Until_Stop;
    -- Try to parse a keyword, rollback if not
    function Try (Str : String) return Boolean;
    -- Fix text: expand variables and remove repetition of separators
    function Fix_Text (Text : Asu_Us) return Asu_Us;
  end Util;
  package body Util is separate;

  -- Parse a value "Value" or 'Value' (of an entity or
  function Parse_Value return Asu_Us is
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
   return Util.Fix_Text (Value);
  end Parse_Value;

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
                  & Asu.To_String (Attribute_Name));
      end if;
      Util.Reset_Curr_Str;
      -- Attribute name must be unique
      if Of_Xml then
        Tree_Mng.Find_Xml_Attribute (Attribute_Name,
                  Attribute_Index, Attribute_Value);
        if Attribute_Index /= 0 then
          Util.Error ("Attribute " & Asu.To_String (Attribute_Name)
                    & " already defined for xml");
        end if;
      else
        if Tree_Mng.Attribute_Exists (Attribute_Name) then
          Util.Error ("Attribute " & Asu.To_String (Attribute_Name)
                    & " already defined for this element");
        end if;
      end if;
      -- Parse value
      Attribute_Value := Parse_Value;
      if Of_Xml then
        Tree_Mng.Add_Xml_Attribute (Attribute_Name, Attribute_Value, Line_No);
      else
        Tree_Mng.Add_Attribute (Attribute_Name, Attribute_Value, Line_No);
      end if;
      Trace ("Parsed attribute " & Asu.To_String (Attribute_Name)
           & ", " & Asu.To_String (Attribute_Value));
      -- Skip to new attribute if not end of element start
      Util.Skip_Separators;
      Char := Util.Read;
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
      Vers : constant String := Asu.To_String (Attribute_Value);
    begin
      if Vers /= "1.0"
      and then Vers /= "1.1" then
        Util.Error ("Expected xml version " & Vers);
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
        Char := Util.Read;
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
    elsif Lower_Str (Util.Get(3)) /= "xml" then
      -- OK, go on
      Util.Unget (3);
    else
      Util.Error ("Invalid processing instruction");
    end if;

    -- Parse instruction until ? or separator
    if not Tree_Mng.Xml_Existst then
      Util.Error ("Invalid processing instruction without xml declaration");
    end if;
    Util.Parse_Until_Char (Util.Instruction & " ");
    Name := Util.Get_Curr_Str;
    if not Util.Name_Ok (Name) then
      Util.Error ("Unvalid processing instruction name"
               & Asu.To_String (Name));
    end if;
    Util.Reset_Curr_Str;
    if Util.Read /= Util.Instruction then
      -- Some text after the name
      Util.Skip_Separators;
      Util.Unget;
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

  -- Parse a directive (<!xxx>)
  procedure Parse_Directive (Only_Comment : in Boolean) is
    Char : Character;
    Entity_Name, Entity_Value : Asu_Us;
    Index : Natural;
  begin
    -- Got <!, what's next?
    -- Process CDATA or ENTITY and return, or continue and skip
    Char := Util.Get;
    Util.Unget;
    if Char = '-' then
      if Util.Get = '-' then
        Util.Parse_Until_Str ("--" & Util.Stop);
        -- Check that no "--" within comment
        Index := Asu.Index (Util.Get_Curr_Str, "--");
        if Index /= 0 and then Index < Asu.Length(Util.Get_Curr_Str) - 2 then
          Util.Error ("Invalid ""--"" in comment");
        end if;
        Trace ("Skipped <!--" & Asu.To_String (Util.Get_Curr_Str));
        Util.Reset_Curr_Str;
        return;
      else
        if Only_Comment then
          Util.Error ("Invalid directive <!-" & Util.Read);
        end if;
        Util.Unget;
      end if;
    elsif Char = '[' then
      if Only_Comment then
        Util.Error ("Invalid directive <![" & Util.Read);
      end if;
      -- Check if "CDATA["
      if Util.Try ("[CDATA[") then
        -- A CDATA block, skip it
        begin
          Util.Parse_Until_Str ("]]" & Util.Stop);
          Trace ("Skipped <![CDATA[" & Asu.To_String (Util.Get_Curr_Str));
          Util.Reset_Curr_Str;
          return;
        exception
          when Util.End_Error =>
            Util.Error ("Unexpected end of file while parsing CDATA");
        end;
      end if;
    end if;
    -- Skip directive
    Util.Parse_Until_Stop;
    Trace ("Skipped <!" & Asu.To_String (Util.Get_Curr_Str) & Util.Stop);
    Util.Reset_Curr_Str;
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
      C1 := Util.Read;
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
          -- Directive or comment
          Parse_Directive (Only_Comment => False);
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
  begin
    -- Skip separators
    Util.Skip_Separators;
    loop
      Line_No := Util.Get_Line_No;
      if Util.Read = Util.Start then
        Char := Util.Get;
        if Char = Util.Slash then
          -- "</" end of this element
          return;
        elsif Char = Util.Directive then
          -- Must be a comment
          Parse_Directive (Only_Comment => True);
          Util.Skip_Separators;
        else
          -- A new sub-element
          Util.Unget;
          Parse_Element (False);
          Util.Skip_Separators;
        end if;
      else
        -- A text, will stop with a new sub-element or
        --  with stop of current element
        Util.Unget;
        Util.Parse_Until_Char (Util.Start & "");
        -- Fix and add this text
        Text := Util.Fix_Text (Util.Get_Curr_Str);
        Tree_Mng.Add_Text (Text, Line_No);
        Trace ("Parsed Text " & Asu.To_String (Text));
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
      Util.Error ("Invalid element name " & Asu.To_String (Util.Get_Curr_Str));
    end if;
    Element_Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    -- Add new element and move to it
    Tree_Mng.Add_Element (Element_Name, Line_No);
    Trace ("Parsing element " & Asu.To_String (Element_Name));
    if Util.Is_Separator (Util.Read) then
      -- Skip separators
      Util.Skip_Separators;
    end if;
    Char := Util.Read;
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
      Trace ("Parsed element " & Asu.To_String (Element_Name));
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
                  & Asu.To_String (Element_Name)
                  & ", got " & Asu.To_String (End_Name));
      end if;
      -- OK
      Trace ("Parsed element " & Asu.To_String (Element_Name));
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
      C1 := Util.Read;
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
  procedure Parse is
  begin
    -- Init
    Util.Init;
    Tree_Mng.Init_Prologue;
    -- Parse prologue then element
    Parse_Prologue;
    Parse_Root_To_End;
  end Parse;

  -- Get parse error message
  function Get_Error_Message return Asu_Us renames Util.Get_Error_Message;

end Parse_Mng;

