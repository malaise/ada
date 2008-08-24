with Ada.Characters.Latin_1;
with Lower_Str, String_Mng;
separate (Xml_Parser)

package body Parse_Mng  is

  -- Entity management
  package Entity_Mng is
    -- Initialise with default entities
    procedure Initialise (The_Entities : in out Entity_List_Mng.List_Type);
    -- Store an entity
    procedure Add (The_Entities : in out Entity_List_Mng.List_Type;
                   Name, Value : in Asu_Us; Parameter : in Boolean);
    -- Check if an entity exists
    procedure Exists (The_Entities : in out Entity_List_Mng.List_Type;
                      Name : in Asu_Us; Parameter : in Boolean;
                      Found : out Boolean);
    -- Get value of an entity. Raises Entity_Not_Found if none
    procedure Get (The_Entities : in out Entity_List_Mng.List_Type;
                   Name : in Asu_Us; Parameter : in Boolean;
                   Got : out Asu_Us);
    Entity_Not_Found : exception;
  end Entity_Mng;
  package body Entity_Mng is separate;

  -- Parsing utilities
  package Util is

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
    procedure Error (Flow : in out Flow_Type;
                     Msg : in String; Line_No : in Natural := 0);
    -- Report Unsupported Cdata
    procedure Cdata_Error (Flow : in out Flow_Type);
    -- Get current line number
    function Get_Line_No (Flow : Flow_Type) return Natural;

    -- Start recording
    procedure Start_Recording (Flow : in out Flow_Type);
    -- Stop recoding and retrieve recorded data
    procedure Stop_Recording (Flow : in out Flow_Type; Recorded : out Asu_Us);

    -- Get character and store in queue
    End_Error : exception;
    procedure Get (Flow : in out Flow_Type; Char : out Character);

    -- Get a string
    procedure Get (Flow : in out Flow_Type; Str : out String);
    -- Get N characters
    procedure Get (Flow : in out Flow_Type; N : in Positive; Str : out String);
    -- Undo some gets (default 1)
    procedure Unget (Flow : in out Flow_Type; N : in Positive := 1);
    -- Read last char got
    procedure Read (Flow : in out Flow_Type; Char : out Character);
    -- Read Str'Length chars got
    procedure Read (Flow : in out Flow_Type; Str : out String);
    -- Injects Str in flow so that it will be got
    procedure Insert (Flow : in out Flow_Type;  Str : in String);

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
    Cdata : constant String := "[CDATA[";
    Doctype : constant String := "DOCTYPE";
    Comment : constant String := "--";
    -- Get_Curr_Str, Try... may raise Cdata_Detected if <![CDATA[ found
    Cdata_Detected : exception;
    -- Detect separator
    function Is_Separator (Char : Character) return Boolean;
    -- Skip separators until a significant char (not separator); got
    procedure Skip_Separators (Flow : in out Flow_Type);
    -- Skip separators, return the skipped separators
    procedure Get_Separators (Flow : in out Flow_Type;
                              Seps : out Asu_Us);
    -- Current significant string, loaded by Parse_Until_xxx
    function Get_Curr_Str (Flow : Flow_Type;
                           Check_Cdata : Boolean := True) return Asu_Us;
    -- Reset current string
    procedure Reset_Curr_Str (Flow : in out Flow_Type);
    -- Parse until Criteria found, or until a separator if Criteria = ""
    -- Sets Curr_Str, consumes all separators if this is the criteria
    procedure Parse_Until_Str (Flow : in out Flow_Type; Criteria : in String);
    -- Parse until one of the chars is found (any separator if space)
    -- Sets Curr_Str, consumes all separators if this is the criteria
    procedure Parse_Until_Char (Flow : in out Flow_Type; Criteria : in String);
    -- Parse until stop character
    -- Sets Curr_Str
    procedure Parse_Until_Stop (Flow : in out Flow_Type);
    -- Parse until a ')' closes the already got '('
    -- Sets Curr_Str
    procedure Parse_Until_Close (Flow : in out Flow_Type);
    -- Parse while name looks valid
    procedure Parse_Name (Flow : in out Flow_Type; Name : out Asu_Us);
    -- Try to parse a keyword, rollback if not
    -- Optionally does not consume the keyword
    -- Space in Str stands for any separator
    -- If Ok and Consume and Str ends with space, consume all separators
    procedure Try (Flow : in out Flow_Type; Str : in String; Ok : out Boolean;
                   Consume : in Boolean := True);
    -- Expand entities: %Var; and &#xx; if in dtd
    --                  &Var; if not in dtd
    -- Both recursively
    procedure Expand_Vars (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out Asu_Us;
                           In_Dtd : in Boolean);
    -- Expand a name if it is a (parameter) entity reference
    -- Error if Text contains % or & but not at beginning
    -- Error if Text contains ; but not at end
    -- Does nothing if not an entity reference
    procedure Expand_Name (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out Asu_Us;
                           In_Dtd : in Boolean);
    -- Fix text: expand entities and remove repetition of separators
    procedure Fix_Text (Ctx : in out Ctx_Type;
                        Dtd : in out Dtd_Type;
                        Text : in out Asu_Us;
                        In_Dtd : in Boolean;
                        Preserve_Spaces : in Boolean);
    -- Remove sepators from text
    function Remove_Separators (Text : Asu_Us) return Asu_Us;
    -- Replace any sequence of separators by a space
    -- Remove Leading and trailing spaces
    function Normalize_Separators (Text : Asu_Us) return Asu_Us;
    -- Skip any Cdata section (<![CDATA[xxx]]>)
    -- If Full_Markup then check for <![CDATA[, else check for [CDATA[
    -- Ok set to True if a CDATA was found
    -- Resets Curr_Str!!!
    procedure Skip_Cdata (Flow : in out Flow_Type;
                          Full_Markup : in Boolean; Ok : out Boolean);
    -- Check for <![CDATA[ in text, raises Cdata_Detected
    procedure Check_Cdata (Str : in Asu_Us);
    -- Check for <![CDATA[ in the next N+9 chars of Flow, raises Cdata_Detected
    procedure Check_Cdata (Flow : in out Flow_Type; N : Natural := 0);
  end Util;
  package body Util is separate;

  -- Parse a directive <! >
  -- Dtd uses Parse_Directive for comment and CDATA
  -- If not In_Dtd, comments might be stored (if Ctx.Parse_Comments)
  procedure Parse_Directive (Ctx : in out Ctx_Type;
                             Adtd : in out Dtd_Type;
                             Allow_Dtd : in Boolean;
                             In_Dtd : in Boolean);

  -- Parse instruction <? >
  -- Dtd adds its own instructions (except xml)
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type);

  -- Parse a value "Value" or 'Value' (of an entity or, attribute default...)
  procedure Parse_Value (Ctx : in out Ctx_Type;
                         Adtd : in out Dtd_Type;
                         In_Dtd : in Boolean;
                         Value : out Asu_Us) is
    use type Asu_Us;
    Char : Character;
  begin
    Util.Get (Ctx.Flow, Char);
    if Char = ''' then
      Util.Parse_Until_Char (Ctx.Flow, "'");
    elsif Char = '"' then
      Util.Parse_Until_Char (Ctx.Flow, """");
    else
      Util.Check_Cdata (Ctx.Flow);
      Util.Error (Ctx.Flow, "Unexpected value delimiter " & Char);
    end if;
    -- Save parsed text
    Value := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);
    -- Fix separators and expand entities
    Util.Fix_Text (Ctx, Adtd, Value, In_Dtd, False);
  end Parse_Value;

  -- Dtd management, uses util and the tree
  package Dtd is
    -- Init (clear) Dtd data
    procedure Init (Adtd : in out Dtd_Type);
    -- Parse a dtd (either a external file or internal flow or string)
    -- Conventions for File_Name:
    -- String flow is when current Ctx string is a dtd
    -- Internal flow is when a DOCTYPE contains "[...]" (string or file)
    -- Otherwise open and parse File_Name
    -- If file name and Name_Raise_Parse, then file Name_Error raises
    --  Parse_Error otherwise File_Error
    String_Flow   : constant String := "";
    Internal_Flow : constant String := ""  & Ada.Characters.Latin_1.Nul;
    procedure Parse (Ctx  : in out Ctx_Type;
                     Adtd : in out Dtd_Type;
                     File_Name : in String;
                     Name_Raise_Parse : in Boolean := True);
    -- Check current element of the tree
    procedure Check_Element (Ctx  : in out Ctx_Type;
                             Adtd : in out Dtd_Type;
                             Check_The_Attributes : Boolean);
    -- Check a whole element tree recursively
    procedure Check_Subtree (Ctx  : in out Ctx_Type;
                             Adtd : in out Dtd_Type);
    -- Perform final checks: that IDREF(s) appear as ID
    procedure Final_Check (Ctx  : in out Ctx_Type;
                           Adtd : in out Dtd_Type);
  end Dtd;

  -- Parse attributes of an element Name='Value' or Name="Value"
  -- Either of xml prologue directive or on current element
  procedure Parse_Attributes (Ctx : in out Ctx_Type;
                              Adtd : in out Dtd_Type;
                              Of_Xml : in Boolean) is
    Attribute_Name, Attribute_Value : Asu_Us;
    Attribute_Index : Natural;
    Char : Character;
    Line_No : Natural;
    Attr_Exists : Boolean;
  begin
    -- Loop on several attributes
    loop
      -- Parse name
      Line_No := Util.Get_Line_No (Ctx.Flow);
      -- Read until a = (looks like a valid attr definition)
      --  or until > or < (no '=' so invalid definition)
      Util.Parse_Until_Char (Ctx.Flow, Util.Equal
                                     & Util.Stop & Util.Start & Util.Slash);
      Attribute_Name := Util.Get_Curr_Str (Ctx.Flow);
      Util.Read (Ctx.Flow, Char);
      if Char /= Util.Equal
      or else not Util.Name_Ok (Attribute_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute name "
                  & Asu_Ts (Attribute_Name));
      end if;
      Util.Reset_Curr_Str (Ctx.Flow);
      -- Attribute name must be unique
      if Of_Xml then
        Tree_Mng.Find_Xml_Attribute (
           Ctx.Prologue.all,
           Attribute_Name,
           Attribute_Index, Attribute_Value);
        if Attribute_Index /= 0 then
          Util.Error (Ctx.Flow, "Attribute " & Asu_Ts (Attribute_Name)
                    & " already defined for xml");
        end if;
        Attr_Exists := False;
      else
        Tree_Mng.Attribute_Exists (Ctx.Elements.all,
                  Attribute_Name, Attr_Exists);
      end if;
      -- Parse value
      Parse_Value (Ctx, Adtd, False, Attribute_Value);
      if Of_Xml then
        Tree_Mng.Add_Xml_Attribute (Ctx.Prologue.all,
                  Attribute_Name, Attribute_Value, Line_No);
      elsif not Attr_Exists then
        -- Keep first definition
        Tree_Mng.Add_Attribute (Ctx.Elements.all,
                  Attribute_Name, Attribute_Value, Line_No);
        if Asu_Ts (Attribute_Name) = "xml:space"
        and then Asu_Ts (Attribute_Value) = "preserve" then
          Tree_Mng.Add_Tuning (Ctx.Elements.all, Tree_Mng.Xml_Space_Preserve);
          Trace ("Added tuning " & Tree_Mng.Xml_Space_Preserve);
        end if;
      end if;
      Trace ("Parsed attribute " & Asu_Ts (Attribute_Name)
           & ", " & Asu_Ts (Attribute_Value));
      -- Skip to new attribute if not end of element start
      Util.Skip_Separators (Ctx.Flow);
      Util.Get (Ctx.Flow, Char);
      -- Stop when ? in directive, or when /, or > in element
      if Of_Xml then
        if Char = Util.Instruction then
          Util.Get (Ctx.Flow, Char);
          if Char = Util.Stop then
            -- ?> OK
            exit;
          else
            Util.Error (Ctx.Flow,
              "Unexpected character for terminating instruction");
          end if;
        end if;
      else
        exit when Char = Util.Slash or else Char = Util.Stop;
      end if;
      Util.Unget (Ctx.Flow);
    end loop;
  end Parse_Attributes;

  -- Check Xml version, encoding and standalone are correctly defined
  --  in xml declaration of DTD or XML
  procedure Check_Xml_Attributes (Ctx : in out Ctx_Type;
                                  Of_Xml : in Boolean) is
    Attribute_Value : Asu_Us;
    Attribute_Index, Next_Index : Natural;
    Nb_Attrs_Set : Natural;
  begin
    Next_Index := 1;
    -- In XML: Version [ Encode ] [ Standalone ]
    -- In Dtd: [ Version ] Encode
    -- Check Version
    Tree_Mng.Find_Xml_Attribute (Ctx.Prologue.all,
           Asu.To_Unbounded_String ("version"),
           Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (Of_Xml and then Attribute_Index = 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml version attribute");
    end if;
    if Attribute_Index /= 0 then
      declare
        Vers : constant String := Asu_Ts (Attribute_Value);
      begin
        if Vers /= "1.0"
        and then Vers /= "1.1" then
          Util.Error (Ctx.Flow, "Unexpected xml version " & Vers);
        end if;
      end;
      Next_Index := Attribute_Index + 1;
    end if;

    -- Check Encoding
    Tree_Mng.Find_Xml_Attribute (Ctx.Prologue.all,
           Asu.To_Unbounded_String ("encoding"),
           Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (not Of_Xml and then Attribute_Index = 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml encoding attribute");
    end if;
    if Attribute_Index /= 0 then
      -- Check encoding value, must be valid name
      -- and also starting with letter and without ":"
      if not Util.Name_Ok (Attribute_Value)
      or else not Util.Is_Letter (Asu.Element (Attribute_Value, 1))
      or else Asu.Index (Attribute_Value, ":") /= 0 then
        Util.Error (Ctx.Flow, "Invalid encoding name");
      end if;
      Next_Index := Attribute_Index + 1;
    end if;

    -- Check Standalone
    Tree_Mng.Find_Xml_Attribute (Ctx.Prologue.all,
           Asu.To_Unbounded_String ("standalone"),
           Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (not Of_Xml and then Attribute_Index /= 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml standalone attribute");
    end if;
    if Attribute_Index /= 0 then
      -- Check standalone value, must be "yes" or "no"
      if Asu_Ts (Attribute_Value) /= "yes"
      and then Asu_Ts (Attribute_Value) /= "no" then
        Util.Error (Ctx.Flow, "Invalid standalone value");
      end if;
      Next_Index := Attribute_Index + 1;
    end if;

    -- No more attribute allowed
    Tree_Mng.Get_Nb_Xml_Attributes (Ctx.Prologue.all, Nb_Attrs_Set);
    if Nb_Attrs_Set /= Next_Index - 1 then
      Util.Error (Ctx.Flow, "Unexpecteed xml attribute");
    end if;
  end Check_Xml_Attributes;

  -- Dtd uses Parse_Attributes and Check_Xml_Attributes
  --  for its XML declaration
  package body Dtd is separate;

  -- Set default Xml version (1.0) if needed
  -- Set encoding from Dtd if needed
  procedure Set_Default_Xml (Ctx : in out Ctx_Type;
                             Adtd : in out Dtd_Type) is
    Nb_Attr : Natural;
    use type Asu_Us;
  begin
    -- No xml attribute? set version
    Tree_Mng.Get_Nb_Xml_Attributes (Ctx.Prologue.all, Nb_Attr);
    if Nb_Attr = 0 then
      Tree_Mng.Add_Xml_Attribute (Ctx.Prologue.all,
          Asu_Tus ("version"), Asu_Tus ("1.0"), Util.Get_Line_No (Ctx.Flow));
    end if;
    -- If Dtd has encoding and Expand
    if Adtd.Encoding = Asu_Null or else not Ctx.Expand then
      return;
    end if;
    -- Current encoding shall be those of DTD
    Trace ("Setting xml encoding from dtd to " & Asu_Ts (Adtd.Encoding));
    Tree_Mng.Set_Xml_Attribute (Ctx.Prologue.all, Asu_Tus ("encoding"),
                                2, Adtd.Encoding);
  end Set_Default_Xml;

  -- Check that XML instruction is set, create one
  -- Inherit the Dtd encoding (if any)
  procedure Check_Xml (Ctx : in out Ctx_Type;
                       Adtd : in out Dtd_Type) is
    Ok : Boolean;
  begin
    Tree_Mng.Xml_Existst (Ctx.Prologue.all, Ok);
    if not Ok then
      -- Add a 'xml' directive
      Tree_Mng.Set_Xml (Ctx.Prologue.all, Util.Get_Line_No (Ctx.Flow));
    end if;
    -- Set default version and inherit encoding if needed
    Set_Default_Xml (Ctx, Adtd);
  end Check_Xml;

  -- Parse an instruction (<?xxx?>)
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type) is
    Char : Character;
    Name : Asu_Us;
    Ok : Boolean;
    Str3 : String (1 .. 3);
  begin
    -- See if this is the xml directive
    Util.Check_Cdata (Ctx.Flow, 3);
    Util.Try (Ctx.Flow, "xml", Ok);
    if Ok then
      Util.Get (Ctx.Flow, Char);
      if Util.Is_Separator (Char) then
        -- Only one xml declaration allowed
        Tree_Mng.Xml_Existst (Ctx.Prologue.all, Ok);
        if Ok then
          Util.Error (Ctx.Flow, "Late or second declaration of xml");
        end if;
        Trace ("Parsing xml declaration");
        Tree_Mng.Set_Xml (Ctx.Prologue.all, Util.Get_Line_No (Ctx.Flow));
        -- Parse xml attributes
        Util.Skip_Separators (Ctx.Flow);
        Util.Get (Ctx.Flow, Char);
        Util.Unget (Ctx.Flow);
        if Char /= Util.Instruction then
          Parse_Attributes (Ctx, Adtd, Of_Xml => True);
        end if;
        Check_Xml_Attributes (Ctx, True);
        Trace ("Parsed xml declaration");
        return;
      else
        Util.Error (Ctx.Flow, "Invalid processing instruction");
      end if;
    else
      Util.Get (Ctx.Flow, Str3);
      if Lower_Str (Str3) = "xml" then
        Util.Error (Ctx.Flow, "Invalid processing instruction");
      else
        -- OK, go on
        Util.Unget (Ctx.Flow, 3);
      end if;
    end if;

    -- Parse instruction until ? or separator
    Check_Xml (Ctx, Adtd);
    Util.Parse_Until_Char (Ctx.Flow, Util.Instruction & Util.Space);
    Name := Util.Get_Curr_Str (Ctx.Flow);
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid processing instruction name"
               & Asu_Ts (Name));
    end if;
    Util.Reset_Curr_Str (Ctx.Flow);
    Util.Read (Ctx.Flow, Char);
    if Char /= Util.Instruction then
      -- Some text after the name
      Util.Skip_Separators (Ctx.Flow);
      Util.Parse_Until_Char (Ctx.Flow, Util.Instruction & "");
    end if;
    -- Store PI and text if any
    Tree_Mng.Add_Pi (Ctx.Prologue.all, Name, Util.Get_Curr_Str (Ctx.Flow),
                     Util.Get_Line_No(Ctx.Flow));
    Util.Reset_Curr_Str (Ctx.Flow);
    -- Skip to the end
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unvalid processing instruction termination");
    end if;
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow,
        "Unexpected end of file while parsing processing instruction");
  end Parse_Instruction;

  -- Parse "<!DOCTYPE" <Spc> <Name> [ <Spc> "SYSTEM" <Spc> <File> ]
  --  [ <Spc> ] [ "[" <IntSubset> "]" [ <Spc> ] ] "!>"
  -- Reject PUBLIC directive
  procedure Parse_Doctype (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type) is
    Doctype_Name, Doctype_File : Asu_Us;
    Ok : Boolean;
    Char : Character;
    Len : Natural;
    use type Asu_Us;
  begin
    -- Only one DOCTYPE allowed
    if Ctx.Doctype.Name /= Asu_Null then
      Util.Error (Ctx.Flow, "Invalid second DOCTYPE directive");
    end if;
    -- Parse and check name
    Util.Parse_Name (Ctx.Flow, Doctype_Name);
    if not Util.Name_Ok (Doctype_Name) then
      Util.Error (Ctx.Flow, "Invalid DOCTYPE name " & Asu_Ts (Doctype_Name));
    end if;
    Ctx.Doctype.Line_No := Util.Get_Line_No (Ctx.Flow);
    Ctx.Doctype.Name := Doctype_Name;
    -- Insert an empty text in prologue
    Tree_Mng.Add_Text (Ctx.Prologue.all, Asu_Null, Util.Get_Line_No (Ctx.Flow));
    -- What's next
    Util.Skip_Separators (Ctx.Flow);
    Util.Check_Cdata (Ctx.Flow, 7);
    Util.Try (Ctx.Flow, "PUBLIC ", Ok);
    if Ok then
      -- A dtd PUBLIC directive: skip public Id
      Util.Get (Ctx.Flow, Char);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Unexpected delimiter of DOCTYPE PUBLIC Id");
      end if;
      Ctx.Doctype.Public := True;
      Ctx.Doctype.Pub_Id := Util.Get_Curr_Str (Ctx.Flow);
      Util.Reset_Curr_Str (Ctx.Flow);
      Util.Skip_Separators (Ctx.Flow);
    else
      -- A dtd SYSTEM directive?
      Util.Try (Ctx.Flow, "SYSTEM ", Ok);
      Ctx.Doctype.Public := False;
    end if;
    if Ok then
      -- Now at dtd URI: file name expected
      Util.Get (Ctx.Flow, Char);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Unexpected delimiter of DOCTYPE external Id");
      end if;
      Doctype_File := Util.Get_Curr_Str (Ctx.Flow);
      Util.Reset_Curr_Str (Ctx.Flow);
      Util.Skip_Separators (Ctx.Flow);
      if Ctx.Use_Dtd then
        if Ctx.Dtd_File /= Asu_Null then
          -- Parse dtd file provided
          Dtd.Parse (Ctx, Adtd, Asu.To_String (Ctx.Dtd_File));
        else
          -- Parse dtd file of doctype
          Dtd.Parse (Ctx, Adtd, Asu.To_String (Doctype_File));
        end if;
      end if;
      Ctx.Doctype.File := Doctype_File;
    end if;
    -- Now see if there is an internal definition section
    Util.Get (Ctx.Flow, Char);
    if Char = '[' then
      -- Internal definition, record the parsing and copy it in Ctx
      Util.Start_Recording (Ctx.Flow);
      Dtd.Parse (Ctx, Adtd, Dtd.Internal_Flow);
      Util.Stop_Recording (Ctx.Flow, Ctx.Doctype.Int_Def);
      -- Remove last ']'
      Len := Asu.Length (Ctx.Doctype.Int_Def);
      Asu.Delete (Ctx.Doctype.Int_Def, Len, Len);
    else
      Util.Unget (Ctx.Flow);
    end if;
    -- Now this should be the end
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character " & Char & " in DOCTYPE");
    end if;
    if not Ctx.Use_Dtd then
      -- Reset dtd info
      Dtd.Init (Adtd);
    end if;
    Trace ("Parsed <!DOCTYPE ... >");
  end Parse_Doctype;

  -- Parse a directive (<!xxx>)
  -- If Allow_Dtd, allow DOCTYPE
  -- Otherwise, allow comments and CDATA only
  procedure Parse_Directive (Ctx : in out Ctx_Type;
                             Adtd : in out Dtd_Type;
                             Allow_Dtd : in Boolean;
                             In_Dtd : in Boolean) is
    Index : Natural;
    Ok : Boolean;
    Comment : Asu_Us;
    use type Asu_Us;
  begin
    -- Got <!, what's next? Comment, CDATA or DOCTYPE
    -- CDATA?
    Util.Skip_Cdata (Ctx.Flow, False, Ok);
    if Ok then
      return;
    end if;

    -- Comment?
    Util.Check_Cdata (Ctx.Flow, 2);
    Util.Try (Ctx.Flow, Util.Comment, Ok);
    if Ok then
      -- "<!--", a comment, skip util "-->"
      Util.Parse_Until_Str (Ctx.Flow, "--" & Util.Stop);
      -- Check that no "--" within comment
      Comment := Util.Get_Curr_Str (Ctx.Flow, Check_Cdata => False);
      Util.Reset_Curr_Str (Ctx.Flow);
      Index := Asu.Index (Comment, "--");
      if Index < Asu.Length(Comment) - 2 then
        Util.Error (Ctx.Flow, "Invalid ""--"" in comment");
      end if;
      -- Remove tailing "-->"
      Comment := Asu.Delete (Comment, Index, Index + 2);
      if not In_Dtd and then Ctx.Parse_Comments then
        if Tree_Mng.Is_Empty (Ctx.Elements.all) then
          -- No element => in prologue
          Tree_Mng.Add_Comment (Ctx.Prologue.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
        else
          Tree_Mng.Add_Comment (Ctx.Elements.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
        end if;
        Trace ("Parsed comment " & Asu_Ts (Comment));
      else
        Trace ("Skipped comment <!--" & Asu_Ts (Comment) & "-->");
      end if;
      return;
    end if;

    -- Doctype?
    Util.Check_Cdata (Ctx.Flow, 8);
    Util.Try (Ctx.Flow, Util.Doctype & " ", Ok);
    if Ok then
      if Allow_Dtd then
        -- Allowed DOCTYPE, parse
        Parse_Doctype (Ctx, Adtd);
        return;
      else
        -- Forbiden DOCTYPE
        Util.Error (Ctx.Flow, "Unexpected DOCTYPE directive");
      end if;
    end if;

    -- Reject directive
    Util.Parse_Until_Stop (Ctx.Flow);
    Util.Error (Ctx.Flow, "Invalid directive <!"
         & Asu_Ts (Util.Get_Curr_Str (Ctx.Flow)) & Util.Stop);
    Util.Reset_Curr_Str (Ctx.Flow);
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file while parsing directive");
  end Parse_Directive;

  -- Parse the prologue
  procedure Parse_Prologue (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Allow_Dtd : in Boolean) is
    C1, C2 : Character;
    use type Asu_Us;
  begin
    -- Loop until end of prologue (<name>)
    loop
      -- Get until a significant character, if any
      begin
        Util.Skip_Separators (Ctx.Flow);
      exception
        when Util.End_Error =>
          exit;
      end;
      Util.Get (Ctx.Flow, C1);
      -- Shall be '<'
      if C1 /= Util.Start then
        Util.Error (Ctx.Flow, "Unexpected character " & C1 & " while expecting "
                  & Util.Start & " in prologue");
      end if;
      Util.Get (Ctx.Flow, C2);
      case C2 is
        when Util.Instruction =>
          Parse_Instruction (Ctx, Adtd);
        when Util.Directive =>
          -- Directive or comment or CDATA
          Check_Xml (Ctx, Adtd);
          Parse_Directive (Ctx, Adtd, Allow_Dtd, False);
        when Util.Start =>
          -- "<<" maybe "<<![CDATA["
          Util.Check_Cdata (Ctx.Flow);
        when others =>
          -- A name go back to before '<'
          Util.Unget (Ctx.Flow);
          Util.Unget (Ctx.Flow);
          exit;
      end case;
    end loop;
    -- Xml directive is mandatory in prologue, which is mandatory in doc
    Check_Xml (Ctx, Adtd);
    -- Parse dtd file if requested to do so and no Doctype
    if Ctx.Use_Dtd
    and then Ctx.Dtd_File /= Asu_Null
    and then Ctx.Doctype.Name = Asu_Null then
      Dtd.Parse (Ctx, Adtd, Asu.To_String (Ctx.Dtd_File));
    end if;
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file");
  end Parse_Prologue;

  -- Parse an element
  procedure Parse_Element (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type;
                           Root : in Boolean);

  -- Parse text or sub-elements of an element (until </)
  procedure Parse_Children (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Text : Asu_Us;
    Char : Character;
    Line_No : Natural;
    Preserve_Txt : Asu_Us;
    Preserve : Boolean;
    Ok : Boolean;
    Has_Child : Boolean;
    use type Asu_Us;
  begin
    -- Try to preserve spaces if current element has this tuning
    Preserve := String_Mng.Locate (Tree_Mng.Get_Tuning (Ctx.Elements.all),
                                   Tree_Mng.Xml_Space_Preserve) /= 0;
    if Preserve then
      Trace ("Preserving spaces of the texts of this element");
    end if;

    -- Detect children
    Has_Child := False;
    Line_No := Util.Get_Line_No (Ctx.Flow);
    Util.Get_Separators (Ctx.Flow, Text);
    loop
      Util.Get (Ctx.Flow, Char);
      if Char = Util.Start then
        Util.Get (Ctx.Flow, Char);
        if Char = Util.Slash then
          -- "</" end of this element
          if not Has_Child and then Text /= Asu_Null then
            -- Element has no child but text between start and stop, add it
            Util.Fix_Text (Ctx, Adtd, Text, False, Preserve);
            Tree_Mng.Add_Text (Ctx.Elements.all, Text, Line_No);
          end if;
          return;
        elsif Char = Util.Directive then
          -- Must be a comment or CDATA
          Parse_Directive (Ctx, Adtd, Allow_Dtd => False, In_Dtd => False);
          Line_No := Util.Get_Line_No (Ctx.Flow);
          Util.Get_Separators (Ctx.Flow, Text);
          Has_Child := True;
        elsif Char = Util.Start then
          Util.Check_Cdata (Ctx.Flow);
          Util.Error (Ctx.Flow, "Unexpected character " & Util.Start);
          Has_Child := True;
        else
          -- A new sub-element
          Util.Unget (Ctx.Flow);
          Parse_Element (Ctx, Adtd, False);
          Line_No := Util.Get_Line_No (Ctx.Flow);
          Util.Get_Separators (Ctx.Flow, Text);
          Has_Child := True;
        end if;
      else
        -- A text, will stop with a new sub-element or
        --  with stop of current element
        -- Concatenate blocks separated by <![CDATA[xxx]]>
        Util.Unget (Ctx.Flow);
        loop
          -- Parse until "Start" and concat
          Util.Parse_Until_Char (Ctx.Flow, Util.Start & "");
          Util.Unget (Ctx.Flow);
          Text := Text & Util.Get_Curr_Str (Ctx.Flow, Check_Cdata => False);
          Util.Reset_Curr_Str (Ctx.Flow);
          -- Skip Cdata if any
          Util.Skip_Cdata (Ctx.Flow, True, Ok);
          -- This "Start" is not a Cdata => End
          exit when not Ok;
        end loop;
        Util.Fix_Text (Ctx, Adtd, Text, False, Preserve);
        Tree_Mng.Add_Text (Ctx.Elements.all, Text, Line_No);
        Has_Child := True;
        Trace ("Parsed Text " & Asu_Ts (Text));
      end if;
    end loop;
  end Parse_Children;

  -- Parse an element (<Name...>)
  procedure Parse_Element (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type;
                           Root : in Boolean) is
    Element_Name, End_Name : Asu_Us;
    Char : Character;
    Line_No : Natural;
    use type Asu_Us;
  begin
    Line_No := Util.Get_Line_No (Ctx.Flow);
    -- Parse name until /, > or a separator
    Util.Parse_Until_Char (Ctx.Flow, "/> ");
    -- Check and store name
    if not Util.Name_Ok (Util.Get_Curr_Str (Ctx.Flow)) then
      Util.Error (Ctx.Flow, "Invalid element name "
                          & Asu_Ts (Util.Get_Curr_Str (Ctx.Flow)));
    end if;
    Element_Name := Util.Get_Curr_Str (Ctx.Flow);
    if Root
    and then Ctx.Doctype.Name /= Asu_Null
    and then Element_Name /= Ctx.Doctype.Name then
      -- Root name must match DOCTYPE name
      Util.Error (Ctx.Flow, "Element name " & Asu_Ts (Element_Name)
           & " does not match doctype name " & Asu_Ts (Ctx.Doctype.Name));
    end if;
    Util.Reset_Curr_Str (Ctx.Flow);
    -- Add new element and move to it
    Tree_Mng.Add_Element (Ctx.Elements.all, Element_Name, Line_No);
    Trace ("Parsing element " & Asu_Ts (Element_Name));
    -- See next significant character
    Util.Read (Ctx.Flow, Char);
    if Util.Is_Separator (Char) then
      -- Skip separators
      Util.Skip_Separators (Ctx.Flow);
      Util.Get (Ctx.Flow, Char);
    end if;
    -- If not / nor >, then parse_attributes
    if Char /= Util.Slash and then Char /= Util.Stop then
      Util.Unget (Ctx.Flow);
      Parse_Attributes (Ctx, Adtd, Of_Xml => False);
      Util.Read (Ctx.Flow, Char);
    end if;
    -- If /, then must be followed by >, return
    if Char = Util.Slash then
      -- <Name [ attributes ]/>
      Util.Get (Ctx.Flow, Char);
      if Char /= Util.Stop then
        Util.Error (Ctx.Flow, "Unexpected char " & Char
                            & " after " & Util.Slash);
      end if;
      -- End of this empty element, check attributes and content
      Dtd.Check_Element (Ctx, Adtd, Check_The_Attributes => True);
      Dtd.Check_Element (Ctx, Adtd, Check_The_Attributes => False);
      Trace ("Parsed element " & Asu_Ts (Element_Name));
      if not Root then
        Tree_Mng.Move_Up (Ctx.Elements.all);
      end if;
      return;
    elsif Char = Util.Stop then
      -- >: parse text and children elements until </
      -- Check attributes first (e.g. xml:space)
      Dtd.Check_Element (Ctx, Adtd, Check_The_Attributes => True);
      Parse_Children (Ctx, Adtd);
      -- Check Name matches
      Util.Parse_Until_Char (Ctx.Flow, Util.Stop & "");
      End_Name := Util.Get_Curr_Str (Ctx.Flow);
      Util.Reset_Curr_Str (Ctx.Flow);
      if End_Name /= Element_Name then
        Util.Error (Ctx.Flow, "Element name mismatch, expected "
                  & Asu_Ts (Element_Name)
                  & ", got " & Asu_Ts (End_Name));
      end if;
      -- End of this non empty element, check children
      Dtd.Check_Element (Ctx, Adtd, Check_The_Attributes => False);
      Trace ("Parsed element " & Asu_Ts (Element_Name));
      if not Root then
        Tree_Mng.Move_Up (Ctx.Elements.all);
      end if;
      return;
    else
      Util.Error (Ctx.Flow, "Unexpected character " & Char
                & " while parsing element");
    end if;
  end Parse_Element;

  -- Parse the root element and until end of file
  procedure Parse_Root_To_End (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type) is
    Root_Found : Boolean;
    C1, C2 : Character;
  begin
    -- Loop until end of file
    Root_Found := False;
    loop
      -- Get until a significant character, if any
      begin
        Util.Skip_Separators (Ctx.Flow);
      exception
        when Util.End_Error =>
          exit;
      end;
      Util.Get (Ctx.Flow, C1);
      -- Shall be '<'
      if C1 /= Util.Start then
        Util.Error (Ctx.Flow, "Unexpected character " & C1 & " while expecting "
                  & Util.Start & " for root");
      end if;
      Util.Get (Ctx.Flow, C2);
      case C2 is
        when Util.Instruction =>
          Util.Error (Ctx.Flow, "Unexpected processing instruction");
        when Util.Directive =>
          -- Directive : only comment
          Parse_Directive (Ctx, Adtd, Allow_Dtd => False, In_Dtd => False);
        when Util.Start =>
          -- "<<" maybe "<<![CDATA["
          Util.Check_Cdata (Ctx.Flow);
        when others =>
          Util.Unget (Ctx.Flow);
          if Root_Found then
            Util.Error (Ctx.Flow, "More that one root element found");
          end if;
          Parse_Element (Ctx, Adtd, True);
          Root_Found := True;
      end case;
    end loop;
    -- One (and only one) root must have been found
    if not Root_Found then
      Util.Error (Ctx.Flow, "No root element found");
    end if;
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file");
  end Parse_Root_To_End;

  -- Main parser (entry point)
  procedure Parse (Ctx : in out Ctx_Type) is
    Adtd : Dtd_Type;
  begin
    -- Init Prologue with an empty root
    Tree_Mng.Init_Prologue (Ctx.Prologue.all);
    -- Reset Dtd
    Dtd.Init (Adtd);
    -- Parse prologue, allow Dtd
    Parse_Prologue (Ctx, Adtd, Allow_Dtd => True);
    -- Parse elements
    Parse_Root_To_End (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx, Adtd);
    -- Clean Dtd before it disapears
    Dtd.Init (Adtd);
  exception
    when Util.Cdata_Detected =>
      Util.Cdata_Error (Ctx.Flow);
  end Parse;

  -- Propagate Dtd convention
  function String_Flow return String is
  begin
    return Dtd.String_Flow;
  end String_Flow;

  -- Parse a Dtd Flow
  procedure Parse_Dtd (Ctx : in out Ctx_Type;
                       Adtd : in out Dtd_Type;
                       File_Name : in String) is
  begin
    -- If File_Name is a file name, then a Name_Error on it
    --  will be propagated as such
    Dtd.Parse (Ctx, Adtd, File_Name, Name_Raise_Parse => False);
  exception
    when Util.Cdata_Detected =>
      Util.Cdata_Error (Ctx.Flow);
  end Parse_Dtd;

   -- Parse the prologue
  procedure Parse_Prologue (Ctx : in out Ctx_Type) is
    Adtd : Dtd_Type;
  begin
    -- Init Prologue with an empty root
    Tree_Mng.Init_Prologue (Ctx.Prologue.all);
    -- Reset Dtd, it will not be used
    Dtd.Init (Adtd);
    -- Parse prologue, disallow Dtd
    Parse_Prologue (Ctx, Adtd, Allow_Dtd => False);
  exception
    when Util.Cdata_Detected =>
      Util.Cdata_Error (Ctx.Flow);
  end Parse_Prologue;

  -- Parse the elements
  procedure Parse_Elements (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type) is
  begin
    -- Parse root element
    Parse_Root_To_End (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx, Adtd);
  exception
    when Util.Cdata_Detected =>
      Util.Cdata_Error (Ctx.Flow);
  end Parse_Elements;

  -- Check a Ctx versus its Dtd
  procedure Check (Ctx : in out Ctx_Type) is
    Adtd : Dtd_Type;
    use type Asu_Us;
  begin
    -- Reset Dtd
    Dtd.Init (Adtd);
    -- Parse Dtd
    if Ctx.Doctype.Name /= Asu_Null then
      if Ctx.Dtd_File /= Asu_Null then
        -- Parse Dtd explicitely forced when user called Parsed
        Dtd.Parse (Ctx, Adtd, Asu.To_String (Ctx.Dtd_File));
      elsif Ctx.Doctype.File /= Asu_Null then
        -- Parse Dtd file set in DOCTYPE of Xml
        Dtd.Parse (Ctx, Adtd, Asu.To_String (Ctx.Doctype.File));
      end if;
      if Ctx.Doctype.Int_Def /= Asu_Null then
        -- Parse internal defs
        Ctx.Flow.Kind := Xml_String;
        Ctx.Flow.In_Str := Ctx.Doctype.Int_Def;
        Ctx.Flow.Xml_Line := 1;
        Dtd.Parse (Ctx, Adtd, Dtd.String_Flow);
      end if;
    end if;
    -- Xml declaration must have a version, which might not be the case
    --  if Ctx comes from Xml_Parser.Generator
    Set_Default_Xml (Ctx, Adtd);
    -- There must be one root
    if Tree_Mng.Is_Empty (Ctx.Elements.all) then
      Util.Error (Ctx.Flow, "No root element found");
    end if;
    -- Check all elements
    Dtd.Check_Subtree (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx, Adtd);
    -- Clean Dtd before it disapears
    Dtd.Init (Adtd);
  exception
    when Util.Cdata_Detected =>
      Util.Cdata_Error (Ctx.Flow);
  end Check;

end Parse_Mng;

