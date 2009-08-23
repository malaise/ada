with Ada.Characters.Latin_1;
with Lower_Str, Upper_Str, String_Mng;
separate (Xml_Parser)

package body Parse_Mng  is

  -- Context where a reference to entity is resolved
  -- In Xml content, in attribute value, in entity value, in dtd
  type Context_List is (Ref_Xml, Ref_Attribute, Ref_Entity, Ref_Dtd);
  -- Is a context in dtd
  function In_Dtd (Context : Context_List) return Boolean is
  begin
    return Context = Ref_Entity or else Context = Ref_Dtd;
  end In_Dtd;

  -- Expand the content of an external parsed entity
  -- Non recursive
  procedure Expand_External_Entity (Ctx : in out Ctx_Type;
                                    Dtd : in out Dtd_Type;
                                    Name, Uri : in Asu_Us;
                                    Text : out Asu_Us);

  -- Entity management
  package Entity_Mng is
    -- Initialise with default entities
    procedure Initialise (The_Entities : in out Entity_List_Mng.List_Type);
    -- Store an entity
    procedure Add (The_Entities : in out Entity_List_Mng.List_Type;
                   Name, Value : in Asu_Us;
                   Parameter : in Boolean;
                   Internal : in Boolean;
                   Parsed : in Boolean);
    -- Check if an entity exists. May raise Invalid_Char_Code
    procedure Exists (The_Entities : in out Entity_List_Mng.List_Type;
                      Name : in Asu_Us;
                      Parameter : in Boolean;
                      Found : out Boolean);

    -- Get value of an entity. Raises Entity_Not_Found if none
    -- May raise Invalid_Char_Code
    procedure Get (Ctx : in out Ctx_Type;
                   Dtd : in out Dtd_Type;
                   Encod : in Encod_List;
                   Context : in Context_List;
                   Name : in Asu_Us;
                   Parameter : in Boolean;
                   Got : out Asu_Us);
    Invalid_Char_Code : exception;
    Entity_Not_Found : exception;
    Entity_Forbidden : exception;
  end Entity_Mng;

  -- Parsing utilities
  package Util is

    ------------------
    -- Syntax check --
    ------------------
    -- Check if char is a letter
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

    ----------------------
    -- Input characters --
    ----------------------
    -- Autodetect encoding family
    procedure Guess_Encoding (Flow : in out Flow_Type);

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
    -- Undo some gets (default 1)
    procedure Unget (Flow : in out Flow_Type; N : in Natural := 1);
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
    Cdata_Start : constant String := Start & Directive & Cdata;
    Cdata_End : constant String := "]]" & Stop;
    Doctype : constant String := "DOCTYPE";
    Comment : constant String := "--";
    Ent_Param : constant Character := '%';
    Ent_Other : constant Character := '&';
    Ent_Char : constant Character := '#';
    Ent_End : constant Character := ';';
    -- Detect separator
    function Is_Separator (Char : Character) return Boolean;
    function Is_Separators (Str : Asu_Us) return Boolean;
    -- Skip separators until a significant char (not separator); got
    procedure Skip_Separators (Flow : in out Flow_Type);
    -- Current significant string, loaded by Parse_Until_xxx
    procedure Get_Curr_Str (Flow : in out Flow_Type;
                            Str : out Asu_Us;
                            Reset : in Boolean := True);
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
    -- Parse until end of flow
    -- Sets Curr_Str
    procedure Parse_Until_End (Flow : in out Flow_Type);
    -- Parse until a ')' closes the already got '('
    -- Sets Curr_Str
    procedure Parse_Until_Close (Flow : in out Flow_Type);
    -- Parse while name looks valid
    procedure Parse_Name (Flow : in out Flow_Type; Name : out Asu_Us);
    -- Try to parse a keyword, rollback if not
    -- Optionally does not consume the keyword
    -- Str = " " stands for any separator
    -- If Ok and Consume and Str ends with space, consume all separators
    procedure Try (Flow : in out Flow_Type; Str : in String; Ok : out Boolean;
                   Consume : in Boolean := True);
    -- Expand entities: %Var; and &#xx; if in dtd
    --                  &Var; and &#xx; if in xml
    -- Stop at '<' when in Xml content
    procedure Expand_Vars (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out Asu_Us;
                           Context : in Context_List);
    -- Expand text (expand vars) returns the index of localized '<'
    --  if any
    procedure Expand_Text (Ctx : in out Ctx_Type;
                            Dtd : in out Dtd_Type;
                            Text : in out Asu_Us;
                            Context : in Context_List;
                            Start_Index : out Natural);
    -- Expand a name if it is a (parameter) entity reference
    -- Error if Text contains % or & but not at beginning
    -- Error if Text contains ; but not at end
    -- Does nothing if not an entity reference
    procedure Expand_Name (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out Asu_Us;
                           Context : in Context_List);
    -- Fix text: Remove repetition of separators
    procedure Fix_Text (Ctx : in out Ctx_Type;
                        Text : in out Asu_Us;
                        Context : in Context_List;
                        Preserve_Spaces : in Boolean);
    -- Remove sepators from text
    function Remove_Separators (Text : Asu_Us) return Asu_Us;
    -- Replace any sequence of separators by a space
    -- Remove Leading and trailing spaces
    function Normalize_Separators (Text : Asu_Us) return Asu_Us;

    -- Push current flow
    procedure Push_Flow (Flow : in out Flow_Type);
    -- Pop and restore current flow
    procedure Pop_Flow (Flow : in out Flow_Type);

  end Util;

  package body Entity_Mng is separate;
  package body Util is separate;

  -- Descriptor of list of children found
  type Children_Desc is record
    -- Result of parsing of children
    Is_Empty : Boolean := True;
    Children : Asu_Us;
    Has_Text : Boolean := False;
    -- Token passed along the parsing of children
    Created : Boolean := False;
    Prev_Is_Text : Boolean := False;
    Preserve : Boolean := False;
  end record;

  -- Parse a directive <! >
  -- Dtd uses Parse_Directive for comment and CDATA
  -- If not In_Dtd, comments might be stored (if Ctx.Parse_Comments)
  -- Can be called by Dtd
  procedure Parse_Directive (Ctx : in out Ctx_Type;
                             Adtd : in out Dtd_Type;
                             Allow_Dtd : in Boolean;
                             Context : in Context_List;
                             Children : access Children_Desc);

  -- Parse a value "Value" or 'Value' (of an entity or, attribute default...)
  -- Can be called by Dtd (for ATTLIST or ENTITY value)
  procedure Parse_Value (Ctx : in out Ctx_Type;
                         Adtd : in out Dtd_Type;
                         Context : in Context_List;
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
      Util.Error (Ctx.Flow, "Unexpected value delimiter " & Char);
    end if;
    -- Save parsed text
    Util.Get_Curr_Str (Ctx.Flow, Value);
    -- Expand entities and fix separators and expand entities
    Util.Expand_Vars (Ctx, Adtd, Value, Context);
    Util.Fix_Text (Ctx, Value, Context, False);
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
                     File_Name : in Asu_Us;
                     Name_Raise_Parse : in Boolean := True);
    -- Perform final checks after DTD parsing: unparsed entities v.s. notations
    procedure Final_Dtd_Check (Ctx  : in out Ctx_Type; Adtd : in out Dtd_Type);
    -- Check attributes of current element of the tree
    procedure Check_Attributes (Ctx  : in out Ctx_Type;
                                Adtd : in out Dtd_Type);
    -- Add current element to list of children
    procedure Add_Current_Element (List : in out Asu_Us; Name : in Asu_Us);
    -- Check that list matches Dtd definition of current element
    procedure Check_Element (Ctx      : in out Ctx_Type;
                             Adtd     : in out Dtd_Type;
                             Children : in Children_Desc);
    -- Check a whole element tree recursively
    procedure Check_Subtree (Ctx  : in out Ctx_Type;
                             Adtd : in out Dtd_Type);
    -- Perform final checks after XML parsing: that IDREF(s) appear as ID
    procedure Final_Check (Ctx  : in out Ctx_Type);
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
      Util.Get_Curr_Str (Ctx.Flow, Attribute_Name);
      Util.Read (Ctx.Flow, Char);
      if Char /= Util.Equal
      or else not Util.Name_Ok (Attribute_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute name "
                  & Asu_Ts (Attribute_Name));
      end if;
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
      Parse_Value (Ctx, Adtd, Ref_Attribute, Attribute_Value);
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
      -- Check this is "UTF-8" or "UTF-16" and that it matches the
      --  guessed encoding
      if Upper_Str (Asu_Ts (Attribute_Value)) = "UTF-8" then
        if Ctx.Flow.Curr_Flow.Encod /= Utf8 then
          Util.Error (Ctx.Flow, "Encoding " & Asu_Ts (Attribute_Value)
                    & " differs from autodetected "
                    & Ctx.Flow.Curr_Flow.Encod'Img);
        end if;
      elsif Upper_Str (Asu_Ts (Attribute_Value)) = "UTF-16" then
        if Ctx.Flow.Curr_Flow.Encod = Utf8 then
          Util.Error (Ctx.Flow, "Encoding " & Asu_Ts (Attribute_Value)
                    & " differs from autodetected "
                    & Ctx.Flow.Curr_Flow.Encod'Img);
        end if;
      else
        Util.Error (Ctx.Flow,
         "Unsupported encoding (only UTF-8 and UTF-16 are supported)");
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

  -- Call Callback of creation if requested
  procedure Call_Callback (Ctx : in out Ctx_Type;
                           In_Prologue : in Boolean;
                           Creation : in Boolean;
                           Has_Children : in Boolean := False;
                           Prev_Is_Text : in Boolean := False) is
    Upd : Node_Update;
  begin
    if Ctx.Callback = null then
      return;
    end if;
    if In_Prologue then
      Tree_Mng.Build_Update (Ctx.Prologue.all, Upd, Creation);
    else
      Tree_Mng.Build_Update (Ctx.Elements.all, Upd, Creation);
    end if;
    Upd.In_Prologue := In_Prologue;
    Upd.Has_Children := Has_Children;
    Upd.Prev_Is_Text := Prev_Is_Text;
    Upd.Level := Ctx.Level;
    if not Creation then
      Upd.Line_No := Util.Get_Line_No(Ctx.Flow);
    end if;
    begin
      Ctx.Callback (Ctx, Upd);
    exception
      when others =>
        raise Callback_Error;
    end;
  end Call_Callback;

  -- Delete current node/tree if callback
  -- Move up if no callback (and not root)
  procedure Move_Del (Ctx : in out Ctx_Type;
                      In_Prologue : in Boolean;
                      Deallocate : in Boolean := False) is
    Tree : Tree_Acc;
  begin
    if In_Prologue then
      Tree := Ctx.Prologue;
    else
      Tree := Ctx.Elements;
    end if;

    if Ctx.Callback /= null then
      Tree.Delete_Tree (Deallocate);
    elsif Tree.Has_Father then
      Tree.Move_Father;
    end if;
  end Move_Del;

  -- Add current element to list of children
  procedure Add_Child (Ctx  : in out Ctx_Type;
                       Adtd : in out Dtd_Type;
                       Children : access Children_Desc) is
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    if not Adtd.Set or else Children = null then
      -- In Prologue (Pi or directive) => no check
      -- No dtd => no check
      return;
    end if;
    Children.Is_Empty := False;
    My_Tree.Read (Ctx.Elements.all, Cell);
    case Cell.Kind is
      when Element =>
        Dtd.Add_Current_Element (Children.Children, Cell.Name);
      when Text =>
        Children.Has_Text := True;
      when Pi | Comment =>
        -- Is_Empty is False
        null;
      when Attribute =>
        Trace ("Adding current attribute as element list");
        raise Internal_Error;
    end case;
  end Add_Child;

  -- Check element's children, tree or desc
  procedure Check_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Desc : access Children_Desc) is
  begin
    Dtd.Check_Element (Ctx, Adtd, Desc.all);
  end Check_Children;

  -- Expand the content of an external parsed entity
  procedure Expand_External_Entity (Ctx : in out Ctx_Type;
                                    Dtd : in out Dtd_Type;
                                    Name, Uri : in Asu_Us;
                                    Text : out Asu_Us) is
    File_Name : Asu_Us;
    Ok : Boolean;
    Char : Character;
    Dummy : My_Tree_Cell;

    use type Asu_Us;
  begin
    Trace ("Ext expanding external entity " & Asu_Ts (Name) & " with URI "
         & Asu_Ts (Uri));
    if Uri = Asu_Null then
      Util.Error (Ctx.Flow, "Invalid external entity URI " & Asu_Ts (Uri)
                          & " for entity " & Asu_Ts (Name) & ".");
    end if;
    Util.Push_Flow (Ctx.Flow);
    -- Init Flow
    File_Name := Build_Full_Name (Uri, Ctx.Flow.Curr_Flow.Name);
    Ctx.Flow.Curr_Flow.Is_File := True;
    Ctx.Flow.Curr_Flow.Kind := Ext_Flow;
    Ctx.Flow.Curr_Flow.Name := Uri;
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
    Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
    File_Mng.Open (Asu_Ts (File_Name), Ctx.Flow.Curr_Flow.File.all);

    -- Parse
    Util.Guess_Encoding (Ctx.Flow);
    -- See if this is the "<?xml " directive
    Util.Try (Ctx.Flow, Util.Start & Util.Instruction & "xml", Ok);
    if Ok then
      Util.Get (Ctx.Flow, Char);
      Ok := Util.Is_Separator (Char);
      if not Ok then
        Util.Error (Ctx.Flow, "Invalid text declaration");
      end if;
    end  if;
    if Ok then
      -- Parse xml directive: on a dummy child of prologue
      Dummy.Line_No := 0;
      if My_Tree.Is_Empty (Ctx.Prologue.all) then
        My_Tree.Insert_Father (Ctx.Prologue.all, Dummy);
      else
        My_Tree.Insert_Child (Ctx.Prologue.all, Dummy, False);
      end if;
      Parse_Attributes (Ctx, Dtd, True);
      Check_Xml_Attributes (Ctx, False);
      My_Tree.Delete_Tree (Ctx.Prologue.all);
      Trace ("Ext parsed xml instruction");
      Util.Skip_Separators (Ctx.Flow);
    end if;

    -- Load content in string
    Util.Parse_Until_End (Ctx.Flow);
    Util.Get_Curr_Str (Ctx.Flow, Text);
    Trace ("Ext expanded as >" & Asu_Ts(Text) & "<");

    -- Done: restore flow
    Reset (Ctx.Flow.Curr_Flow);
    Util.Pop_Flow (Ctx.Flow);
  exception
    when File_Error =>
     Util.Error (Ctx.Flow, "Cannot open external entity file "
                         & Asu_Ts (File_Name));
  end Expand_External_Entity;

  -- Set default Xml version (1.0) if needed
  -- Set encoding from Dtd if needed
  procedure Set_Default_Xml (Ctx : in out Ctx_Type) is
    Nb_Attr : Natural;
    use type Asu_Us;
  begin
    -- No xml attribute? set version
    Tree_Mng.Get_Nb_Xml_Attributes (Ctx.Prologue.all, Nb_Attr);
    if Nb_Attr = 0 then
      Tree_Mng.Add_Xml_Attribute (Ctx.Prologue.all,
          Asu_Tus ("version"), Asu_Tus ("1.0"), Util.Get_Line_No (Ctx.Flow));
    end if;
  end Set_Default_Xml;

  -- Check that XML instruction is set, create one
  -- Inherit the Dtd encoding (if any)
  procedure Check_Xml (Ctx : in out Ctx_Type) is
    Ok : Boolean;
  begin
    Tree_Mng.Move_Root (Ctx.Prologue.all);
    Tree_Mng.Xml_Existst (Ctx.Prologue.all, Ok);
    if not Ok then
      -- Add a 'xml' directive
      Tree_Mng.Set_Xml (Ctx.Prologue.all, Util.Get_Line_No (Ctx.Flow));
    end if;
    -- Set default version
    Set_Default_Xml (Ctx);
    -- Callback creation if Xml has been created here
    if not Ok then
      Call_Callback (Ctx, True, True);
      Ctx.Level := 1;
    end if;
  end Check_Xml;

  -- Parse an instruction (<?xxx?>)
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type;
                               Children : access Children_Desc) is
    Char : Character;
    Name, Value : Asu_Us;
    Ok : Boolean;
    Str3 : String (1 .. 3);
    In_Prologue : constant Boolean := Tree_Mng.Is_Empty (Ctx.Elements.all);
  begin
    -- See if this is the xml directive
    if In_Prologue then
      -- No element => in prologue
      Util.Try (Ctx.Flow, "xml ", Ok);
      if Ok then
        -- Only one xml declaration allowed
        Tree_Mng.Move_Root (Ctx.Prologue.all);
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
        Call_Callback (Ctx, True, True);
        Ctx.Level := 1;
        Trace ("Parsed xml declaration");
        return;
      else
        -- A PI in prologue => insert Xml if it is missing
        Check_Xml (Ctx);
      end if;
    end if;

    -- Xml not allowed in prologue of elements
    Util.Get (Ctx.Flow, Str3);
    if Lower_Str (Str3) = "xml" then
      Util.Error (Ctx.Flow, "Invalid processing instruction");
    else
      -- OK, go on
      Util.Unget (Ctx.Flow, 3);
    end if;

    -- Parse instruction until ? or separator
    Util.Parse_Until_Char (Ctx.Flow, Util.Instruction & Util.Space);
    Util.Get_Curr_Str (Ctx.Flow, Name);
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid processing instruction name"
               & Asu_Ts (Name));
    end if;
    Util.Read (Ctx.Flow, Char);
    if Char = Util.Instruction then
      -- Skip to the end
      Util.Get (Ctx.Flow, Char);
      if Char /= Util.Stop then
        Util.Error (Ctx.Flow, "Invalid processing instruction termination");
      end if;
    else
      -- Some text after the name, get it until "?>"
      Util.Skip_Separators (Ctx.Flow);
      Util.Parse_Until_Str (Ctx.Flow, Util.Instruction & Util.Stop);
      -- Skip "?>"
      Util.Get_Curr_Str (Ctx.Flow, Value);
      Value := Asu.Delete (Value, Asu.Length (Value) - 1, Asu.Length (Value));
    end if;

    -- Add node
    Trace ("Parsed <?" & Asu_Ts (Name) & " "
         & Asu_Ts (Value) & "?>");
    if In_Prologue then
      -- No element => in prologue
      Tree_Mng.Move_Root (Ctx.Prologue.all);
      Tree_Mng.Add_Pi (Ctx.Prologue.all, Name, Value,
                       Util.Get_Line_No(Ctx.Flow));
      Call_Callback (Ctx, True, True);
    else
      Tree_Mng.Add_Pi (Ctx.Elements.all, Name, Value,
                       Util.Get_Line_No(Ctx.Flow));
      -- Add this child
      if Children /= null then
        Call_Callback (Ctx, False, True,
                       Prev_Is_Text => Children.Prev_Is_Text);
        Add_Child (Ctx, Adtd, Children);
      else
        Call_Callback (Ctx, False, True);
      end if;
      Move_Del (Ctx, In_Prologue);
    end if;
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow,
        "Unexpected end of file while parsing processing instruction");
  end Parse_Instruction;

  -- Parse "<!DOCTYPE" <Spc> <Name> [ <Spc> "SYSTEM" <Spc> <File> ]
  --  [ <Spc> ] [ "[" <IntSubset> "]" [ <Spc> ] ] "!>"
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
    Trace ("Parsing doctype " & Asu_Ts (Doctype_Name));
    Ctx.Doctype.Line_No := Util.Get_Line_No (Ctx.Flow);
    Ctx.Doctype.Name := Doctype_Name;
    -- Insert an empty text in prologue
    Tree_Mng.Move_Root (Ctx.Prologue.all);
    Tree_Mng.Add_Text (Ctx.Prologue.all, Asu_Null, Util.Get_Line_No (Ctx.Flow));
    -- What's next
    Util.Skip_Separators (Ctx.Flow);
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
      Util.Get_Curr_Str (Ctx.Flow, Ctx.Doctype.Pub_Id);
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
      Util.Get_Curr_Str (Ctx.Flow, Doctype_File);
      Util.Skip_Separators (Ctx.Flow);
      if Ctx.Use_Dtd
      and then Ctx.Dtd_File = Asu_Null then
        -- Parse dtd file of doctype directive if no alternate file
        Util.Push_Flow (Ctx.Flow);
        Dtd.Parse (Ctx, Adtd, Build_Full_Name (Doctype_File,
                                               Ctx.Flow.Curr_Flow.Name));
        Util.Pop_Flow (Ctx.Flow);
      end if;
      Ctx.Doctype.File := Doctype_File;
    end if;
    -- Now see if there is an internal definition section
    Util.Get (Ctx.Flow, Char);
    if Char = '[' then
      -- Internal definition, record the parsing and copy it in Ctx
      Util.Start_Recording (Ctx.Flow);
      Dtd.Parse (Ctx, Adtd, Asu_Tus (Dtd.Internal_Flow));
      Util.Stop_Recording (Ctx.Flow, Ctx.Doctype.Int_Def);
      -- Remove last ']'
      Len := Asu.Length (Ctx.Doctype.Int_Def);
      Asu.Delete (Ctx.Doctype.Int_Def, Len, Len);
      if Ctx.Dtd_File /= Asu_Null then
        -- This Dtd internal definition is overriden by an alternate file
        Clean_Dtd (Adtd);
      end if;
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
    Call_Callback (Ctx, True, True);
    Move_Del (Ctx, True);
    Trace ("Parsed <!DOCTYPE ... >");
  end Parse_Doctype;

  -- Parse a directive (<!xxx>)
  -- If Allow_Dtd, allow DOCTYPE
  -- Otherwise, allow comments and CDATA only
  procedure Parse_Directive (Ctx : in out Ctx_Type;
                             Adtd : in out Dtd_Type;
                             Allow_Dtd : in Boolean;
                             Context : in Context_List;
                             Children : access Children_Desc) is
    Index : Natural;
    Ok : Boolean;
    Comment : Asu_Us;
    use type Asu_Us;
  begin

    -- Comment?
    Util.Try (Ctx.Flow, Util.Comment, Ok);
    if Ok then
      -- "<!--", a comment, skip util "-->"
      Util.Parse_Until_Str (Ctx.Flow, "--" & Util.Stop);
      -- Check that no "--" within comment
      Util.Get_Curr_Str (Ctx.Flow, Comment);
      Index := Asu.Index (Comment, "--");
      if Index < Asu.Length(Comment) - 2 then
        Util.Error (Ctx.Flow, "Invalid ""--"" in comment");
      end if;
      -- Remove tailing "-->"
      Comment := Asu.Delete (Comment, Index, Index + 2);
      -- Add node
      if not In_Dtd (Context) and then Ctx.Parse_Comments then
        if Tree_Mng.Is_Empty (Ctx.Elements.all) then
          -- No element => in prologue
          Tree_Mng.Move_Root (Ctx.Prologue.all);
          Tree_Mng.Add_Comment (Ctx.Prologue.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
          Call_Callback (Ctx, True, True);
        else
          Tree_Mng.Add_Comment (Ctx.Elements.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
          -- Add this child
          if Children /= null then
            Call_Callback (Ctx, False, True,
                           Prev_Is_Text => Children.Prev_Is_Text);
            Add_Child (Ctx, Adtd, Children);
          else
            Call_Callback (Ctx, False, True);
          end if;
        end if;
        Move_Del (Ctx, Tree_Mng.Is_Empty (Ctx.Elements.all));
        Trace ("Parsed comment " & Asu_Ts (Comment));
      else
        Trace ("Skipped comment <!--" & Asu_Ts (Comment) & "-->");
      end if;
      return;
    end if;

    -- Doctype?
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
    Util.Get_Curr_Str (Ctx.Flow, Comment);
    Util.Error (Ctx.Flow, "Invalid directive <!"
         & Asu_Ts (Comment) & Util.Stop);
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
    -- Autodetect encoding and check
    if Ctx.Flow.Curr_Flow.Is_File then
      Util.Guess_Encoding (Ctx.Flow);
      Trace ("Detected encoding format " & Ctx.Flow.Curr_Flow.Encod'Img);
    else
      Ctx.Flow.Curr_Flow.Encod := Utf8;
    end if;

    Ctx.Level := 0;
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
          Parse_Instruction (Ctx, Adtd, null);
        when Util.Directive =>
          -- Directive or comment or CDATA
          Check_Xml (Ctx);
          Parse_Directive (Ctx, Adtd, Allow_Dtd, Ref_Xml, null);
        when others =>
          -- A name go back to before '<'
          Util.Unget (Ctx.Flow);
          Util.Unget (Ctx.Flow);
          exit;
      end case;
    end loop;
    -- Xml directive is mandatory in prologue, which is mandatory in doc
    Check_Xml (Ctx);
    -- Parse dtd alternate file if requested to do so
    if Ctx.Use_Dtd
    and then Ctx.Dtd_File /= Asu_Null then
      Util.Push_Flow (Ctx.Flow);
      -- Parse dtd file provided instead of doctype directive
      Dtd.Parse (Ctx, Adtd, Build_Full_Name (Ctx.Dtd_File,
                                             Ctx.Flow.Curr_Flow.Name));
      Util.Pop_Flow (Ctx.Flow);
    end if;
    -- Perform final checks on Dtd (unparsed entities v;s. notations)
    Dtd.Final_Dtd_Check (Ctx, Adtd);
    Tree_Mng.Move_Root (Ctx.Prologue.all);
    -- Delete completely the prologue if callback
    Move_Del (Ctx, True, True);
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file");
  end Parse_Prologue;

  -- Parse an element (<Name...>)
  procedure Parse_Element (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type;
                           Parent_Children : access Children_Desc;
                           Root : in Boolean);

  -- Parse the children of current element
  procedure Parse_Children (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Children : access Children_Desc;
                            Allow_End : Boolean := False);

  -- Parse text
  procedure Parse_Text (Ctx : in out Ctx_Type;
                        Adtd : in out Dtd_Type;
                        Children : access Children_Desc) is
    Text, Tail, Head, Cdata : Asu_Us;
    Index : Natural;
    Ok : Boolean;
    use type Asu_Us;
  begin
    -- Concatenate blocks of expanded text and CDATA sections

    -- Loop as long a flow does not contain and does not expand
    --  to '<', skipping "<![CDATA["
    Cdata_In_Flow:
    loop

      -- Parse until '<' or End of flow
      --  save Text, and save Cdata in Tail
      Tail := Asu_Null;
      begin
        Util.Parse_Until_Char (Ctx.Flow, Util.Start & "");
        Util.Unget (Ctx.Flow);
        -- Save Text
        Util.Get_Curr_Str (Ctx.Flow, Text);
      exception
        when Util.End_Error =>
          -- End of flow, save text
          Util.Get_Curr_Str (Ctx.Flow, Text);
          if Text = Asu_Null then
            -- End of flow and no text
            exit Cdata_In_Flow;
          end if;
      end;
      Trace ("Txt Got text >" & Asu_Ts (Text) & "<");
      -- Check "<![CDATA["
      Util.Try (Ctx.Flow, Util.Cdata_Start, Ok);
      if Ok then
        Util.Parse_Until_Str (Ctx.Flow, Util.Cdata_End);
        Util.Get_Curr_Str (Ctx.Flow, Cdata);
        Trace ("Txt Got cdata >" & Asu_Ts (Cdata) & "<");
        Cdata := Util.Cdata_Start & Cdata & Util.Cdata_End;
        if Text = Asu_Null then
          -- No text: This is fixed (in Head), and we will re-loop reading
          Head := Head & Cdata;
        else
          -- Some text: This will be appended to expanded text
          Tail := Cdata;
        end if;
      elsif Text = Asu_Null then
        -- A valid '<' found and no text before it
        exit Cdata_In_Flow;
      end if;

      -- Loop as long as expansion does not generate a '<'
      --  or as long as this is a "<![CDATA["
      -- At the end, Head contains expanded text and CDATA
      --  and Tail contains non expanded flow
      Head := Asu_Null;
      if Ctx.Expand then
        Cdata_In_Text:
        loop
          -- Done when no more text to expand
          exit Cdata_In_Text when Text = Asu_Null;
          -- Expand Text and check if it generated a '<'
          Util.Expand_Text (Ctx, Adtd, Text, Ref_Xml, Index);
          if Text /= Asu_Null then
            -- Expansion lead to something => not empty
            Children.Is_Empty := False;
          end if;

          if Index /= 0 then
            if Index + Util.Cdata_Start'Length - 1 <= Asu.Length (Text)
            and then Asu.Slice (Text, Index,
                                Index + Util.Cdata_Start'Length - 1)
                     = Util.Cdata_Start then

              -- Expansion stopped but this is a Cdata, locate the end of CDATA
              Index := String_Mng.Locate (Asu_Ts (Text), Util.Cdata_End,
                       Index + Util.Cdata_Start'Length);
              if Index = 0 then
                Util.Error (Ctx.Flow, "Unterminated CDATA section");
              end if;
              -- Head takes the expanded text and the CDATA section
              Head := Head & Asu.Slice (Text, 1,
                                  Index + Util.Cdata_End'Length - 1);
              -- Text is the remaining unexpanded text, fixed
              Asu.Delete (Text, 1, Index + Util.Cdata_End'Length - 1);
              Util.Fix_Text (Ctx, Text, Ref_Xml, Children.Preserve);
              -- And we loop

            else
              -- There is a '<' but not CDATA: prepend it to the tail
              Tail := Asu.Slice (Text, Index, Asu.Length (Text)) & Tail;
              Head := Head & Asu.Slice (Text, 1, Index - 1);
              -- And done (completely)
              exit Cdata_In_Flow;
            end if;
          else
            -- There is no '<' in expanded text
            -- Head takes the expanded text and the initial CDATA
            Head := Head & Text & Tail;
            -- Nothing more to expand => Need to read from flow
            Text := Asu_Null;
          end if;
        end loop Cdata_In_Text;
      else
        -- No expansion
        Head := Text;
      end if;

    end loop Cdata_In_Flow;
    Trace ("Txt expanded text >" & Asu_Ts (Head)
         & "< tail >" & Asu_Ts (Tail) & "<");

    if Head /= Asu_Null then
      -- If there are only sperators, skip them
      if not Util.Is_Separators (Head) then
        -- Notify on father creation if needed
        if not Children.Created then
          Call_Callback (Ctx, False, True, True, Children.Prev_Is_Text);
          Ctx.Level := Ctx.Level + 1;
          Children.Created := True;
        end if;
        -- Fix text to insert
        -- Insert and notify this child
        Tree_Mng.Add_Text (Ctx.Elements.all, Head, Util.Get_Line_No (Ctx.Flow));
        Add_Child (Ctx, Adtd, Children);
        Call_Callback (Ctx, False, True, False, Children.Prev_Is_Text);
        Move_Del (Ctx, False);
        Children.Prev_Is_Text := True;
        Trace ("Txt added text >" & Asu_Ts (Head) & "<");
      end if;
    end if;

    -- Now handle tail if not empty
    if Tail = Asu_Null then
      return;
    end if;
    -- Save current flow
    Util.Push_Flow (Ctx.Flow);
    -- Prepare new string flow, keep file name
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.Same_Line := True;
    Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    Ctx.Flow.Curr_Flow.In_Str := Tail;
    Ctx.Flow.Curr_Flow.In_Stri := 0;
    -- Parse new flow as xml content
    Trace ("Txt switching input to " & Asu_Ts (Tail));
    Parse_Children (Ctx, Adtd, Children, Allow_End => True);
    Trace ("Txt switching back");
    -- Restore flow
    Util.Pop_Flow (Ctx.Flow);

  end Parse_Text;

  -- Parse text or sub-elements of an element (until </)
  -- Children and Is_mixed are set with list of children
  procedure Parse_Children (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Children : access Children_Desc;
                            Allow_End : Boolean := False) is
    Prev_Was_Text : constant Boolean := Children.Prev_Is_Text;
    -- Create parent node with children if needed
    procedure Create (Has_Children : in Boolean) is
    begin
      if not Children.Created then
        -- Creation of element
        Call_Callback (Ctx, False, True, Has_Children, Prev_Was_Text);
        if Has_Children then
          Ctx.Level := Ctx.Level + 1;
        end if;
        Children.Created := True;
      end if;
    end Create;

    Char : Character;
  begin
    -- Try to preserve spaces if current element has this tuning
    Children.Preserve := String_Mng.Locate (
            Tree_Mng.Get_Tuning (Ctx.Elements.all),
                                 Tree_Mng.Xml_Space_Preserve) /= 0;
    if Children.Preserve then
      Trace ("Preserving spaces of the texts of this element");
    end if;

    -- Detect children
    loop
      begin
        Util.Get (Ctx.Flow, Char);
      exception
        when Util.End_Error =>
          if Allow_End then
            -- End the text flow
            return;
          else
            raise;
          end if;
      end;
      if Char = Util.Start then
        Util.Get (Ctx.Flow, Char);
        if Char = Util.Slash then
          if Children.Created then
            -- Element was created
            Ctx.Level := Ctx.Level - 1;
            Call_Callback (Ctx, False, False, False, Children.Prev_Is_Text);
          else
            -- Empty element
            Create (False);
          end if;
          return;
        elsif Char = Util.Directive then
          -- Must be a comment or CDATA
          Create (True);
          Parse_Directive (Ctx, Adtd, Allow_Dtd => False,
                                      Context => Ref_Xml,
                                      Children => Children);
          Children.Prev_Is_Text := False;
        elsif Char = Util.Instruction then
          Create (True);
          Parse_Instruction (Ctx, Adtd, Children);
          Children.Prev_Is_Text := False;
        elsif Char = Util.Start then
          Util.Error (Ctx.Flow, "Unexpected character " & Util.Start);
        else
          -- A new sub-element
          Create (True);
          Util.Unget (Ctx.Flow);
          Parse_Element (Ctx, Adtd, Children, False);
          Children.Prev_Is_Text := False;
        end if;
      else
        -- A text, will stop with a new sub-element or
        --  with stop of current element
        Util.Unget (Ctx.Flow);
        Parse_Text (Ctx, Adtd, Children);
      end if;
    end loop;
  end Parse_Children;

  -- Parse an element (<Name...>)
  procedure Parse_Element (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type;
                           Parent_Children : access Children_Desc;
                           Root : in Boolean) is
    Element_Name, End_Name : Asu_Us;
    Char : Character;
    Line_No : Natural;
    My_Children : aliased Children_Desc;
    use type Asu_Us;
  begin
    My_Children.Prev_Is_Text := Parent_Children.Prev_Is_Text;
    Line_No := Util.Get_Line_No (Ctx.Flow);
    -- Parse name until /, > or a separator
    Util.Parse_Until_Char (Ctx.Flow, "/> ");
    -- Check and store name
    Util.Get_Curr_Str (Ctx.Flow, Element_Name);
    if not Util.Name_Ok (Element_Name) then
      Util.Error (Ctx.Flow, "Invalid element name " & Asu_Ts (Element_Name));
    end if;
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
    -- Add ourself as child of our parent
    Add_Child (Ctx, Adtd, Parent_Children);
    Trace ("Parsing element " & Asu_Ts (Element_Name));
    -- See first significant character after name
    Util.Read (Ctx.Flow, Char);
    if Util.Is_Separator (Char) then
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
      Dtd.Check_Attributes (Ctx, Adtd);
      Check_Children (Ctx, Adtd,  My_Children'Access);
      Call_Callback (Ctx, False, True, False, Parent_Children.Prev_Is_Text);
      Move_Del (Ctx, False);
      Parent_Children.Prev_Is_Text := False;
      Trace ("Parsed element " & Asu_Ts (Element_Name));
      return;
    elsif Char = Util.Stop then
      -- >: parse text and children elements until </
      -- Check attributes first (e.g. xml:space)
      Dtd.Check_Attributes (Ctx, Adtd);
      Trace ("Parsing children of " & Asu_Ts (Element_Name));
      Parse_Children (Ctx, Adtd, My_Children'Access);
      Trace ("Parsed children of " & Asu_Ts (Element_Name));
      -- Check Name matches
      Util.Parse_Until_Char (Ctx.Flow, Util.Stop & "");
      Util.Get_Curr_Str (Ctx.Flow, End_Name);
      if End_Name /= Element_Name then
        Util.Error (Ctx.Flow, "Element name mismatch, expected "
                  & Asu_Ts (Element_Name)
                  & ", got " & Asu_Ts (End_Name));
      end if;
      -- End of this non empty element, check children
      Check_Children (Ctx, Adtd,  My_Children'Access);
      Move_Del (Ctx, False);
      Trace ("Parsed element " & Asu_Ts (Element_Name));
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
    My_Children : aliased Children_Desc;
  begin
    -- Loop until end of file
    Root_Found := False;
    Ctx.Level := 0;
    loop
      -- Get until a significant character, if any
      Util.Skip_Separators (Ctx.Flow);
      begin
        Util.Get (Ctx.Flow, C1);
      exception
        when Util.End_Error =>
          exit;
      end;
      -- Shall be '<'
      if C1 /= Util.Start then
        Util.Error (Ctx.Flow, "Unexpected character " & C1 & " while expecting "
                  & Util.Start & " for root");
      end if;
      Util.Get (Ctx.Flow, C2);
      case C2 is
        when Util.Instruction =>
          Parse_Instruction (Ctx, Adtd, null);
        when Util.Directive =>
          -- Directive : only comment
          Parse_Directive (Ctx, Adtd, Allow_Dtd => False, Children => null,
                                      Context => Ref_Xml);
        when others =>
          Util.Unget (Ctx.Flow);
          if Root_Found then
            Util.Error (Ctx.Flow, "More that one root element found");
          end if;
          Parse_Element (Ctx, Adtd, My_Children'Access, Root => True);
          Root_Found := True;
          -- Insert dummy child for tail
          Tree_Mng.Add_Element (Ctx.Elements.all, Asu_Null,
                                Util.Get_Line_No (Ctx.Flow));
      end case;
    end loop;
    -- One (and only one) root must have been found
    if not Root_Found then
      Util.Error (Ctx.Flow, "No root element found");
    end if;
    -- Deallocate if in callback mode (at least there is the tail)
    if Ctx.Callback /= null then
      -- Deallocate
      Ctx.Elements.Move_Root;
      Move_Del (Ctx, False, True);
    else
      -- Delete Tail if it is empty
      Ctx.Elements.Move_Root;
      Ctx.Elements.Move_Child (Eldest =>False);
      if Ctx.Elements.Children_Number = 0 then
        Ctx.Elements.Delete_Current;
      else
        Ctx.Elements.Move_Root;
      end if;
    end if;
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file");
  end Parse_Root_To_End;

  -- Main parser (entry point)
  procedure Parse_Xml (Ctx : in out Ctx_Type) is
    Adtd : Dtd_Type;
    use type Asu_Us;
  begin
    if Ctx.Flow.Curr_Flow.Is_File then
      File_Mng.Open (Asu_Ts (Ctx.Flow.Curr_Flow.Name),
                     Ctx.Flow.Curr_Flow.File.all);
    end if;
    -- Init Prologue with an empty root
    Tree_Mng.Init_Prologue (Ctx.Prologue.all);
    -- Reset Dtd
    Dtd.Init (Adtd);
    -- Parse prologue, allow Dtd
    Parse_Prologue (Ctx, Adtd, Allow_Dtd => True);
    -- Parse elements
    Parse_Root_To_End (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx);
    -- Clean Dtd before it disapears
    Dtd.Init (Adtd);
    if Ctx.Flow.Curr_Flow.Is_File then
      File_Mng.Close (Ctx.Flow.Curr_Flow.File.all);
    end if;
  end Parse_Xml;

  -- Propagate Dtd convention
  function String_Flow return String is
  begin
    return Dtd.String_Flow;
  end String_Flow;

  -- Parse a standalone Dtd Flow
  procedure Parse_Dtd (Ctx : in out Ctx_Type;
                       Adtd : in out Dtd_Type) is
  begin
    -- If File_Name is a file name, then a Name_Error on it
    --  will be propagated as such
    Dtd.Parse (Ctx, Adtd, Ctx.Flow.Curr_Flow.Name,
               Name_Raise_Parse => False);
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
  end Parse_Prologue;

  -- Parse the elements
  procedure Parse_Elements (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type) is
  begin
    -- Parse root element
    Parse_Root_To_End (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx);
  end Parse_Elements;

  -- Check a Ctx versus its Dtd
  procedure Check (Ctx : in out Ctx_Type) is
    Adtd : Dtd_Type;
    use type Asu_Us;
  begin
    -- Reset Dtd
    Dtd.Init (Adtd);
    Util.Push_Flow (Ctx.Flow);
    -- Parse Dtd
    if Ctx.Dtd_File /= Asu_Null then
      -- Parse alternate Dtd provided by caller
      Dtd.Parse (Ctx, Adtd, Build_Full_Name (Ctx.Dtd_File));
    elsif Ctx.Doctype.Name /= Asu_Null then
      if Ctx.Doctype.File /= Asu_Null then
        -- Parse Dtd file set in DOCTYPE of Xml
        Dtd.Parse (Ctx, Adtd, Build_Full_Name (Ctx.Doctype.File));
      end if;
      if Ctx.Doctype.Int_Def /= Asu_Null then
        -- Parse internal defs
        Ctx.Flow.Curr_Flow.Is_File := False;
        Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
        Ctx.Flow.Curr_Flow.In_Str := Ctx.Doctype.Int_Def;
        Ctx.Flow.Curr_Flow.In_Stri := 0;
        Ctx.Flow.Curr_Flow.Line := Ctx.Doctype.Line_No;
        Ctx.Flow.Curr_Flow.Same_Line := True;
        Dtd.Parse (Ctx, Adtd, Asu_Tus (Dtd.String_Flow));
      end if;
    end if;
    -- Restore flow
    Util.Pop_Flow (Ctx.Flow);
    -- Xml declaration must have a version, which might not be the case
    --  if Ctx comes from Xml_Parser.Generator
    Set_Default_Xml (Ctx);
    -- There must be one root
    if Tree_Mng.Is_Empty (Ctx.Elements.all) then
      Util.Error (Ctx.Flow, "No root element found");
    end if;
    -- Check all elements
    Dtd.Check_Subtree (Ctx, Adtd);
    -- Perform final checks versus dtd
    Dtd.Final_Check (Ctx);
    -- Clean Dtd before it disapears
    Dtd.Init (Adtd);
  end Check;

end Parse_Mng;

