with Ada.Characters.Latin_1;
with Lower_Str, Upper_Str, Mixed_Str, String_Mng, As.U.Utils;
separate (Xml_Parser)

package body Parse_Mng  is

  -- Context where a reference to entity is resolved
  -- In Xml content, in attribute value, in entity value,
  --  in dtd (outside or inside markup or inside content description)
  type Context_List is (Ref_Xml, Ref_Attribute,
                        Ref_Entity, Ref_Dtd, Ref_Dtd_Mark, Ref_Dtd_Content);
  -- Is a context in dtd
  function In_Dtd (Context : Context_List) return Boolean is
  begin
    return Context >= Ref_Entity;
  end In_Dtd;

  -- Expand the content of an external parsed entity
  -- Non recursive
  procedure Expand_External_Entity (Ctx : in out Ctx_Type;
                                    Adtd : in out Dtd_Type;
                                    Name, Uri : in As.U.Asu_Us;
                                    Text : out As.U.Asu_Us);

  -- Entity management
  package Entity_Mng is
    -- Initialise with default entities
    procedure Initialise (
          The_Entities : in out Entity_List_Mng.Unique_List_Type);
    -- Store an entity
    procedure Add (The_Entities : in out Entity_List_Mng.Unique_List_Type;
                   Name, Value : in As.U.Asu_Us;
                   Parameter : in Boolean;
                   Internal : in Boolean;
                   Intern_Dtd : in Boolean;
                   Parsed : in Boolean);
    -- Check if an entity exists. May raise Invalid_Char_Code
    procedure Exists (The_Entities : in out Entity_List_Mng.Unique_List_Type;
                      Name : in As.U.Asu_Us;
                      Parameter : in Boolean;
                      Found : out Boolean);

    -- Get value of an entity. Raises Entity_Not_Found if none
    -- May raise Invalid_Char_Code
    procedure Get (Ctx : in out Ctx_Type;
                   Dtd : in out Dtd_Type;
                   Context : in Context_List;
                   Name : in As.U.Asu_Us;
                   Parameter : in Boolean;
                   Got : out As.U.Asu_Us);
    Invalid_Char_Code : exception;
    Entity_Not_Found : exception;
    Entity_Forbidden : exception;
    Entity_Standalone : exception;
  end Entity_Mng;

  -- Parsing utilities
  package Util is

    ------------------
    -- Syntax check --
    ------------------
    -- Check if char is a letter
    function Is_Valid_Encoding (Name : As.U.Asu_Us) return Boolean;

    -- Check that Name is valid
    function Name_Ok (Name : As.U.Asu_Us;
                      Allow_Token : Boolean := False) return Boolean;
    -- Check that Str defines valid names separated by Seps
    function Names_Ok (Str : As.U.Asu_Us;
                       Seps : String;
                       Allow_Token : Boolean := False) return Boolean;
    -- Report an error, raises Parsing_Error.
    procedure Error (Flow : in out Flow_Type;
                     Msg : in String; Line_No : in Natural := 0);
    -- Report a warning
    procedure Warning (Ctx : in out Ctx_Type;
                       Msg  : in String; Line_No : in Natural := 0);

    ----------------------
    -- Input characters --
    ----------------------
    -- Autodetect encoding family
    -- Sets the encoding of flow Utf16xx, or to Utf8 for any
    -- byte-oriented encoding
    subtype Detected_Encod_List is Encod_List range Utf8 .. Utf16_Be;
    procedure Guess_Encoding (Flow : in out Flow_Type);

    -- Load a character encoding map
    procedure Load_Map (Flow : in out Flow_Type;
                        Name : in String);

    -- Get current line number
    function Get_Line_No (Flow : Flow_Type) return Natural;

    -- Start recording
    procedure Start_Recording (Flow : in out Flow_Type);
    -- Stop recoding and retrieve recorded data
    procedure Stop_Recording (Flow : in out Flow_Type;
                              Recorded : out As.U.Asu_Us);

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
    function Is_Separators (Str : As.U.Asu_Us) return Boolean;
    -- Skip separators until a significant char (not separator); got
    procedure Skip_Separators (Flow : in out Flow_Type);
    -- Current significant string, loaded by Parse_Until_xxx
    procedure Get_Curr_Str (Flow : in out Flow_Type;
                            Str : out As.U.Asu_Us;
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
                           Text : in out As.U.Asu_Us;
                           Context : in Context_List);
    -- Expand text (expand vars) returns the index of localized '<'
    --  if any
    procedure Expand_Text (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out As.U.Asu_Us;
                           Context : in Context_List;
                           Start_Index : out Natural);
    -- Expand a name if it is a (parameter) entity reference
    -- Error if Text contains % or & but not at beginning
    -- Error if Text contains ; but not at end
    -- Does nothing if not an entity reference
    procedure Expand_Name (Ctx : in out Ctx_Type;
                           Dtd : in out Dtd_Type;
                           Text : in out As.U.Asu_Us;
                           Context : in Context_List);
    -- Fix text: replace any separator by a space
    procedure Normalize (Text : in out As.U.Asu_Us);
    -- Replace any sequence of spaces by a space
    -- Remove Leading and trailing spaces
    procedure Normalize_Spaces (Text : in out As.U.Asu_Us);
    -- Remove from Text the separators around Seps
    procedure Remove_Separators (Text : in out As.U.Asu_Us;
                                 Seps : in String);
    -- Remove (no expanded) entities from text
    procedure Remove_Entities (Text : in out As.U.Asu_Us);

    -- Push current flow
    procedure Push_Flow (Flow : in out Flow_Type);
    -- Pop and restore current flow
    procedure Pop_Flow (Flow : in out Flow_Type);

  end Util;

  package body Entity_Mng is separate;
  package body Util is separate;

  -- Resolve an URI:
  -- if not "://" -> Build_Full_Name (Uri), return File
  -- if "file://" -> Build_Full_Name (Tail), return File
  -- if "http://" -> Fetch content, return String
  -- Else Error: unsupported URI scheme
  procedure Resolve_Uri (Ctx : in out Ctx_Type;
                         Uri : in As.U.Asu_Us;
                         Is_File : out Boolean;
                         Content : out As.U.Asu_Us) is separate;

  -- Descriptor of list of children found
  type Children_Desc is record
    -- Constraint when parsing children
    Space_Allowed : Boolean := True;
    -- Result of parsing of children
    Is_Empty : Boolean := True;
    Children : As.U.Asu_Us;
    Has_Text : Boolean := False;
    Is_Mixed : Boolean := False;
    -- Token passed along the parsing of children
    Father : As.U.Asu_Us;
    Created : Boolean := False;
    In_Mixed : Boolean := False;
    Preserve : Boolean := False;
    First_Child : Boolean := True;
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
                         Value : out As.U.Asu_Us) is
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
    -- Normalize attribute
    if Ctx.Expand and then Ctx.Normalize and then Context = Ref_Attribute then
      Util.Normalize (Value);
    end if;
    -- Expand entities
    Util.Expand_Vars (Ctx, Adtd, Value, Context);
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
                     File_Name : in As.U.Asu_Us;
                     Name_Raise_Parse : in Boolean := True);
    -- Perform final checks after DTD parsing: unparsed entities v.s. notations
    procedure Final_Dtd_Check (Ctx  : in out Ctx_Type; Adtd : in out Dtd_Type);
    -- Check attributes of current element of the tree
    procedure Check_Attributes (Ctx  : in out Ctx_Type;
                                Adtd : in out Dtd_Type);
    -- Is this element defined as Mixed
    procedure Is_Mixed (Adtd : in out Dtd_Type;
                        Elt  : in As.U.Asu_Us;
                        Yes  : out Boolean);
    -- Is this element defined in internal dtd or else has not Content def
    procedure Can_Have_Spaces (Adtd : in out Dtd_Type;
                               Elt  : in As.U.Asu_Us;
                               Yes  : out Boolean);
    -- Is this attribute of this element CDATA
    procedure Is_Cdata (Adtd      : in out Dtd_Type;
                        Elt, Attr : in As.U.Asu_Us;
                        Yes       : out Boolean);
    -- Has this element the xml:spaces=preserve
    function Has_Preserve (Ctx : Ctx_Type; Elt  : As.U.Asu_Us) return Boolean;

    -- Add current element to list of children
    procedure Add_Current_Element (List : in out As.U.Asu_Us;
                                   Name : in As.U.Asu_Us);
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
                              Of_Xml : in Boolean;
                              Elt_Name : in As.U.Asu_Us := As.U.Asu_Null) is
    Attribute_Name, Attribute_Value, Unnormalized : As.U.Asu_Us;
    Attribute_Index : Natural;
    Char : Character;
    Line_No : Natural;
    Attr_Exists, Attr_Cdata : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Loop on several attributes
    loop
      -- Parse name
      Line_No := Util.Get_Line_No (Ctx.Flow);
      -- Read until a = (looks like a valid attr definition)
      --  or until > or < (no '=' so invalid definition)
      Util.Parse_Until_Char (Ctx.Flow, Util.Equal & Util.Stop & Util.Start
                                     & Util.Slash & Util.Space);
      Util.Get_Curr_Str (Ctx.Flow, Attribute_Name);
      Util.Read (Ctx.Flow, Char);
      if Util.Is_Separator (Char) then
        Util.Get (Ctx.Flow, Char);
      end if;
      if Char /= Util.Equal
      or else not Util.Name_Ok (Attribute_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute name "
                  & Attribute_Name.Image);
      end if;
      -- Attribute name must be unique
      if Of_Xml then
        Tree_Mng.Find_Xml_Attribute (
           Ctx.Prologue.all,
           Attribute_Name,
           Attribute_Index, Attribute_Value);
        if Attribute_Index /= 0 then
          Util.Error (Ctx.Flow, "Attribute " & Attribute_Name.Image
                    & " already defined for xml");
        end if;
        Attr_Exists := False;
      else
        Tree_Mng.Attribute_Exists (Ctx.Elements.all,
                  Attribute_Name, Attr_Exists);
        if Attr_Exists then
          -- Elt_Name is always set when not Of_Xml
          Util.Error (Ctx.Flow, "Attribute " & Attribute_Name.Image
                    & " already defined for element " & Elt_Name.Image);
        end if;
      end if;

      -- Parse value
      Util.Skip_Separators (Ctx.Flow);
      Parse_Value (Ctx, Adtd, Ref_Attribute, Attribute_Value);
      if Of_Xml then
        Tree_Mng.Add_Xml_Attribute (Ctx.Prologue.all,
                  Attribute_Name, Attribute_Value, Line_No);
      else
        -- Keep first definition
        -- If expand, then Normalize separators of non CDATA attributes
        Dtd.Is_Cdata (Adtd, Elt_Name, Attribute_Name, Attr_Cdata);
        if Ctx.Expand and then Ctx.Normalize and then not Attr_Cdata then
          Trace ("Attribute " & Attribute_Name.Image & " is not CDATA");
          Unnormalized := Attribute_Value;
          Util.Normalize_Spaces (Attribute_Value);
          if Ctx.Standalone and then Unnormalized /= Attribute_Value then
            Util.Error (Ctx.Flow,
              "Normalization of attribute " & Attribute_Name.Image
            & " in standalone document is impacted by not CDATA declaration");
          end if;
        end if;
        Tree_Mng.Add_Attribute (Ctx.Elements.all,
                  Attribute_Name, Attribute_Value, Line_No);
        if Attribute_Name.Image = Tree_Mng.Xml_Space
        and then Attribute_Value.Image = Tree_Mng.Preserve then
          Tree_Mng.Add_Tuning (Ctx.Elements.all, Tree_Mng.Xml_Space_Preserve);
          Trace ("Added tuning " & Tree_Mng.Xml_Space_Preserve);
        end if;
      end if;
      Trace ("Parsed attribute " & Attribute_Name.Image
           & ", " & Attribute_Value.Image);
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
    Attribute_Value : As.U.Asu_Us;
    Attribute_Index, Next_Index : Natural;
    Nb_Attrs_Set : Natural;
  begin
    Next_Index := 1;
    -- In XML: Version [ Encode ] [ Standalone ]
    -- In Dtd: [ Version ] Encode
    -- Check Version
    Tree_Mng.Find_Xml_Attribute (Ctx.Prologue.all,
           As.U.Tus ("version"), Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (Of_Xml and then Attribute_Index = 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml version attribute");
    end if;
    if Attribute_Index /= 0 then
      declare
        Vers : constant String := Attribute_Value.Image;
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
           As.U.Tus ("encoding"), Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (not Of_Xml and then Attribute_Index = 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml encoding attribute");
    end if;
    if Attribute_Index /= 0 then
      -- Check encoding value, must be valid name
      -- and also starting with letter and without ":"
      if not Util.Is_Valid_Encoding (Attribute_Value) then
        Util.Error (Ctx.Flow, "Invalid encoding name");
      end if;
      -- Check this is "UTF-8", "UTF-16" or "ISO-8859-1" and that it matches
      --  the guessed encoding
      if Upper_Str (Attribute_Value.Image) = "UTF-8" then
        if Ctx.Flow.Curr_Flow.Encod /= Utf8 then
          Util.Error (Ctx.Flow, "Encoding " & Attribute_Value.Image
                    & " differs from autodetected "
                    & Ctx.Flow.Curr_Flow.Encod'Img);
        end if;
      elsif Upper_Str (Attribute_Value.Image) = "UTF-16" then
        if Ctx.Flow.Curr_Flow.Encod /= Utf16_Le
        and then Ctx.Flow.Curr_Flow.Encod /= Utf16_Be then
          Util.Error (Ctx.Flow, "Encoding " & Attribute_Value.Image
                    & " differs from autodetected "
                    & Ctx.Flow.Curr_Flow.Encod'Img);
        end if;
      elsif Upper_Str (Attribute_Value.Image) = "ISO-8859-1" then
        -- Guessing set encoding to Utf8 for any byte-based format
        if Ctx.Flow.Curr_Flow.Encod /= Utf8 then
          Util.Error (Ctx.Flow, "Encoding " & Attribute_Value.Image
                    & " differs from autodetected "
                    & Ctx.Flow.Curr_Flow.Encod'Img);
        else
          Ctx.Flow.Curr_Flow.Encod := Latin1;
        end if;
      elsif Ctx.Flow.Curr_Flow.Encod = Utf8 then
        -- Try to load a map of encoding
        Util.Load_Map (Ctx.Flow,
                       Upper_Str (Attribute_Value.Image));
      else
        Util.Error (Ctx.Flow, "Inconsistent encoding (UTF-16 detected)");
      end if;
      Next_Index := Attribute_Index + 1;
    end if;

    -- Check Standalone
    Tree_Mng.Find_Xml_Attribute (Ctx.Prologue.all,
           As.U.Tus ("standalone"), Attribute_Index, Attribute_Value);
    if (Attribute_Index /= 0 and then Attribute_Index /= Next_Index)
    or else (not Of_Xml and then Attribute_Index /= 0) then
      Util.Error (Ctx.Flow, "Missing or invalid xml standalone attribute");
    end if;
    if Attribute_Index /= 0 then
      -- Check standalone value, must be "yes" or "no"
      -- Ctx.Standalone is False by default
      if Attribute_Value.Image = "yes" then
        Ctx.Standalone := True;
      elsif Attribute_Value.Image = "no" then
        Ctx.Standalone := False;
      else
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
                           Stage : Stage_List;
                           Creation : in Boolean;
                           Has_Children : in Boolean := False;
                           In_Mixed : in Boolean := False) is
    Upd : Node_Update;
  begin
    if Ctx.Callback = null then
      return;
    end if;
    if Stage = Prologue then
      Tree_Mng.Build_Update (Ctx.Prologue.all, Upd, Creation);
    else
      Tree_Mng.Build_Update (Ctx.Elements.all, Upd, Creation);
    end if;
    Upd.Stage := Stage;
    Upd.Has_Children := Has_Children;
    Upd.In_Mixed := In_Mixed;
    Upd.Level := Ctx.Level;
    if not Creation then
      Upd.Line_No := Util.Get_Line_No(Ctx.Flow);
    end if;
    begin
      Ctx.Callback (Ctx, Upd);
    exception
      when Error:others =>
        Trace ("Callback raised " & Ada.Exceptions.Exception_Name (Error));
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
  begin
    if not Adtd.Set or else Children = null then
      -- In Prologue (Pi or directive) => no check
      -- No dtd => no check
      return;
    end if;
    Children.Is_Empty := False;
    Ctx.Elements.Read (Cell);
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

  -- Expand the content of an external parsed entity
  procedure Expand_External_Entity (Ctx  : in out Ctx_Type;
                                    Adtd : in out Dtd_Type;
                                    Name, Uri : in As.U.Asu_Us;
                                    Text : out As.U.Asu_Us) is
    Full_File : As.U.Asu_Us;
    Ok : Boolean;
    Char : Character;
    Dummy : My_Tree_Cell;
    Is_Recorded : Boolean;
    Is_File : Boolean;
    use type As.U.Asu_Us;
  begin
    Trace ("Ext expanding external entity " & Name.Image & " with URI "
         & Uri.Image);
    if Uri.Is_Null then
      Util.Error (Ctx.Flow, "Invalid external entity URI " & Uri.Image
                          & " for entity " & Name.Image & ".");
    end if;
    Util.Push_Flow (Ctx.Flow);

    -- Init Flow
    -- Check validity of Uri
    Full_File := Build_Full_Name (Uri, Ctx.Flow.Curr_Flow.Name);
    if Full_File.Image = Dtd.String_Flow
    or else Full_File = Dtd.Internal_Flow then
      Util.Error (Ctx.Flow, "Invalid external entity file name");
    end if;
    -- Expand URI
    Resolve_Uri (Ctx, Uri, Is_File, Full_File);
    if not Is_File then
      -- Full_File is the content of the Dtd fetched (by http)
      Ctx.Flow.Curr_Flow.File := null;
      Ctx.Flow.Curr_Flow.In_Str := Full_File;
      Ctx.Flow.Curr_Flow.In_Stri := 0;
      Trace ("Ext parsing http result");
    else
      Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
      Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
      File_Mng.Open (Full_File.Image, Ctx.Flow.Curr_Flow.File.all);
      Trace ("Ext parsing file");
    end if;
    Ctx.Flow.Curr_Flow.Is_File := Is_File;
    Ctx.Flow.Curr_Flow.Kind := Ext_Flow;
    Ctx.Flow.Curr_Flow.Name := Uri;
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;

    -- Suspend recording
    Is_Recorded := Ctx.Flow.Recording;
    Ctx.Flow.Recording := False;

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
      if Ctx.Prologue.Is_Empty then
        Ctx.Prologue.Insert_Father (Dummy);
      else
        Ctx.Prologue.Insert_Child (Dummy, False);
      end if;
      Parse_Attributes (Ctx, Adtd, Of_Xml => True);
      Check_Xml_Attributes (Ctx, False);
      Ctx.Prologue.Delete_Tree;
      Trace ("Ext parsed xml instruction");
      Util.Skip_Separators (Ctx.Flow);
    end if;

    -- Load content in string
    Util.Parse_Until_End (Ctx.Flow);
    Util.Get_Curr_Str (Ctx.Flow, Text);
    Trace ("Ext expanded as >" & Text.Image & "<");

    -- Done: restore flow
    if Is_File then
      File_Mng.Close (Ctx.Flow.Curr_Flow.File.all);
    end if;
    Util.Pop_Flow (Ctx.Flow);
    Ctx.Flow.Recording := Is_Recorded;
  exception
    when File_Error =>
      Util.Error (Ctx.Flow, "Cannot open external entity file "
                         & Full_File.Image);
  end Expand_External_Entity;

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
    -- Callback creation if Xml has been created here
    if not Ok then
      -- In prologue, Creation of the XML directive
      Call_Callback (Ctx, Prologue, True);
      Ctx.Level := 1;
    end if;
  end Check_Xml;

  -- Parse an instruction (<?xxx?>)
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type;
                               Children : access Children_Desc) is
    Char : Character;
    Name, Value : As.U.Asu_Us;
    Ok : Boolean;
    Str_Xml : String (1 .. 5);
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
        -- In prologue, Creation of the XML directive
        Call_Callback (Ctx, Prologue, True);
        Ctx.Level := 1;
        Trace ("Parsed xml declaration");
        return;
      else
        -- A PI in prologue => insert Xml if it is missing
        Check_Xml (Ctx);
      end if;
    end if;

    -- Xml not allowed in prologue of elements, see if "xml?>" or "xml "
    Util.Get (Ctx.Flow, Str_Xml);
    Name := As.U.Tus (Str_Xml);
    Util.Normalize (Name);
    Str_Xml := Name.Image;
    if Lower_Str (Str_Xml) = "xml" & Util.Instruction & Util.Stop
    or else Lower_Str (Str_Xml(1 .. 4)) = "xml" & Util.Space then
      Util.Error (Ctx.Flow, "Invalid processing instruction");
    else
      -- OK, go on
      Util.Unget (Ctx.Flow, Str_Xml'Length);
    end if;

    -- Parse instruction until ? or separator
    Util.Parse_Until_Char (Ctx.Flow, Util.Instruction & Util.Space);
    Util.Get_Curr_Str (Ctx.Flow, Name);
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid processing instruction name"
               & Name.Image);
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
      Value.Delete (Value.Length - 1, Value.Length);
    end if;

    -- Add node
    Trace ("Parsed <?" & Name.Image & " " & Value.Image & "?>");
    if In_Prologue then
      -- No element => in prologue
      Tree_Mng.Move_Root (Ctx.Prologue.all);
      Tree_Mng.Add_Pi (Ctx.Prologue.all, Name, Value,
                       Util.Get_Line_No(Ctx.Flow));
      -- In_Prologue, Creation of the PI
      Call_Callback (Ctx, Prologue, True);
    else
      Tree_Mng.Add_Pi (Ctx.Elements.all, Name, Value,
                       Util.Get_Line_No(Ctx.Flow));
      -- Add this child
      if Children /= null then
        -- In Elements, Creation of the PI
        Call_Callback (Ctx, Elements, True,
                       In_Mixed => Children.Is_Mixed);
        Add_Child (Ctx, Adtd, Children);
      else
        -- In tail, Creation of the PI
        Call_Callback (Ctx, Tail, True);
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
    Doctype_Name, Doctype_File, Full_File : As.U.Asu_Us;
    Ok : Boolean;
    Char : Character;
    Len : Natural;
    Is_File : Boolean;
  begin
    -- Only one DOCTYPE allowed
    if not Ctx.Doctype.Name.Is_Null then
      Util.Error (Ctx.Flow, "Invalid second DOCTYPE directive");
    end if;
    -- Parse and check name
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & Util.Stop & '[');
    Util.Unget (Ctx.Flow);
    Util.Skip_Separators (Ctx.Flow);
    Util.Get_Curr_Str (Ctx.Flow, Doctype_Name);
    if not Util.Name_Ok (Doctype_Name) then
      Util.Error (Ctx.Flow, "Invalid DOCTYPE name " & Doctype_Name.Image);
    end if;
    Trace ("Parsing doctype " & Doctype_Name.Image);
    Ctx.Doctype.Line_No := Util.Get_Line_No (Ctx.Flow);
    Ctx.Doctype.Name := Doctype_Name;
    -- Insert an empty text in prologue
    Tree_Mng.Move_Root (Ctx.Prologue.all);
    Tree_Mng.Add_Text (Ctx.Prologue.all, As.U.Asu_Null,
                       Util.Get_Line_No (Ctx.Flow));
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
      and then Ctx.Dtd_File.Is_Null
      and then not Doctype_File.Is_Null then
        -- Parse dtd file of doctype directive if no alternate file
        Util.Push_Flow (Ctx.Flow);
        -- Check validity of dtd file
        Full_File := Build_Full_Name (Doctype_File, Ctx.Flow.Curr_Flow.Name);
        if Full_File.Image = Dtd.String_Flow
        or else Full_File.Image = Dtd.Internal_Flow then
          Util.Error (Ctx.Flow, "Invalid Dtd file name");
        end if;
        -- Expand URI
        Resolve_Uri (Ctx, Doctype_File, Is_File, Full_File);
        if not Is_File then
          -- Full_File is the content of the Dtd fetched (by http)
          Ctx.Flow.Curr_Flow.Name := Doctype_File;
          Ctx.Flow.Curr_Flow.Line := 1;
          Ctx.Flow.Curr_Flow.Same_Line := False;
          Ctx.Flow.Curr_Flow.In_Str := Full_File;
          Ctx.Flow.Curr_Flow.In_Stri := 0;
          Full_File := As.U.Tus (Dtd.String_Flow);
          Trace ("Parsing http dtd");
        end if;
        Dtd.Parse (Ctx, Adtd, Full_File);
        Util.Pop_Flow (Ctx.Flow);
      end if;
      Ctx.Doctype.File := Doctype_File;
    end if;
    -- Now see if there is an internal definition section
    Util.Get (Ctx.Flow, Char);
    if Char = '[' then
      -- Internal definition, record the parsing and copy it in Ctx
      Util.Start_Recording (Ctx.Flow);
      Dtd.Parse (Ctx, Adtd, As.U.Tus (Dtd.Internal_Flow));
      Util.Stop_Recording (Ctx.Flow, Ctx.Doctype.Int_Def);
      -- Remove last ']'
      Len := Ctx.Doctype.Int_Def.Length;
      Ctx.Doctype.Int_Def.Delete (Len, Len);
      if not Ctx.Dtd_File.Is_Null then
        -- This Dtd internal definition is overriden by an alternate file
        Trace ("Dtd internal def overwritten by external");
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
    if not Ctx.Use_Dtd or else not Ctx.Expand then
      -- Reset dtd info
      Trace ("Dtd reset cause not used or no expand");
      Dtd.Init (Adtd);
    end if;
    if Ctx.Expand or else not Ctx.Use_Dtd then
      -- Keep the lists of elements in which dtd sets to preserve spaces
      --  only when not Ctx.Expand
      Ctx.Preserved.Set_Null;
    end if;
    -- In prologue, Creation of the Doctype
    Call_Callback (Ctx, Prologue, True);
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
    Comment : As.U.Asu_Us;
  begin

    -- Comment?
    Util.Try (Ctx.Flow, Util.Comment, Ok);
    if Ok then
      -- "<!--", a comment, skip util "-->"
      Util.Parse_Until_Str (Ctx.Flow, "--" & Util.Stop);
      -- Check that no "--" within comment
      Util.Get_Curr_Str (Ctx.Flow, Comment);
      Index := Comment.Locate ("--");
      if Index < Comment.Length - 2 then
        Util.Error (Ctx.Flow, "Invalid ""--"" in comment");
      end if;
      -- Remove tailing "-->"
      Comment.Delete (Index, Index + 2);
      -- Add node
      if not In_Dtd (Context) and then Ctx.Parse_Comments then
        if Tree_Mng.Is_Empty (Ctx.Elements.all) then
          -- No element => in prologue
          Tree_Mng.Move_Root (Ctx.Prologue.all);
          Tree_Mng.Add_Comment (Ctx.Prologue.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
          -- In prologue, creation of the comment
          Call_Callback (Ctx, Prologue, True);
        else
          Tree_Mng.Add_Comment (Ctx.Elements.all, Comment,
                                Util.Get_Line_No (Ctx.Flow));
          -- Add this child
          if Children /= null then
            -- In elements, creation of the comment
            Call_Callback (Ctx, Elements, True,
                           In_Mixed => Children.Is_Mixed);
            Add_Child (Ctx, Adtd, Children);
          else
            -- In tail, creation of the comment
            Call_Callback (Ctx, Tail, True);
          end if;
        end if;
        Move_Del (Ctx, Tree_Mng.Is_Empty (Ctx.Elements.all));
        Trace ("Parsed comment " & Comment.Image);
      else
        -- In Dtd or comments not to be parsed
        Trace ("Skipped comment <!--" & Comment.Image & "-->");
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
         & Comment.Image & Util.Stop);
  exception
    when Util.End_Error =>
      Util.Error (Ctx.Flow, "Unexpected end of file while parsing directive");
  end Parse_Directive;

  -- Parse the prologue
  procedure Parse_Prologue (Ctx : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Allow_Dtd : in Boolean) is
    C1, C2 : Character;
  begin
    -- Autodetect encoding and check
    if Ctx.Flow.Curr_Flow.Is_File then
      Util.Guess_Encoding (Ctx.Flow);
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
    and then not Ctx.Dtd_File.Is_Null then
      Util.Push_Flow (Ctx.Flow);
      -- Parse dtd file provided instead of doctype directive
      Dtd.Parse (Ctx, Adtd, Build_Full_Name (Ctx.Dtd_File,
                                             Ctx.Flow.Curr_Flow.Name));
      Util.Pop_Flow (Ctx.Flow);
    end if;
    -- Perform final checks on Dtd (unparsed entities v.s. notations)
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
    -- This list is either empty (<node></node>) or contains
    --  Text, Cdata, Text...
    Texts : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
    Text, Tmp_Text, Cdata, Tail : As.U.Asu_Us;
    Start_Index, Index : Natural;
    Moved, Cdata_Found, Last_Is_Text, Normalize : Boolean;
    Next_Char : Character;

    -- Depending on Last_Is_Text, append Txt to last or insert it
    procedure Insert_Text (Txt : in As.U.Asu_Us) is
      Tmp : As.U.Asu_Us;
    begin
      if Last_Is_Text then
        Texts.Read (Tmp, As.U.Utils.Asu_Dyn_List_Mng.Current);
        Tmp.Append (Txt);
        Texts.Modify (Tmp, As.U.Utils.Asu_Dyn_List_Mng.Current);
      else
        Texts.Insert (Txt);
      end if;
    end Insert_Text;

    use type As.U.Asu_Us;
  begin
    -- Concatenate blocks of expanded text and CDATA sections
    -- First should be text (even empty)
    Last_Is_Text := False;

    -- Loop as long a flow does not contain and does not expand
    --  to '<', skipping "<![CDATA["
    -- Save expanded text and CDATA in Head, and save '<' and following in Tail
    Read_Flow:
    loop

      -- Parse until '<' or End of flow
      --  save Text, and save Cdata in Tail
      begin
        Util.Parse_Until_Char (Ctx.Flow, Util.Start & "");
        Util.Unget (Ctx.Flow);
        -- Save Text
        Util.Get_Curr_Str (Ctx.Flow, Text);
      exception
        when Util.End_Error =>
          -- End of flow, save text
          Util.Get_Curr_Str (Ctx.Flow, Text);
          if Text.Is_Null then
            -- End of flow and no text
            exit Read_Flow;
          end if;
      end;
      Trace ("Txt - Got text >" & Text.Image & "<");

      -- Loop as long as expansion does not generate a '<'
      --  or as long as this is a "<![CDATA["
      -- At the end, Texts contains expanded text and CDATA
      --  with indication of last_Is_Text
      --  and Tail contains non expanded flow
      Cdata_In_Text:
      loop
        -- Done when no more text to expand
        exit Cdata_In_Text when Text.Is_Null;
        if Ctx.Expand then
          -- Expand Text and check if it generated a '<'
          Util.Expand_Text (Ctx, Adtd, Text, Ref_Xml, Index);
          Tmp_Text := Text;
        else
          -- Handle full text
          Index := 0;
          -- See if text without entities is empty
          Tmp_Text := Text;
          Util.Remove_Entities (Tmp_Text);
        end if;
        if not Tmp_Text.Is_Null then
          -- Expansion or text without entities lead to something => not empty
          Children.Is_Empty := False;
        end if;
        if not Children.Space_Allowed then
          -- No space allowed within this element
          -- doc is standalone, element has content and is not internal
          Tmp_Text := Text;
          Util.Normalize (Tmp_Text);
          if String_Mng.Locate (Tmp_Text.Image, Util.Space & "") /= 0 then
            Util.Error (Ctx.Flow, "element with content, defined in external"
               & " markup declaration has spaces in standalone document");
          end if;
        end if;

        if Index /= 0 then
          if Index + Util.Cdata_Start'Length - 1 <= Text.Length
          and then Text.Slice (Index, Index + Util.Cdata_Start'Length - 1)
                   = Util.Cdata_Start then

            -- Expansion stopped but this is a Cdata, locate the end of CDATA
            Start_Index := Index;
            Index := String_Mng.Locate (Text.Image, Util.Cdata_End,
                       Index + Util.Cdata_Start'Length);
            if Index = 0 then
              Util.Error (Ctx.Flow, "Unterminated CDATA section");
            end if;
            -- Extract Cdata and apply policy
            Cdata := As.U.Uslice (Text,
                     Start_Index +  Util.Cdata_Start'Length,
                     Index - 1);
            case Ctx.Cdata_Policy is
              when Keep_Cdata_Section =>
                Cdata := Util.Cdata_Start & Cdata & Util.Cdata_End;
              when Remove_Cdata_Markers =>
                null;
              when Remove_Cdata_Section =>
                Cdata.Set_Null;
            end case;
            -- Insert the expanded text and the CDATA section
            Insert_Text (Text.Uslice (1, Start_Index - 1));
            Trace ("Txt -- appended text >" & Text.Slice (1, Start_Index - 1)
                 & "<");
            Texts.Insert (Cdata);
            Trace ("Txt -- appended CDATA >" & Cdata.Image & "<");
            -- Text is the remaining unexpanded text, fixed
            Text.Delete (1, Index + Util.Cdata_End'Length - 1);
            Last_Is_Text := False;
            -- And we loop

          else
            -- There is a '<' but not CDATA: prepend it to the tail
            Tail := Text.Slice (Index, Text.Length) & Tail;
            Insert_Text (Text.Uslice (1, Index - 1));
            Trace ("Txt -- appended text >" & Text.Slice (1, Index - 1) & "<");
            Last_Is_Text := True;
            Trace ("Txt -- tail >" & Tail.Image & "<");
            -- And done (completely)
            exit Read_Flow;
          end if;
        else
         -- There is no '<' in expanded text
          -- Store the expanded text
          Insert_Text (Text);
          Trace ("Txt -- appended text >" & Text.Image & "<");
          Last_Is_Text := True;
          -- Nothing more to expand => Need to read from flow
          exit Cdata_In_Text;
        end if;
      end loop Cdata_In_Text;

      -- Check "<![CDATA[" in flow
      Util.Try (Ctx.Flow, Util.Cdata_Start, Cdata_Found);
      if Cdata_Found then
        -- Get Cdata (without markers)
        begin
          Util.Parse_Until_Str (Ctx.Flow, Util.Cdata_End);
        exception
          when Util.End_Error =>
            Util.Error (Ctx.Flow, "Unterminated CDATA section");
        end;
        Util.Get_Curr_Str (Ctx.Flow, Cdata);
        Cdata.Delete (Cdata.Length  - Util.Cdata_End'Length + 1,
                      Cdata.Length);
        Trace ("Txt - Got CDATA >" & Cdata.Image & "<");
        -- Apply policy
        case Ctx.Cdata_Policy is
          when Keep_Cdata_Section =>
            Cdata := Util.Cdata_Start & Cdata & Util.Cdata_End;
          when Remove_Cdata_Markers =>
            null;
          when Remove_Cdata_Section =>
            Cdata.Set_Null;
        end case;
        if not Last_Is_Text then
          Texts.Insert (As.U.Asu_Null);
        end if;
        Texts.Insert (Cdata);
        Trace ("Txt - appended CDATA >" & Cdata.Image & "<");
        Last_Is_Text := False;
      else
        -- Text is got and is followed by '<' but not CDATA
        --  => end of this text char
        exit Read_Flow;
      end if;

    end loop Read_Flow;

    -- See if this is the end of father or beginning of a brother
    if Children.First_Child then
      if Tail.Is_Null then
        -- Scan input flow
        begin
          Util.Get (Ctx.Flow, Next_Char);
          Util.Get (Ctx.Flow, Next_Char);
          Util.Unget (Ctx.Flow, 2);
        exception
          when Util.End_Error =>
            Next_Char := Util.Start;
        end;
      elsif Tail.Length >= 2 then
        Next_Char := Tail.Element (2);
      else
        -- Tail too short!
        Next_Char := Util.Start;
      end if;
      if Next_Char /= Util.Slash and then Next_Char /= Util.Directive
      and then Next_Char /= Util.Instruction then
        -- This is the beginning of a brother
        Trace ("Txt not only text child");
        Children.First_Child := False;
      end if;
    end if;

    -- See if we normalize this text
    Normalize := Ctx.Expand and then Ctx.Normalize
                 and then not Children.Preserve
                 and then not Children.First_Child;

    -- Now build the text from the list
    Text.Set_Null;
    if not Texts.Is_Empty then
      Texts.Rewind;
      loop
        -- Read Text
        Texts.Read (Tmp_Text, Moved => Moved);
        if Normalize then
          -- Normalize and remove leading and trailing spaces
          Util.Normalize (Tmp_Text);
          Tmp_Text := As.U.Tus (
              String_Mng.Strip (Tmp_Text.Image, String_Mng.Both));
        end if;
        Text.Append (Tmp_Text);
        if Moved then
          Texts.Read (Tmp_Text, Moved => Moved);
          Text.Append (Tmp_Text);
        end if;
        exit when not Moved;
      end loop;
    end if;
    Trace ("Parsed text >" & Text.Image & "<");

    -- If there are only separators and if we are allowed, skip them
    if not Text.Is_Null
    and then (not Util.Is_Separators (Text)
              or else not Ctx.Normalize or else Children.Preserve) then
      -- Notify on father creation if needed
      if not Children.Created then
        -- First text child of this element, so this element is mixed
        -- In elements, Creation of element that has children
        Tree_Mng.Set_Is_Mixed (Ctx.Elements.all, True);
        Children.Is_Mixed := True;
        Trace ("Txt setting mixed on father");
        Call_Callback (Ctx, Elements, True, True,
                       In_Mixed => Children.In_Mixed);
        Ctx.Level := Ctx.Level + 1;
        Children.Created := True;
      end if;
      -- Fix text to insert
      -- Insert and notify this child
      Tree_Mng.Add_Text (Ctx.Elements.all, Text, Util.Get_Line_No (Ctx.Flow));
      if not Ctx.Expand then
        -- When not expanding, add child only if not empty
        Tmp_Text := Text;
        Util.Remove_Entities (Tmp_Text);
        if not Tmp_Text.Is_Null then
            Add_Child (Ctx, Adtd, Children);
        end if;
      elsif not Ctx.Normalize then
        -- When preserving spaces, add child only if not only separators
        if not Util.Is_Separators (Text) then
          Add_Child (Ctx, Adtd, Children);
        end if;
      else
        Add_Child (Ctx, Adtd, Children);
      end if;
      -- In elements, Creation of this text element
      Call_Callback (Ctx, Elements, True, False,
                     In_Mixed => Children.Is_Mixed);
      Move_Del (Ctx, False);
      Trace ("Txt added text child >" & Text.Image & "<");
    end if;

    -- Now handle tail if not empty
    if Tail.Is_Null then
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
    Trace ("Txt switching input to " & Tail.Image);
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
    -- Create parent node with children if needed
    procedure Create (Has_Children : in Boolean) is
    begin
      if not Children.Created then
        -- Creation of element
        Call_Callback (Ctx, Elements, True, Has_Children,
                       In_Mixed => Children.In_Mixed);
        if Has_Children then
          Ctx.Level := Ctx.Level + 1;
        end if;
        Children.Created := True;
      end if;
    end Create;

    Char : Character;
    Ok : Boolean;
    Str2 : String (1 .. 2);
  begin

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
            -- Element was created, close it
            Ctx.Level := Ctx.Level - 1;
            Call_Callback (Ctx, Elements, False, False,
                           In_Mixed => Children.In_Mixed);
          else
            -- Empty element <elt></elt>: Create element and close it
            Tree_Mng.Set_Is_Mixed (Ctx.Elements.all, True);
            Children.Is_Mixed := True;
            Create (True);
            Ctx.Level := Ctx.Level - 1;
            Call_Callback (Ctx, Elements, False, False,
                           In_Mixed => Children.In_Mixed);
          end if;
          return;
        elsif Char = Util.Directive then
          -- Must be a comment, DOCTYPE or CDATA
          -- Check "<![CDATA["
          Util.Unget (Ctx.Flow, 2);
          Util.Try (Ctx.Flow, Util.Cdata_Start, Ok, False);
          if Ok then
            -- CDATA => Text
            Parse_Text (Ctx, Adtd, Children);
          else
            -- Directive: must be a comment or DOCTYPE
            Util.Get (Ctx.Flow, Str2);
            Create (True);
            Parse_Directive (Ctx, Adtd, Allow_Dtd => False,
                                        Context => Ref_Xml,
                                        Children => Children);
          end if;
        elsif Char = Util.Instruction then
          Create (True);
          Parse_Instruction (Ctx, Adtd, Children);
          Children.First_Child := False;
        elsif Char = Util.Start then
          Util.Error (Ctx.Flow, "Unexpected character " & Util.Start);
        else
          -- A new sub-element
          Create (True);
          Util.Unget (Ctx.Flow);
          Parse_Element (Ctx, Adtd, Children, False);
          Children.First_Child := False;
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
    Element_Name, End_Name : As.U.Asu_Us;
    Char : Character;
    Line_No : Natural;
    My_Children : aliased Children_Desc;
    use type As.U.Asu_Us;
  begin
    Line_No := Util.Get_Line_No (Ctx.Flow);
    -- Parse name until /, > or a separator
    Util.Parse_Until_Char (Ctx.Flow, "/> ");
    -- Check and store name
    Util.Get_Curr_Str (Ctx.Flow, Element_Name);
    if not Util.Name_Ok (Element_Name) then
      Util.Error (Ctx.Flow, "Invalid element name " & Element_Name.Image);
    end if;
    if Root
    and then not Ctx.Doctype.Name.Is_Null
    and then Element_Name /= Ctx.Doctype.Name then
      -- Root name must match DOCTYPE name
      Util.Error (Ctx.Flow, "Element name " & Element_Name.Image
           & " does not match doctype name " & Ctx.Doctype.Name.Image);
    end if;
    Util.Reset_Curr_Str (Ctx.Flow);
    -- Add new element and move to it
    Tree_Mng.Add_Element (Ctx.Elements.all, Element_Name, Line_No);
    -- Add ourself as child of our parent
    Add_Child (Ctx, Adtd, Parent_Children);
    -- Space are forbidden if standalone and content
    if not Ctx.Standalone then
      My_Children.Space_Allowed := True;
    else
      Dtd.Can_Have_Spaces (Adtd, Element_Name, My_Children.Space_Allowed);
    end if;
    -- Is current element mixed?
    Dtd.Is_Mixed (Adtd, Element_Name, My_Children.Is_Mixed);
    if My_Children.Is_Mixed then
      Tree_Mng.Set_Is_Mixed (Ctx.Elements.all, True);
    end if;
    Trace ("Parsing element " & Element_Name.Image
         & ", allowing space: " & Mixed_Str (My_Children.Space_Allowed'Img)
         & ", mixed: " & Mixed_Str (My_Children.Is_Mixed'Img));
    My_Children.Father := Element_Name;
    My_Children.In_Mixed := Parent_Children.Is_Mixed;
    My_Children.First_Child := True;
    -- See first significant character after name
    Util.Read (Ctx.Flow, Char);
    if Util.Is_Separator (Char) then
      Util.Skip_Separators (Ctx.Flow);
      Util.Get (Ctx.Flow, Char);
    end if;

    -- If not / nor >, then parse_attributes
    if Char /= Util.Slash and then Char /= Util.Stop then
      Util.Unget (Ctx.Flow);
      Parse_Attributes (Ctx, Adtd, Of_Xml => False, Elt_Name => Element_Name);
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
      Tree_Mng.Set_Put_Empty (Ctx.Elements.all, True);
      Dtd.Check_Attributes (Ctx, Adtd);
      Dtd.Check_Element (Ctx, Adtd,  My_Children);
      -- Create this element with no child (Close)
      Call_Callback (Ctx, Elements, True, False,
                     In_Mixed => Parent_Children.Is_Mixed);
      Move_Del (Ctx, False);
      Trace ("Parsed element " & Element_Name.Image);
      return;
    elsif Char = Util.Stop then
      -- >: parse text and children elements until </
      -- Check attributes first (e.g. xml:space)
      Dtd.Check_Attributes (Ctx, Adtd);
      -- Try to preserve spaces if current element has this tuning
      -- Tuning set by Dtd (if default is preserve and no value in Xml)
      --  or by attribute value in Xml. In both cases it in Tree
      My_Children.Preserve := String_Mng.Locate (
              Tree_Mng.Get_Tuning (Ctx.Elements.all),
                                   Tree_Mng.Xml_Space_Preserve) /= 0;
      -- If Dtd reset (not Expand), still apply space preservation
      My_Children.Preserve := My_Children.Preserve
                              or else Dtd.Has_Preserve (Ctx, Element_Name);
      if My_Children.Preserve then
        Trace ("Preserving spaces of the texts of " & Element_Name.Image);
      end if;
      Trace ("Parsing children of " & Element_Name.Image);
      Parse_Children (Ctx, Adtd, My_Children'Access);
      Trace ("Parsed children of " & Element_Name.Image);
      -- Check Name matches
      Util.Parse_Until_Char (Ctx.Flow, Util.Stop & "");
      Util.Get_Curr_Str (Ctx.Flow, End_Name);
      if End_Name /= Element_Name then
        Util.Error (Ctx.Flow, "Element name mismatch, expected "
                  & Element_Name.Image
                  & ", got " & End_Name.Image);
      end if;
      -- End of this non empty element, check children
      Tree_Mng.Set_Put_Empty (Ctx.Elements.all, False);
      Dtd.Check_Element (Ctx, Adtd,  My_Children);
      Move_Del (Ctx, False);
      Trace ("Parsed element " & Element_Name.Image);
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
          -- Instruction in Tail
          Parse_Instruction (Ctx, Adtd, null);
        when Util.Directive =>
          -- Directive in Tail: only comment
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
          Tree_Mng.Add_Element (Ctx.Elements.all, As.U.Asu_Null,
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
  begin
    if Ctx.Flow.Curr_Flow.Is_File then
      File_Mng.Open (Ctx.Flow.Curr_Flow.Name.Image,
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
    -- Perform final checks on Dtd (unparsed entities v.s. notations)
    Dtd.Final_Dtd_Check (Ctx, Adtd);
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
    Full_File : As.U.Asu_Us;
    Is_File : Boolean;
  begin
    -- Reset Dtd
    Dtd.Init (Adtd);
    Util.Push_Flow (Ctx.Flow);
    -- Parse Dtd
    if not Ctx.Dtd_File.Is_Null then
      -- Parse alternate Dtd provided by caller
      Dtd.Parse (Ctx, Adtd, Build_Full_Name (Ctx.Dtd_File,
                                       Ctx.Flow.Curr_Flow.Name));
    elsif not Ctx.Doctype.Name.Is_Null then
      if not Ctx.Doctype.File.Is_Null then
        -- Check validity of dtd file
        Full_File := Build_Full_Name (Ctx.Doctype.File,
                                      Ctx.Flow.Curr_Flow.Name);
        if Full_File.Image = Dtd.String_Flow
        or else Full_File.Image = Dtd.Internal_Flow then
          Util.Error (Ctx.Flow, "Invalid Dtd file name");
        end if;
        -- Expand URI
        Resolve_Uri (Ctx, Ctx.Doctype.File, Is_File, Full_File);
        if not Is_File then
          -- Full_File is the content of the Dtd fetched (by http)
          Ctx.Flow.Curr_Flow.Name := Ctx.Doctype.File;
          Ctx.Flow.Curr_Flow.Line := 1;
          Ctx.Flow.Curr_Flow.Same_Line := False;
          Ctx.Flow.Curr_Flow.In_Str := Full_File;
          Ctx.Flow.Curr_Flow.In_Stri := 0;
          Full_File := As.U.Tus (Dtd.String_Flow);
          Trace ("Parsing http dtd");
        end if;
        Dtd.Parse (Ctx, Adtd, Full_File);
      end if;
      if not Ctx.Doctype.Int_Def.Is_Null then
        -- Parse internal defs
        Ctx.Flow.Curr_Flow.Is_File := False;
        Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
        Ctx.Flow.Curr_Flow.In_Str := Ctx.Doctype.Int_Def;
        Ctx.Flow.Curr_Flow.In_Stri := 0;
        Ctx.Flow.Curr_Flow.Line := Ctx.Doctype.Line_No;
        Ctx.Flow.Curr_Flow.Same_Line := True;
        Dtd.Parse (Ctx, Adtd, As.U.Tus (Dtd.String_Flow));
      end if;
    end if;
    -- Restore flow
    Util.Pop_Flow (Ctx.Flow);
    -- Perform final checks on Dtd (unparsed entities v.s. notations)
    Dtd.Final_Dtd_Check (Ctx, Adtd);
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

