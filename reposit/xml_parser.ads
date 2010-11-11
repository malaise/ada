with Ada.Finalization;
with As.U; use As.U;
with Queues, Trees, Hashed_List.Unique, Text_Char, Dynamic_List,
     Unlimited_Pool, Byte_To_Unicode;
-- Parse Xml file or string.
-- Call callback while parsing or provide read access to the tree after parsing.
-- Limitations:
--  * Only the System Id of the DOCTYPE and of external parsed ENTITY is used,
--    Public Id (if any) is skipped.
--  * Only local file, "file://" and "http://" schemes are supported in URIs
--    (parsing error).
--  * Only UTF-8, UTF-16 and ISO-8859-1 are natively supported, other character
--    maps can be defined in $XML_PARSER_MAP_DIR.
package Xml_Parser is

  -- Version incremented at each significant change
  Major_Version : constant String := "26";
  function Version return String;

  -----------
  -- TYPES --
  -----------
  -- Generic node type
  -- A node is either an element or a text or a comment
  type Node_Kind_List is (Element, Text, Pi, Comment);
  type Node_Type (Kind : Node_Kind_List := Element) is private;
  No_Node : constant Node_Type;

  -- Element type
  subtype Element_Type is Node_Type(Element);
  -- A Text
  subtype Text_Type is Node_Type(Text);
  -- A Processing Instruction
  subtype Pi_Type is Node_Type(Pi);
  -- A Comment
  subtype Comment_Type is Node_Type(Comment);

  -- An attribute of an element
  type Attribute_Rec is record
    Name : Asu_Us;
    Value : Asu_Us;
    Unparsed : Boolean := False;
  end record;
  -- The attributes of an element
  type Attributes_Array is array (Positive range <>) of Attribute_Rec;
  type Attributes_Access is access all Attributes_Array;

   -- Number of children of an element
  subtype Child_Range is Trees.Child_Range;
  subtype Child_Index is Child_Range range 1 .. Child_Range'Last;
  -- The children of an element
  type Nodes_Array is array (Child_Index range <>) of Node_Type;

  -- A parsing context
  type Ctx_Type is tagged limited private;


  -- What to do with CDATA sections
  type Cdata_Policy_List is (Keep_Cdata_Section,    -- Keep markers and Cdata
                             Remove_Cdata_Markers,  -- Remove markers
                             Remove_Cdata_Section); -- Remove whole section


  -- Context status
  type Ctx_Status_List is (
    Clean,              -- Nothing parsed (initial status and after Clean)
    Parsed_Prologue,    -- }
    Parsed_Prologue_Cb, -- } Prologue parsed (can scan prologue and parse elts)
    Parsed_Elements,    -- Elements parsed OK (can scan prologue and elts)
    Error,              -- Parse error detected
    Init);              -- Initialized for the Generator

  -----------------------------
  -- NOTE ABOUT THE PROLOGUE --
  -----------------------------
  -- In xml V1.0, the prologue consists of an optional xml directive
  --  (<?xml attributes?>) then optional processing instructions
  --  (<?name text?>), DOCTYPE and comments.
  -- In Xml V1.1 the xml directive and version is mandatory.
  -- So the Prologue is an element of name "xml" with possible attributes
  --  (no attribute means that there is no Xml directive) and children:
  --  for PIs: PIs each with the PITarget as name
  --  for Comments: comments
  --  for the doctype: an empty text

  ----------------------------
  -- NOTE ABOUT THE DOCTYPE --
  ----------------------------
  -- The DOCTYPE is parsed during the prologue parsing, it can be retrieved
  --  when the Prologue has a child of type text (empty)
  -- PUBLIC directive is not processed

  -------------------------
  -- NOTE ABOUT THE TAIL --
  -------------------------
  -- The tail (Comments and PIs after the root element) are attached
  --  to a dummy child of the root element. This child (if any) is the
  --  last child of root and has no name.

  -------------------------------------
  -- NOTE ABOUT THE PARSING CALLBACK --
  -------------------------------------
  -- When a callback is provided to Parse, then no tree is build but nodes
  --  are directly provided. Prologue items all have a level of 0 and no child
  -- Only elements have attributes and children. When it has children an element
  --  is created (Creation = True), then its children (recusively) then it is
  --  closed (Creation = False)
  -- Only PIs have a value
  -- Is_Mixed on element if this element has mixed content
  -- In_Mixed on anything if within a Is_Mixed element
  --  indent shall be skipped
  type Stage_List is (Prologue, Elements, Tail);
  type Node_Update is new Ada.Finalization.Controlled with record
    Stage : Stage_List := Prologue;
    Line_No : Natural := 0;
    Level : Natural := 0;
    Name : Asu_Us;
    Value : Asu_Us;
    Creation : Boolean := True;
    Is_Mixed : Boolean := False;
    In_Mixed : Boolean := False;
    Kind : Node_Kind_List := Element;
    -- Only for Kind Element
    Has_Children : Boolean := False;
    -- Only for Kind Element
    Attributes : Attributes_Access := null;
  end record;
  -- If the callback raises an exception the parse raises
  Callback_Error : exception;
  type Parse_Callback_Access is access
            procedure (Ctx  : in Ctx_Type;
                       Node : in Node_Update);

  -- The callback called in case of warning
  type Warning_Callback_Access is access
            procedure (Ctx : in Ctx_Type;
                       Warning : in String);

  ------------------
  -- FILE PARSING --
  ------------------
  -- Parse a Xml file, stdin if empty
  -- On option, allows retrieval of comments (usefull for formatter)
  -- On option, does not expand General entities nor set attributes with
  --  default values (usefull for formatter)
  -- On option skip CDATA sections or markers
  -- On option keep separators unchanged (in attributes and text)
  -- On option does not check compliance with Dtd
  -- On option force a dtd file different from DOCTYPE directive
  -- If a warning callback is set then it is called for each warning detected
  -- If a Parse_Cb is set then it is called for each node creation et for
  --  each element end and no tree is build (see above)
  -- May raise File_Error if error accessing the File_Name,
  --           Status_Error if Ctx is not clean
  Stdin : constant String := "";
  procedure Parse (Ctx       : out Ctx_Type;
                   File_Name : in String;
                   Ok        : out Boolean;
                   Comments  : in Boolean := False;
                   Expand    : in Boolean := True;
                   Cdata     : in Cdata_Policy_List := Remove_Cdata_Markers;
                   Normalize : in Boolean := True;
                   Use_Dtd   : in Boolean := True;
                   Dtd_File  : in String  := "";
                   Warn_Cb   : in Warning_Callback_Access := null;
                   Parse_Cb  : in Parse_Callback_Access := null);
  File_Error, Status_Error : exception;

  -- Return current status of context
  function Get_Status (Ctx : Ctx_Type) return Ctx_Status_List;

  -- Return the error message if Parse error
  -- May raise Status_Error if Ctx is clean
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String;

  -- Clean parsing context, when the Prologue and Element trees
  --  are not used any more
  procedure Clean (Ctx : in out Ctx_Type);

  --------------------
  -- STRING PARSING --
  --------------------
  -- Parse a Dtd, optionally check for some warnings
  -- Set Error to error string, or empty string if OK
  type Dtd_Type is limited private;
  procedure Parse_Dtd_File (
      File_Name : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out Asu_Us);

  procedure Parse_Dtd_String (
      Str       : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out Asu_Us);

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type);

  -- Parse the prologue of a string
  -- Then one can call Get_Prologue on Ctx
  --  (Calling Get_Root_Element will raise Use_Error);
  -- On option, allows retrieval of comments (usefull for formatter)
  -- On option, does not expand General entities (usefull for formatter)
  -- On option skip CDATA sections or markers
  -- On option keep separators (in attributes and text) unchanged
  -- May raise Status_Error if Ctx is not clean
  procedure Parse_Prologue (Ctx       : out Ctx_Type;
                            Str       : in String;
                            Ok        : out Boolean;
                            Comments  : in Boolean := False;
                            Expand    : in Boolean := True;
                            Cdata     : in Cdata_Policy_List
                                      := Remove_Cdata_Markers;
                            Normalize : in Boolean := True;
                            Warn_Cb   : in Warning_Callback_Access := null;
                            Parse_Cb  : in Parse_Callback_Access := null);

  -- Parse the elements (after the prologue) and tail of a string with a dtd
  -- may raise Status_Error if Ctx is clean
  --           End_Error if Ctx has already parsed elements
  --           Parse_Error if Parse_Prologue was not ok
  Parse_Error : exception;
  procedure Parse_Elements (Ctx      : in out Ctx_Type;
                            Dtd      : in out Dtd_Type;
                            Ok       : out Boolean);
  End_Error : exception;

  -----------
  -- CHECK --
  -----------
  -- Check the Ctx: parse the DTD (if any) and check the Ctx versus it
  --  (same effect as Parse, but on a context set or modified in
  --  Xml_Parser.Generator)
  procedure Check (Ctx : in out Ctx_Type;
                   Ok  : out Boolean;
                   Warn_Cb  : in Warning_Callback_Access := null);

  -------------------------
  -- NAME AND ATTRIBUTES --
  -------------------------
  -- All the following operations may raise Invalid_Node if the Element has
  --  not been returned by Parse, Get_xxx...
  Invalid_Node : exception;
  function Is_Valid (Node : Node_Type) return Boolean;
  -- They may raise Status_Error if the Ctx is clean
  -- They may raise Use_Error if the Ctx and the Element do not match
  Use_Error : exception;

  -- Get Prologue of a parsed context (after Parse or Parse_Prologue)
  --  may raise Parse_Error if Parse was not ok
  function Get_Prologue (Ctx : Ctx_Type) return Element_Type;
  -- Get elements'root after Parse or Parse_Elements
  --  may raise Status_Error if called before Parse_Elements
  --            Parse_Error if Parse was not ok
  function Get_Root_Element (Ctx : Ctx_Type) return Element_Type;

  -- Get Doctype characteristics (prologue must have been parsed)
  Doctype_Not_Set : exception;
  procedure Get_Doctype (Ctx : in Ctx_Type;
       Name    : out Asu_Us;
       Public  : out Boolean;
       Pub_Id  : out Asu_Us;
       File    : out Asu_Us;
       Int_Def : out Asu_Us);

  -- Get the Target of a PI
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return String;
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type)
                    return Asu_Us;
  -- Get a PI data
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return String;
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type)
           return Asu_Us;

  -- Get the line number of the beginning of the declaration of a node
  -- 0 if not the result of parsing of text
  function Get_Line_No (Ctx  : Ctx_Type;
                        Node : Node_Type) return Natural;

  -- Get the name of an element
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return String;
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return Asu_Us;
  -- Get the attributes of an element
  function Get_Attributes (Ctx     : Ctx_Type;
                           Element : Element_Type) return Attributes_Array;
  function Get_Nb_Attributes (Ctx     : Ctx_Type;
                              Element : Element_Type) return Natural;
  -- May raise Invalid_Index
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Index   : Positive) return Attribute_Rec;
  Invalid_Index : exception;
  -- May raise Attribute_Not_Found
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Name    : String) return Attribute_Rec;
  Attribute_Not_Found : exception;


  ----------------
  -- NAVIGATION --
  ----------------
  -- Get the Children of an element
  function Get_Children (Ctx     : Ctx_Type;
                         Element : Element_Type) return Nodes_Array;
  function Get_Nb_Children (Ctx     : Ctx_Type;
                            Element : Element_Type) return Child_Range;
  -- Get one Child of an element
  -- May raise Invalid_Index
  function Get_Child (Ctx     : Ctx_Type;
                      Element : Element_Type;
                      Index   : Child_Index) return Node_Type;

  -- Get a brother of an node
  -- May raise No_Brother
  No_Brother : exception;
  function Has_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Boolean;
  function Get_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Node_Type;

  -- Get the father of an element
  -- May raise No_Parent if Element is the Root_Element or the Prologue
  No_Parent : exception;
  function Get_Parent (Ctx  : Ctx_Type;
                       Node : Node_Type) return Element_Type;
  function Is_Root (Ctx  : Ctx_Type;
                    Node : Node_Type) return Boolean;

  ----------------------
  -- TEXT and COMMENT --
  ----------------------
  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type) return String;
  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type)
                     return Asu_Us;

  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return String;
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type)
                        return Asu_Us;

  --------------------------
  -- UNPARSED ENTITY info --
  --------------------------
  -- URI and PudId of an unparsed entity
  type Unparsed_Entity_Info_Rec is record
    Entity_System_Id : Asu_Us;
    Entity_Public_Id : Asu_Us;
    Notation_Name    : Asu_Us;
    Notation_System_Id : Asu_Us;
    Notation_Public_Id : Asu_Us;
  end record;

  -- Get info on an unparsed entity and its associated notation
  --  may raise Status_Error if Ctx prologue is not Parsed
  --            Unknown_Entity if not such unparsed entity
  Unknown_Entity : exception;
  procedure Get_Unparsed_Entity_Info (Ctx    : in out Ctx_Type;
                                      Entity : in String;
                                      Info   : out Unparsed_Entity_Info_Rec);

  -----------------
  -- Specif TAGS --
  -----------------
  -- Shall the Element, if empty, be put with EmptyElemTag (<element/>) or
  --  with STag and ETag (<element></elememt>)
  -- By default it is False except if
  --  - Parsed element is empty with EmptyElemTag (</element>)
  --  - or Generator.Set_Put_Empty (True) is called on the element
  function Get_Put_Empty (Ctx     : Ctx_Type;
                          Element : Element_Type) return Boolean;

  -- Is this element Mixed: either Mixed in Dtd or its first child is Text
  -- Set by the parsing or by calling Check
  function Get_Is_Mixed (Ctx     : Ctx_Type;
                         Element : Element_Type) return Boolean;

  ------------------------
  -- General EXCEPTIONS --
  ------------------------
  -- If internal logic error (in parsing)
  Internal_Error : exception;

private

  ---------------
  -- NODE TYPE --
  ---------------
  -- Internal tree
  type Internal_Kind_List is (Element, Text, Pi, Comment, Attribute);
  type My_Tree_Cell is record
    -- Line in source file
    Line_No : Natural := 0;
    -- Kind:  Element, Text or Attribute
    Kind : Internal_Kind_List;
    -- Number of attributes when Kind is Element
    Nb_Attributes : Natural := 0;
    -- Element name or Attribute name or text or comment...
    Name : Asu_Us;
    -- Attribute value or PI content
    Value : Asu_Us;
    -- Is this attribute an Unparsed entity or a list of unparsed entities
    Unparsed : Boolean := False;
    -- Put empty element with EmptyElemTag
    Put_Empty : Boolean := False;
    -- Is this element containing Mixed: either Mixed in Dtd
    --  or its first child is text
    Is_Mixed : Boolean := False;
  end record;
  package My_Tree is new Trees.Tree(My_Tree_Cell);

  type Tree_Acc is access all My_Tree.Tree_Type;
  -- Exported node type
  Clean_Magic : constant Float := -1.0;
  type Node_Type (Kind : Node_Kind_List := Element) is record
    -- Magic number of the context
    Magic : Float := Clean_Magic;
    -- In prologue or a real element
    In_Prologue : Boolean := False;
    -- Position in tree
    Tree_Access : My_Tree.Position_Access := My_Tree.No_Position;
  end record;
  No_Node : constant Node_Type (Element)
          := (Element, Clean_Magic, False, My_Tree.No_Position);
  ---------------------
  -- INPUT FLOW TYPE --
  ---------------------
  -- Longest keywork + <![CDATA[
  Max_Buf_Len : constant := 21;
  -- Don't skip current data from recording
  No_Skip_Rec : constant Integer := -1;
  package My_Circ is new Queues.Circ (Max_Buf_Len, Character);

  -- Current flow is...
  type Flow_Kind_List is (Xml_Flow, Dtd_Flow, Int_Dtd_Flow, Ext_Flow);
  -- Current encoding
  type Encod_List is (Utf8, Utf16_Le, Utf16_Be, Latin1, Other);
  -- Number of single UTF8 bytes re-inserted in flow when in UTF16
  subtype Bytes_Range is Natural;
  type File_Access is access all Text_Char.File_Type;

  type Flow_Info_Type is record
    -- Is it a file or string
    Is_File : Boolean := True;
    -- Is it a xml (or entity) or dtd
    Kind : Flow_Kind_List := Xml_Flow;
    -- File name (empty if stdin or string)
    Name : Asu_Us;
    -- Current line No
    Line : Natural := 0;
    -- Is it a string expanded in original flow
    --  so keep Line unchanged
    Same_Line : Boolean := False;
    -- Encoding of each kind of flow
    Encod : Encod_List := Utf8;
    -- Map when encoding is other
    Map : Byte_To_Unicode.Map;
    -- Remaining bytes when UTF8 characters
    -- are re-inserted in a UTF16 flow
    Nb_Bytes : Bytes_Range := 0;
    -- Prev char read was a Cr
    Prev_Char_Was_Cr : Boolean := False;
    -- If Flow is a file (Text_Char)
    File : File_Access;
    -- If Flow is a String
    In_Str : Asu_Us;
    In_Stri : Natural := 0;
  end record;

  -- Pool of flows (when switching to new flow then back)
  package Flow_Pool_Mng is new Unlimited_Pool (Flow_Info_Type);

  -- Pool of files to deallocate
  package File_Pool_Mng is new Unlimited_Pool (File_Access);

  type Flow_Type is record
    -- To know how many where got before End_Error
    Nb_Got : Natural := 0;
    -- Circular buffer of read characters
    Circ : My_Circ.Circ_Type;
    -- Error message
    Err_Msg : Asu_Us;
    -- Current significant string, loaded by Parse_Until_xxx
    Curr_Str : Asu_Us;
    -- Recorded input characters
    Recording : Boolean := False;
    Skip_Recording : Integer := No_Skip_Rec;
    Recorded : Asu_Us;
    -- Current flow
    Curr_Flow : Flow_Info_Type;
    -- Previous Xml flow, dtd flow, External entity flow, text flow...
    Flows : Flow_Pool_Mng.Pool_Type;
    -- Created text files
    Files : File_Pool_Mng.Pool_Type;
  end record;

  --------------
  -- DTD TYPE --
  --------------
  -- The stored entities
  type Entity_Type is record
    Name : Asu_Us;
    Value : Asu_Us;
    Parameter : Boolean;
    Internal : Boolean;
    Intern_Dtd : Boolean;
    Parsed : Boolean;
  end record;
  type Entity_Access is access all Entity_Type;
  procedure Set (To : out Entity_Type; Val : in Entity_Type);
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean;
  function Image (Entity : Entity_Type) return String;
  package H_Entity_List_Mng is new Hashed_List (Entity_Type, Entity_Access,
             Set, "=", Image);
  package Entity_List_Mng is new H_Entity_List_Mng.Unique;

  -- Dtd info rec
  type Info_Rec is record
    -- Kind#Element_name[#Attribute_Name]
    -- Kind is Elt, Atl, Att
    Name : Asu_Us;
    -- Elt: Possible children, first chars is <type> ::= E|A|M|C
    --  (empty, any, mixed or children), then
    --  for Mixed the list of "#<name>#<name>#" without #PCDATA
    --   (empty if only #PCDATA)
    --  for Children the regexp of "#<name>#"
    -- Atl: Possible attributes, list of "#<name>##<type><default>#"
    --  <type> ::= S|I|R|r|T|t|Y|y|N|E (String, ID, IDREF, IDREFS,
    --  NMTOKEN, NMTOKENS, ENTITY, ENTITIES, NOTATION or enum)
    --  <default> ::= R|I|F|D (required, implied, fixed or default)
    -- Att: for a fixed of any type or the default of not enum, the value
    --   for an Enum, the list of possible "<name>#" and, if there is a default
    --   this value is the first
    List : Asu_Us;
    Line : Natural;
  end record;
  type Info_Access is access all Info_Rec;
  procedure Set (To : out Info_Rec; Val : in Info_Rec);
  function Image (Element : Info_Rec) return String;
  function "=" (Current : Info_Rec; Criteria : Info_Rec) return Boolean;
  package H_Info_Mng is new Hashed_List (Info_Rec, Info_Access,
                                         Set, "=", Image);
  package Info_Mng is new H_Info_Mng.Unique;

  -- Unparsed entity or notation info
  type Unparsed_Type is record
    -- Entity or Notation
    Is_Entity : Boolean := True;
    -- Name
    Name : Asu_Us;
    -- Line where defined
    Line_No : Natural := 0;
    -- Ids
    System_Id : Asu_Us;
    Public_Id : Asu_Us;
    -- Notation name (when this is an entity)
    Notation : Asu_Us;
  end record;
  type Unparsed_Access is access all Unparsed_Type;
  procedure Set (To : out Unparsed_Type; Val : in Unparsed_Type);
  function "=" (Current : Unparsed_Type; Criteria : Unparsed_Type)
               return Boolean;
  function Image (Unparsed : Unparsed_Type) return String;
  package H_Unparsed_List_Mng is new Hashed_List (Unparsed_Type,
             Unparsed_Access, Set, "=", Image);
  package Unparsed_List_Mng is new H_Unparsed_List_Mng.Unique;


  type Dtd_Type is record
    -- Is there a dtd set, otherwise check is always ok
    Set : Boolean := False;
    -- Is there already xml instruction found in the dtd
    Xml_Found : Boolean := False;
    -- Encoding directive of dtd
    Encoding :  Asu_Us;
    -- Parsed info
    Info_List : Info_Mng.Unique_List_Type;
    -- Parsed entities
    Entity_List : Entity_List_Mng.Unique_List_Type;
    -- Notation attributes: #Elt##Attr#Elt##Attr#...
    Notation_Attrs : Asu_Us;
    -- Internal elements #@Elt# or attributes #Elt##Attr#
    -- ELEMENT or ATTLIST defined in internal dtd
    Internals : Asu_Us;
    -- Are we in an INCLUDE directive
    In_Include : Boolean := False;
  end record;

  --------------------
  -- IDs and IDREFs --
  --------------------
  type Id_Cell is record
    -- Line where the ID or IDREF is defined
    Line_No : Natural := 0;
    -- ID name
    Name : Asu_Us;
  end record;
  type Id_Cell_Access is access all Id_Cell;
  procedure Set (To : out Id_Cell;  Val : in Id_Cell);
  function Image (Element : Id_Cell) return String;
  function "=" (Current : Id_Cell; Criteria : Id_Cell) return Boolean;

  -- Unique list of IDs
  package H_Id_List_Mng is new Hashed_List (Id_Cell, Id_Cell_Access,
                                          Set, "=", Image);
  package Id_List_Mng is new H_Id_List_Mng.Unique;
  type Id_List_Access is access Id_List_Mng.Unique_List_Type;

  -- List of IDREFs found
  package Idref_Dyn_List_Mng is new Dynamic_List (Id_Cell);
  package Idref_List_Mng renames Idref_Dyn_List_Mng.Dyn_List;
  type Idref_List_Access is access Idref_List_Mng.List_Type;

  ------------------
  -- DOCTYPE info --
  ------------------
  type Doctype_Type is record
    -- Line of the DOCTYPE directive
    Line_No : Natural := 0;
    -- Name, file (ID+URI) and internal definition if any
    -- Empty name for no DOCTYPE
    Name    : Asu_Us;
    Public  : Boolean := False;
    Pub_Id  : Asu_Us;
    File    : Asu_Us;
    Int_Def : Asu_Us;
  end record;

  ------------------
  -- CONTEXT TYPE --
  ------------------
  type Ctx_Type is limited new Ada.Finalization.Limited_Controlled with record
    Status  : Ctx_Status_List := Clean;
    Magic : Float := Clean_Magic;
    -- Input flow description
    Flow : Flow_Type;
    -- Parse or skip comments
    Parse_Comments : Boolean := False;
    -- Expand or not general entities and attributes with default values
    Expand : Boolean := True;
    -- What to do with CDATA sections
    Cdata_Policy : Cdata_Policy_List := Remove_Cdata_Markers;
    -- Normalize separators in attributes and texts
    Normalize : Boolean := True;
    -- Use Dtd
    Use_Dtd : Boolean := True;
    Dtd_File : Asu_Us;
    -- Check also and report warnings
    Warnings : Warning_Callback_Access := null;
    -- Call a callback i.o. feeding trees
    Callback : Parse_Callback_Access := null;
    Level : Natural := 0;
    -- Prologue, parsed elements and tail
    Prologue : Tree_Acc := new My_Tree.Tree_Type;
    Elements : Tree_Acc := new My_Tree.Tree_Type;
    -- Doctype name, file and a tag of internal definitions
    Doctype : Doctype_Type;
    -- Standalone
    Standalone : Boolean := False;
    -- Unique list of Ids
    Ids : Id_List_Access := new Id_List_Mng.Unique_List_Type;
    -- List of Idrefs
    Idrefs : Idref_List_Access := new Idref_List_Mng.List_Type;
    -- Unparsed entities and Notations
    Unparsed_List : Unparsed_List_Mng.Unique_List_Type;
  end record;
  overriding procedure Finalize (Ctx : in out Ctx_Type);

  overriding procedure Finalize (Node : in out Node_Update);

  -- For Xml_Generator
  function Get_Magic return Float;
  function Get_Tree (Ctx : Ctx_Type;
                     Node : Node_Type) return Tree_Acc;
end Xml_Parser;

