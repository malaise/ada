with Ada.Finalization;
with Long_Longs;
private with Magic_Numbers, Queues, Text_Char, Byte_To_Unicode,
             Long_Long_Limited_Pool, Hashed_List.Unique;
with As.U, Trees, Trilean;
-- Parse a Xml file or string.
-- Call callback while parsing, or provide access to the tree after parsing.
-- Limitations:
--  * Only the System Id of the DOCTYPE and of external parsed ENTITY is used,
--    Public Id (if any) is ignored.
--  * Only the schemes: path to local file, "file://" and "http://" are
--     supported in URIs (other schemes raise parsing error).
--  * Only (xx-)ASCII, UTF-8, UTF-16 and ISO-8859-1 encodings are natively
--     supported.
--    Some other encodings may be handled by defining the environment variable
--    XML_PARSER_MAP_DIR to the path where Byte_To_Unicode can find the mapping
--    file, named <ENCODING>.xml (in uppercase, ex: ISO-8859-9.xml).
--  * Namespaces are not checked for the validity of URI references.
package Xml_Parser is

  -- Version incremented at each significant change
  Major_Version : constant String := "44";
  function Version return String;

  -----------
  -- TYPES --
  -----------
  -- Generic node type
  -- A node is either an element, a text, a processing instruction (PI)
  --  or a comment
  type Node_Kind_List is (Element, Text, Pi, Comment);
  type Node_Type (Kind : Node_Kind_List := Element) is private;
  No_Node : constant Node_Type;

  -- Specific node types
  -- An Element
  subtype Element_Type is Node_Type(Element);
  -- A Text
  subtype Text_Type is Node_Type(Text);
  -- A Processing Instruction (PI)
  subtype Pi_Type is Node_Type(Pi);
  -- A Comment
  subtype Comment_Type is Node_Type(Comment);

  -- An attribute of an element
  type Attribute_Rec is record
    Name : As.U.Asu_Us;            -- Attribute name
    Namespace : As.U.Asu_Us;       -- Namespace (if parsed with Namespace)
    Value : As.U.Asu_Us;           -- Attribute value
    Unparsed : Boolean := False;   -- Is it an unparsed entity
  end record;
  -- The attributes of an element
  type Attributes_Array is array (Positive range <>) of Attribute_Rec;
  type Attributes_Access is access all Attributes_Array;

   -- Number of children of an element
  subtype Child_Range is Trees.Child_Range;
  subtype Child_Index is Child_Range range 1 .. Child_Range'Last;
  -- The children of an element
  type Nodes_Array is array (Child_Index range <>) of Node_Type;

  -- A parsing context (token for all the operations)
  type Ctx_Type is tagged limited private;

  -- Context status
  type Ctx_Status_List is (
    Clean,              -- Nothing parsed (initial status and after Clean)
    Parsed_Prologue,    -- }
    Parsed_Prologue_Cb, -- } Prologue parsed (can scan prologue and parse elts)
    Parsed_Elements,    -- Elements parsed OK (can scan prologue and elts)
    Error,              -- Parsing error detected
    Init,               -- Initialized, for the XML Generator
    Unparsed);          -- Parsed with callback, only unparsed entities infos
                        --  can be retrieved

  -- What to do with CDATA sections
  type Cdata_Policy_List is (Keep_Cdata_Section,    -- Keep markers and Cdata
                             Remove_Cdata_Markers,  -- Remove markers
                             Remove_Cdata_Section); -- Remove whole section

  -----------
  -- NOTES --
  -----------
  -- About NAMESPACES
  -------------------
  -- When the Namespaces option is set then:
  --  - the document is checked to be namespace-well-formed and namespace-valid
  --  - namespace information of elements (Get_Namespace) and attributes
  --    (Namespace field) is filled, otherwise it is empty
  -- Note that the names of element and attribute remain qualified and that the
  --  validity of URIs is not checked

  -- About the PROLOGUE
  ---------------------
  -- In xml V1.0, the prologue consists in an optional xml directive
  --  (<?xml attributes?>) then optional processing instructions
  --  (<?name text?>), DOCTYPE and comments.
  -- In Xml V1.1 the xml directive is mandatory.
  -- In both case, the attribute "version" of xml is mandatory.
  -- So, the Prologue is an element of name "xml" with possible attributes
  --  (no attribute means that there is no Xml directive) and children:
  --  for a PI: a Pi node
  --  for a comment: a Comment node
  --  for the doctype: an empty Text

  -- About the DOCTYPE
  --------------------
  -- The DOCTYPE is parsed during the prologue parsing, it can be retrieved
  --  when the Prologue has a child of type Text (empty)
  -- Note that the PUBLIC directive, if any, is not processed

  -- About the WARNINGS
  ---------------------
  -- The warnings reported by the parser are:
  -- - ATTLIST already existing for element and attribute => merge directives
  -- - attribute already defined for element => discard new definition
  -- - entity already defined => discard new definition
  -- - unknown element used in child definition => discard
  -- - unknown element used in ATTLIST => discard
  -- - inconsistency between EMPTY definition of element and an EmptyElemTag
  -- The callback called in case of warning
  type Warning_Callback_Access is access
            procedure (Ctx : in Ctx_Type;
                       Warning : in String);

  -- About the PARSING CALLBACK
  -----------------------------
  -- When a callback is provided to Parse, then no tree is build, but the nodes
  --  are directly provided. Prologue items all have a level of 0 and no child
  -- Only elements have namespace, attributes and children.
  --  When it has children, an element is created (Creation = True,
  --  Has_Children = True), then its children (recusively) then it is closed
  --  (Creation = False), otherwise it is only created (Has_Children = False)
  --  and Empty_Info indicates if it is an EmptyElemTag
  -- Only PIs have a value
  -- Is_Mixed is set on element if this element has mixed content: children will
  --  be appended
  -- In_Mixed is set on anything within a Is_Mixed element: indent shall be
  --  skipped for these nodes
  -- After the parsing, the Ctx has status Unparsed and only the info on
  --  unparsed entities can be got from it

  -- Parsing stage. The tail is the part (comments, PIs) after the closure of
  --  the root element
  type Stage_List is (Prologue, Elements, Tail);

  -- The "Empty" info about an element
  -- Usefull to display the element, either as EmptyElemTag (<Element/>)
  --  or with STag and ETag (<Element></Element>)
  --  or with STag, Lf, Indent then ETag (<Element>
  --                                      </Element>)
  type Empty_Info_List is (
    Tag_Empty,   -- Parsed as <EmptyElemTag/> or Generator.Set_Tag_Empty (True)
                 --  was called, whatever the Dtd can define
    Def_Empty,   -- Not Tag_Empty but defined as EMPTY in the Dtd
    Not_Empty);  -- None of the cases above

  -- Line number in file
  subtype Line_Range is Long_Longs.Llu_Natural;

  -- A node update transmitted to the callback
  type Node_Update is new Ada.Finalization.Controlled with record
    -- Stage where the node is parsed
    Stage : Stage_List := Prologue;
    -- Line in file
    Line_No : Line_Range := 0;
    -- Level of the node
    Level : Natural := 0;
    -- Kind of the node
    Kind : Node_Kind_List := Element;
    -- Name of the node
    Name : As.U.Asu_Us;
    -- Creation or closure
    Creation : Boolean := True;
    -- Only for Kind Pi
    Value : As.U.Asu_Us;
    -- The following fields are meaningful only for Kind Elements
    Namespace : As.U.Asu_Us;
    -- Attributes are set only at child creation (not at closure)
    Attributes : Attributes_Access := null;
    -- True if DTD specifies #PCDATA or EMPTY,
    --  or, without DTD, if first child is text
    Is_Mixed : Boolean := False;
    -- True if parent is Mixed
    In_Mixed : Boolean := False;
    -- True if node has children (of any kind)
    Has_Children : Boolean := False;
    -- Empty info
    Empty_Info : Empty_Info_List := Not_Empty;
  end record;

  -- If the callback raises an exception, then the parsing raises:
  Callback_Error : exception;

  -- The callback that is called during parsing, instead of building the tree
  type Parse_Callback_Access is access
            procedure (Ctx  : in Ctx_Type;
                       Node : in Node_Update);

  ------------------
  -- FILE PARSING --
  ------------------
  -- Parse a Xml file, stdin if File_Name is empty
  -- On option, allow retrieval of comments (usefull for a formatter)
  -- On option skip CDATA sections or keep CDATA markers
  -- On option, do not expand general entities nor set attributes with
  --  default values (usefull for a formatter)
  -- On option, keep separators unchanged in attributes and text
  -- On option, make text compatible ('>' -> "&gt;")
  -- On option do not check compliance with Dtd
  -- On option force an external Dtd different from the DOCTYPE directive
  -- On option check and fill namespace informations
  -- If a warning callback is set then it is called for each warning detected
  -- If a parsing callback is set then it is called for each node creation
  --  and for each element end, and no tree is build (see above)
  -- May raise File_Error if error accessing the File_Name,
  --           Status_Error if Ctx is not clean
  Stdin : constant String := "";
  procedure Parse (Ctx        : out Ctx_Type;
                   File_Name  : in String;
                   Ok         : out Boolean;
                   Comments   : in Boolean := False;
                   Cdata      : in Cdata_Policy_List := Remove_Cdata_Markers;
                   Expand     : in Boolean := True;
                   Normalize  : in Boolean := True;
                   Compatible : in Boolean := False;
                   Use_Dtd    : in Boolean := True;
                   Dtd_File   : in String  := "";
                   Namespace  : in Boolean := False;
                   Warn_Cb    : in Warning_Callback_Access := null;
                   Parse_Cb   : in Parse_Callback_Access := null);
  File_Error, Status_Error : exception;

  -- Return the current status of the parsing context
  function Get_Status (Ctx : Ctx_Type) return Ctx_Status_List;

  -- Return the error message if Parse error
  -- May raise Status_Error if Ctx is clean
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String;

  -- Clean the parsing context, when the Prologue, Element and Tail trees
  --  are not used any more
  procedure Clean (Ctx : in out Ctx_Type);

  -----------------
  -- DTD PARSING --
  -----------------
  -- Parse a Dtd, stdin if File_Name is empty
  -- Optionally check for some warnings
  -- The Dtd can then be used to Parse_Elements (after Parse_Prologue)
  -- Set Error to error string, or empty string if OK
  type Dtd_Type is limited private;
  procedure Parse_Dtd_File (
      File_Name : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out As.U.Asu_Us);

  -- Same but the Dtd is in a string
  procedure Parse_Dtd_String (
      Str       : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out As.U.Asu_Us);

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type);

  --------------------
  -- STRING PARSING --
  --------------------
  -- Parse the prologue of a string
  -- Then one can call Get_Prologue on Ctx
  --  (Calling Get_Root_Element will raise Status_Error);
  -- May raise Status_Error if Ctx is not clean
  procedure Parse_Prologue (Ctx        : out Ctx_Type;
                            Str        : in String;
                            Dtd        : out Dtd_Type;
                            Ok         : out Boolean;
                            Comments   : in Boolean := False;
                            Cdata      : in Cdata_Policy_List
                                       := Remove_Cdata_Markers;
                            Expand     : in Boolean := True;
                            Normalize  : in Boolean := True;
                            Compatible : in Boolean := False;
                            Use_Dtd    : in Boolean := True;
                            Dtd_File   : in String  := "";
                            Namespace  : in Boolean := False;
                            Warn_Cb    : in Warning_Callback_Access := null;
                            Parse_Cb   : in Parse_Callback_Access := null);

  -- Parse the elements (after the prologue) and the tail of a string with a dtd
  -- The options are inherited from the parsing of the prologue but the Dtd
  --  may be different (e.g. generated by Parse_Dtd_xxx)
  -- May raise Status_Error if Ctx is clean
  --           End_Error if Ctx has already parsed elements
  --           Parse_Error if Parse_Prologue was not ok
  End_Error : exception;
  Parse_Error : exception;
  procedure Parse_Elements (Ctx      : in out Ctx_Type;
                            Dtd      : in out Dtd_Type;
                            Ok       : out Boolean);

  -----------
  -- CHECK --
  -----------
  -- Check the Ctx: parse the DTD (if any) and check the Ctx versus it
  --  (same effect as Parse, but on a context that has been set or modified by
  --  Xml_Parser.Generator). Updates the namespaces, the Is_Mixed and the
  --  Empty_Info of the elements
  -- For Trileans, Other means "leave as it is" (which is the value
  --  by default or the one set in Parse)
  -- Comments and Cdata flags of the context are not modified (they don't
  --  impact the check)
  procedure Check (Ctx : in out Ctx_Type;
                   Ok  : out Boolean;
                   Expand     : in Trilean.Trilean := Trilean.Other;
                   Normalize  : in Trilean.Trilean := Trilean.Other;
                   Compatible : in Trilean.Trilean := Trilean.Other;
                   Use_Dtd    : in Trilean.Trilean := Trilean.Other;
                   Dtd_File   : in String  := "";
                   Namespace  : in Trilean.Trilean := Trilean.Other;
                   Warn_Cb    : in Warning_Callback_Access := null);

  -----------------------------
  -- PROLOGUE, ROOT and TAIL --
  -----------------------------
  -- All the following operations may raise Invalid_Node if the Node has
  --  not been returned by Get_xxx... They also may raise
  --  Status_Error if the Ctx is clean or unparsed
  --  Use_Error if the Ctx and the Element do not match
  --  (Element obtained from another context)
  Invalid_Node : exception;
  Use_Error : exception;

  -- Is a Node valid (returned by Get_xxx)
  function Is_Valid (Node : Node_Type) return Boolean;

  -- Get the line number of the beginning of the declaration of a node
  -- 0 if not the result of parsing of a file
  function Get_Line_No (Ctx  : Ctx_Type;
                        Node : Node_Type) return Line_Range;

  -- Get Prologue of a parsed context (after Parse or Parse_Prologue)
  --  may raise Parse_Error if Parse was not ok
  function Get_Prologue (Ctx : Ctx_Type) return Element_Type;

  -- Get Elements'root, after Parse or Parse_Elements
  --  may raise Status_Error if called before Parse_Elements
  --            Parse_Error if Parse was not ok
  --            No_Root if the Ctx is initialized but no root is defined
  No_Root : exception;
  function Get_Root_Element (Ctx : Ctx_Type) return Element_Type;

  -- Get tail, after Parse or Parse_Elements
  -- The tail contains the Comments and PIs after the root element, if any
  -- Returns a dummy element (with empty name) the children of which are
  --  the Comments and PIs of the tail
  -- May raise Status_Error if called before Parse_Elements
  --            Parse_Error if Parse was not ok
  function Get_Tail (Ctx : Ctx_Type) return Element_Type;

  -- Get Doctype characteristics (prologue must have been parsed)
  --  may raise Parse_Error if Parse was not ok
  Doctype_Not_Set : exception;
  procedure Get_Doctype (Ctx : in Ctx_Type;
       Name    : out As.U.Asu_Us;
       Public  : out Boolean;
       Pub_Id  : out As.U.Asu_Us;
       File    : out As.U.Asu_Us;
       Int_Def : out As.U.Asu_Us);

  ---------------------------------
  -- ELEMENT NAME and ATTRIBUTES --
  ---------------------------------
  -- Get the name of an element
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return String;
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return As.U.Asu_Us;

  -- Get the namespace name of an element (if Ctx is parsed with Namespace
  --  option, otherwise return empty string / Asu_Null)
  function Get_Namespace (Ctx     : Ctx_Type;
                          Element : Element_Type) return String;
  function Get_Namespace (Ctx     : Ctx_Type;
                          Element : Element_Type) return As.U.Asu_Us;

  -- Get the attributes of an element
  function Get_Attributes (Ctx     : Ctx_Type;
                           Element : Element_Type) return Attributes_Array;
  function Get_Nb_Attributes (Ctx     : Ctx_Type;
                              Element : Element_Type) return Natural;
  -- Get one attribute of an element. May raise Invalid_Index
  Invalid_Index : exception;
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Index   : Positive) return Attribute_Rec;
  -- Search by name an attribute of an element
  -- May raise Attribute_Not_Found
  Attribute_Not_Found : exception;
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Name    : String) return String;
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Name    : String) return As.U.Asu_Us;

  --------------------------
  -- PI, TEXT and COMMENT --
  --------------------------
  -- Get the Target of a PI
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return String;
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return As.U.Asu_Us;
  -- Get the data of a PI
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return String;
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return As.U.Asu_Us;

  -- Get text of a Text node
  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type) return String;
  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type)
                     return As.U.Asu_Us;

  -- Because of the parsing logic in Callback mode (see Get_Is_Mixed)
  --  the parsing without DTD may lead to pieces of text that are made
  --  of separators. This allows checking it
  function Is_Separators (Text : String) return Boolean;
  function Is_Separators (Text : As.U.Asu_Us) return Boolean;

  -- Get the content of a comment
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return String;
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type)
                        return As.U.Asu_Us;

  ----------------
  -- NAVIGATION --
  ----------------
  -- Get the children of an element
  function Get_Children (Ctx     : Ctx_Type;
                         Element : Element_Type) return Nodes_Array;
  function Get_Nb_Children (Ctx     : Ctx_Type;
                            Element : Element_Type) return Child_Range;
  -- Get one child of an element
  -- May raise Invalid_Index
  function Get_Child (Ctx     : Ctx_Type;
                      Element : Element_Type;
                      Index   : Child_Index) return Node_Type;

  -- Get a brother of a node
  -- May raise No_Brother
  No_Brother : exception;
  function Has_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Boolean;
  function Get_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Node_Type;

  -- Get the father of a node
  -- May raise No_Parent if Node is the Prologue, the Root_Element or the Tail
  No_Parent : exception;
  function Get_Parent (Ctx  : Ctx_Type;
                       Node : Node_Type) return Element_Type;

  -- Return True if Node is the Prologue, the Root_Element or the Tail
  function Is_Root (Ctx  : Ctx_Type;
                    Node : Node_Type) return Boolean;

  --------------------------
  -- UNPARSED ENTITY info --
  --------------------------
  -- URI, PublicId and associated notation of an unparsed entity
  type Unparsed_Entity_Info_Rec is record
    Entity_System_Id : As.U.Asu_Us;
    Entity_Public_Id : As.U.Asu_Us;
    Notation_Name    : As.U.Asu_Us;
    Notation_System_Id : As.U.Asu_Us;
    Notation_Public_Id : As.U.Asu_Us;
  end record;

  -- Get info on an unparsed entity and its associated notation
  --  may raise Status_Error if Ctx prologue is not Parsed
  --            Unknown_Entity if not such unparsed entity
  Unknown_Entity : exception;
  procedure Get_Unparsed_Entity_Info (Ctx    : in out Ctx_Type;
                                      Entity : in String;
                                      Info   : out Unparsed_Entity_Info_Rec);

  -------------------
  -- Specific TAGS --
  -------------------
  -- An element is Tag_Empty if:
  --  - the element is parsed empty with the EmptyElemTag (</Element>)
  --  - or Generator.Set_Tag_Empty (True) has been called on the element
  -- Otherwise it is Def_Empty if the element is defined as EMPTY in the Dtd
  -- Otherwise it is Not_Empty
  function Get_Empty_Info (Ctx     : Ctx_Type;
                           Element : Element_Type) return Empty_Info_List;

  -- Is this element Mixed: either Mixed in Dtd or its first child is Text
  -- Set by the parsing or by calling Check
  -- Note: Without Dtd and in callback mode, the first child of an element
  --  is the last opportunity to set the Is_Mixed tag on the father
  --  For homogeneity, the same criteria applies on tree mode and in Check
  function Get_Is_Mixed (Ctx     : Ctx_Type;
                         Element : Element_Type) return Boolean;

  -- Update the Is_Mixed tag of each element accorting to ALL its
  --  children: If an element hasn't Is_Mixed set and has a child
  --  of kind Text, then set its tag
  -- Can be used only in tree mode (or after check) and usefull only if no Dtd
  -- WARNING: The children of an element now re-tagged Is_Mixed, have been
  --  parsed without the information that they were part of a mixed element;
  --  for example, line-feeds and indentation have been skipped. Now they will
  --  be processed (displayed) with the new information that the element is
  --  mixed. For example, line-feeds and indentation will not be generated by
  --  the Generator
  -- May raise Status_Error if Ctx is not parsed nor checked
  procedure Update_Is_Mixed (Ctx : in out Ctx_Type);

  ------------------------
  -- General EXCEPTIONS --
  ------------------------
  -- If internal logic error is raise (during parsing)
  Internal_Error : exception;

private

  ---------------
  -- NODE TYPE --
  ---------------
  -- Internal tree
  type Internal_Kind_List is (Element, Text, Pi, Comment, Attribute);
  type My_Tree_Cell is record
    -- Line in source file
    Line_No : Line_Range := 0;
    -- Kind:  Element, Text or Attribute
    Kind : Internal_Kind_List;
    -- Number of attributes when Kind is Element
    Nb_Attributes : Natural := 0;
    -- Element name or Attribute name or text or comment...
    Name : As.U.Asu_Us;
    -- Namespace name of the element or attribute
    Namespace : As.U.Asu_Us;
    -- Attribute value or PI content
    Value : As.U.Asu_Us;
    -- Is this attribute an Unparsed entity or a list of unparsed entities
    Unparsed : Boolean := False;
    -- Put empty element with EmptyElemTag
    Empty_Info : Empty_Info_List := Not_Empty;
    -- Is this element containing Mixed: either Mixed in Dtd
    --  or its first child is text
    Is_Mixed : Boolean := False;
  end record;
  package My_Tree is new Trees.Tree (My_Tree_Cell);

  type Tree_Acc is access all My_Tree.Tree_Type;
  type Branch_List is (Prologue_Br, Elements_Br, Tail_Br);
  -- Exported node type
  Clean_Magic : constant Magic_Numbers.Extended_Magic_Long
              := Magic_Numbers.Magic_Long0;
  type Node_Type (Kind : Node_Kind_List := Element) is record
    -- Magic number of the context
    Magic : Magic_Numbers.Extended_Magic_Long := Clean_Magic;
    -- In prologue or a real element or in tail
    Branch : Branch_List := Prologue_Br;
    -- Position in tree
    Tree_Access : My_Tree.Position_Access := My_Tree.No_Position;
  end record;
  No_Node : constant Node_Type (Element) := (others => <>);

  ---------------------
  -- INPUT FLOW TYPE --
  ---------------------
  -- Longest keywork + <![CDATA[
  Max_Buf_Len : constant := 21;
  -- Don't skip current data from recording
  No_Skip_Rec : constant Long_Longs.Ll_Integer := -1;
  package My_Circ is new Queues.Circ (Character);

  -- Current flow is...
  -- Guess is used for tracing (errors, warnings) and means "gesss from Ctx"
  type Put_Flow_Kind_List is (Guess,
                              Xml_Flow, Dtd_Flow, Int_Dtd_Flow, Ext_Flow);
  subtype Flow_Kind_List is Put_Flow_Kind_List range Xml_Flow .. Ext_Flow;
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
    Name : As.U.Asu_Us;
    -- Current line No
    Line : Line_Range := 0;
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
    -- If Flow is a file (Text_Char)
    File : File_Access;
    -- If Flow is a String
    In_Str : As.U.Asu_Us;
    In_Stri : Natural := 0;
  end record;

  -- Pool of flows (when switching to new flow then back)
  procedure Set (To : out Flow_Info_Type; Val : in Flow_Info_Type);
  package Flow_Pool_Mng is new Long_Long_Limited_Pool (
    Data_Type => Flow_Info_Type, Set => Set);

  -- Pool of files to deallocate
  procedure Set (To : out File_Access; Val : in File_Access);
  package File_Pool_Mng is new Long_Long_Limited_Pool (
    Data_Type => File_Access, Set => Set);

  type Flow_Type is record
    -- To know how many where got before End_Error
    Nb_Got : Natural := 0;
    -- Circular buffer of read characters
    Circ : My_Circ.Circ_Type (Max_Buf_Len);
    -- Error message
    Err_Msg : As.U.Asu_Us;
    -- Current significant string, loaded by Parse_Until_xxx
    Curr_Str : As.U.Asu_Us;
    -- Recorded input characters
    Recording : Boolean := False;
    Skip_Recording : Long_Longs.Ll_Integer := No_Skip_Rec;
    Recorded : As.U.Asu_Us;
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
    Name : As.U.Asu_Us;
    Value : As.U.Asu_Us;
    Parameter : Boolean;
    Internal : Boolean;
    Intern_Dtd : Boolean;
    Parsed : Boolean;
  end record;
  type Entity_Access is access all Entity_Type;
  procedure Set (To : out Entity_Type; Val : in Entity_Type);
  overriding function "=" (Current : Entity_Type;
                           Criteria : Entity_Type) return Boolean;
  function Image (Entity : Entity_Type) return String;
  package H_Entity_List_Mng is new Hashed_List (Entity_Type, Entity_Access,
             Set, "=", Image);
  package Entity_List_Mng is new H_Entity_List_Mng.Unique;

  -- Dtd info rec
  type Info_Rec is record
    -- Kind#Element_name[#Attribute_Name]
    -- Kind is Elt, Atl, Att
    Name : As.U.Asu_Us;
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
    List : As.U.Asu_Us;
    -- Is it defined in external or internal Dtd, and at which line
    Flow_Kind : Flow_Kind_List;
    Line : Line_Range;
  end record;
  type Info_Access is access all Info_Rec;
  procedure Set (To : out Info_Rec; Val : in Info_Rec);
  function Image (Element : Info_Rec) return String;
  overriding function "=" (Current : Info_Rec;
                           Criteria : Info_Rec) return Boolean;
  package H_Info_Mng is new Hashed_List (Info_Rec, Info_Access,
                                         Set, "=", Image);
  package Info_Mng is new H_Info_Mng.Unique;

  -- Unparsed entity or notation info
  type Unparsed_Type is record
    -- Entity or Notation
    Is_Entity : Boolean := True;
    -- Name
    Name : As.U.Asu_Us;
    -- Line where defined
    Flow_Kind : Flow_Kind_List;
    Line_No : Line_Range := 0;
    -- Ids
    System_Id : As.U.Asu_Us;
    Public_Id : As.U.Asu_Us;
    -- Notation name (when this is an entity)
    Notation : As.U.Asu_Us;
  end record;

  -- A dtd info
  type Dtd_Type is record
    -- Is there a dtd set, otherwise check is always ok
    Set : Boolean := False;
    -- Is there already xml instruction found in the dtd
    Xml_Found : Boolean := False;
    -- Encoding directive of dtd
    Encoding :  As.U.Asu_Us;
    -- Parsed info
    Info_List : Info_Mng.Unique_List_Type;
    -- Parsed entities
    Entity_List : Entity_List_Mng.Unique_List_Type;
    -- Notation attributes: #Elt##Attr#Elt##Attr#...
    Notation_Attrs : As.U.Asu_Us;
    -- Internal elements #@Elt# or attributes #Elt##Attr#
    -- ELEMENT or ATTLIST defined in internal dtd
    Internals : As.U.Asu_Us;
    -- Are we in an INCLUDE directive? Count levels
    Include_Level : Natural := 0;
  end record;

  ------------------------------------------------------------
  -- IDs, IDREFs, UNPARSED ENTITIES, NAMESPACES and DOCTYPE --
  ------------------------------------------------------------
  type Id_Cell is record
    -- Line where the ID or IDREF is defined
    Line_No : Line_Range := 0;
    -- ID name
    Name : As.U.Asu_Us;
  end record;
  type Id_Cell_Access is access all Id_Cell;
  procedure Set (To : out Id_Cell;  Val : in Id_Cell);
  function Image (Element : Id_Cell) return String;
  overriding function "=" (Current : Id_Cell;
                           Criteria : Id_Cell) return Boolean;

  -- Unique list of IDs
  package H_Id_List_Mng is new Hashed_List (Id_Cell, Id_Cell_Access,
                                          Set, "=", Image);
  package Id_List_Mng is new H_Id_List_Mng.Unique;
  type Id_List_Access is access Id_List_Mng.Unique_List_Type;

  -- List of IDREFs found
  package Idref_List_Mng renames Id_List_Mng;
  subtype Idref_List_Access is Id_List_Access;

  -- Unparsed entities
  type Unparsed_Access is access all Unparsed_Type;
  procedure Set (To : out Unparsed_Type; Val : in Unparsed_Type);
  overriding function "=" (Current : Unparsed_Type;
                           Criteria : Unparsed_Type) return Boolean;
  function Image (Unparsed : Unparsed_Type) return String;
  package H_Unparsed_List_Mng is new Hashed_List (Unparsed_Type,
             Unparsed_Access, Set, "=", Image);
  package Unparsed_List_Mng is new H_Unparsed_List_Mng.Unique;

  -- Namespaces
  type Namespace_Type is record
    Prefix : As.U.Asu_Us;
    Namespace : As.U.Asu_Us;
  end record;
  type Namespace_Access is access all Namespace_Type;
  procedure Set (To : out Namespace_Type; Val : in Namespace_Type);
  overriding function "=" (Current : Namespace_Type;
                           Criteria : Namespace_Type) return Boolean;
  function Image (Namespace : Namespace_Type) return String;
  package Namespace_List_Mng is new Hashed_List (Namespace_Type,
             Namespace_Access, Set, "=", Image);

  -- Doctype info
  type Doctype_Type is record
    -- Line of the DOCTYPE directive
    Line_No : Line_Range := 0;
    -- Name, file (ID+URI) and internal definition if any
    -- Empty name for no DOCTYPE
    Name    : As.U.Asu_Us;
    Public  : Boolean := False;
    Pub_Id  : As.U.Asu_Us;
    File    : As.U.Asu_Us;
    Int_Def : As.U.Asu_Us;
  end record;

  ------------------
  -- CONTEXT TYPE --
  ------------------
  type Ctx_Type is limited new Ada.Finalization.Limited_Controlled with record
    Status  : Ctx_Status_List := Clean;
    Magic : Magic_Numbers.Extended_Magic_Long := Clean_Magic;
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
    -- Make text "compatible"
    Compatible : Boolean := False;
    -- List of Elements with "xml:space 'preserve'" in dtd
    Preserved : As.U.Asu_Us;
    -- Use Dtd
    Use_Dtd : Boolean := True;
    Dtd_File : As.U.Asu_Us;
    -- Check and fill namespaces
    Namespace : Boolean := False;
    -- Check also and report warnings
    Warnings : Warning_Callback_Access := null;
    -- Call a callback i.o. feeding trees
    Callback : Parse_Callback_Access := null;
    Level : Natural := 0;
    -- Prologue, parsed elements and tail
    Stage : Stage_List := Prologue;
    Prologue : Tree_Acc := new My_Tree.Tree_Type;
    Elements : Tree_Acc := new My_Tree.Tree_Type;
    Tail     : Tree_Acc := new My_Tree.Tree_Type;
    -- Doctype name, file and a tag of internal definitions
    Doctype : Doctype_Type;
    -- Standalone
    Standalone : Boolean := False;
    -- Unique list of Ids
    Ids : Id_List_Access := new Id_List_Mng.Unique_List_Type;
    -- List of Idrefs
    Idrefs : Idref_List_Access := new Idref_List_Mng.Unique_List_Type;
    -- Unparsed entities and Notations
    Unparsed_List : Unparsed_List_Mng.Unique_List_Type;
    -- Namespaces
    Namespace_List : Namespace_List_Mng.List_Type;
  end record;

  ------------------
  -- FINALIZATION --
  ------------------
  overriding procedure Finalize (Ctx : in out Ctx_Type);

  overriding procedure Finalize (Node : in out Node_Update);
  overriding procedure Adjust   (Node : in out Node_Update);

  --------------------------
  -- OPERATIONS for CHILDREN
  --------------------------
  -- For Xml_Generator
  function Get_Magic return Magic_Numbers.Magic_Long;
  function Get_Tree (Ctx : Ctx_Type;
                     Node : Node_Type) return Tree_Acc;
  function Name_Ok (Name : As.U.Asu_Us;
                    Allow_Token : Boolean := False) return Boolean;
  function Is_Valid_Encoding (Name : As.U.Asu_Us) return Boolean;
  function Is_Valid_Pubid (Name : As.U.Asu_Us) return Boolean;

  -- For Xml_Generator and internal
  -- If Name is empty then return Name
  -- Otherwise return Namespace^NameSuffix
  function Expand_Name (Name, Namespace : As.U.Asu_Us) return As.U.Asu_Us;

end Xml_Parser;

