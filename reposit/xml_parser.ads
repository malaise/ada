with Ada.Strings.Unbounded, Ada.Finalization;
with Queues, Trees, Unique_List, Text_Char;
-- Parse Xml file or string, and provide read access to the corresponding tree
-- The following features of DTD are not supported (parsing error):
--  - ENTITY, ENTITIES and NOTATION attribute type
--  - SYSTEM and PUBLIC external entity
--  - NOTATION directive
-- The following limitations apply to the DOCTYPE directive of xml:
--  - Only the system URI of the DOCTYPE is used, PUBLIC Id (if any) is skipped.
--  - Only local file reference is fetched, no http :-) (parsing error)
-- The following restrictions applies to all the parsing:
--  - The Xml versions (1.0 or 1.1) are not checked; 1.1 applies.
--  - CDATA sections are detected only when a markup ('<') is expected or
--    within text. Not "anywhere character data may occur" (parsing error).
--  - The detection and expansion of parameter entity references may not be
--    complete.
package Xml_Parser is

  -- Version incremented at each significant change
  Major_Version : constant String := "4";
  function Version return String;

  -----------
  -- TYPES --
  -----------
  -- Generic node type
  -- A node is either an element or a text or a comment
  type Node_Kind_List is (Element, Text, Comment);
  type Node_Type (Kind : Node_Kind_List := Element) is private;

  -- Element type
  subtype Element_Type is Node_Type(Element);
  -- A Text
  subtype Text_Type is Node_Type(Text);
  -- A Comment
  subtype Comment_Type is Node_Type(Comment);

  -- An attribute of an element
  type Attribute_Rec is record
    Name : Ada.Strings.Unbounded.Unbounded_String;
    Value : Ada.Strings.Unbounded.Unbounded_String;
  end record;
  -- The attributes of an element
  type Attributes_Array is array (Positive range <>) of Attribute_Rec;

   -- Number of children of an element
  subtype Child_Range is Trees.Child_Range;
  subtype Child_Index is Child_Range range 1 .. Child_Range'Last;
  -- The children of an element
  type Nodes_Array is array (Child_Index range <>) of Node_Type;

  -- A parsing context
  type Ctx_Type is tagged limited private;


  -----------------------------
  -- NOTE ABOUT THE PROLOGUE --
  -----------------------------
  -- In xml V1.0, the prologue consists of an optional xml directive
  --  (<?xml attributes?>) then optional processing instructions
  --  (<?name text?>), DOCTYPE and comments.
  -- In Xml V1.1 the xml directive and version is mandatory.
  -- So the Prologue is an element of name "xml" with attributes (no
  --  attributes if no xml directive). Its children are:
  --  for PIs: elements each with the directive name
  --  for Comments: comments
  --  for the doctype: an empty text

  ----------------------------
  -- NOTE ABOUT THE DOCTYPE --
  ----------------------------
  -- The DOCTYPE is parsed during the prologue parsing, it can be retrieved
  --  when the Prologue has a child of type text (empty)
  -- PUBLIC directive is not processed

  ------------------
  -- FILE PARSING --
  ------------------
  -- Parse a Xml file, stdin if empty
  -- On option, allows retrieval of comments (usefull for formatter)
  -- On option, does not expand General entities nor set attributes with
  --  default values (usefull for formatter)
  -- On option does not check compliance with Dtd
  -- On option force a dtd file different from DOCTYPE directive
  -- May raise File_Error if error accessing the File_Name,
  --           Status_Error if Ctx is not clean
  Stdin : constant String := "";
  procedure Parse (Ctx       : out Ctx_Type;
                   File_Name : in String;
                   Ok        : out Boolean;
                   Comments  : in Boolean := False;
                   Expand    : in Boolean := True;
                   Use_Dtd   : in Boolean := True;
                   Dtd_File  : in String  := "");
  File_Error, Status_Error : exception;

  -- Return the error message if Parse error
  -- May raise Status_Error if Ctx is clean
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String;

  -- Clean parsing context, when the Prologue and Element trees
  --  are not used any more
  procedure Clean (Ctx : in out Ctx_Type);


  --------------------
  -- STRING PARSING --
  --------------------
  -- Parse a Dtd
  -- may raise Status_Error if Dtd is not clean
  --           Parse_Error if error while parsing the dtd
  type Dtd_Type is limited private;
  procedure Parse_Dtd_File (File_Name : in String;
                            Dtd       : out Dtd_Type);
  procedure Parse_Dtd_String (Str : in String;
                              Dtd : out Dtd_Type);
  Parse_Error : exception;

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type);

  -- Parse the prologue of a string
  -- Then one can call Get_Prologue on Ctx
  --  (Calling Get_Root_Element will raise Use_Error);
  -- On option, allows retrieval of comments (usefull for formatter)
  -- On option, does not expand General entities (usefull for formatter)
  -- may raise Status_Error if Ctx is not clean
  procedure Parse_Prologue (Ctx      : out Ctx_Type;
                            Str      : in String;
                            Ok       : out Boolean;
                            Comments : in Boolean := False;
                            Expand   : in Boolean := True);

  -- Parse the elements (after the prologue) of a string with a dtd
  -- may raise Status_Error if Ctx is clean
  --           End_Error if Ctx has already parsed elements
  --           Parse_Error if Parse_Prologue was not ok
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
                   Ok  : out Boolean);

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
       Name    : out Ada.Strings.Unbounded.Unbounded_String;
       Public  : out Boolean;
       Pub_Id  : out Ada.Strings.Unbounded.Unbounded_String;
       File    : out Ada.Strings.Unbounded.Unbounded_String;
       Int_Def : out Ada.Strings.Unbounded.Unbounded_String);

  -- Get a PI data (use Get_Name to get the PITarget)
  -- May raise Invalid_Node if not is not of the prologue
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Element_Type) return String;
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Element_Type)
           return Ada.Strings.Unbounded.Unbounded_String;

  -- Get the line number of the beginning of the declaration of a node
  -- 0 if not the result of parsing of text
  function Get_Line_No (Ctx  : Ctx_Type;
                        Node : Node_Type) return Natural;

  -- Get the name of an element
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return String;
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type)
                    return Ada.Strings.Unbounded.Unbounded_String;
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
                     return Ada.Strings.Unbounded.Unbounded_String;
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return String;
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type)
                        return Ada.Strings.Unbounded.Unbounded_String;

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
  type Internal_Kind_List is (Element, Text, Comment, Attribute);
  type My_Tree_Cell is record
    -- Line in source file
    Line_No : Natural := 0;
    -- Kind:  Element, Text or Attribute
    Kind : Internal_Kind_List;
    -- Number of attributes when Kind is Element
    Nb_Attributes : Natural := 0;
    -- Element name or Attribute name or text or comment
    Name : Ada.Strings.Unbounded.Unbounded_String;
    -- Attribute value
    Value : Ada.Strings.Unbounded.Unbounded_String;
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

  ---------------------
  -- INPUT FLOW TYPE --
  ---------------------
  -- Longest keywork + <![CDATA[
  Max_Buf_Len : constant := 21;
  -- Don't skip current data from recording
  No_Skip_Rec : constant Integer := -1;
  package My_Circ is new Queues.Circ (Max_Buf_Len, Character);
  type Flow_Kind_List is (Xml_File, Xml_String, Dtd_File);
  type Flow_Type is record
    -- Is the flow a file or a string
    Kind : Flow_Kind_List := Xml_File;

    -- To know how many where got before End_Error
    Nb_Got : Natural;
    -- Circular buffer of read characters
    Circ : My_Circ.Circ_Type;
    -- Current line of input and in dtd
    Xml_Line : Natural := 0;
    Dtd_Line : Natural := 0;
    -- Error message
    Err_Msg : Ada.Strings.Unbounded.Unbounded_String;
    -- Current significant string, loaded by Parse_Until_xxx
    Curr_Str : Ada.Strings.Unbounded.Unbounded_String;
    -- Recorded input characters
    Recording : Boolean := False;
    Skip_Recording : Integer := No_Skip_Rec;
    Recorded : Ada.Strings.Unbounded.Unbounded_String;
    -- Saved line of input (when switching to dtd file and back)
    -- Inputs flows: string and index, or files
    In_Str : Ada.Strings.Unbounded.Unbounded_String;
    In_Stri : Natural := 0;
    Xml_File : Text_Char.File_Type;
    Dtd_File : Text_Char.File_Type;
  end record;

  --------------
  -- DTD TYPE --
  --------------
  -- The stored entities
  type Entity_Type is record
    Parameter : Boolean;
    Name : Ada.Strings.Unbounded.Unbounded_String;
    Value : Ada.Strings.Unbounded.Unbounded_String;
  end record;
  type Entity_Access is access all Entity_Type;
  procedure Set (To : out Entity_Type; Val : in Entity_Type);
  function Image (Entity : Entity_Type) return String;
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean;
  package Entity_List_Mng is new Unique_List (Entity_Type, Entity_Access,
             Set, Image, "=");

  -- Dtd info rec
  type Info_Rec is record
    -- Kind'Img#Element_name[#Attribute_Name]
    Name : Ada.Strings.Unbounded.Unbounded_String;
    -- Elt: Possible children, first chars is <type> ::= E|A|M|C
    --  (empty, any, mixed or children), then
    --  for Mixed the list of "#<name>#<name>#" without #PCDATA
    --   (empty if only #PCDATA)
    --  for Children the regexp of "#<name>#"
    -- Atl: Possible attributes, list of "#<name>##<type><default>#"
    --  <type> ::= S|I|R|r|T|t|E (String, ID, IDREF, IDREFS, NMTOKEN, NMTOKENS
    --   or enum)
    --  <default> ::= R|I|F|D (required, implied, fixed or default)
    -- Att: for a fixed of any type or the a default of not enum, the value
    --   for an Enum, the list of possible "<name>#" and, if there is a default
    --   this value is the first
    List : Ada.Strings.Unbounded.Unbounded_String;
  end record;

  -- Unique list of Info_Rec
  type Info_Access is access all Info_Rec;
  procedure Set (To : out Info_Rec; Val : in Info_Rec);
  function Image (Element : Info_Rec) return String;
  function "=" (Current : Info_Rec; Criteria : Info_Rec) return Boolean;
  package Info_Mng is new Unique_List (Info_Rec, Info_Access, Set, Image, "=");

  type Dtd_Type is record
    -- Is there a dtd set, otherwise check is always ok
    Set : Boolean := False;
    -- Is there already xml instruction found in the dtd
    Xml_Found : Boolean := False;
    -- Encoding directive of dtd
    Encoding :  Ada.Strings.Unbounded.Unbounded_String;
    -- Parsed info
    Info_List : Info_Mng.List_Type;
    -- Parsed entities
    Entity_List : Entity_List_Mng.List_Type;
    -- Are we in an INCLUDE directive
    In_Include : Boolean := False;
  end record;

  ------------------
  -- DOCTYPE info --
  ------------------
  type Doctype_Type is record
    -- Line of the DOCTYPE directive
    Line_No : Natural := 0;
    -- Name, file (ID+URI) and internal definition if any
    -- Empty name for no DOCTYPE
    Name    : Ada.Strings.Unbounded.Unbounded_String;
    Public  : Boolean := False;
    Pub_Id  : Ada.Strings.Unbounded.Unbounded_String;
    File    : Ada.Strings.Unbounded.Unbounded_String;
    Int_Def : Ada.Strings.Unbounded.Unbounded_String;
  end record;
    
  ------------------
  -- CONTEXT TYPE --
  ------------------
  type Ctx_Status_List is (Clean, Parsed_Prologue, Parsed_Elements, Error,
                           Init);
  type Ctx_Type is limited new Ada.Finalization.Limited_Controlled with record
    Status  : Ctx_Status_List := Clean;
    Magic : Float := Clean_Magic;
    -- Input flow description
    Flow : Flow_Type;
    -- Parse or skip comments
    Parse_Comments : Boolean := False;
    -- Expand or not general entities and attributes with default values
    Expand : Boolean := True;
    -- Use Dtd
    Use_Dtd : Boolean := True;
    Dtd_File : Ada.Strings.Unbounded.Unbounded_String;
    -- Prologue and parsed elements
    Prologue : Tree_Acc := new My_Tree.Tree_Type;
    Elements : Tree_Acc := new My_Tree.Tree_Type;
    -- Doctype name, file and a tag of internal definitions
    Doctype : Doctype_Type;
  end record;
  procedure Finalize (Ctx : in out Ctx_Type) renames Clean;

  -- For Xml_Generator
  function Get_Magic return Float;
  function Get_Tree (Ctx : Ctx_Type;
                     Node : Node_Type) return Tree_Acc;
end Xml_Parser;

