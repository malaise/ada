with Ada.Strings.Unbounded;
with Trees, Unique_List;
package Xml_Parser is

  -----------
  -- TYPES --
  -----------
  -- Generic node type
  -- A node is either an element or a text
  type Node_Kind_List is (Element, Text);
  type Node_Type (Kind : Node_Kind_List := Element) is private;

  -- Element type
  subtype Element_Type is Node_Type(Element);

  -- A Text
  subtype Text_Type is Node_Type(Text);

  -- An attribute of an element
  type Attribute_Rec is record
    Name : Ada.Strings.Unbounded.Unbounded_String;
    Value : Ada.Strings.Unbounded.Unbounded_String;
  end record;
  -- The attributes of an element
  type Attributes_Array is array (Positive range <>) of Attribute_Rec;

  -- The children of an element
  type Nodes_Array is array (Positive range <>) of Node_Type;

  -- All operations except Parse may raise Invalid_Node
  -- If the Element has not been returned by Parse, Get_xxx...

  -------------
  -- PARSING --
  -------------
  -- Parse a Xml file, stdin if empty
  -- May raise File_Error if error accessing the File_Name,
  --   Parse_Error if error while parsing the file (or its dtd)
  procedure Parse (File_Name : in String;
                   Prologue     : out Element_Type;
                   Root_Element : out Element_Type);
  File_Error, Parse_Error : exception;
  -- Return the error message if Parse_Error
  function Get_Parse_Error_Message return String;

  -- Clean a parsed trees
  -- May raise Not_Root if one of elements not root
  Not_Root : exception;
  procedure Clean (Prologue     : in out Element_Type;
                   Root_Element : in out Element_Type);

  -- Get the line number of the beginning of the declaration of a node
  function Get_Line_No (Node : Node_Type) return Positive;


  -----------------------------
  -- NOTE ABOUT THE PROLOGUE --
  -----------------------------
  -- In xml V1.0, the prologue consists of an optional xml directive
  --  (<?xml attributes?>) then optional processing instructions
  --  (<?name text?>).
  -- So the Prologue is an element with attributes (empty name and no
  --  attributes if no xml directive). Its children are elements,
  --  each with the directive name and each with a text child.


  -------------------------
  -- NAME AND ATTRIBUTES --
  -------------------------
  -- Get the name of an element
  function Get_Name (Element : in Element_Type)
                    return Ada.Strings.Unbounded.Unbounded_String;
  -- Get the attributes of an element
  function Get_Attributes (Element : in Element_Type) return Attributes_Array;
  function Get_Nb_Attributes (Element : in Element_Type) return Natural;
  -- May raise Invalid_Index
  function Get_Attribute (Element : in Element_Type;
                          Index   : in Positive) return Attribute_Rec;

  ----------------
  -- NAVIGATION --
  ----------------
  -- Get the Children of an element (elements or texts)
  function Get_Children (Element : in Element_Type) return Nodes_Array;
  function Get_Nb_Children (Element : in Element_Type) return Natural;
  -- May raise Invalid_Index
  function Get_Child (Element : in Element_Type;
                      Index   : in Positive) return Node_Type;

  -- Get the father of an element
  -- May raise No_Parent if Element is the Root_Element or the Prologue
  No_Parent : exception;
  function Get_Parent (Element : in Element_Type) return Node_Type;
  function Is_Root (Element : in Element_Type) return Boolean;

  ----------
  -- TEXT --
  ----------
  function Get_Text (Text : in Text_Type) return String;
  function Get_Text (Text : in Text_Type)
                    return Ada.Strings.Unbounded.Unbounded_String;

  --------------------
  -- STRING PARSING --
  --------------------
  type Dtd_Type is limited private;

  ----------------
  -- EXCEPTIONS --
  ----------------
  -- Raised by all operations except parsing
  Invalid_Node : exception;
  -- Raised by Get_Attribute or Get_Child
  Invalid_Index : exception;
  -- If internal logic error
  Internal_Error : exception;

private

  ---------------
  -- NODE TYPE --
  ---------------
  -- Internal internal tree
  type Internal_Kind_List is (Element, Text, Attribute);
  type My_Tree_Cell is record
    -- Line in source file
    Line_No : Positive := 1;
    -- Kind:  Element, Text or Attribute
    Kind : Internal_Kind_List;
    -- Number of attributes when Kind is Element
    Nb_Attributes : Natural := 0;
    -- Element name or Attribute name or text
    Name : Ada.Strings.Unbounded.Unbounded_String;
    -- Attribute value
    Value : Ada.Strings.Unbounded.Unbounded_String;
  end record;
  package My_Tree is new Trees.Tree(My_Tree_Cell);

  -- Exported node type
  type Node_Type (Kind : Node_Kind_List := Element) is record
    Tree_Access : My_Tree.Position_Access := My_Tree.No_Position;
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
    -- Atl: Possible attributes, list of "<name>#<type><default>#"
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
    Set : Boolean;
   -- Is there already xml instruction found in the dtd
    Xml_Found : Boolean;
    -- Parsed info
    Info_List : Info_Mng.List_Type;
    -- Parsed entities
    Entity_List : Entity_List_Mng.List_Type;
  end record;

end Xml_Parser;

