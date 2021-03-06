-- Generates a Xml file (or stdout) or string from a tree that has been either
--  parsed or build by application, or parsed then modified by application
package Xml_Parser.Generator is

  -- Version incremented at each significant change
  Major_Version : constant String := "19";
  function Version return String;

  -- The context, possibly filled by parsing a file or string in tree mode
  --  possibly filled or modified by unsing the services hereafter
  type Ctx_Type is new Xml_Parser.Ctx_Type with private;

  ----------------------------
  -- SET OR MODIFY THE TREE --
  ----------------------------
  ----------------------------------
  -- Prologue specific operations --
  ----------------------------------
  -- Commands valid only on or under the prologue
  -- Set XML version (1.0 or 1.1)
  -- If version is not set, then "1.0" will be assumed
  subtype Major_Range is Positive range 1 .. 1;
  subtype Minor_Range is Natural range 0 .. 1;
  procedure Set_Version (Ctx   : in out Ctx_Type;
                         Major : in Major_Range;
                         Minor : in Minor_Range);

  -- Set the encoding and the standalone of XML directive
  -- May raise Invalid_Argument if Encoding is not a valid encoding name
  procedure Set_Encoding (Ctx : in out Ctx_Type;
                          Encoding : in String);
  procedure Set_Standalone (Ctx  : in out Ctx_Type;
                            Standalone : in Boolean);

  -- Clear Xml information (attributes)
  procedure Clear_Xml (Ctx : in out Ctx_Type);

  -- General policy for adding nodes to the prologue:
  ---------------------------------------------------
  -- If Node is the prologue then append child as its last child by default,
  --  or as its first child if not Append_Next.
  -- If Node is a child of the prologue then append as its next brother by
  --  default, or as its previous brother if not Append_Next.

  -- Set the DOCTYPE text
  -- May raise Invalid_Argument if
  --  - Pub_Id is not a valid PublicId
  --  - File is not a valid PublicId
  --  - Pub_Id is not a valid System literal (i.e. contains ''' and '"')
  --  - Pub_Id is set but Public is False
  -- Int_Def is not parsed (will be parsed by Xml_Parser.Check)
  -- May raise Doctype_Already_Set if Doctype already set
  -- May raise Invalid_Node if Node is not of prologue
  Doctype_Already_Set : exception;
  procedure Add_Doctype (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Name     : in String;
                         Public   : in Boolean;
                         Pub_Id   : in String;
                         File     : in String;
                         Int_Def  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True);

  -- Add a processing instruction in the prologue
  -- Pi shall have the the form
  --  "[ <spaces> ] <PITarget> [ <spaces> <Pi_Data> ]"
  -- May raise Invalid_Argument if
  --  - PITarget is not a valid name
  --  - Pi_Data is not a valid Pi dada (i.e. contains "?>")
  -- May raise Invalid_Node if Node is not of prologue
  procedure Add_Pi (Ctx      : in out Ctx_Type;
                    Node     : in Node_Type;
                    Pi       : in String;
                    New_Node : out Node_Type;
                    Append_Next : in Boolean := True);

  -- Add a comment in prologue, valid only for comments in the prologue
  -- May raise Invalid_Argument if Comment is not valid (i.e. contains "--")
  -- May raise Invalid_Node if Node is not of prologue
  procedure Add_Comment (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Comment  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True);

  -- Clear the whole prologue
  procedure Clear_Prologue (Ctx : in out Ctx_Type);

  -------------------------------------------
  -- Elements and Tail specific operations --
  -------------------------------------------
  -- May raise Invalid_Node if used in Prologue
  --  or if Adding an Element or Text to the Tail

  -- Insert a child element, text, Pi or comment, and move to it
  -- For a Text, the Name is the text
  -- For a Pi the Name is "<PITarget> [ <spaces> <Pi_Data> ]"
  -- For a Comment the Name is the comment
  -- May raise Invalid_Argument if Name is not valid for the Kind (see
  --  Set_Name/Pi/Text/Comment)
  -- May raise Invalid_Node if in Prologue
  --   or if Kind is an Element or Text in Tail
  procedure Add_Child (Ctx      : in out Ctx_Type;
                       Element  : in Element_Type;
                       Name     : in String;
                       Kind     : in Node_Kind_List;
                       New_Node : out Node_Type;
                       Append   : in Boolean := True);

  -- Insert a brother element, text, Pi or comment, and move to it
  -- For a Text, the Name is the text
  -- For a Pi the Name is "<PITarget> [ <spaces> <Pi_Data> ]"
  -- For a Comment the Name is the comment
  -- May raise Invalid_Argument if Name is not valid for the Kind (see
  --  Set_Name/Pi/Text/Comment)
  -- May raise Invalid_Node if in Prologue
  -- May raise Invalid_Node if in Prologue
  --   or if Kind is an Element or a Text in Tail
  --   or adding a brother to the root element or the the tail element
  procedure Add_Brother (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Name     : in String;
                         Kind     : in Node_Kind_List;
                         New_Node : out Node_Type;
                         Next     : in Boolean := True);

  -- Set/change the PITarget and data of a Pi
  -- Content must have the form "<PITarget> [ <spaces> <Pi_Data> ]"
  -- May raise Invalid_Argument if
  --  - PITarget is not a valid name
  --  - Pi_Data is not a valid Pi dada (i.e. contains "?>")
  procedure Set_Pi (Ctx     : in out Ctx_Type;
                    Pi    : in out Pi_Type;
                    Content : in String);

  -- Set/change the text of a Comment
  -- May raise Invalid_Argument if Content is no valid (i.e. contains "--")
  procedure Set_Comment (Ctx     : in out Ctx_Type;
                         Comment : in out Comment_Type;
                         Content : in String);

  ----------------------------------
  -- Elements specific operations --
  ----------------------------------
  -- May raise Invalid_Node if used in Prologue

  -- Set (change) the name of an element
  -- May raise Invalid_Argument if Name is not valid
  -- May raise Invalid_Node if in Tail
  procedure Set_Name (Ctx     : in out Ctx_Type;
                      Element : in out Element_Type;
                      Name    : in String);

  -- Set all the attributes of an element
  -- May raise Invalid_Argument if a name is not valid
  --  or a value is not valid (i.e. mixes '"' and ''', contains '<' or
  --  contains an invalid reference to entity or char)
  -- May raise Invalid_Node if in Tail
  procedure Set_Attributes (Ctx        : in out Ctx_Type;
                            Element    : in out Element_Type;
                            Attributes : in Attributes_Array);

  -- Delete the attributes of an element
  -- May raise Invalid_Node if in Tail
  procedure Del_Attributes (Ctx     : in out Ctx_Type;
                            Element : in out Element_Type);

  -- Add an attribute to current element
  -- May raise Invalid_Argument if Name or Value is not valid (see
  --  Set_Attributes)
  -- May raise Invalid_Node if in Tail
  procedure Add_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String);

  -- Set the value of an attribute of current element
  -- May raise Invalid_Argument if Name or Value is not valid (see
  --  Set_Attributes)
  -- May raise Attribute_Not_Found if Name is not found
  -- May raise Invalid_Node if in Tail
  procedure Set_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String);

  -- Delete an attribute of current element
  -- May raise Invalid_Argument if Name is not valid
  -- May raise May raise Attribute_Not_Found if Name is not found
  -- May raise Invalid_Node if in Tail
  procedure Del_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name : in String);

  -- Swap two elements (and their children)
  -- May raise Invalid_Node if one is in Prologue or in Tail
  -- May raise Invalid_Node is one is ancestor of the other
  procedure Swap (Ctx  : in out Ctx_Type;
                  Elt1 : in out Element_Type;
                  Elt2 : in out Element_Type);

  -- Copy Src element (and its children) as Next (or prev) Child (or brother)
  --  of Dst
  -- May raise Invalid_Node if one is in Prologue or in Tail
  -- May raise Invalid_Node is Src is ancestor of the Dst
  procedure Copy (Ctx      : in out Ctx_Type;
                  Src      : in Element_Type;
                  Dst      : in Element_Type;
                  New_Node : out Node_Type;
                  Child    : in Boolean := True;
                  Next     : in Boolean := True);

  -- Set the Empty_Info of the element to Tag_Empty
  -- Shall the Element, if empty, be put with EmptyElemTag (<element/>) or
  --  with STag and ETag (<element></elememt>)
  -- By default it is Def_Empty or Not_Empty (depending on Dtd) except if
  --  - Parsed element is empty with EmptyElemTag (</element>)
  --  - or Generator.Set_Tag_Empty (True) is called on the element
  -- May raise Invalid_Node if in Prologue or in Tail
  procedure Set_Tag_Empty (Ctx        : in out Ctx_Type;
                           Element    : in out Element_Type;
                           Tag_Empty  : in Boolean);


  -- Set/change the text of a Text element
  -- May raise Invalid_Argument if Content contains
  -- - Invalid CData sections
  -- - Invalid references
  -- May raise Invalid_Node if in Prologue or in Tail
  procedure Set_Text (Ctx     : in out Ctx_Type;
                      Text    : in out Text_Type;
                      Content : in String);

  -----------------------
  -- Common operations --
  -----------------------
  -- Delete current node and its children, return its father
  -- May raise Invalid_Node if being the Prologue or Element root
  procedure Delete_Node (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         New_Node : out Node_Type);

  -- Delete all children of current element
  procedure Delete_Children (Ctx     : in out Ctx_Type;
                             Element : in out Element_Type);

  ----------------
  -- Exceptions --
  ----------------
  -- When moving outside the tree (father of root..,
  --  to many brothers or children)
  No_Element : exception;
  -- When invalid value of format of name, attribute, text...
  Invalid_Argument : exception;

  ----------------
  -- CONVERSION --
  ----------------
  -- Convert a Text for XML
  -- Outside CDATA sections, replace "&" by "&amp;", "<" by "&lt;"
  --  and "]]>" by "]]&gt;"
  function Text2Xml (Str : String) return String;
  -- Convert Text from Xml
  -- Outside CDATA sections, replace "&amp;" by "&", "&lt;" by "<"
  --  and "&gt;" by ">"
  function Xml2Text (Str : String) return String;

  -- Convert an attribute value for Xml
  -- Replace any "'" by "&apos;" and any """" by "&quot;"
  function Attr2Xml (Str : String) return String;
  -- Convert an attribute value from Xml
  -- Replace any "&apos;" by "'" and any "&quot;" by """"
  function Xml2Attr (Str : String) return String;

  -- Convert the content of a context, after it has been checked with expand
  --  so that it can be put/set as a valid Xml flow:
  -- Apply Text2Xml to each Text node and Attr2Xml to each attribute value.
  procedure Tree2Xml (Ctx : in out Ctx_Type);

  ----------------
  -- GENERATION --
  ----------------
  -- Kind of output format
  -- Raw is all in one physical line and no extra space
  -- Fill_Width is formated, with several attributes per line up to width
  -- One_Per_Line is formated, with one attribute per line
  type Format_Kind_List is (Raw, Fill_Width, One_Per_Line);
  Default_Width : constant Natural := 80;
  Infinite_Width : constant Natural := 0;
  type Format_Definition is record
    -- Kind of output format
    Kind : Format_Kind_List := Fill_Width;
    -- Split (non empty) element that has no child on two lines
    -- Significant only when not Raw
    Split_No_Child : Boolean := False;
    -- Width for element and attributes
    -- Significant only when Fill_Width
    Width : Natural := Default_Width;
  end record;

  Default_Format : constant Format_Definition := (others => <>);
  -- Put in a file (stdout if name is empty)
  -- It is important (but not mandatory) that the Ctx has been checked
  --  (Xml_Parser.Check) before being put if new nodes have been inserted
  --  so that their Is_Mixed attribute is set correctly
  -- Raises File_Error if Pb with file
  Stdout : constant String := "";
  procedure Put (Ctx       : in out Ctx_Type;
                 File_Name : in String;
                 Format    : in Format_Definition := Default_Format;
                 Namespace : in Boolean := False);

  -- Dumps in a string
  -- Same as for Put, the Ctx should have been checked if new nodes
  function Put (Ctx       : Ctx_Type;
                Format    : Format_Definition := Default_Format;
                Namespace : Boolean := False) return String;
  procedure Put (Ctx       : in Ctx_Type;
                 Str       : out As.U.Asu_Us;
                 Format    : in Format_Definition := Default_Format;
                 Namespace : in Boolean := False);


  -- Put a node update image in a string
  function Image (Ctx    : Xml_Parser.Ctx_Type;
                  Update : Node_Update;
                  Format : Format_Definition := Default_Format;
                  Namespace : Boolean := False) return String;
  procedure Image (Ctx       : in Xml_Parser.Ctx_Type;
                   Update    : in Node_Update;
                   Str       : out As.U.Asu_Us;
                   Format    : in Format_Definition := Default_Format;
                   Namespace : in Boolean := False);

private
  type Ctx_Type is new Xml_Parser.Ctx_Type with null record;
end Xml_Parser.Generator;

