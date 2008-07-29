with Ada.Strings.Unbounded, Ada.Finalization;
with Trees;
-- Generates a Xml file (or stdout), or string from a tree
package Xml_Parser.Generator is

  Version : constant String := "V4.1";

  type Ctx_Type is new Xml_Parser.Ctx_Type with private;

  ----------------------------------
  -- PROLOGUE SPECIFIC OPERATIONS --
  ----------------------------------
  -- Set version
  subtype Major_Range is Positive range 1 .. 1;
  subtype Minor_Range is Natural range 0 .. 1;
  procedure Set_Version (Ctx   : in out Ctx_Type;
                         Major : in Major_Range;
                         Minor : in Minor_Range);

  -- Set the encoding and the standalone of XML directive
  -- If version is not set, then "1.0" is assumed
  procedure Set_Encoding (Ctx : in out Ctx_Type;
                          Encoding : in String);
  procedure Set_Standalone (Ctx  : in out Ctx_Type;
                            Standalone : in Boolean);

  -- Clear Xml information (attributes)
  procedure Clear_Xml (Ctx : in out Ctx_Type);

  -- Set the DOCTYPE text, as the last or next child (if Append_Next)
  --  or as first or previous child (if not Append_Next) of prologue
  -- May raise Invalid_Node if Node is not of prologue
  -- May raise Invalid_Argument if not Public and Pub_Id is set
  -- May raise Doctype_Already_Set if Doctype already set
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

  -- Add a processing instruction as the last or next child (if Append_Next)
  -- May raise Invalid_Node if Node is not of prologue
  procedure Add_Pi (Ctx      : in out Ctx_Type;
                    Node     : in Node_Type;
                    Name     : in String; Value : in String;
                    New_Node : out Node_Type;
                    Append_Next : in Boolean := True);

  -- Add a comment in prologue as the last or next child of prologue
  -- May raise Invalid_Node if Node is not of prologue
  procedure Add_Comment (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Comment  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True);

  -- Clear the whole prologue
  procedure Clear_Prologue (Ctx : in out Ctx_Type);

  ----------------------------------
  -- ELEMENTS SPECIFIC OPERATIONS --
  ----------------------------------
  -- May raise Invalid_Node if used on Prologue

  -- Set (change) the name of an element
  -- May raise Invalid_Argument if invalid name
  procedure Set_Name (Ctx     : in out Ctx_Type;
                      Element : in out Element_Type;
                      Name    : in String);


  -- Add an attribute to current element
  -- May raise Invalid_Argument if invalid name
  procedure Add_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String);

  -- Set all the attributes of an element
  -- May raise Invalid_Argument if a name is invalid
  procedure Set_Attributes (Ctx        : in out Ctx_Type;
                            Element    : in out Element_Type;
                            Attributes : in Attributes_Array);

  -- Delete the attributes of an element
  procedure Del_Attributes (Ctx     : in out Ctx_Type;
                            Element : in out Element_Type);

  -- Insert a child element, text or comment, and move to it
  -- May raise Invalid_Node if in prologue
  procedure Add_Child (Ctx      : in out Ctx_Type;
                       Element  : in Element_Type;
                       Name     : in String;
                       Kind     : in Node_Kind_List;
                       New_Node : out Node_Type;
                       Append   : in Boolean := True);

  -- Insert a brother element, text or comment, and move to it
  -- May raise Invalid_Node if in prologue
  procedure Add_Brother (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Name     : in String;
                         Kind     : in Node_Kind_List;
                         New_Node : out Node_Type;
                         Next     : in Boolean := True);

  -- Swap two elements (and their children)
  -- May raise Invalid_Node if one is in prologue
  -- May raise Invalid_Node is one is ancestor of the other
  procedure Swap (Ctx  : in out Ctx_Type;
                  Elt1 : in out Element_Type;
                  Elt2 : in out Element_Type);

  -- Copy Src element (and its children) as Next (or prev) Child (or brother)
  --  of Dst
  -- May raise Invalid_Node if one is in prologue
  -- May raise Invalid_Node is Src is ancestor of the Dst
  procedure Copy (Ctx   : in out Ctx_Type;
                  Src   : in out Element_Type;
                  Dst   : in out Element_Type;
                  Child : in Boolean := True;
                  Next  : in Boolean := True);

  -- Set the text of a Text element
  procedure Set_Text (Ctx     : in out Ctx_Type;
                      Text    : in out Text_Type;
                      Content : in String);

  -- Set the text of a Comment
  procedure Set_Comment (Ctx     : in out Ctx_Type;
                         Comment : in out Comment_Type;
                         Content : in String);

  -----------------------
  -- COMMON OPERATIONS --
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
  -- EXCEPTIONS --
  ----------------
  -- When moving outside the tree (father of root..,
  --  to many brothers or children)
  No_Element : exception;
  -- When invalid value of format of name, attribute, text...
  Invalid_Argument : exception;

  ----------------
  -- GENERATION --
  ----------------
  -- Kind of output format
  -- Raw is all in one physical line and no extra space
  -- Fill_Width is formated, with several attributes per line up to width
  -- One_Per_Line is formated, with one attribute per line
  type Format_Kind_List is (Raw, Fill_Width, One_Per_Line);
  Default_Format : constant Format_Kind_List := Fill_Width;
  Default_Width : constant Natural := 80;
  -- Put in a file (stdout if name is empty)
  -- Raises File_Error if Pb with file
  Stdout : constant String := "";
  procedure Put (Ctx       : in out Ctx_Type;
                 File_Name : in String;
                 Format    : in Format_Kind_List := Default_Format;
                 Width     : in Natural := Default_Width);

  -- Dumps in a string
  function Set (Ctx    : Ctx_Type;
                Format : Format_Kind_List := Default_Format;
                Width  : Natural := Default_Width) return String;
  procedure Set (Ctx    : in Ctx_Type;
                 Str    : out Ada.Strings.Unbounded.Unbounded_String;
                 Format : in Format_Kind_List := Default_Format;
                 Width  : in Natural := Default_Width);

private
  type Ctx_Type is new Xml_Parser.Ctx_Type with null record;
end Xml_Parser.Generator;

