with Ada.Strings.Unbounded, Ada.Finalization;
with Trees;
-- Generates a Xml file (or stdout), or string from a tree
package Xml_Parser.Generator is

  Version : constant String := "V2.3";

  type Xml_Dscr_Type is tagged limited private;

  -- Internal element kind list
  type Internal_Kind_List is (Element, Text, Comment, Attribute);
  -- Possible kind of a child or brother
  subtype Kind_List is Internal_Kind_List range Element .. Comment;

  -- Number of children of an element
  subtype Child_Range is Trees.Child_Range;

  -- Reset a XML descriptor,
  -- Initialise a Prologue and set root element
  -- Optional xml directive with version (only 1.0 and 1.1 are OK))
  -- No xml_directive is possible (in XML standard 1.0 but not in V1.1)
  --   if Major=0
  procedure Reset (Dscr  : in out Xml_Dscr_Type;
                   Major : in Natural; Minor : in Natural;
                   Root_Name : in String);

  -- All calls may raise Status_Error if performed on a Dscr not (re)set
  -- Attributes (and xml encoding and standalone) must be set before children
  --  (and texts and doctype and PIs).

  -------------------------
  -- PROLOGUE OPERATIONS --
  -------------------------
  -- Set the encoding and the standalone of XML directive
  -- May raise Has_Children if Set_Doctype or Add_Pi or Add_Comment
  procedure Set_Encoding (Dscr : in out Xml_Dscr_Type;
                          Encoding : in String);
  procedure Set_Standalone (Dscr : in out Xml_Dscr_Type;
                            Standalone : in Boolean);
  -- Set the DOCTYPE text
  -- May raise Has_Children if Doctype already set
  -- May raise Invalid_Argument if not Public and Pub_Id is set
  procedure Set_Doctype (Dscr    : in out Xml_Dscr_Type;
                         Name    : in String;
                         Public  : in Boolean;
                         Pub_Id  : in String;
                         File    : in String;
                         Int_Def : in String);

  -- Add a processing instruction in the prologue
  procedure Add_Pi (Dscr : in out Xml_Dscr_Type;
                    Name : in String; Value : in String);

  -- Add a comment to the prolog
  procedure Add_Comment (Dscr : in out Xml_Dscr_Type;
                         Text : in String);

  ----------------------
  -- TREE OF ELEMENTS --
  ----------------------
  -- May raise No_Element if no father (at root)
  procedure Move_Father (Dscr : in out Xml_Dscr_Type);

  -- Modify the tree:
  -- Add an attribute to current element
  -- May raise No_Element if current element is text
  -- May raise Has_Children if Add_Child/Brother already used
  procedure Add_Attribute (Dscr  : in out Xml_Dscr_Type;
                           Name, Value : in String);

  -- Insert a child element, text or comment, and move to it
  -- May raise No_Element if current element is text
  procedure Add_Child (Dscr : in out Xml_Dscr_Type;
                       Name : in String;
                       Kind : in Kind_List);

  -- Insert a brother element, text or comment, and move to it
  procedure Add_Brother (Dscr : in out Xml_Dscr_Type;
                         Name : in String;
                         Kind : in Kind_List);

  ----------------
  -- EXCEPTIONS --
  ----------------
  -- When Dscr not (re)set
  Status_Error : exception;
  -- When invalid value of format of name, attribute, text...
  Invalid_Argument : exception;
  -- When adding attibutes after children
  Has_Children : exception;
  -- When adding child not on an element, invalid move or insertion
  No_Element : exception;

  ----------------
  -- GENERATION --
  ----------------
  -- Kind of output format
  -- Raw is all in one physical line and no extra space
  -- Fill_Columns is formated, with several attributes per line up to width
  -- One_Per_Line is formated, with one attribute per line
  type Format_Kind_List is (Raw, Fill_Width, One_Per_Line);
  Default_Format : constant Format_Kind_List := Fill_Width;
  Default_Width : constant Natural := 80;
  -- Put in a file (stdout if name is empty)
  -- Raises File_Error if Pb with file
  Stdout : constant String := "";
  procedure Put (Dscr      : in out Xml_Dscr_Type;
                 File_Name : in String;
                 Format    : in Format_Kind_List := Default_Format;
                 Width     : in Natural := Default_Width);

  -- Dumps in a string
  function Set (Dscr   : Xml_Dscr_Type;
                Format : Format_Kind_List := Default_Format;
                Width  : Natural := Default_Width) return String;
  procedure Set (Dscr   : in Xml_Dscr_Type;
                 Str    : out Ada.Strings.Unbounded.Unbounded_String;
                 Format : in Format_Kind_List := Default_Format;
                 Width  : in Natural := Default_Width);

  ----------------
  -- EXCEPTIONS --
  ----------------
  -- When opening/writing file
  File_Error : exception;
  -- Internal inconsitency of tree
  Internal_Error : exception;

private

  -- In prologue: attributes of Prologue are xml directive
  --  PIs are Element children of Prologue
  --  Doctype is Text child of Prologue
  -- A cell of the tree
  type My_Tree_Cell is record
    -- Kind:  Attribute, Element, Text or Comment
    Kind : Internal_Kind_List;
    -- Number of attributes when Kind is Element
    Nb_Attributes : Natural := 0;
    -- Element name or Attribute name or text
    Name : Ada.Strings.Unbounded.Unbounded_String;
    -- Attribute value
    Value : Ada.Strings.Unbounded.Unbounded_String;
  end record;

  -- A tree
  package My_Tree is new Trees.Tree(My_Tree_Cell);
  type Tree_Acc is access all My_Tree.Tree_Type;

  -- A XML description
  type Xml_Dscr_Type is limited
          new Ada.Finalization.Limited_Controlled with record
    -- The prologue (XML & Doctype & PIs as its text)
    Prologue : Tree_Acc := new My_Tree.Tree_Type;
    -- Is xml set
    Xml_Set : Boolean := False;
    -- Doctype information
    Doc_Name    : Ada.Strings.Unbounded.Unbounded_String;
    Doc_Public  : Boolean := False;
    Doc_Pub_Id  : Ada.Strings.Unbounded.Unbounded_String;
    Doc_File    : Ada.Strings.Unbounded.Unbounded_String;
    Doc_Int_Def : Ada.Strings.Unbounded.Unbounded_String;
    -- The tree of elements, attributes and texts
    Elements : Tree_Acc := new My_Tree.Tree_Type;
  end record;

  procedure Finalize (Dscr : in out Xml_Dscr_Type);

end Xml_Parser.Generator;

