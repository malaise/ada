with Environ, Text_Char, Basic_Proc;
package body Xml_Parser is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;

  -- Trace debug message
  Debug_Level : Integer := -1;
  procedure Trace (Msg : in String);

  -- File management
  package File_Mng is
    -- Open file, raises File_Error if name error
    procedure Open (File_Name : in String; File : in out Text_Char.File_Type);
    procedure Close (File : in out Text_Char.File_Type);
  end File_Mng;
  package body File_Mng is separate;

  -- My tree manipulation
  package Tree_Mng is
    -- The prologue and the tree of elements
    Prologue : My_Tree.Tree_Type;
    Tree : My_Tree.Tree_Type;
    -- Add an element, move to it
    procedure Add_Element (Name : in Asu_Us; Line : in Positive);
    -- Move up
    procedure Move_Up;
    -- Add an attribute to current element, remain on current element
    procedure Add_Attribute (Name, Value : in Asu_Us; Line : in Positive);
    -- Check if an attribute exists for current element
    function Attribute_Exists (Name : Asu_Us) return Boolean;
    -- Get an attribute (if it exists, otherwise "")
    function Get_Attribute (Name : Asu_Us) return Asu_Us;
    -- Add a text to current element, remain on current element
    procedure Add_Text (Text : in Asu_Us; Line : in Positive);
    -- Initialise an empty prologue
    procedure Init_Prologue;
    -- Set xml directive, add a xml attribute
    procedure Set_Xml (Line : in Positive);
    procedure Add_Xml_Attribute (Name, Value : in Asu_Us; Line : in Positive);
    -- Check xml is set, find an attribute (Index is 0 if not found)
    function Xml_Existst return Boolean;
    procedure Find_Xml_Attribute (Name : in Asu_Us;
                                  Index : out Natural;
                                  Value : out Asu_Us);
    -- Add a processing instruction
    procedure Add_Pi (Name, Text : in Asu_Us; Line : in Positive);
  end Tree_Mng;
  package body Tree_Mng is separate;

  -- Entity management
  package Entity_Mng is
    -- Initialise with default entities
    procedure Initialise;
    -- Store an entity or a parameter-entity
    procedure Add (Name, Value : in Asu_Us; Parameter : in Boolean);
    -- Check if an entity exists
    function Exists (Name : Asu_Us; Parameter : Boolean) return Boolean;
    -- Get value of an entity. Raises Entity_Not_Found if none
    function Get (Name : Asu_Us; Parameter : Boolean) return Asu_Us;
    Entity_Not_Found : exception;
  end Entity_Mng;
  package body Entity_Mng is separate;


  -- Parses the content of the file into the tree
  package Parse_Mng is
    -- Parse the file. Raises exceptions
    procedure Parse (File : in out Text_Char.File_Type);
    -- Get parse error message
    function Get_Error_Message return Asu_Us;
  end Parse_Mng;
  package body Parse_Mng is separate;


  -----------
  -- DEBUG --
  -----------
  -- Trace debug message
  procedure Trace (Msg : in String) is
  begin
    if Debug_Level = -1 then
      -- First call, set debug
      if Environ.Is_Yes ("XML_PARSER_DEBUG") then
        Debug_Level := 1;
      else
        Debug_Level := 0;
      end if;
    end if;
    if Debug_Level = 1 then
      Basic_Proc.Put_Line_Error (Msg);
    end if;
  end Trace;


  -------------
  -- PARSING --
  -------------
  -- Parse a Xml file, stdin if empty
  -- May raise File_Error, Parse_Error
  procedure Parse (File_Name : in String;
                   Prologue     : out Element_Type;
                   Root_Element : out Element_Type) is
    Xml_File : Text_Char.File_Type;
    Prol, Root : Node_Type;
  begin
    -- Open file
    File_Mng.Open (File_Name, Xml_File);
    -- Reset and init entities
    Entity_Mng.Initialise;
    -- Parse this file
    Parse_Mng.Parse (Xml_File);
    -- Close file
    File_Mng.Close (Xml_File);
    -- Set tree roots
    My_Tree.Move_Root (Tree_Mng.Prologue);
    Prologue := (Kind => Element,
                 Tree_Access => My_Tree.Get_Position (Tree_Mng.Prologue));
    My_Tree.Move_Root (Tree_Mng.Tree);
    Root_Element:= (Kind => Element,
                    Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
  end Parse;

  -- Return the error message if Parse_Error
  function Get_Parse_Error_Message return String is
  begin
    return Asu_Ts (Parse_Mng.Get_Error_Message);
  end Get_Parse_Error_Message;

  -- Clean a parsed tree
  -- May raise Not_Root if not root
  procedure Clean (Prologue     : in out Element_Type;
                   Root_Element : in out Element_Type) is
    use type My_Tree.Position_Access;
  begin
    -- Nodes must be set
    if Prologue.Tree_Access = My_Tree.No_Position
    or else Root_Element.Tree_Access = My_Tree.No_Position then
      raise Invalid_Node;
    end if;
    -- Clean prologue tree
    My_Tree.Set_Position (Tree_Mng.Prologue, Prologue.Tree_Access);
    if My_Tree.Has_Father (Tree_Mng.Prologue) then
      raise Not_Root;
    end if;
    My_Tree.Delete_Tree (Tree_Mng.Prologue);
    -- Clean element tree
    My_Tree.Set_Position (Tree_Mng.Tree, Root_Element.Tree_Access);
    if My_Tree.Has_Father (Tree_Mng.Tree) then
      raise Not_Root;
    end if;
    My_Tree.Delete_Tree (Tree_Mng.Tree);
  end Clean;


  -- Read internal tree cell of a node
  function Get_Node (Node : Node_Type) return My_Tree_Cell is
    Cell : My_Tree_Cell;
    use type My_Tree.Position_Access;
  begin
    -- Node must be set
    if Node.Tree_Access = My_Tree.No_Position then
      raise Invalid_Node;
    end if;
    -- Read cell in tree
    My_Tree.Set_Position (Tree_Mng.Tree, Node.Tree_Access);
    My_Tree.Read (Tree_Mng.Tree, Cell);
    -- Check kinds match
    if (Node.Kind = Element and then Cell.Kind /= Element)
    or else (Node.Kind = Text and then Cell.Kind /= Text) then
      raise Invalid_Node;
    end if;
    return Cell;
  end Get_Node;

  -- Line number of start of declaration of node
  function Get_Line_No (Node : Node_Type) return Positive is
    Cell : My_Tree_Cell := Get_Node (Node);
  begin
    return Cell.Line_No;
  end Get_Line_No;

  -------------------------
  -- NAME AND ATTRIBUTES --
  -------------------------
  -- Get the name of an element
  function Get_Name (Element : in Element_Type)
                    return Ada.Strings.Unbounded.Unbounded_String is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    Cell := Get_Node (Element);
    return Cell.Name;
  end Get_Name;

  -- Get the attributes of an element
  function Get_Attributes (Element : in Element_Type)
                          return Attributes_Array is
    Cell : My_Tree_Cell := Get_Node (Element);
    A : Attributes_Array (1 .. Cell.Nb_Attributes);
  begin
    for I in A'Range loop
      if I = A'First then
        My_Tree.Move_Child (Tree_Mng.Tree);
      else
        My_Tree.Move_Brother (Tree_Mng.Tree, False);
      end if;
      My_Tree.Read (Tree_Mng.Tree, Cell);
      if Cell.Kind /= Attribute then
        raise Internal_Error;
      end if;
      A(I).Name := Cell.Name;
      A(I).Value := Cell.Value;
    end loop;
    return A;
  end Get_Attributes;

  function Get_Nb_Attributes (Element : in Element_Type) return Natural is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    return Cell.Nb_Attributes;
  end Get_Nb_Attributes;

  -- May raise Invalid_Index
  function Get_Attribute (Element : in Element_Type;
                          Index   : in Positive) return Attribute_Rec is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    if Index > Cell.Nb_Attributes then
      raise Invalid_Index;
    end if;
    for I in 1 .. Index loop
      if I = 1 then
        My_Tree.Move_Child (Tree_Mng.Tree);
      else
        My_Tree.Move_Brother (Tree_Mng.Tree, False);
      end if;
    end loop;
    My_Tree.Read (Tree_Mng.Tree, Cell);
    if Cell.Kind /= Attribute then
      raise Internal_Error;
    end if;
    return (Cell.Name, Cell.Value);
  end Get_Attribute;


  ----------------
  -- NAVIGATION --
  ----------------
  -- Get the Children of an element (elements or texts)
  function Get_Children (Element : in Element_Type) return Nodes_Array is
    Cell : My_Tree_Cell := Get_Node (Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := My_Tree.Children_Number (Tree_Mng.Tree) - Cell.Nb_Attributes;
    N : Nodes_Array (1 .. Nb_Nodes);
    Child : My_Tree_Cell;
  begin
    for I in 1 .. My_Tree.Children_Number (Tree_Mng.Tree) loop
      if I = 1 then
        My_Tree.Move_Child (Tree_Mng.Tree);
      else
        My_Tree.Move_Brother (Tree_Mng.Tree, False);
      end if;
      if I > Cell.Nb_Attributes then
        My_Tree.Read (Tree_Mng.Tree, Child);
        if Child.Kind = Xml_Parser.Element then
          N (I - Cell.Nb_Attributes) :=
                (Kind =>  Xml_Parser.Element,
                 Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
        elsif Child.Kind = Text then
          N (I - Cell.Nb_Attributes) :=
                (Kind => Text,
                 Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
        else
          -- Attribute
          raise Internal_Error;
        end if;
      end if;
    end loop;
    return N;
  end Get_Children;

  function Get_Nb_Children (Element : in Element_Type) return Natural is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    -- Nb of nodes is Nb in tree - attributes
    return My_Tree.Children_Number (Tree_Mng.Tree) - Cell.Nb_Attributes;
  end Get_Nb_Children;

  -- May raise Invalid_Index
  function Get_Child (Element : in Element_Type;
                      Index   : in Positive) return Node_Type is
    Cell : My_Tree_Cell := Get_Node (Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := My_Tree.Children_Number (Tree_Mng.Tree) - Cell.Nb_Attributes;
    N : Node_Type;
    Child : My_Tree_Cell;
  begin
    if Index > Nb_Nodes then
      raise Invalid_Index;
    end if;
    for I in 1 .. My_Tree.Children_Number (Tree_Mng.Tree) loop
      if I = 1 then
        My_Tree.Move_Child (Tree_Mng.Tree);
      else
        My_Tree.Move_Brother (Tree_Mng.Tree, False);
      end if;
      if I = Cell.Nb_Attributes + Index then
        My_Tree.Read (Tree_Mng.Tree, Child);
        if Child.Kind = Xml_Parser.Element then
          N := (Kind =>  Xml_Parser.Element,
                Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
        elsif Child.Kind = Text then
          N := (Kind => Text,
                Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
        else
          -- Attribute
          raise Internal_Error;
        end if;
      end if;
    end loop;
    return N;
  end Get_Child;


  -- Get the father of an element
  -- May raise No_Parent
  function Get_Parent (Element : in Element_Type) return Node_Type is
    Cell : My_Tree_Cell := Get_Node (Element);
    N : Node_Type;
  begin
    if not My_Tree.Has_Father (Tree_Mng.Tree) then
      raise No_Parent;
    end if;
    My_Tree.Move_Father (Tree_Mng.Tree);
    My_Tree.Read (Tree_Mng.Tree, Cell);
    if Cell.Kind = Xml_Parser.Element then
      N := (Kind =>  Xml_Parser.Element,
            Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
    elsif Cell.Kind = Text then
      N := (Kind => Text,
            Tree_Access => My_Tree.Get_Position (Tree_Mng.Tree));
    else
      -- Attribute
      raise Internal_Error;
    end if;
    return N;
  end Get_Parent;

  function Is_Root (Element : in Element_Type) return Boolean is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    return not My_Tree.Has_Father (Tree_Mng.Tree);
  end Is_Root;


   ----------
   -- TEXT --
   ----------
  function Get_Text (Text : in Text_Type) return String is
  begin
    return Asu_Ts (Get_Text (Text));
  end Get_Text;

  function Get_Text (Text : in Text_Type)
                     return Ada.Strings.Unbounded.Unbounded_String is
    Cell : My_Tree_Cell := Get_Node (Text);
  begin
    if Cell.Kind /= Xml_Parser.Text then
      raise Internal_Error;
    end if;
    return Cell.Name;
  end Get_Text;

end Xml_Parser;


