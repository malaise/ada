with Environ, Basic_Proc;
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
    -- Add an element, move to it
    procedure Add_Element (Elements : in out My_Tree.Tree_Type;
                           Name : in Asu_Us; Line : in Positive);
    -- Move up
    procedure Move_Up (Elements : in out My_Tree.Tree_Type);
    -- Add an attribute to current element, remain on current element
    procedure Add_Attribute (Elements : in out My_Tree.Tree_Type;
                             Name, Value : in Asu_Us; Line : in Positive);
    -- Check if an attribute exists for current element
    procedure Attribute_Exists (Elements : in out My_Tree.Tree_Type;
                              Name : in Asu_Us; Exists : out Boolean);
    -- Get an attribute (if it exists, otherwise "")
    procedure Get_Attribute (Elements : in out My_Tree.Tree_Type;
                           Name : in Asu_Us; Value : out Asu_Us);
    -- Add a text to current element, remain on current element
    procedure Add_Text (Elements : in out My_Tree.Tree_Type;
                        Text : in Asu_Us; Line : in Positive);
    -- Initialise an empty prologue
    procedure Init_Prologue (Prologue : in out My_Tree.Tree_Type);
    -- Set xml directive, add a xml attribute
    procedure Set_Xml (Prologue : in out My_Tree.Tree_Type;
                       Line : in Positive);
    procedure Add_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                 Name, Value : in Asu_Us; Line : in Positive);
    -- Check xml is set, find an attribute (Index is 0 if not found)
    procedure Xml_Existst (Prologue : in out My_Tree.Tree_Type;
                         Exists : out Boolean);
    procedure Find_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                  Name : in Asu_Us;
                                  Index : out Natural;
                                  Value : out Asu_Us);
    -- Add a processing instruction
    procedure Add_Pi (Prologue : in out My_Tree.Tree_Type;
                      Name, Text : in Asu_Us; Line : in Positive);
  end Tree_Mng;
  package body Tree_Mng is separate;

  --------------------
  -- Dtd definition --
  --------------------
  -- Entities
  procedure Set (To : out Entity_Type; Val : in Entity_Type) is
  begin
    To := Val;
  end Set;
  function Image (Entity : Entity_Type) return String is
  begin
    if Entity.Parameter then
      return "%" & Asu_Ts (Entity.Name);
    else
      return Asu_Ts (Entity.Name);
    end if;
  end Image;
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean is
    use type Asu_Us;
  begin
    return Current.Parameter = Criteria.Parameter
    and then Current.Name = Criteria.Name;
  end "=";
  -- Infos
  procedure Set (To : out Info_Rec; Val : in Info_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Info_Rec) return String is
  begin
    return  Ada.Strings.Unbounded.To_String (Element.Name);
  end Image;
  function "=" (Current : Info_Rec; Criteria : Info_Rec) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  -- Parses the content of the file into the tree
  package Parse_Mng is
    -- Parse the file. Raises exceptions
    procedure Parse (Ctx : in out Ctx_Type);
    -- Get parse error message
    function Get_Error_Message (Ctx : Ctx_Type) return Asu_Us;
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
                   Ctx          : out Ctx_Type;
                   Prologue     : out Element_Type;
                   Root_Element : out Element_Type) is
    Prol, Root : Node_Type;
  begin
    if not Ctx.Clean then
      raise Status_Error;
    end if;
    -- Be sure context is clean
    Clean (Ctx);
    -- No it will not be clean
    Ctx.Clean := False;
    -- Open file
    File_Mng.Open (File_Name, Ctx.Flow.Xml_File);
    Ctx.Flow.Kind := Xml_File;
    Ctx.Flow.Xml_Line := 1;
    -- Parse this file
    Parse_Mng.Parse (Ctx);
    -- Close the file
    File_Mng.Close (Ctx.Flow.Xml_File);
    -- Set tree roots
    My_Tree.Move_Root (Ctx.Prologue);
    Prologue := (Kind => Element,
                 Tree => Ctx.Prologue'Unrestricted_Access,
                 Tree_Access => My_Tree.Get_Position (Ctx.Prologue));
    My_Tree.Move_Root (Ctx.Elements);
    Root_Element:= (Kind => Element,
                    Tree => Ctx.Elements'Unrestricted_Access,
                    Tree_Access => My_Tree.Get_Position (Ctx.Elements));
  exception
    when File_Error | Parse_Error =>
      raise;
    when others =>
      raise Internal_Error;
  end Parse;

  -- Return the error message if Parse_Error
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String is
  begin
    return Asu_Ts (Parse_Mng.Get_Error_Message (Ctx));
  end Get_Parse_Error_Message;

  -- Clean a parsing context
  procedure Clean (Ctx : in out Ctx_Type) is
    use type My_Tree.Position_Access;
  begin
    -- Clean input flow
    Ctx.Flow.Kind := Xml_File;
    Ctx.Flow.Nb_Got := 0;
    loop
      begin
        My_Circ.Discard_Last (Ctx.Flow.Circ);
      exception
        when My_Circ.Circ_Empty =>
          exit;
      end;
    end loop;
    Ctx.Flow.Xml_Line := 0;
    Ctx.Flow.Dtd_Line := 0;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ctx.Flow.Curr_Str := Asu_Null;

    Ctx.Flow.Str := Asu_Null;
    if Text_Char.Is_Open (Ctx.Flow.Xml_File) then
      Text_Char.Close (Ctx.Flow.Xml_File);
    end if;
    if Text_Char.Is_Open (Ctx.Flow.Dtd_File) then
      Text_Char.Close (Ctx.Flow.Dtd_File);
    end if;
    -- Clean prologue tree
    if not My_Tree.Is_Empty (Ctx.Prologue) then
      My_Tree.Move_Root (Ctx.Prologue);
      My_Tree.Delete_Tree (Ctx.Prologue);
    end if;
    -- Clean element tree
    if not My_Tree.Is_Empty (Ctx.Elements) then
      My_Tree.Move_Root (Ctx.Elements);
      My_Tree.Delete_Tree (Ctx.Elements);
    end if;
    -- Clean Dtd
    Ctx.Dtd.Set := False;
    Ctx.Dtd.Xml_Found := False;
    Info_Mng.Delete_List (Ctx.Dtd.Info_List);
    Entity_List_Mng.Delete_List (Ctx.Dtd.Entity_List);
    -- Context is clean
    Ctx.Clean := True;
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
    My_Tree.Set_Position (Node.Tree.all, Node.Tree_Access);
    My_Tree.Read (Node.Tree.all, Cell);
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
        My_Tree.Move_Child (Element.Tree.all);
      else
        My_Tree.Move_Brother (Element.Tree.all, False);
      end if;
      My_Tree.Read (Element.Tree.all, Cell);
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
        My_Tree.Move_Child (Element.Tree.all);
      else
        My_Tree.Move_Brother (Element.Tree.all, False);
      end if;
    end loop;
    My_Tree.Read (Element.Tree.all, Cell);
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
             := My_Tree.Children_Number (Element.Tree.all)
              - Cell.Nb_Attributes;
    N : Nodes_Array (1 .. Nb_Nodes);
    Child : My_Tree_Cell;
  begin
    for I in 1 .. My_Tree.Children_Number (Element.Tree.all) loop
      if I = 1 then
        My_Tree.Move_Child (Element.Tree.all);
      else
        My_Tree.Move_Brother (Element.Tree.all, False);
      end if;
      if I > Cell.Nb_Attributes then
        My_Tree.Read (Element.Tree.all, Child);
        if Child.Kind = Xml_Parser.Element then
          N (I - Cell.Nb_Attributes) :=
                (Kind =>  Xml_Parser.Element,
                 Tree => Element.Tree,
                 Tree_Access => My_Tree.Get_Position (Element.Tree.all));
        elsif Child.Kind = Text then
          N (I - Cell.Nb_Attributes) :=
                (Kind => Text,
                 Tree => Element.Tree,
                 Tree_Access => My_Tree.Get_Position (Element.Tree.all));
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
    return My_Tree.Children_Number (Element.Tree.all) - Cell.Nb_Attributes;
  end Get_Nb_Children;

  -- May raise Invalid_Index
  function Get_Child (Element : in Element_Type;
                      Index   : in Positive) return Node_Type is
    Cell : My_Tree_Cell := Get_Node (Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := My_Tree.Children_Number (Element.Tree.all) - Cell.Nb_Attributes;
    N : Node_Type;
    Child : My_Tree_Cell;
  begin
    if Index > Nb_Nodes then
      raise Invalid_Index;
    end if;
    for I in 1 .. My_Tree.Children_Number (Element.Tree.all) loop
      if I = 1 then
        My_Tree.Move_Child (Element.Tree.all);
      else
        My_Tree.Move_Brother (Element.Tree.all, False);
      end if;
      if I = Cell.Nb_Attributes + Index then
        My_Tree.Read (Element.Tree.all, Child);
        if Child.Kind = Xml_Parser.Element then
          N := (Kind =>  Xml_Parser.Element,
                Tree => Element.Tree,
                Tree_Access => My_Tree.Get_Position (Element.Tree.all));
        elsif Child.Kind = Text then
          N := (Kind => Text,
                Tree => Element.Tree,
                Tree_Access => My_Tree.Get_Position (Element.Tree.all));
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
    if not My_Tree.Has_Father (Element.Tree.all) then
      raise No_Parent;
    end if;
    My_Tree.Move_Father (Element.Tree.all);
    My_Tree.Read (Element.Tree.all, Cell);
    if Cell.Kind = Xml_Parser.Element then
      N := (Kind =>  Xml_Parser.Element,
            Tree => Element.Tree,
            Tree_Access => My_Tree.Get_Position (Element.Tree.all));
    elsif Cell.Kind = Text then
      N := (Kind => Text,
            Tree => Element.Tree,
            Tree_Access => My_Tree.Get_Position (Element.Tree.all));
    else
      -- Attribute
      raise Internal_Error;
    end if;
    return N;
  end Get_Parent;

  function Is_Root (Element : in Element_Type) return Boolean is
    Cell : My_Tree_Cell := Get_Node (Element);
  begin
    return not My_Tree.Has_Father (Element.Tree.all);
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

