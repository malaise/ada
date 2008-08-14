with Ada.Exceptions;
with Environ, Basic_Proc, Rnd, Exception_Messenger;
package body Xml_Parser is

  -- Version incremented at each significant change
  Minor_Version : constant String := "8";
  function Version return String is
  begin
    return "V" & Major_Version & "." & Minor_Version;
  end Version;

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
                           Name : in Asu_Us; Line : in Natural);
    -- Move up
    procedure Move_Up (Elements : in out My_Tree.Tree_Type);
    -- Add an attribute to current element, remain on current element
    procedure Add_Attribute (Elements : in out My_Tree.Tree_Type;
                             Name, Value : in Asu_Us; Line : in Natural);
    -- Check if an attribute exists for current element
    procedure Attribute_Exists (Elements : in out My_Tree.Tree_Type;
                              Name : in Asu_Us; Exists : out Boolean);
    -- Get an attribute (if it exists, otherwise "")
    procedure Get_Attribute (Elements : in out My_Tree.Tree_Type;
                           Name : in Asu_Us; Value : out Asu_Us);
    -- Initialise an empty prologue
    procedure Init_Prologue (Prologue : in out My_Tree.Tree_Type);
    -- Set xml directive, add a xml attribute
    procedure Set_Xml (Prologue : in out My_Tree.Tree_Type;
                       Line : in Natural);
    procedure Add_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                 Name, Value : in Asu_Us; Line : in Natural);
    -- Check xml is set, find an attribute (Index is 0 if not found)
    procedure Xml_Existst (Prologue : in out My_Tree.Tree_Type;
                         Exists : out Boolean);
    procedure Find_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                  Name : in Asu_Us;
                                  Index : out Natural;
                                  Value : out Asu_Us);
    procedure Get_Nb_Xml_Attributes (Prologue : in out My_Tree.Tree_Type;
                                     Number : out Natural);
    -- Add a processing instruction
    procedure Add_Pi (Prologue : in out My_Tree.Tree_Type;
                      Name, Text : in Asu_Us; Line : in Natural);

    -- Is a tree (elements or prologue) empty
    function Is_Empty (Tree : My_Tree.Tree_Type) return Boolean;

    -- Add a text to current cell (of elements or prologue)
    -- remain on current cell
    procedure Add_Text (Tree : in out My_Tree.Tree_Type;
                        Text : in Asu_Us; Line : in Natural);

    -- Add a comment to current cell (of elements or prologue)
    -- remain on current cell
    procedure Add_Comment (Tree : in out My_Tree.Tree_Type;
                           Comment : in Asu_Us; Line : in Natural);
  end Tree_Mng;
  package body Tree_Mng is separate;

  -- Dtd definition: entities
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
  -- Dtd definition: infos
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
    -- Parse the Xml file. Raises exceptions
    procedure Parse (Ctx : in out Ctx_Type);
    -- Parse a Dtd Flow
    -- Use String_Flow as File_Name when Flow is Ctx.String, otherwise
    --  File_Name is the file name
    function String_Flow return String;
    procedure Parse_Dtd (Ctx : in out Ctx_Type;
                         Adtd : in out Dtd_Type;
                         File_Name : in String);
    -- Parse the prologue
    procedure Parse_Prologue (Ctx : in out Ctx_Type);
    -- Parse the elements
    procedure Parse_Elements (Ctx : in out Ctx_Type;
                              Adtd : in out Dtd_Type);
    -- Check the Ctx vs its Dtd, Raises exceptions
    procedure Check (Ctx : in out Ctx_Type);
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

  function Get_Magic return Float is
  begin
    return Rnd.Float_Random (0.0, Float(Integer'Last));
  end Get_Magic;

  -------------
  -- PARSING --
  -------------
  -- Parse a Xml file, stdin if empty
  -- May raise File_Error, Parse_Error
  procedure Parse (Ctx             : out Ctx_Type;
                   File_Name       : in String;
                   Ok              : out Boolean;
                   Comments        : in Boolean := False;
                   Expand_Entities : in Boolean := True) is
  begin
    if Ctx.Status /= Clean then
      raise Status_Error;
    end if;
    -- Be sure context is clean
    Clean (Ctx);
    -- No it will not be clean
    Ctx.Magic := Get_Magic;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Open file
    File_Mng.Open (File_Name, Ctx.Flow.Xml_File);
    Ctx.Flow.Kind := Xml_File;
    Ctx.Flow.Xml_Line := 1;
    -- Parse this file
    Ctx.Parse_Comments := Comments;
    Ctx.Expand_Entities := Expand_Entities;
    Parse_Mng.Parse (Ctx);
    -- Close the file
    File_Mng.Close (Ctx.Flow.Xml_File);
    Ctx.Status := Parsed_Elements;
    Ok := True;
  exception
    when File_Error | Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse;

  -- Return the error message if Parse_Error
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String is
  begin
    if Ctx.Status = Clean or else Ctx.Status = Init then
      raise Status_Error;
    end if;
    return Asu_Ts (Ctx.Flow.Err_Msg);
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
    Ctx.Flow.Recording := False;
    Ctx.Flow.Recorded := Asu_Null;

    Ctx.Flow.In_Str := Asu_Null;
    Ctx.Flow.In_Stri := 0;
    if Text_Char.Is_Open (Ctx.Flow.Xml_File) then
      File_Mng.Close (Ctx.Flow.Xml_File);
    end if;
    if Text_Char.Is_Open (Ctx.Flow.Dtd_File) then
      File_Mng.Close (Ctx.Flow.Dtd_File);
    end if;
    Ctx.Parse_Comments := False;
    Ctx.Expand_Entities := True;
    -- Clean prologue tree
    if not My_Tree.Is_Empty (Ctx.Prologue.all) then
      My_Tree.Move_Root (Ctx.Prologue.all);
      My_Tree.Delete_Tree (Ctx.Prologue.all);
    end if;
    -- Clean element tree
    if not My_Tree.Is_Empty (Ctx.Elements.all) then
      My_Tree.Move_Root (Ctx.Elements.all);
      My_Tree.Delete_Tree (Ctx.Elements.all);
    end if;
    -- Clean Doctype info
    Ctx.Doctype.Line_No := 1;
    Ctx.Doctype.Name    := Asu_Null;
    Ctx.Doctype.Public  := False;
    Ctx.Doctype.Pub_Id  := Asu_Null;
    Ctx.Doctype.File    := Asu_Null;
    Ctx.Doctype.Int_Def := Asu_Null;
    -- Context is clean
    Ctx.Magic := Clean_Magic;
    Ctx.Status := Clean;
  end Clean;

  -- Dtd parsing
  procedure Parse_Dtd_File (File_Name : in String;
                            Dtd       : out Dtd_Type) is
    Ctx : Ctx_Type;
  begin
    Clean_Dtd (Dtd);
    -- File Name_Error raises File_Error
    Parse_Mng.Parse_Dtd (Ctx, Dtd, File_Name);
    Clean (Ctx);
  end Parse_Dtd_File;

  procedure Parse_Dtd_String (Str : in String;
                              Dtd : out Dtd_Type) is
    Ctx : Ctx_Type;
  begin
    Ctx.Flow.Kind := Xml_String;
    Ctx.Flow.In_Str := Asu_Tus (Str);
    Ctx.Flow.Xml_Line := 1;
    Clean_Dtd (Dtd);
    Parse_Mng.Parse_Dtd (Ctx, Dtd, Parse_Mng.String_Flow);
    Clean (Ctx);
  end Parse_Dtd_String;

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type) is
  begin
    -- Clean Dtd
    Dtd.Set := False;
    Dtd.Xml_Found := False;
    Dtd.In_Include := False;
    Info_Mng.Delete_List (Dtd.Info_List);
    Entity_List_Mng.Delete_List (Dtd.Entity_List);
  end Clean_Dtd;

  -- Parse the prologue of a string
  -- may raise Status_Error if Ctx is not clean
  --    Parse_Error while parsing the string
  procedure Parse_Prologue (Ctx             : out Ctx_Type;
                            Str             : in String;
                            Ok              : out Boolean;
                            Comments        : in Boolean := False;
                            Expand_Entities : in Boolean := True) is
  begin
    if Ctx.Status /= Clean then
      raise Status_Error;
    end if;
    -- Be sure context is clean
    Clean (Ctx);
    -- Now it will not be clean
    Ctx.Magic := Get_Magic;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Parse the prologue string
    Ctx.Flow.Kind := Xml_String;
    Ctx.Flow.In_Str := Asu_Tus (Str);
    Ctx.Flow.Xml_Line := 1;
    Ctx.Parse_Comments := Comments;
    Ctx.Expand_Entities := Expand_Entities;
    Parse_Mng.Parse_Prologue (Ctx);
    -- Close the file
    Ctx.Status := Parsed_Prologue;
    Ok := True;
  exception
    when Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Prologue;

  -- Parse the elements (after the prologue) of a string with a dtd
  -- may raise Status_Error if Ctx is clean
  --    End_Error if Ctx has already parsed elements
  --    Parse_Error while parsing the string
  procedure Parse_Elements (Ctx       : in out Ctx_Type;
                            Dtd       : in out Dtd_Type;
                            Ok        : out Boolean) is
    Loc_Parse_Error : exception;
  begin
    -- Status must be Parsed_Prologue
    if Ctx.Status = Clean or else Ctx.Status = Init then
      raise Status_Error;
    elsif Ctx.Status = Parsed_Elements then
      raise End_Error;
    elsif Ctx.Status = Error then
      raise Loc_Parse_Error;
    end if;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Parse
    Parse_Mng.Parse_Elements (Ctx, Dtd);
    -- Close the file
    Ctx.Status := Parsed_Elements;
    Ok := True;
  exception
    when Status_Error | End_Error =>
      raise;
    when Loc_Parse_Error =>
      -- Raising Parse_Error because previous parsing detected error
      raise Parse_Error;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Elements;

  -----------
  -- CHECK --
  -----------
  -- Check the Ctx: parse the DTD (if any) and check the Ctx versus it
  --  (same effect as Xml_Parse.Parse)
  procedure Check (Ctx : in out Ctx_Type;
                   Ok  : out Boolean) is
  begin
    -- Status must be Parsed_Prologue
    if Ctx.Status = Clean or else Ctx.Status = Error then
      raise Status_Error;
    end if;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Check this context
    if not My_Tree.Is_Empty (Ctx.Elements.all) then
      My_Tree.Move_Root (Ctx.Elements.all);
    end if;
    Parse_Mng.Check (Ctx);
    Ctx.Status := Parsed_Elements;
    Ok := True;
  exception
      when File_Error | Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Check;

  ----------------
  -- NAVIGATION --
  ----------------
  -- Read internal tree cell of a node
  function Get_Tree (Ctx : Ctx_Type;
                     Node : Node_Type) return Tree_Acc is
    use type My_Tree.Position_Access;
  begin
    -- Node must be set
    if Node.Tree_Access = My_Tree.No_Position then
      raise Invalid_Node;
    end if;
    if Ctx.Status /= Parsed_Prologue
    and then Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    -- Magic must match
    if Ctx.Magic /= Node.Magic then
      raise Use_Error;
    end if;
    if Node.In_Prologue then
      return Ctx.Prologue;
    else
      return Ctx.Elements;
    end if;
  end Get_Tree;

  function Get_Cell (Tree : Tree_Acc;
                     Node : Node_Type) return My_Tree_Cell is
    Cell : My_Tree_Cell;
    use type My_Tree.Position_Access;
  begin
    -- Read cell in tree
    My_Tree.Set_Position (Tree.all, Node.Tree_Access);
    My_Tree.Read (Tree.all, Cell);
    -- Check kinds match
    if (Node.Kind = Element and then Cell.Kind /= Element)
    or else (Node.Kind = Text and then Cell.Kind /= Text)
    or else (Node.Kind = Comment and then Cell.Kind /= Comment) then
      raise Invalid_Node;
    end if;
    return Cell;
  end Get_Cell;

  -- Get Doctype characteristics (prologue must have been parsed)
  procedure Get_Doctype (Ctx : in Ctx_Type;
       Name    : out Ada.Strings.Unbounded.Unbounded_String;
       Public  : out Boolean;
       Pub_Id  : out Ada.Strings.Unbounded.Unbounded_String;
       File    : out Ada.Strings.Unbounded.Unbounded_String;
       Int_Def : out Ada.Strings.Unbounded.Unbounded_String) is
    use type Asu_Us;
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status = Clean then
      raise Status_Error;
    end if;
    if Ctx.Doctype.Name = Asu_Null then
      raise Doctype_Not_Set;
    end if;
    Name    := Ctx.Doctype.Name;
    Public  := Ctx.Doctype.Public;
    Pub_Id  := Ctx.Doctype.Pub_Id;
    File    := Ctx.Doctype.File;
    Int_Def := Ctx.Doctype.Int_Def;
  end Get_Doctype;

  -- Get a PI data (use Get_Name to get the PITarget)
  -- May raise Invalid_Node if not is not of the prologue
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Element_Type) return String is
  begin
    return Asu_Ts (Get_Pi (Ctx, Pi_Node));
  end Get_Pi;

  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Element_Type)
           return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Pi_Node), Pi_Node);
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status = Clean then
      raise Status_Error;
    elsif not Pi_Node.In_Prologue then
      raise Invalid_Node;
    end if;
    return Cell.Value;
  end Get_Pi;

  -- Get Prologue of a parsed context (after Parse or Parse_Prologue)
  function Get_Prologue (Ctx : Ctx_Type) return Element_Type is
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status = Clean then
      raise Status_Error;
    end if;
    -- Only prologue or full parsing completed
    My_Tree.Move_Root (Ctx.Prologue.all);
    return (Kind => Element,
            Magic => Ctx.Magic,
            In_Prologue => True,
            Tree_Access => My_Tree.Get_Position (Ctx.Prologue.all));
  end Get_Prologue;

  -- Get elements'root after Parse or Parse_Elements
  --  may raise Status_Error if called before Parse_Elements
  function Get_Root_Element (Ctx : Ctx_Type) return Element_Type is
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    -- Only prologue or full parsing completed
    My_Tree.Move_Root (Ctx.Elements.all);
    return (Kind => Element,
            Magic => Ctx.Magic,
            In_Prologue => False,
            Tree_Access => My_Tree.Get_Position (Ctx.Elements.all));
  end Get_Root_Element;

  -- Line number of start of declaration of node
  function Get_Line_No (Ctx  : Ctx_Type;
                        Node : Node_Type) return Natural is
    Cell : constant My_Tree_Cell := Get_Cell (Get_Tree (Ctx, Node), Node);
  begin
    return Cell.Line_No;
  end Get_Line_No;

  -------------------------
  -- NAME AND ATTRIBUTES --
  -------------------------
  -- Get the name of an element
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return String is
  begin
    return Asu.To_String (Get_Name (Ctx, Element));
  end Get_Name;

  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type)
                     return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Name;
  end Get_Name;

  -- Get the attributes of an element
  function Get_Attributes (Ctx     : Ctx_Type;
                           Element : Element_Type) return Attributes_Array is
    Tree : Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
    A : Attributes_Array (1 .. Cell.Nb_Attributes);
  begin
    for I in A'Range loop
      if I = A'First then
        My_Tree.Move_Child (Tree.all);
      else
        My_Tree.Move_Brother (Tree.all, False);
      end if;
      My_Tree.Read (Tree.all, Cell);
      if Cell.Kind /= Attribute then
        Trace ("Expecting kind attribute, found " & Cell.Kind'Img);
        raise Internal_Error;
      end if;
      A(I).Name := Cell.Name;
      A(I).Value := Cell.Value;
    end loop;
    return A;
  end Get_Attributes;

  function Get_Nb_Attributes (Ctx     : Ctx_Type;
                              Element : Element_Type) return Natural is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Nb_Attributes;
  end Get_Nb_Attributes;

  -- May raise Invalid_Index
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Index   : Positive) return Attribute_Rec is
    Tree : Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
  begin
    if Index > Cell.Nb_Attributes then
      raise Invalid_Index;
    end if;
    for I in 1 .. Index loop
      if I = 1 then
        My_Tree.Move_Child (Tree.all);
      else
        My_Tree.Move_Brother (Tree.all, False);
      end if;
    end loop;
    My_Tree.Read (Tree.all, Cell);
    if Cell.Kind /= Attribute then
      Trace ("Expecting kind attribute, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return (Cell.Name, Cell.Value);
  end Get_Attribute;


  ----------------
  -- NAVIGATION --
  ----------------
  function Is_Valid (Node : Node_Type) return Boolean is
    use type My_Tree.Position_Access;
  begin
    return Node.Tree_Access /= My_Tree.No_Position;
  end Is_Valid;

  -- Get the Children of an element (elements or texts or comments)
  function Get_Children (Ctx     : Ctx_Type;
                         Element : Element_Type) return Nodes_Array is
    Tree : Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := My_Tree.Children_Number (Tree.all)
                - Cell.Nb_Attributes;
    N : Nodes_Array (1 .. Nb_Nodes);
    Child : My_Tree_Cell;
  begin
    for I in 1 .. My_Tree.Children_Number (Tree.all) loop
      if I = 1 then
        My_Tree.Move_Child (Tree.all);
      else
        My_Tree.Move_Brother (Tree.all, False);
      end if;
      if I > Cell.Nb_Attributes then
        My_Tree.Read (Tree.all, Child);
        case Child.Kind is
          when Xml_Parser.Element =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind =>  Xml_Parser.Element,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => My_Tree.Get_Position (Tree.all));
          when Xml_Parser.Text =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Text,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => My_Tree.Get_Position (Tree.all));
          when Xml_Parser.Comment =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Comment,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => My_Tree.Get_Position (Tree.all));
          when Xml_Parser.Attribute =>
            -- Attribute
            Trace ("Expecting kind text or element or comment, found attribute");
            raise Internal_Error;
        end case;
      end if;
    end loop;
    return N;
  end Get_Children;

  function Get_Nb_Children (Ctx     : Ctx_Type;
                            Element : Element_Type) return Natural is
    Tree : Tree_Acc := Get_Tree (Ctx, Element);
    Cell : constant My_Tree_Cell := Get_Cell (Tree, Element);
  begin
    -- Nb of nodes is Nb in tree - attributes
    return My_Tree.Children_Number (Tree.all) - Cell.Nb_Attributes;
  end Get_Nb_Children;

  -- May raise Invalid_Index
  function Get_Child (Ctx     : Ctx_Type;
                      Element : Element_Type;
                      Index   : Positive) return Node_Type is
    Tree : Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := My_Tree.Children_Number (Tree.all) - Cell.Nb_Attributes;
    N : Node_Type;
    Child : My_Tree_Cell;
  begin
    if Index > Nb_Nodes then
      raise Invalid_Index;
    end if;
    for I in 1 .. My_Tree.Children_Number (Tree.all) loop
      if I = 1 then
        My_Tree.Move_Child (Tree.all);
      else
        My_Tree.Move_Brother (Tree.all, False);
      end if;
      if I = Cell.Nb_Attributes + Index then
        My_Tree.Read (Tree.all, Child);
        case Child.Kind is
          when Xml_Parser.Element =>
            N := (Kind =>  Xml_Parser.Element,
                  Magic => Element.Magic,
                  In_Prologue => Element.In_Prologue,
                  Tree_Access => My_Tree.Get_Position (Tree.all));
          when Xml_Parser.Text =>
            N := (Kind => Xml_Parser.Text,
                  Magic => Element.Magic,
                  In_Prologue => Element.In_Prologue,
                  Tree_Access => My_Tree.Get_Position (Tree.all));
          when Xml_Parser.Comment =>
            N := (Kind => Xml_Parser.Comment,
                  Magic => Element.Magic,
                  In_Prologue => Element.In_Prologue,
                  Tree_Access => My_Tree.Get_Position (Tree.all));
          when  Xml_Parser.Attribute =>
            -- Attribute
            Trace ("Expecting kind element or text or comment, found attribute");
            raise Internal_Error;
        end case;
      end if;
    end loop;
    return N;
  end Get_Child;

  -- May raise No_Brother
  function Get_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Node_Type is
    Tree : Tree_Acc := Get_Tree (Ctx, Node);
    Cell : My_Tree_Cell := Get_Cell (Tree, Node);
    N : Node_Type;
  begin
    if not My_Tree.Has_Brother (Tree.all, not Next) then
      raise No_Brother;
    end if;
    My_Tree.Move_Brother (Tree.all, not Next);
    My_Tree.Read (Tree.all, Cell);
    case Cell.Kind is
      when Xml_Parser.Element =>
        N := (Kind =>  Xml_Parser.Element,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => My_Tree.Get_Position (Tree.all));
      when Xml_Parser.Text =>
        N := (Kind => Xml_Parser.Text,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => My_Tree.Get_Position (Tree.all));
      when Xml_Parser.Comment =>
        N := (Kind => Xml_Parser.Comment,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => My_Tree.Get_Position (Tree.all));
      when  Xml_Parser.Attribute =>
        -- Attribute
        Trace ("Expecting kind element or text or comment, found attribute");
        raise Internal_Error;
    end case;
    return N;
  end Get_Brother;

  -- Get the father of an element
  -- May raise No_Parent
  function Get_Parent (Ctx     : Ctx_Type;
                       Node : Node_Type) return Element_Type is
    Tree : Tree_Acc := Get_Tree (Ctx, Node);
    Cell : My_Tree_Cell := Get_Cell (Tree, Node);
    N : Node_Type;
  begin
    if not My_Tree.Has_Father (Tree.all) then
      raise No_Parent;
    end if;
    My_Tree.Move_Father (Tree.all);
    My_Tree.Read (Tree.all, Cell);
    if Cell.Kind = Xml_Parser.Element then
      N := (Kind =>  Xml_Parser.Element,
            Magic => Node.Magic,
            In_Prologue => Node.In_Prologue,
            Tree_Access => My_Tree.Get_Position (Tree.all));
    else
      -- Attribute or text or comment as parent of something!
      Trace ("Expecting kind element, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Element_Type (N);
  end Get_Parent;

  function Is_Root (Ctx  : Ctx_Type;
                    Node : Node_Type) return Boolean is
    Tree : Tree_Acc := Get_Tree (Ctx, Node);
    Cell : constant My_Tree_Cell := Get_Cell (Tree, Node);
  begin
    return not My_Tree.Has_Father (Tree.all);
  end Is_Root;

   -- TEXT
  function  Get_Text (Ctx  : Ctx_Type;
                      Text : Text_Type) return String is
  begin
    return Asu_Ts (Get_Text (Ctx, Text));
  end Get_Text;

  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type)
                     return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Text), Text);
  begin
    if Cell.Kind /= Xml_Parser.Text then
      Trace ("Expecting kind text, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Cell.Name;
  end Get_Text;

   -- Comment
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return String is
  begin
    return Asu_Ts (Get_Comment (Ctx, Comment));
  end Get_Comment;

  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type)
                        return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Comment), Comment);
  begin
    if Cell.Kind /= Xml_Parser.Comment then
      Trace ("Expecting kind Comment, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Cell.Name;
  end Get_Comment;

end Xml_Parser;

