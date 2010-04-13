with Ada.Exceptions, Ada.Unchecked_Deallocation;
with Environ, Basic_Proc, Rnd, Exception_Messenger, Directory;
package body Xml_Parser is

  -- Version incremented at each significant change
  Minor_Version : constant String := "1";
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

  -- Used in Tree_Mng when building a new update
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Attributes_Array, Attributes_Access);

  -- Unique list of IDs
  procedure Set (To : out Id_Cell;  Val : in Id_Cell) is
  begin
    To := Val;
  end Set;
  function Image (Element : Id_Cell) return String is
  begin
    return Asu_Ts (Element.Name);
  end Image;
  function "=" (Current : Id_Cell; Criteria : Id_Cell) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

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
    -- Move to root
    procedure Move_Root (Elements : in out My_Tree.Tree_Type);

    -- Add an element, move to it
    procedure Add_Element (Elements : in out My_Tree.Tree_Type;
                           Name : in Asu_Us; Line : in Natural);
    -- Add specific tuning to element (xml:space=preserve)
    Xml_Space : constant String := "xml:space";
    Preserve : constant String := "preserve";
    Xml_Space_Preserve : constant String := Xml_Space & "=" & Preserve;
    procedure Add_Tuning (Elements : in out My_Tree.Tree_Type;
                          Tuning : in String);
    -- Set Put_Empty
    procedure Set_Put_Empty (Elements : in out My_Tree.Tree_Type;
                             Put_Empty : in Boolean);

    -- Get all tuning of an element
    function Get_Tuning (Elements : My_Tree.Tree_Type) return String;
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
    -- Sets or overwrites a xml attribute at a given index
    procedure Set_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                  Name : in Asu_Us; Index : in Positive; Value : in Asu_Us);
    pragma Unreferenced (Set_Xml_Attribute);

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
    procedure Add_Pi (Tree : in out My_Tree.Tree_Type;
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

    -- Build the Node_Update associated to current Node
    -- The In_Prologue of the Update is not modified
    procedure Build_Update (Tree : in out My_Tree.Tree_Type;
                            Update : in out Node_Update;
                            Creation : in Boolean);
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
  -- Entities differ if one is parameter and not the other
  --  or if names differ
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean is
    use type Asu_Us;
  begin
    return Current.Parameter = Criteria.Parameter
    and then Current.Name = Criteria.Name;
  end "=";
  -- Dtd definition: unparsed entities and notations
  procedure Set (To : out Unparsed_Type; Val : in Unparsed_Type) is
  begin
    To := Val;
  end Set;
  function Image (Unparsed : Unparsed_Type) return String is
  begin
    if Unparsed.Is_Entity then
      return "E:" & Asu_Ts (Unparsed.Name);
    else
      return "N:" & Asu_Ts (Unparsed.Name);
    end if;
  end Image;
  function "=" (Current : Unparsed_Type; Criteria : Unparsed_Type)
               return Boolean is
    use type Asu_Us;
  begin
    return Current.Is_Entity = Criteria.Is_Entity
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

  -- If file path is relative and father non null, use father path
  -- If file path is relative and father null, use current dir
  function Build_Full_Name (In_File : in Asu_Us;
                            Father  : in Asu_Us := Asu_Null) return Asu_Us is
    use type Asu_Us;
  begin
    -- If Stdin or string or full path: keep it
    if In_File = Asu_Null or else Asu.Element (In_File, 1) = '/' then
      return In_File;
    end if;
    if Father /= Asu_Null then
      -- Father path & Current file path
      return Asu_Tus (Directory.Dirname (Asu_Ts (Father))) & In_File;
    else
      -- Current dir & Current file path
      return Asu_Tus (Directory.Get_Current) & '/' & In_File;
    end if;
  end Build_Full_Name;

  -- Deallocate a text char file
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Text_Char.File_Type, File_Access);

  -- Reset a Flow_Info
  procedure Reset (Flow_Info : in out Flow_Info_Type) is
  begin
    Flow_Info.Is_File := True;
    Flow_Info.Kind := Xml_Flow;
    Flow_Info.Name := Asu_Null;
    Flow_Info.Line := 0;
    Flow_Info.Same_Line := False;
    Flow_Info.Encod := Utf8;
    Flow_Info.Nb_Bytes := 0;
    Flow_Info.Prev_Char_Was_Cr := False;
    Flow_Info.File := null;
    Flow_Info.In_Str := Asu_Null;
    Flow_Info.In_Stri := 0;
  end Reset;

  -- Parses the content of the file into the tree
  package Parse_Mng is
    -- Use String_Flow as File_Name when Flow is Ctx.String, otherwise
    --  File_Name is the file name and will be open
    -- Parse the Xml flow. Raises exceptions
    procedure Parse_Xml (Ctx : in out Ctx_Type);
    -- Parse a Dtd Flow
    function String_Flow return String;
    procedure Parse_Dtd (Ctx : in out Ctx_Type;
                         Adtd : in out Dtd_Type);
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
  procedure Parse (Ctx       : out Ctx_Type;
                   File_Name : in String;
                   Ok        : out Boolean;
                   Comments  : in Boolean := False;
                   Expand    : in Boolean := True;
                   Use_Dtd   : in Boolean := True;
                   Dtd_File  : in String  := "";
                   Warn_Cb   : in Warning_Callback_Access := null;
                   Parse_Cb  : in Parse_Callback_Access := null) is
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
    -- Open file of Xml flow
    Ctx.Flow.Curr_Flow.Is_File := True;
    Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    Ctx.Flow.Curr_Flow.Name := Build_Full_Name (Asu_Tus (File_Name));
    Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
    Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    -- Parse this file
    Ctx.Parse_Comments := Comments;
    Ctx.Expand := Expand;
    Ctx.Use_Dtd := Use_Dtd;
    Ctx.Dtd_File := Asu_Tus (Dtd_File);
    Ctx.Warnings := Warn_Cb;
    Ctx.Callback := Parse_Cb;
    Parse_Mng.Parse_Xml (Ctx);
    -- Update status
    if Ctx.Callback = null then
      Ctx.Status := Parsed_Elements;
    else
      Clean (Ctx);
    end if;
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
    when Storage_Error =>
      raise;
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse;

  -- Return current status of context
  function Get_Status (Ctx : Ctx_Type) return Ctx_Status_List is
  begin
    return Ctx.Status;
  end Get_Status;

  -- Return the error message if Parse_Error
  function Get_Parse_Error_Message (Ctx : Ctx_Type) return String is
  begin
    case Ctx.Status is
      when Clean | Init | Parsed_Prologue_Cb =>
        raise Status_Error;
      when Parsed_Prologue | Parsed_Elements | Error =>
        return Asu_Ts (Ctx.Flow.Err_Msg);
    end case;
  end Get_Parse_Error_Message;

  -- Clean a parsing context
  procedure Clean (Ctx : in out Ctx_Type) is
    use type My_Tree.Position_Access;
    File : File_Access;
  begin
    -- Clean input flow
    Ctx.Flow.Nb_Got := 0;
    Ctx.Flow.Circ.Clear;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ctx.Flow.Curr_Str := Asu_Null;
    Ctx.Flow.Recording := False;
    Ctx.Flow.Skip_Recording := No_Skip_Rec;
    Ctx.Flow.Recorded := Asu_Null;
    Ctx.Flow.Flows.Clear;

    -- Clear Current flow
    Reset (Ctx.Flow.Curr_Flow);

    -- Clear allocated text files
    for I in 1 .. Ctx.Flow.Files.Length loop
      Ctx.Flow.Files.Pop (File);
      Deallocate (File);
    end loop;

    Ctx.Parse_Comments := False;
    Ctx.Expand := True;
    Ctx.Use_Dtd := True;
    Ctx.Dtd_File := Asu_Null;
    Ctx.Warnings := null;
    Ctx.Callback := null;
    Ctx.Level := 0;
    -- Clean prologue tree
    if not Ctx.Prologue.Is_Empty then
      Ctx.Prologue.Move_Root;
      Ctx.Prologue.Delete_Tree;
    end if;
    -- Clean element tree
    if not Ctx.Elements.Is_Empty then
      Ctx.Elements.Move_Root;
      Ctx.Elements.Delete_Tree;
    end if;
    -- Clean Doctype info
    Ctx.Doctype.Line_No := 1;
    Ctx.Doctype.Name    := Asu_Null;
    Ctx.Doctype.Public  := False;
    Ctx.Doctype.Pub_Id  := Asu_Null;
    Ctx.Doctype.File    := Asu_Null;
    Ctx.Doctype.Int_Def := Asu_Null;
    -- Clean Standalone tag
    Ctx.Standalone := False;
    -- Clean IDs and unparsed entities
    Ctx.Ids.Delete_List;
    Ctx.Idrefs.Delete_List;
    Ctx.Unparsed_List.Delete_List;
    -- Context is clean
    Ctx.Magic := Clean_Magic;
    Ctx.Status := Clean;
  end Clean;

  -- Dtd parsing
  procedure Parse_Dtd_Internal (Ctx      : in out Ctx_Type;
                                Dtd      : out Dtd_Type;
                                Error    : out Asu_Us) is
  begin
    Error := Asu_Null;
    Parse_Mng.Parse_Dtd (Ctx, Dtd);
    Clean (Ctx);
  exception
    when Parse_Occ:Parse_Error =>
      -- Retrieve and set parsing error message
      Clean (Ctx);
      Error := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Parse_Occ)));
    when Error_Occ:others =>
      Trace ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Dtd_Internal;

  procedure Parse_Dtd_File (
      File_Name : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out Ada.Strings.Unbounded.Unbounded_String) is
    Ctx : Ctx_Type;
  begin
    Clean_Dtd (Dtd);
    -- Flow is file
    Ctx.Flow.Curr_Flow.Is_File := True;
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    Ctx.Flow.Curr_Flow.Name := Build_Full_Name (Asu_Tus (File_Name));
    -- File Name_Error raises File_Error
    Ctx.Warnings := Warn_Cb;
    Parse_Dtd_Internal (Ctx, Dtd, Error);
  end Parse_Dtd_File;

  procedure Parse_Dtd_String (
      Str     : in String;
      Warn_Cb : in Warning_Callback_Access := null;
      Dtd     : out Dtd_Type;
      Error   : out Ada.Strings.Unbounded.Unbounded_String) is
    Ctx : Ctx_Type;
  begin
    Clean_Dtd (Dtd);
    -- Flow is string
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    Ctx.Flow.Curr_Flow.Name := Asu_Tus (Parse_Mng.String_Flow);
    Ctx.Flow.Curr_Flow.In_Str := Asu_Tus (Str);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    Ctx.Warnings := Warn_Cb;
    Parse_Dtd_Internal (Ctx, Dtd, Error);
  end Parse_Dtd_String;

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type) is
  begin
    -- Clean Dtd
    Dtd.Set := False;
    Dtd.Xml_Found := False;
    Dtd.Encoding := Asu_Null;
    Dtd.In_Include := False;
    Info_Mng.Delete_List (Dtd.Info_List);
    Entity_List_Mng.Delete_List (Dtd.Entity_List);
  end Clean_Dtd;

  -- Parse the prologue of a string
  -- may raise Status_Error if Ctx is not clean
  --    Parse_Error while parsing the string
  procedure Parse_Prologue (Ctx      : out Ctx_Type;
                            Str      : in String;
                            Ok       : out Boolean;
                            Comments : in Boolean := False;
                            Expand   : in Boolean := True;
                            Warn_Cb  : in Warning_Callback_Access := null;
                            Parse_Cb : in Parse_Callback_Access := null) is
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
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    Ctx.Flow.Curr_Flow.Name := Asu_Tus (Parse_Mng.String_Flow);
    Ctx.Flow.Curr_Flow.In_Str := Asu_Tus (Str);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    Ctx.Parse_Comments := Comments;
    Ctx.Expand := Expand;
    Ctx.Warnings := Warn_Cb;
    Ctx.Callback := Parse_Cb;
    Parse_Mng.Parse_Prologue (Ctx);
    -- Update status
    if Ctx.Callback = null then
      Ctx.Status := Parsed_Prologue;
    else
      Ctx.Status := Parsed_Prologue_Cb;
    end if;
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
    when Storage_Error =>
      raise;
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
    -- Status must be Parsed_Prologue [_Cb]
    case Ctx.Status is
      when Clean | Init =>
        raise Status_Error;
      when Parsed_Elements =>
        raise End_Error;
      when Error =>
        raise Loc_Parse_Error;
      when Parsed_Prologue | Parsed_Prologue_Cb =>
        null;
    end case;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Parse
    Parse_Mng.Parse_Elements (Ctx, Dtd);
    if Ctx.Callback = null then
      Ctx.Status := Parsed_Elements;
    else
      Clean (Ctx);
    end if;
    -- Close the file
    Ok := True;
  exception
    when Status_Error | End_Error =>
      raise;
    when Loc_Parse_Error =>
      -- Raising Parse_Error because previous parsing detected error
      Trace ("Parse error because prologue did not parse");
      raise Parse_Error;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := Asu_Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Storage_Error =>
      raise;
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
                   Ok  : out Boolean;
                   Warn_Cb  : in Warning_Callback_Access := null) is
  begin
    -- Status must be Parsed_xxx or Init
    if Ctx.Status = Clean
    or else Ctx.Status = Error
    or else Ctx.Status = Parsed_Prologue_Cb then
      raise Status_Error;
    end if;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg := Asu_Null;
    Ok := False;
    -- Check this context
    if not Ctx.Elements.Is_Empty then
      Ctx.Elements.Move_Root;
    end if;
    Ctx.Warnings := Warn_Cb;
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
    when Storage_Error =>
      raise;
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
    Tree.Set_Position (Node.Tree_Access);
    Tree.Read (Cell);
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

 -- Get the Target of a PI
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return String is
  begin
    return Asu_Ts (Get_Target (Ctx, Pi_Node));
  end Get_Target;

  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type)
                    return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Pi_Node), Pi_Node);
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status = Clean then
      raise Status_Error;
    end if;
    return Cell.Name;
  end Get_Target;

  -- Get a PI data
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return String is
  begin
    return Asu_Ts (Get_Pi (Ctx, Pi_Node));
  end Get_Pi;

  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type)
           return Ada.Strings.Unbounded.Unbounded_String is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Pi_Node), Pi_Node);
  begin
    if Ctx.Status = Error then
      raise Parse_Error;
    elsif Ctx.Status = Clean then
      raise Status_Error;
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
    Ctx.Prologue.Move_Root;
    return (Kind => Element,
            Magic => Ctx.Magic,
            In_Prologue => True,
            Tree_Access => Ctx.Prologue.Get_Position);
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
    Ctx.Elements.Move_Root;
    return (Kind => Element,
            Magic => Ctx.Magic,
            In_Prologue => False,
            Tree_Access => Ctx.Elements.Get_Position);
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
    Tree : constant Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
    A : Attributes_Array (1 .. Cell.Nb_Attributes);
  begin
    for I in A'Range loop
      if I = A'First then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      Tree.Read (Cell);
      if Cell.Kind /= Attribute then
        Trace ("Expecting kind attribute, found " & Cell.Kind'Img);
        raise Internal_Error;
      end if;
      A(I).Name := Cell.Name;
      A(I).Value := Cell.Value;
      A(I).Unparsed := Cell.Unparsed;
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
    Tree : constant Tree_Acc := Get_Tree (Ctx, Element);
    Cell : My_Tree_Cell := Get_Cell (Tree, Element);
  begin
    if Index > Cell.Nb_Attributes then
      raise Invalid_Index;
    end if;
    for I in 1 .. Index loop
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
    end loop;
    Tree.Read (Cell);
    if Cell.Kind /= Attribute then
      Trace ("Expecting kind attribute, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return (Cell.Name, Cell.Value, Cell.Unparsed);
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
    Tree : constant Tree_Acc := Get_Tree (Ctx, Element);
    Cell : constant My_Tree_Cell := Get_Cell (Tree, Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural
             := Tree.Children_Number - Cell.Nb_Attributes;
    N : Nodes_Array (1 .. Nb_Nodes);
    Child : My_Tree_Cell;
  begin
    for I in 1 .. Tree.Children_Number loop
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      if I > Cell.Nb_Attributes then
        Tree.Read (Child);
        case Child.Kind is
          when Xml_Parser.Element =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind =>  Xml_Parser.Element,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Text =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Text,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Pi =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Pi,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Comment =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Comment,
                   Magic => Element.Magic,
                   In_Prologue => Element.In_Prologue,
                   Tree_Access => Tree.Get_Position);
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
    Tree : constant Tree_Acc := Get_Tree (Ctx, Element);
    Cell : constant My_Tree_Cell := Get_Cell (Tree, Element);
  begin
    -- Nb of nodes is Nb in tree - attributes
    return Tree.Children_Number - Cell.Nb_Attributes;
  end Get_Nb_Children;

  -- May raise Invalid_Index
  function Get_Child (Ctx     : Ctx_Type;
                      Element : Element_Type;
                      Index   : Positive) return Node_Type is
    Tree : constant Tree_Acc := Get_Tree (Ctx, Element);
    Cell : constant My_Tree_Cell := Get_Cell (Tree, Element);
    -- Nb of nodes is Nb in tree - attributes
    Nb_Nodes : constant Natural := Tree.Children_Number - Cell.Nb_Attributes;
    N : Node_Type;
    Child : My_Tree_Cell;
  begin
    if Index > Nb_Nodes then
      raise Invalid_Index;
    end if;
    for I in 1 .. Tree.Children_Number loop
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      if I = Cell.Nb_Attributes + Index then
        exit;
      end if;
    end loop;

    Tree.Read (Child);
    case Child.Kind is
      when Xml_Parser.Element =>
        N := (Kind =>  Xml_Parser.Element,
              Magic => Element.Magic,
              In_Prologue => Element.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Text =>
        N := (Kind => Xml_Parser.Text,
              Magic => Element.Magic,
              In_Prologue => Element.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Pi =>
        N := (Kind => Xml_Parser.Pi,
              Magic => Element.Magic,
              In_Prologue => Element.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Comment =>
        N := (Kind => Xml_Parser.Comment,
              Magic => Element.Magic,
              In_Prologue => Element.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when  Xml_Parser.Attribute =>
        -- Attribute
        Trace ("Expecting kind element or text or comment, found attribute");
        raise Internal_Error;
    end case;
    return N;
  end Get_Child;

  -- May raise No_Brother
  function Get_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Node_Type is
    Tree : constant Tree_Acc := Get_Tree (Ctx, Node);
    Cell : My_Tree_Cell := Get_Cell (Tree, Node);
    N : Node_Type;
  begin
    if not Tree.Has_Brother (not Next) then
      raise No_Brother;
    end if;
    Tree.Move_Brother (not Next);
    Tree.Read (Cell);
    case Cell.Kind is
      when Xml_Parser.Element =>
        N := (Kind =>  Xml_Parser.Element,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Text =>
        N := (Kind => Xml_Parser.Text,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Pi =>
        N := (Kind => Xml_Parser.Pi,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Comment =>
        N := (Kind => Xml_Parser.Comment,
              Magic => Node.Magic,
              In_Prologue => Node.In_Prologue,
              Tree_Access => Tree.Get_Position);
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
    Tree : constant Tree_Acc := Get_Tree (Ctx, Node);
    Cell : My_Tree_Cell := Get_Cell (Tree, Node);
    N : Node_Type;
  begin
    if not Tree.Has_Father then
      raise No_Parent;
    end if;
    Tree.Move_Father;
    Tree.Read (Cell);
    if Cell.Kind = Xml_Parser.Element then
      N := (Kind =>  Xml_Parser.Element,
            Magic => Node.Magic,
            In_Prologue => Node.In_Prologue,
            Tree_Access => Tree.Get_Position);
    else
      -- Attribute or text or comment as parent of something!
      Trace ("Expecting kind element, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Element_Type (N);
  end Get_Parent;

  function Is_Root (Ctx  : Ctx_Type;
                    Node : Node_Type) return Boolean is
    Tree : constant Tree_Acc := Get_Tree (Ctx, Node);
  begin
    return not Tree.Has_Father;
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

  -- Unparsed entity
  procedure Get_Unparsed_Entity_Info (Ctx    : in out Ctx_Type;
                                      Entity : in String;
                                      Info   : out Unparsed_Entity_Info_Rec) is
    Rec : Unparsed_Type;
  begin
    case Ctx.Status is
      when Clean | Init =>
        raise Status_Error;
      when Error =>
        raise Parse_Error;
      when Parsed_Elements | Parsed_Prologue | Parsed_Prologue_Cb =>
        null;
    end case;
    -- Get entity
    Rec.Is_Entity := True;
    Rec.Name := Asu_Tus (Entity);
    begin
      Ctx.Unparsed_List.Read (Rec, Rec);
    exception
      when Unparsed_List_Mng.Not_In_List =>
        raise Unknown_Entity;
    end;
    Info.Entity_System_Id := Rec.System_Id;
    Info.Entity_Public_Id := Rec.Public_Id;
    Info.Notation_Name := Rec.Notation;
    -- Get notation
    Rec.Is_Entity := False;
    Rec.Name := Rec.Notation;
    begin
      Ctx.Unparsed_List.Read (Rec, Rec);
    exception
      when Unparsed_List_Mng.Not_In_List =>
        -- Should not occur (checked at end of Dtd parsing)
        raise Internal_Error;
    end;
    Info.Notation_System_Id := Rec.System_Id;
    Info.Notation_Public_Id := Rec.Public_Id;
  end Get_Unparsed_Entity_Info;

  -- Shall the Element, if empty, be put with EmptyElemTag (<element/>) or
  -- with STag and ETag (<element></elememt>)
  function Get_Put_Empty (Ctx     : Ctx_Type;
                          Element : Element_Type) return Boolean is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Put_Empty;
  end Get_Put_Empty;

  -----------------------------------
  -- Deallocation and Finalization --
  -----------------------------------
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (My_Tree.Tree_Type, Tree_Acc);
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Id_List_Mng.List_Type, Id_List_Access);
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Idref_List_Mng.List_Type, Idref_List_Access);
  overriding procedure Finalize (Ctx : in out Ctx_Type) is
  begin
    -- Clean contenxt and its data
    Clean (Ctx);
    -- Deallocate trees and ID lists
    Deallocate (Ctx.Prologue);
    Deallocate (Ctx.Elements);
    Deallocate (Ctx.Ids);
    Deallocate (Ctx.Idrefs);
  end Finalize;

  overriding procedure Finalize (Node : in out Node_Update) is
  begin
    Deallocate (Node.Attributes);
  end Finalize;

end Xml_Parser;

