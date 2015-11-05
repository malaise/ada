with Ada.Exceptions, Ada.Unchecked_Deallocation;
with Trace.Loggers, Exception_Messenger, Directory, Str_Util,
     Regular_Expressions;
package body Xml_Parser is

  -- Version incremented at each significant change
  Minor_Version : constant String := "0";
  function Version return String is
  begin
    return "V" & Major_Version & "." & Minor_Version;
  end Version;

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
    return Element.Name.Image;
  end Image;
  function "=" (Current : Id_Cell; Criteria : Id_Cell) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  -- Trace a debug message
  Logger : Trace.Loggers.Logger;
  procedure Debug (Msg : in String);
  function Debug_On return Boolean;
  -- Other specific severities
  -- Callback arguments
  use type Trace.Severities;
  Cb_Severity : constant Trace.Severities := Trace.Debug * 2;

  -- File management
  package File_Mng is
    -- Open file, raises File_Error if name error
    procedure Open (File_Name : in String; File : in out Text_Char.File_Type);
    procedure Close (File : in out Text_Char.File_Type);
  end File_Mng;

  package body File_Mng is
    -- Open file, raises File_Error if name error
    procedure Open (File_Name : in String; File : in out Text_Char.File_Type) is
    begin
      if Text_Char.Is_Open (File) then
        raise Text_Char.Status_Error;
      end if;
      if File_Name = Stdin then
        File.Open_All ("");
      else
        File.Open_All (File_Name);
      end if;
    exception
      when Text_Char.Name_Error =>
        raise File_Error;
    end Open;

    procedure Close (File : in out Text_Char.File_Type) is
    begin
      File.Close_All;
    end Close;
  end File_Mng;

  -- My tree manipulation
  package Tree_Mng is
    -- Move to root
    procedure Move_Root (Elements : in out My_Tree.Tree_Type);

    -- Add an element, move to it
    procedure Add_Element (Elements : in out My_Tree.Tree_Type;
                           Name : in As.U.Asu_Us; Line : in Natural);
    -- Set namespace of current element
    procedure Set_Namespace (Elements : in out My_Tree.Tree_Type;
                             Namespace : in As.U.Asu_Us);
    -- Add specific tuning to element (xml:space=preserve)
    Xml_Space : constant String := "xml:space";
    Preserve : constant String := "preserve";
    Xml_Space_Preserve : constant String := Xml_Space & "=" & Preserve;
    procedure Add_Tuning (Elements : in out My_Tree.Tree_Type;
                          Tuning : in String);
    -- Set Empty_Info
    procedure Set_Empty_Info (Elements : in out My_Tree.Tree_Type;
                              Empty_Info : in  Empty_Info_List);
    -- Set Is_Mixed
    procedure Set_Is_Mixed (Elements : in out My_Tree.Tree_Type;
                            Is_Mixed : in Boolean);

    -- Get all tuning of an element
    function Get_Tuning (Elements : My_Tree.Tree_Type) return String;
    -- Add an attribute to current element, remain on current element
    procedure Add_Attribute (Elements : in out My_Tree.Tree_Type;
                             Name, Value : in As.U.Asu_Us;
                             Line : in Natural);
    -- Check if an attribute exists for current element
    function Attribute_Exists (Elements : in out My_Tree.Tree_Type;
                               Name : in As.U.Asu_Us) return Boolean;
    -- Get an attribute (if it exists, otherwise "")
    function Get_Attribute (Elements : in out My_Tree.Tree_Type;
                            Name : in As.U.Asu_Us) return As.U.Asu_Us;
    -- Initialise an empty prologue
    procedure Init_Prologue (Prologue : in out My_Tree.Tree_Type);
    -- Set xml directive, add a xml attribute
    procedure Set_Xml (Prologue : in out My_Tree.Tree_Type;
                       Line : in Natural);
    procedure Add_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                 Name, Value : in As.U.Asu_Us;
                                 Line : in Natural);
    -- Sets or overwrites a xml attribute at a given index
    procedure Set_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                  Name : in As.U.Asu_Us; Index : in Positive;
                  Value : in As.U.Asu_Us);
    pragma Unreferenced (Set_Xml_Attribute);

    -- Check xml is set, find an attribute (Index is 0 if not found)
    function Xml_Existst (Prologue : in out My_Tree.Tree_Type) return Boolean;
    procedure Find_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                  Name : in As.U.Asu_Us;
                                  Index : out Natural;
                                  Value : out As.U.Asu_Us);
    function Get_Nb_Xml_Attributes (Prologue : in out My_Tree.Tree_Type)
                                   return Natural;
    -- Add a processing instruction
    procedure Add_Pi (Tree : in out My_Tree.Tree_Type;
                      Name, Text : in As.U.Asu_Us; Line : in Natural);

    -- Add a text to current cell (of elements or prologue)
    -- remain on current cell
    procedure Add_Text (Tree : in out My_Tree.Tree_Type;
                        Text : in As.U.Asu_Us; Line : in Natural);

    -- Add a comment to current cell (of elements or prologue)
    -- remain on current cell
    procedure Add_Comment (Tree : in out My_Tree.Tree_Type;
                           Comment : in As.U.Asu_Us; Line : in Natural);

    -- Build the Node_Update associated to current Node
    -- The Stage of the Update is not modified
    procedure Build_Update (Tree : in out My_Tree.Tree_Type;
                            Update : in out Node_Update;
                            Creation : in Boolean);
  end Tree_Mng;
  package body Tree_Mng is separate;

  -- Parsed entities
  procedure Set (To : out Entity_Type; Val : in Entity_Type) is
  begin
    To := Val;
  end Set;
  function Image (Entity : Entity_Type) return String is
  begin
    return (if Entity.Parameter then "%" else "") & Entity.Name.Image;
  end Image;
  -- Entities differ if one is parameter and not the other
  --  or if names differ
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Parameter = Criteria.Parameter
    and then Current.Name = Criteria.Name;
  end "=";

  -- Unparsed entities and notations
  procedure Set (To : out Unparsed_Type; Val : in Unparsed_Type) is
  begin
    To := Val;
  end Set;
  function Image (Unparsed : Unparsed_Type) return String is
  begin
    return (if Unparsed.Is_Entity then "E:" else "N:") & Unparsed.Name.Image;
  end Image;
  function "=" (Current : Unparsed_Type; Criteria : Unparsed_Type)
               return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Is_Entity = Criteria.Is_Entity
    and then Current.Name = Criteria.Name;
  end "=";

  -- Parsed infos
  procedure Set (To : out Info_Rec; Val : in Info_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Info_Rec) return String is
  begin
    return Element.Name.Image;
  end Image;
  function "=" (Current : Info_Rec; Criteria : Info_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  -- Namespaces
  procedure Set (To : out Namespace_Type; Val : in Namespace_Type) is
  begin
    To := Val;
  end Set;
  function Image (Namespace : Namespace_Type) return String is
  begin
    return Namespace.Prefix.Image;
  end Image;
  function "=" (Current : Namespace_Type; Criteria : Namespace_Type)
               return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Prefix = Criteria.Prefix;
  end "=";

  -- If file path is relative and father non null, use father path
  -- If file path is relative and father null, use current dir
  function Build_Full_Name (In_File : in As.U.Asu_Us;
                            Father  : in As.U.Asu_Us := As.U.Asu_Null)
           return As.U.Asu_Us is
    use type As.U.Asu_Us;
  begin
    -- If Stdin or string or full path: keep it
    if In_File.Is_Null or else In_File.Element (1) = '/' then
      return In_File;
    end if;
    if not Father.Is_Null then
      -- Father path & Current file path
      return Directory.Dirname (Father.Image) & In_File;
    else
      -- Current dir & Current file path
      return As.U.Tus (Directory.Get_Current) & '/' & In_File;
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
    Flow_Info.Name.Set_Null;
    Flow_Info.Line := 0;
    Flow_Info.Same_Line := False;
    Flow_Info.Encod := Utf8;
    Flow_Info.Nb_Bytes := 0;
    Flow_Info.File := null;
    Flow_Info.In_Str.Set_Null;
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
    procedure Clean_Dtd (Adtd : in out Dtd_Type);
    -- Parse the prologue
    procedure Parse_Prologue (Ctx : in out Ctx_Type;
                              Adtd : out Dtd_Type);
    -- Parse the elements
    procedure Parse_Elements (Ctx : in out Ctx_Type;
                              Adtd : in out Dtd_Type);
    -- Check the Ctx vs its Dtd, Raises exceptions
    procedure Check (Ctx : in out Ctx_Type);
    -- Update Is_Mixed of elements from fill set of children
    procedure Update_Is_Mixed (Ctx : in out Ctx_Type);

    -- Propagate checks to the Generator
    function Name_Ok (Name : As.U.Asu_Us;
                    Allow_Token : Boolean := False) return Boolean;
    function Is_Valid_Encoding (Name : As.U.Asu_Us) return Boolean;
    function Is_Valid_Pubid (Name : As.U.Asu_Us) return Boolean;
    -- Text utility
    function Is_Separators (Text : As.U.Asu_Us) return Boolean;
  end Parse_Mng;
  package body Parse_Mng is separate;

  ---------------------------------------
  -- Propagate checks to the Generator --
  ---------------------------------------
  function Name_Ok (Name : As.U.Asu_Us;
                    Allow_Token : Boolean := False) return Boolean
                    renames Parse_Mng.Name_Ok;
  function Is_Valid_Encoding (Name : As.U.Asu_Us) return Boolean
                             renames Parse_Mng.Is_Valid_Encoding;
  function Is_Valid_Pubid (Name : As.U.Asu_Us) return Boolean
                          renames Parse_Mng.Is_Valid_Pubid;

  -----------
  -- DEBUG --
  -----------
  -- Debug debug message
  procedure Debug (Msg : in String) is
    use type Trilean.Trilean;
  begin
    Logger.Log_Debug (Msg, "Xml_Parser");
  end Debug;

  function Debug_On return Boolean is
    use type Trilean.Trilean;
  begin
    if not Logger.Is_Init then
      -- First call, set debug
      Logger.Init ("Xml_Parser");
    end if;
    return Logger.Debug_On;
  end Debug_On;

  function Get_Magic return Magic_Numbers.Magic_Long is
  begin
    return Magic_Numbers.Generate;
  end Get_Magic;

  -------------
  -- PARSING --
  -------------
  procedure Clean (Ctx : in out Ctx_Type;
                   Reset_Unparsed_Entities : in Boolean);
  procedure Clean (Ctx : in out Ctx_Type) is
  begin
    Clean (Ctx, Reset_Unparsed_Entities => True);
  end Clean;

  -- Parse a Xml file, stdin if empty
  -- May raise File_Error, Parse_Error
  procedure Parse (Ctx       : out Ctx_Type;
                   File_Name : in String;
                   Ok        : out Boolean;
                   Comments  : in Boolean := False;
                   Cdata     : in Cdata_Policy_List := Remove_Cdata_Markers;
                   Expand    : in Boolean := True;
                   Normalize : in Boolean := True;
                   Use_Dtd   : in Boolean := True;
                   Dtd_File  : in String  := "";
                   Namespace : in Boolean := False;
                   Warn_Cb   : in Warning_Callback_Access := null;
                   Parse_Cb  : in Parse_Callback_Access := null) is
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
    Ctx.Flow.Err_Msg.Set_Null;
    Ok := False;
    -- Open file of Xml flow
    Ctx.Flow.Curr_Flow.Is_File := True;
    Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    Ctx.Flow.Curr_Flow.Name := Build_Full_Name (As.U.Tus (File_Name));
    Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
    Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    -- Parse this file
    Ctx.Parse_Comments := Comments;
    Ctx.Expand := Expand;
    Ctx.Cdata_Policy := Cdata;
    Ctx.Normalize := Normalize;
    Ctx.Use_Dtd := Use_Dtd;
    Ctx.Dtd_File := As.U.Tus (Dtd_File);
    Ctx.Namespace := Namespace;
    Ctx.Warnings := Warn_Cb;
    Ctx.Callback := Parse_Cb;
    Parse_Mng.Parse_Xml (Ctx);
    -- Update status
    if Ctx.Callback = null then
      Ctx.Status := Parsed_Elements;
    else
      -- Keep unparsed entities after successful parsing with callback
      Clean (Ctx, Reset_Unparsed_Entities => False);
    end if;
    Ok := True;
  exception
    when File_Error | Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      declare
        Loc_Occ : Ada.Exceptions.Exception_Occurrence;
      begin
        Ada.Exceptions.Save_Occurrence (Loc_Occ, Error_Occ);
        Exception_Messenger.Exception_Message (Loc_Occ, Ctx.Flow.Err_Msg);
      end;
      Ok := False;
    when Storage_Error =>
      raise;
    when Error_Occ:others =>
      Debug ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
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
      when Clean | Init | Unparsed | Parsed_Prologue_Cb =>
        raise Status_Error;
      when Parsed_Prologue | Parsed_Elements | Error =>
        return Ctx.Flow.Err_Msg.Image;
    end case;
  end Get_Parse_Error_Message;

  -- Clean a parsing context
  procedure Clean (Ctx : in out Ctx_Type;
                   Reset_Unparsed_Entities : in Boolean) is
    use type My_Tree.Position_Access;
    File : File_Access;
  begin
    -- Clean input flow
    Ctx.Flow.Nb_Got := 0;
    Ctx.Flow.Circ.Clear;
    Ctx.Flow.Err_Msg.Set_Null;
    Ctx.Flow.Curr_Str.Set_Null;
    Ctx.Flow.Recording := False;
    Ctx.Flow.Skip_Recording := No_Skip_Rec;
    Ctx.Flow.Recorded.Set_Null;
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
    Ctx.Cdata_Policy := Remove_Cdata_Markers;
    Ctx.Normalize := True;
    Ctx.Use_Dtd := True;
    Ctx.Dtd_File.Set_Null;
    Ctx.Namespace := False;
    Ctx.Warnings := null;
    Ctx.Callback := null;
    Ctx.Level := 0;
    -- Clean prologue tree
    if not Ctx.Prologue.Is_Empty then
      Ctx.Prologue.Move_Root;
      Ctx.Prologue.Delete_Tree;
    end if;
    Ctx.Stage := Prologue;
    -- Clean element tree
    if not Ctx.Elements.Is_Empty then
      Ctx.Elements.Move_Root;
      Ctx.Elements.Delete_Tree;
    end if;
    -- Clean tail
    if not Ctx.Tail.Is_Empty then
      Ctx.Tail.Move_Root;
      Ctx.Tail.Delete_Tree;
    end if;
    -- Clean Doctype info
    Ctx.Doctype.Line_No := 1;
    Ctx.Doctype.Name.Set_Null;
    Ctx.Doctype.Public  := False;
    Ctx.Doctype.Pub_Id.Set_Null;
    Ctx.Doctype.File.Set_Null;
    Ctx.Doctype.Int_Def.Set_Null;
    -- Clean Standalone tag
    Ctx.Standalone := False;
    -- Clean IDs and unparsed entities
    Ctx.Ids.Delete_List;
    Ctx.Idrefs.Delete_List;
    if Reset_Unparsed_Entities then
      Ctx.Unparsed_List.Delete_List;
      -- Context is clean
      Ctx.Status := Clean;
    else
      -- Only unparsed entities can now be got
      Ctx.Status := Unparsed;
    end if;
    Ctx.Magic := Clean_Magic;
  end Clean;

  -- Dtd parsing
  procedure Parse_Dtd_Internal (Ctx      : in out Ctx_Type;
                                Dtd      : out Dtd_Type;
                                Error    : out As.U.Asu_Us) is
  begin
    Error.Set_Null;
    Parse_Mng.Parse_Dtd (Ctx, Dtd);
    Clean (Ctx);
  exception
    when File_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and set parsing error message
      Clean (Ctx);
      declare
        Loc_Occ : Ada.Exceptions.Exception_Occurrence;
      begin
        Ada.Exceptions.Save_Occurrence (Loc_Occ, Error_Occ);
        Exception_Messenger.Exception_Message (Loc_Occ, Error);
      end;
    when Error_Occ:others =>
      Debug ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Dtd_Internal;

  procedure Parse_Dtd_File (
      File_Name : in String;
      Warn_Cb   : in Warning_Callback_Access := null;
      Dtd       : out Dtd_Type;
      Error     : out As.U.Asu_Us) is
    Ctx : Ctx_Type;
  begin
    Clean_Dtd (Dtd);
    -- Flow is file
    Ctx.Flow.Curr_Flow.Is_File := True;
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    Ctx.Flow.Curr_Flow.Name := Build_Full_Name (As.U.Tus (File_Name));
    -- File Name_Error raises File_Error
    Ctx.Warnings := Warn_Cb;
    Parse_Dtd_Internal (Ctx, Dtd, Error);
  end Parse_Dtd_File;

  procedure Parse_Dtd_String (
      Str     : in String;
      Warn_Cb : in Warning_Callback_Access := null;
      Dtd     : out Dtd_Type;
      Error   : out As.U.Asu_Us) is
    Ctx : Ctx_Type;
  begin
    Clean_Dtd (Dtd);
    -- Flow is string
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    Ctx.Flow.Curr_Flow.Name := As.U.Tus (Parse_Mng.String_Flow);
    Ctx.Flow.Curr_Flow.In_Str := As.U.Tus (Str);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    Ctx.Warnings := Warn_Cb;
    Parse_Dtd_Internal (Ctx, Dtd, Error);
  end Parse_Dtd_String;

  -- Clean a dtd
  procedure Clean_Dtd (Dtd : in out Dtd_Type) is
  begin
    -- Clean Dtd
    Parse_Mng.Clean_Dtd (Dtd);
  end Clean_Dtd;

  -- Parse the prologue of a string
  -- may raise Status_Error if Ctx is not clean
  --    Parse_Error while parsing the string
  procedure Parse_Prologue (Ctx       : out Ctx_Type;
                            Str       : in String;
                            Dtd       : out Dtd_Type;
                            Ok        : out Boolean;
                            Comments  : in Boolean := False;
                            Cdata     : in Cdata_Policy_List
                                      := Remove_Cdata_Markers;
                            Expand    : in Boolean := True;
                            Normalize : in Boolean := True;
                            Use_Dtd   : in Boolean := True;
                            Dtd_File  : in String  := "";
                            Namespace : in Boolean := False;
                            Warn_Cb   : in Warning_Callback_Access := null;
                            Parse_Cb  : in Parse_Callback_Access := null) is
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
    Ctx.Flow.Err_Msg.Set_Null;
    Ok := False;
    -- Parse the prologue string
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    Ctx.Flow.Curr_Flow.Name := As.U.Tus (Parse_Mng.String_Flow);
    Ctx.Flow.Curr_Flow.In_Str := As.U.Tus (Str);
    Ctx.Flow.Curr_Flow.Line := 1;
    Ctx.Flow.Curr_Flow.Same_Line := False;
    Ctx.Parse_Comments := Comments;
    Ctx.Expand := Expand;
    Ctx.Cdata_Policy := Cdata;
    Ctx.Normalize := Normalize;
    Ctx.Use_Dtd := Use_Dtd;
    Ctx.Dtd_File := As.U.Tus (Dtd_File);
    Ctx.Namespace := Namespace;
    Ctx.Warnings := Warn_Cb;
    Ctx.Callback := Parse_Cb;
    Parse_Mng.Parse_Prologue (Ctx, Dtd);
    -- Update status
    Ctx.Status := (if Ctx.Callback = null then Parsed_Prologue
                   else Parsed_Prologue_Cb);
    Ok := True;
  exception
    when Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
     declare
        Loc_Occ : Ada.Exceptions.Exception_Occurrence;
      begin
        Ada.Exceptions.Save_Occurrence (Loc_Occ, Error_Occ);
        Exception_Messenger.Exception_Message (Loc_Occ, Ctx.Flow.Err_Msg);
      end;
      Ok := False;
    when Storage_Error =>
      raise;
    when Error_Occ:others =>
      Debug ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Prologue;

  -- Parse the elements (after the prologue) and tail of a string with a dtd
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
      when Clean | Init | Unparsed =>
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
    Ctx.Flow.Err_Msg.Set_Null;
    Ok := False;
    -- Parse
    Parse_Mng.Parse_Elements (Ctx, Dtd);
    if Ctx.Callback = null then
      Ctx.Status := Parsed_Elements;
    else
      -- Keep unparsed entities after successful parsing with callback
      Clean (Ctx, Reset_Unparsed_Entities => False);
    end if;
    -- Close the file
    Ok := True;
  exception
    when Status_Error | End_Error =>
      raise;
    when Loc_Parse_Error =>
      -- Raising Parse_Error because previous parsing detected error
      Debug ("Parse error because prologue did not parse");
      raise Parse_Error;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := As.U.Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Storage_Error =>
      raise;
    when Error_Occ:others =>
      Debug ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Parse_Elements;

  -----------
  -- CHECK --
  -----------
  -- Check the Ctx: parse the DTD (if any) and check the Ctx versus it
  --  (same effect as Xml_Parse.Parse)
  procedure Check (Ctx : in out Ctx_Type;
                   Ok  : out Boolean;
                   Expand    : in Trilean.Trilean := Trilean.Other;
                   Normalize : in Trilean.Trilean := Trilean.Other;
                   Use_Dtd   : in Trilean.Trilean := Trilean.Other;
                   Dtd_File  : in String  := "";
                   Namespace : in Trilean.Trilean := Trilean.Other;
                   Warn_Cb   : in Warning_Callback_Access := null) is
    use type Trilean.Trilean;
  begin
    -- Status must be Parsed_Prologue, Parsed_Elements or Init
    if Ctx.Status /= Parsed_Prologue
    and then Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    -- In case of exception...
    Ctx.Status := Error;
    Ctx.Flow.Err_Msg.Set_Null;
    Ok := False;
    -- Set attributes
    if Expand /= Trilean.Other then
      Ctx.Expand := Trilean.Tri2Boo (Expand);
    end if;
    if Normalize /= Trilean.Other then
      Ctx.Normalize := Trilean.Tri2Boo (Normalize);
    end if;
    if Use_Dtd /= Trilean.Other then
      Ctx.Use_Dtd := Trilean.Tri2Boo (Use_Dtd);
      Ctx.Dtd_File := As.U.Tus (Dtd_File);
    end if;
    if Namespace /= Trilean.Other then
      Ctx.Namespace := Trilean.Tri2Boo (Namespace);
    end if;
    Ctx.Warnings := Warn_Cb;
    -- Check
    Parse_Mng.Check (Ctx);
    Ctx.Status := Parsed_Elements;
    Ok := True;
  exception
      when File_Error | Status_Error =>
      raise;
    when Error_Occ:Parse_Error =>
      -- Retrieve and store parsing error message
      Ctx.Status := Error;
      Ctx.Flow.Err_Msg := As.U.Tus (
        Exception_Messenger.Exception_Message(
          Ada.Exceptions.Save_Occurrence (Error_Occ)));
      Ok := False;
    when Storage_Error =>
      raise;
    when Error_Occ:others =>
      Debug ("Got exception " & Ada.Exceptions.Exception_Name (Error_Occ));
      raise Internal_Error;
  end Check;

  -----------------------------
  -- PROLOGUE, ROOT and TAIL --
  -----------------------------
  -- Read internal tree cell of a node
  function Get_Tree (Ctx : Ctx_Type;
                     Node : Node_Type) return Tree_Acc is
    use type My_Tree.Position_Access, Magic_Numbers.Extended_Magic_Long;
  begin
    -- Node must be set
    if Node.Tree_Access = My_Tree.No_Position then
      raise Invalid_Node;
    end if;
    -- Status must be Parsed_Prologue, Parsed_Elements or Init
    if Ctx.Status /= Parsed_Prologue
    and then Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    -- Magic must be set and must match
    if Node.Magic = Clean_Magic
    or else Node.Magic /= Ctx.Magic then
      raise Use_Error;
    end if;
    return (case Node.Branch is
      when Prologue_Br => Ctx.Prologue,
      when Elements_Br => Ctx.Elements,
      when Tail_Br     => Ctx.Tail);
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

  -- Check that status is compatible with get
  procedure Check_For_Get (Status : Ctx_Status_List) is
  begin
    if Status = Error then
      raise Parse_Error;
    elsif Status = Clean or else Status = Unparsed then
      raise Status_Error;
    end if;
  end Check_For_Get;

  -- Is a Node valid (returned by Get_xxx)
  function Is_Valid (Node : Node_Type) return Boolean is
    use type My_Tree.Position_Access;
  begin
    return Node.Tree_Access /= My_Tree.No_Position;
  end Is_Valid;

  -- Line number of start of declaration of node
  function Get_Line_No (Ctx  : Ctx_Type;
                        Node : Node_Type) return Natural is
    Cell : constant My_Tree_Cell := Get_Cell (Get_Tree (Ctx, Node), Node);
  begin
    return Cell.Line_No;
  end Get_Line_No;

  -- Get Prologue of a parsed context (after Parse or Parse_Prologue)
  function Get_Prologue (Ctx : Ctx_Type) return Element_Type is
  begin
    Check_For_Get (Ctx.Status);
    -- Only prologue or full parsing completed
    Ctx.Prologue.Move_Root;
    return (Kind => Element,
            Magic => Ctx.Magic,
            Branch => Prologue_Br,
            Tree_Access => Ctx.Prologue.Get_Position);
  end Get_Prologue;

  -- Get elements'root after Parse or Parse_Elements
  --  may raise Status_Error if called before Parse_Elements
  function Get_Root_Element (Ctx : Ctx_Type) return Element_Type is
  begin
    Check_For_Get (Ctx.Status);
    if Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    if Ctx.Elements.Is_Empty then
      -- Ctx is initialized (prologue is set) but the root is not set
      --  (with the Generator)
      raise No_Root;
    end if;
    -- Only prologue or full parsing completed
    Ctx.Elements.Move_Root;
    return (Kind => Element,
            Magic => Ctx.Magic,
            Branch => Elements_Br,
            Tree_Access => Ctx.Elements.Get_Position);
  end Get_Root_Element;

  -- Get tail:
  function Get_Tail (Ctx : Ctx_Type) return Element_Type is
  begin
    Check_For_Get (Ctx.Status);
    if Ctx.Status /= Parsed_Elements
    and then Ctx.Status /= Init then
      raise Status_Error;
    end if;
    -- Only prologue or full parsing completed
    Ctx.Tail.Move_Root;
    return (Kind => Element,
            Magic => Ctx.Magic,
            Branch => Tail_Br,
            Tree_Access => Ctx.Tail.Get_Position);
  end Get_Tail;

  -- Get Doctype characteristics (prologue must have been parsed)
  procedure Get_Doctype (Ctx : in Ctx_Type;
       Name    : out As.U.Asu_Us;
       Public  : out Boolean;
       Pub_Id  : out As.U.Asu_Us;
       File    : out As.U.Asu_Us;
       Int_Def : out As.U.Asu_Us) is
  begin
    Check_For_Get (Ctx.Status);
    if Ctx.Doctype.Name.Is_Null then
      raise Doctype_Not_Set;
    end if;
    Name    := Ctx.Doctype.Name;
    Public  := Ctx.Doctype.Public;
    Pub_Id  := Ctx.Doctype.Pub_Id;
    File    := Ctx.Doctype.File;
    Int_Def := Ctx.Doctype.Int_Def;
  end Get_Doctype;

  -------------------------
  -- NAME AND ATTRIBUTES --
  -------------------------
  -- Get the name of an element
  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return String is
  begin
    return Get_Name (Ctx, Element).Image;
  end Get_Name;

  function Get_Name (Ctx     : Ctx_Type;
                     Element : Element_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Name;
  end Get_Name;

   -- Get the namespace name of an element
  function Get_Namespace (Ctx     : Ctx_Type;
                          Element : Element_Type) return String is
  begin
    return Get_Namespace (Ctx, Element).Image;
  end Get_Namespace;

  function Get_Namespace (Ctx     : Ctx_Type;
                          Element : Element_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Namespace;
  end Get_Namespace;

  -- If Namespace is empty then return Name
  -- Otherwise return Namespace^NameSuffix
  function Expand_Name (Name, Namespace : As.U.Asu_Us) return As.U.Asu_Us is
    Index : Natural := 0;
    use type As.U.Asu_Us;
  begin
    if Namespace.Is_Null then
      return Name;
    end if;
    Index := Str_Util.Locate (Name.Image, ":");
    if Index = 0 then
      -- DefaultNamespace^Name
      return Namespace & "^" & Name;
    else
      -- Namespace^NameSuffix
      return Namespace & "^" & Name.Slice (Index + 1, Name.Length);
    end if;
  end Expand_Name;

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
        Debug ("Expecting kind attribute, found " & Cell.Kind'Img);
        raise Internal_Error;
      end if;
      A(I).Name := Cell.Name;
      A(I).Namespace := Cell.Namespace;
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
      Debug ("Expecting kind attribute, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return (Cell.Name, Cell.Namespace, Cell.Value, Cell.Unparsed);
  end Get_Attribute;

  -- May raise Attribute_Not_Found
  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Name    : String) return As.U.Asu_Us is
    Attributes : constant Attributes_Array
               := Get_Attributes (Ctx, Element);
  begin
    for I in Attributes'Range loop
      if Attributes(I).Name.Image = Name then
        return Attributes(I).Value;
      end if;
    end loop;
    raise Attribute_Not_Found;
  end Get_Attribute;

  function Get_Attribute (Ctx     : Ctx_Type;
                          Element : Element_Type;
                          Name    : String) return String is
  begin
    return Get_Attribute (Ctx, Element, Name).Image;
  end Get_Attribute;

  --------------------------
  -- PI, TEXT and COMMENT --
  --------------------------
  -- Get the Target of a PI
  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return String is
  begin
    return Get_Target (Ctx, Pi_Node).Image;
  end Get_Target;

  function Get_Target (Ctx     : Ctx_Type;
                       Pi_Node : Pi_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Pi_Node), Pi_Node);
  begin
    return Cell.Name;
  end Get_Target;

  -- Get a PI data
  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return String is
  begin
    return Get_Pi (Ctx, Pi_Node).Image;
  end Get_Pi;

  function Get_Pi (Ctx : in Ctx_Type;
                   Pi_Node : Pi_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Pi_Node), Pi_Node);
  begin
    return Cell.Value;
  end Get_Pi;

   -- TEXT
  function  Get_Text (Ctx  : Ctx_Type;
                      Text : Text_Type) return String is
  begin
    return Get_Text (Ctx, Text).Image;
  end Get_Text;

  function Get_Text (Ctx  : Ctx_Type;
                     Text : Text_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Text), Text);
  begin
    if Cell.Kind /= Xml_Parser.Text then
      Debug ("Expecting kind text, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Cell.Name;
  end Get_Text;

  function Is_Separators (Text : String) return Boolean is
  begin
    return Is_Separators (As.U.Tus (Text));
  end Is_Separators;

  function Is_Separators (Text : As.U.Asu_Us) return Boolean
           renames Parse_Mng.Is_Separators;

   -- Comment
  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return String is
  begin
    return Get_Comment (Ctx, Comment).Image;
  end Get_Comment;

  function Get_Comment (Ctx     : Ctx_Type;
                        Comment : Comment_Type) return As.U.Asu_Us is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Comment), Comment);
  begin
    if Cell.Kind /= Xml_Parser.Comment then
      Debug ("Expecting kind Comment, found " & Cell.Kind'Img);
      raise Internal_Error;
    end if;
    return Cell.Name;
  end Get_Comment;

  ----------------
  -- NAVIGATION --
  ----------------
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
                   Branch => Element.Branch,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Text =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Text,
                   Magic => Element.Magic,
                   Branch => Element.Branch,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Pi =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Pi,
                   Magic => Element.Magic,
                   Branch => Element.Branch,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Comment =>
            N (I - Cell.Nb_Attributes) :=
                  (Kind => Xml_Parser.Comment,
                   Magic => Element.Magic,
                   Branch => Element.Branch,
                   Tree_Access => Tree.Get_Position);
          when Xml_Parser.Attribute =>
            -- Attribute
            Debug ("Expecting kind text or element or comment, found attribute");
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
              Branch => Element.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Text =>
        N := (Kind => Xml_Parser.Text,
              Magic => Element.Magic,
              Branch => Element.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Pi =>
        N := (Kind => Xml_Parser.Pi,
              Magic => Element.Magic,
              Branch => Element.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Comment =>
        N := (Kind => Xml_Parser.Comment,
              Magic => Element.Magic,
              Branch => Element.Branch,
              Tree_Access => Tree.Get_Position);
      when  Xml_Parser.Attribute =>
        -- Attribute
        Debug ("Expecting kind element or text or comment, found attribute");
        raise Internal_Error;
    end case;
    return N;
  end Get_Child;

  function Has_Brother (Ctx  : Ctx_Type;
                        Node : Node_Type;
                        Next : Boolean := True) return Boolean is
    Tree : constant Tree_Acc := Get_Tree (Ctx, Node);
  begin
    Tree.Set_Position (Node.Tree_Access);
    return Tree.Has_Brother (not Next);
  end Has_Brother;

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
              Branch => Node.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Text =>
        N := (Kind => Xml_Parser.Text,
              Magic => Node.Magic,
              Branch => Node.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Pi =>
        N := (Kind => Xml_Parser.Pi,
              Magic => Node.Magic,
              Branch => Node.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Comment =>
        N := (Kind => Xml_Parser.Comment,
              Magic => Node.Magic,
              Branch => Node.Branch,
              Tree_Access => Tree.Get_Position);
      when Xml_Parser.Attribute =>
        -- Attribute
        Debug ("Expecting kind element or text or comment, found attribute");
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
            Branch => Node.Branch,
            Tree_Access => Tree.Get_Position);
    else
      -- Attribute or text or comment as parent of something!
      Debug ("Expecting kind element, found " & Cell.Kind'Img);
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

  --------------------------
  -- UNPARSED ENTITY info --
  --------------------------
  procedure Get_Unparsed_Entity_Info (Ctx    : in out Ctx_Type;
                                      Entity : in String;
                                      Info   : out Unparsed_Entity_Info_Rec) is
    Rec : Unparsed_Type;
  begin
    case Ctx.Status is
      when Clean =>
        raise Status_Error;
      when Error =>
        raise Parse_Error;
      when Init | Parsed_Elements | Parsed_Prologue | Parsed_Prologue_Cb
         | Unparsed =>
        -- Ok
        null;
    end case;
    -- Get entity
    Rec.Is_Entity := True;
    Rec.Name := As.U.Tus (Entity);
    begin
      Ctx.Unparsed_List.Read (Rec);
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
      Ctx.Unparsed_List.Read (Rec);
    exception
      when Unparsed_List_Mng.Not_In_List =>
        -- Should not occur (checked at end of Dtd parsing)
        raise Internal_Error;
    end;
    Info.Notation_System_Id := Rec.System_Id;
    Info.Notation_Public_Id := Rec.Public_Id;
  end Get_Unparsed_Entity_Info;

  -------------------
  -- Specific TAGS --
  -------------------
  -- Shall the Element, if empty, be put with EmptyElemTag (<element/>) or
  -- with STag and ETag (<element></elememt>)
  function Get_Empty_Info (Ctx     : Ctx_Type;
                          Element : Element_Type) return Empty_Info_List is
    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Empty_Info;
  end Get_Empty_Info;

  -- Is this element Mixed: either Mixed in Dtd or its first child is Text
  function Get_Is_Mixed (Ctx     : Ctx_Type;
                         Element : Element_Type) return Boolean is    Cell : constant My_Tree_Cell
         := Get_Cell (Get_Tree (Ctx, Element), Element);
  begin
    return Cell.Is_Mixed;
  end Get_Is_Mixed;

  -- Update the Is_Mixed tag of each element accorting to ALL its
  --  children: If an element hasn't Is_Mixed set and has a child
  --  of kind Text, then set its tag
  --  may raise Status_Error if Ctx is not parsed nor checked
  procedure Update_Is_Mixed (Ctx : in out Ctx_Type) is
  begin
    if Ctx.Status /= Parsed_Elements then
      raise Status_Error;
    end if;
    Parse_Mng.Update_Is_Mixed (Ctx);
  end Update_Is_Mixed;

  -----------------------------------
  -- Deallocation and Finalization --
  -----------------------------------
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (My_Tree.Tree_Type, Tree_Acc);
  procedure Deallocate is new Ada.Unchecked_Deallocation
   (Id_List_Mng.Unique_List_Type, Id_List_Access);
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

  overriding procedure Adjust (Node : in out Node_Update) is
  begin
    -- Copy attributes
    if Node.Attributes /= null then
      Node.Attributes := new Attributes_Array'(Node.Attributes.all);
    end if;
  end Adjust;
begin
  -- Ensure that Invalid_Pcre_Version exception is not masked
  Regular_Expressions.Check_Pcre_Version;
end Xml_Parser;

