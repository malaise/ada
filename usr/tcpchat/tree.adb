with Basic_Proc, Xml_Parser, Str_Util, Text_Line, Mixed_Str, Any_Def;
with Debug, Matcher, Variables;
package body Tree is

  -- "Global" variables
  Ctx : Xml_Parser.Ctx_Type;
  Line_Feed : constant String := Text_Line.Line_Feed_Str;
  Version : As.U.Asu_Us;

  ----------------------
  -- Common utilities --
  ----------------------
  -- Log an error
  procedure Error (Xnode : in Xml_Parser.Node_Type;
                   Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("XML ERROR at line "
      & Natural'Image (Ctx.Get_Line_No (Xnode)) & ": " & Msg & ".");
    raise Parse_Error;
  end Error;

  -- Get TimeoutMs or Name if set, else default
  function Get_Timeout (Xnode : Xml_Parser.Element_Type;
                        Default_Timeout : Integer;
                        Name : String := "TimeoutMs") return Integer is
    Val : As.U.Asu_Us;
  begin
    Val := As.U.Tus (Ctx.Get_Attribute (Xnode, Name));
    if Val.Image = "None" then
      return Infinite_Ms;
    else
      return Integer'Value (Val.Image);
    end if;
  exception
    when Xml_Parser.Attribute_Not_Found =>
      return Default_Timeout;
    when Constraint_Error =>
      Error (Xnode, "Invalid timeout value for " & Name);
      raise Parse_Error;
  end Get_Timeout;

  -- Strip leading and tailing spaces
  function Strip (Txt : As.U.Asu_Us) return As.U.Asu_Us is
  begin
    return As.U.Tus (Str_Util.Strip (Txt.Image, Str_Util.Both));
  end Strip;

  -- Node Kind s already set, Name as well if needed
  -- Timeout is initialised to default
  -- Get all other attributes
  -- Check consistency
  -- Allow empty text or not
  procedure Get_Attributes (Xnode : in Xml_Parser.Element_Type;
                            Node : in out Node_Rec) is
    Tnode : Xml_Parser.Node_Type;
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Xnode);
    Oper_Index : Natural;
    Assign : As.U.Asu_Us;

  begin
    -- Set Critext
    case Node.Kind is
      when Expect | Condif | Repeat | Read | Parse =>
        -- chat, expect, if, elsif, while, read: Critext is in attribute Crit
        Node.Critext := Ctx.Get_Attribute (Xnode, "Crit");
      when Set =>
        -- set: Critext is in attribute Var
        Node.Critext := Strip (Ctx.Get_Attribute (Xnode, "Expr"));
      when Send | Log =>
        -- send, log: A text child is the text
        if Ctx.Get_Nb_Children (Xnode) /= 0 then
          Tnode := Ctx.Get_Child (Xnode, 1);
          Node.Critext := Ctx.Get_Text (Tnode);
        else
          Node.Critext.Set_Null ;
        end if;
      when Call | Eval =>
        -- call, eval: A child "command" of current node hosts the text
        Tnode := Ctx.Get_Child (Xnode, 1);
        if Ctx.Get_Nb_Children (Tnode) /= 0 then
          Tnode := Ctx.Get_Child (Tnode, 1);
          Node.Critext := Strip (Ctx.Get_Text (Tnode));
        else
          Node.Critext.Set_Null ;
        end if;
        -- Empty command forbidden
        if Node.Critext.Is_Null then
          Error (Xnode, "Empty command");
          raise Parse_Error;
        end if;
      when Chdir =>
        -- chdir: variable name attribute Dir
        Node.Critext := Strip (Ctx.Get_Attribute (Xnode, "Dir"));
        -- Empty path forbidden
        if Node.Critext.Is_Null then
          Error (Xnode, "Empty directory path");
          raise Parse_Error;
        end if;
      when others =>
        Node.Critext.Set_Null ;
    end case;

   -- Set Expression
    case Node.Kind is
      -- if, elsif, while, parse: attribute Expr
      when Condif | Repeat | Parse =>
        Node.Expression := Ctx.Get_Attribute (Xnode, "Expr");
      when Set | Eval =>
        -- set: variable name attribute Var
        Node.Expression := Strip (Ctx.Get_Attribute (Xnode, "Var"));
        -- Empty expression forbidden
        if Node.Expression.Is_Null then
          Error (Xnode, "Empty variable name");
          raise Parse_Error;
        end if;
      when others =>
        Node.Expression.Set_Null ;
    end case;

    -- Get Timeout, Regexp, Compute, Assign, NewLine, IfUnset attributes
    -- Set inder of Oper attribute
    Oper_Index := 0;
    for I in Attrs'Range loop
      if Attrs(I).Name.Image = "Timeout" then
        Node.Timeout := Integer'Value (Attrs(I).Value.Image);
      elsif Attrs(I).Name.Image = "Regexp" then
        if Attrs(I).Value.Image = "true" then
          Node.Regexp := True;
        end if;
      elsif Attrs(I).Name.Image = "Compute" then
        if Attrs(I).Value.Image = "true" then
          Node.Compute := True;
        end if;
      elsif Attrs(I).Name.Image = "Assign"
      or else Attrs(I).Name.Image = "Variable" then
        -- Remove heading and trailing spaces
        Assign := Strip (Attrs(I).Value);
      elsif Attrs(I).Name.Image = "NewLine"
      and then Attrs(I).Value.Image = "true" then
        Node.Critext.Append (Line_Feed);
      elsif Attrs(I).Name.Image = "OnlyIfNotSet"
      and then Attrs(I).Value.Image = "true" then
        Node.Ifunset := True;
      elsif Attrs(I).Name.Image = "Oper" then
        Oper_Index := I;
      end if;
    end loop;
    -- Set Regexp for Parse
    if Node.Kind = Parse then
      Node.Regexp := True;
    end if;

    -- Check Assign versus Regexp
    if not Node.Regexp and then not Assign.Is_Null
    and then Assign.Image /= "${0}" then
      Error (Xnode, "Assignment must be ""${0}"" if expression is not a Regex");
    end if;

    -- Set Oper
    Node.Oper := Equal;
    if Oper_Index /= 0 then
      declare
        Oper : constant String
             := Ctx.Get_Attribute (Xnode, Oper_Index).Value.Image;
      begin
        if Oper = "=" then
          Node.Oper := Equal;
        elsif Oper = "/=" then
          Node.Oper := Noteq;
        elsif Oper = ">" then
          Node.Oper := Greater;
        elsif Oper = "<" then
          Node.Oper := Smaller;
        elsif Oper = ">=" then
          Node.Oper := Greatereq;
        elsif Oper = "<=" then
          Node.Oper := Smallereq;
        else
          Error (Xnode, "Invalid operation ""Oper""");
        end if;
      end;
    end if;

    -- Check expansion and maybe Regexp
    Matcher.Check (Node, Assign);
  exception
    when Matcher.Match_Error =>
      Error (Xnode, "Invalid expresssion");
  end Get_Attributes;

  -- For dump of a node with some text before
  function Dump (Init_Str : String;
                 Node : Node_Rec;
                 Level : Natural) return Boolean is
    Tab : constant String (1 .. 2 * Level) := (others => ' ');
    Text : As.U.Asu_Us;
    use type Any_Def.Any_Kind_List;
  begin
    Text.Set (Init_Str);
    Text.Append (Tab & Mixed_Str (Node.Kind'Img) & ": " );
    if not Node.Name.Is_Null then
      Text.Append (Node.Name.Image & " ");
    end if;
    case Node.Kind is
      when Condif | Repeat | Parse | Set | Eval =>
        Text.Append ("Expr: " & Node.Expression.Image & " ");
      when others =>
        null;
    end case;
    case Node.Kind is
      when Expect | Condif | Repeat | Read | Set | Call | Eval | Chdir =>
        Text.Append ("Text: >" & Node.Critext.Image & "< ");
      when Send | Log =>
        Text.Append ("Text: >" &
            Str_Util.Substit (Node.Critext.Image, Line_Feed, "[LF]") &  "< ");
      when others =>
        null;
    end case;
    case Node.Kind is
      when Selec | Read | Skip | Wait =>
        Text.Append ("Timeout: " & Node.Timeout'Img & " ");
      when others =>
        null;
    end case;
    if Node.Regexp then
      Text.Append ("Regexp ");
    end if;
    if Node.Compute then
      Text.Append ("Compute ");
    end if;
    case Node.Kind is
      when Set | Eval =>
        if Node.Ifunset then
          Text.Append ("IfUnset ");
        end if;
      when others =>
        null;
    end case;
    if Node.Kind = Expect or else Node.Kind = Read
    or else Node.Kind = Parse then
      Text.Append ("Assign: " );
      for I in Node.Assign'Range loop
        exit when Node.Assign(I).Value.Kind = Any_Def.None_Kind;
        Text.Append (Node.Assign(I).Name.Image & "="
                   & Node.Assign(I).Value.Str.Image & " ");
      end loop;
    end if;

    Debug.Logger.Log_Debug (Text.Image);
    return True;
  end Dump;
  -- For dump of the tree (iterator)
  function Dump (Node : Node_Rec; Level : Natural) return Boolean is
  begin
    return Dump ("", Node, Level);
  end Dump;

  -----------------------
  -- Building own tree --
  -----------------------
  -- The instructions that have no child (except text)
  Has_Child : constant array (Node_Kind) of Boolean :=
    (Read | Skip | Wait | Send | Log | Parse => False, others => True);
  -- The instructions tha have a (optional) "error" block of instructions
  Has_Error : constant array (Node_Kind) of Boolean :=
    (Call | Eval | Set | Chdir => True, others => False);
  -- Recursive insertion of a node
  procedure Insert_Node (Xnode : in Xml_Parser.Element_Type;
                         Current_Timeout : in Integer;
                         As_Child : in Boolean) is
    Name : constant String := Ctx.Get_Name (Xnode);
    Node : Node_Rec;
    Default_Timeout : Integer;
    Xchild, Xtmp : Xml_Parser.Element_Type;
    Nb_Children : Natural;

    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    -- Fill new node
    Debug.Logger.Log_Debug ("Getting node " & Name);
    -- Propagate default timeout from father
    Default_Timeout := Current_Timeout;

    -- Set Kind, Name if needed. Get specific attributes
    Node.Timeout := Default_Timeout;
    if Name = "chats" then
      -- Chats is a Selec
      Node.Kind := Selec;
      Version := Ctx.Get_Attribute (Xnode, "Version");
    elsif Name = "chat" then
      -- Chat is an Expect
      Node.Kind := Expect;
      Node.Name := Strip (Ctx.Get_Attribute (Xnode, "Name"));
      if Node.Name.Is_Null then
        Error (Xnode, "Empty chat name");
      end if;
      -- Set default timeout for children
      Default_Timeout := Get_Timeout (Xnode, Infinite_Ms,
                                      "InputDefaultTimeoutMs");
    elsif Name = "select" then
      Node.Kind := Selec;
    elsif Name = "expect" then
      Node.Kind := Expect;
    elsif Name = "default" then
      Node.Kind := Default;
    elsif Name = "timeout" then
      Node.Kind := Timeout;

    elsif Name = "cond" then
      Node.Kind := Cond;
    elsif Name = "if"
    or else Name = "elsif" then
      Node.Kind := Condif;
    elsif Name = "else" then
      Node.Kind := Condelse;

    elsif Name = "while" then
      Node.Kind := Repeat;

    elsif Name = "read" then
      Node.Kind := Read;
    elsif Name = "skip" then
      Node.Kind := Skip;
    elsif Name = "wait" then
      Node.Kind := Wait;
      -- Delay is mandatory
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms, "DelayMs");
      if Node.Timeout < 0 then
        Error (Xnode, "Invalid delay");
      end if;
    elsif Name = "send" then
      Node.Kind := Send;
    elsif Name = "log" then
      Node.Kind := Log;
    elsif Name = "call" then
      Node.Kind := Call;
    elsif Name = "eval" then
      Node.Kind := Eval;

    elsif Name = "set" then
      Node.Kind := Set;
    elsif Name = "assign" then
      Node.Kind := Set;

    elsif Name = "parse" then
      Node.Kind := Parse;
    elsif Name = "chdir" then
      Node.Kind := Chdir;
    elsif Name = "close" then
      Node.Kind := Close;
    else
      Error (Xnode, "Unexpected node " & Name);
    end if;

    -- Get Crit, Expr and all attributes
    Get_Attributes (Xnode, Node);

    -- Insert and dump node
    if Chats.Is_Empty then
      Chats.Insert_Father (Node);
    elsif As_Child then
      Chats.Insert_Child (Node, True);
    else
      Chats.Insert_Brother (Node, False);
    end if;
    if Debug.Logger.Debug_On then
      Dummy := Dump (Node, 1);
    end if;

    -- Instructions known to have no child (except text)
    if not Has_Child (Node.Kind) then
      return;
    end if;

    -- Move into error block (last XML child) if any
    -- No other child in our tree if there is an error block
    Xtmp := Xnode;
    if Has_Error (Node.Kind) then
      Nb_Children := Ctx.Get_Nb_Children (Xtmp);
      if Nb_Children /= 0 then
        Xtmp := Ctx.Get_Child (Xtmp, Nb_Children);
        if Ctx.Get_Name (Xtmp) /= "error" then
          -- Not an error block,
          return;
        end if;
      end if;
    end if;

    -- Insert children
    if Ctx.Get_Nb_Children (Xtmp) /= 0 then
      Debug.Logger.Log_Debug ("  Inserting children of "
                            & Mixed_Str (Node.Kind'Img));
      for I in 1 .. Ctx.Get_Nb_Children (Xtmp) loop
        Xchild := Ctx.Get_Child (Xtmp, I);
        Debug.Logger.Log_Debug ("    Inserting child of "
                              & Mixed_Str (Node.Kind'Img));
        Insert_Node (Xchild, Default_Timeout, As_Child => I = 1);
      end loop;
      Chats.Move_Father;
      Debug.Logger.Log_Debug ("  End of entries of "
                            & Mixed_Str (Node.Kind'Img));
    end if;

  end Insert_Node;

  -------------------------------
  -- Parse file and build tree --
  -------------------------------
  procedure Parse (File_Name : in As.U.Asu_Us) is
    Ok : Boolean;
    Xnode : Xml_Parser.Element_Type;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    begin
      Ctx.Parse (File_Name.Image, Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("XML ERROR: " & Ctx.Get_Parse_Error_Message);
        raise Parse_Error;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error ("XML ERROR: Cannot access to file "
          & File_Name.Image & ".");
        raise Parse_Error;
    end;
    Debug.Logger.Log_Debug ("File " & File_Name.Image & " parsed OK.");

    -- Build Tree
    Debug.Logger.Log_Debug ("Building tree:");
    Xnode := Ctx.Get_Root_Element;
    Insert_Node (Xnode, Infinite_Ms, As_Child => True);
    -- Clean up
    Ctx.Clean;

    -- Done
    Chats.Move_Root;
    Variables.Reset;

    -- Dump
    Debug.Logger.Log_Debug ("Dump:");
    if Debug.Logger.Debug_On then
      Chats.Iterate (Dump'Access);
    end if;

  end Parse;

  function Get_Version return String is
  begin
    return Version.Image;
  end Get_Version;
end Tree;

