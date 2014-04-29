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

  -- Node Kind, Name are already set if needed
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
      when Expect | Condif | Repeat | Read | Set =>
        -- chat, expect, if, elsif, while, read, set:
        --  Critext is in attribute Crit
        Node.Critext := Ctx.Get_Attribute (Xnode, "Crit");
      when Send | Log =>
        -- send, log: A text child is the text
        Tnode := Ctx.Get_Child (Xnode, 1);
        Node.Critext := Ctx.Get_Text (Tnode);
      when Call | Eval =>
        -- call, eval: A child "command" of current node hosts the text
        Tnode := Ctx.Get_Child (Xnode, 1);
        Tnode := Ctx.Get_Child (Xnode, 1);
        Node.Critext := Ctx.Get_Text (Tnode);
        -- Empty command forbidden
        if Node.Critext.Is_Null then
          Error (Xnode, "Empty command");
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
      when Set =>
        -- set: variable name attribute Var
        Node.Expression := Ctx.Get_Attribute (Xnode, "Var");
        -- Empty expression forbidden
        if Node.Critext.Is_Null then
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
        Assign := As.U.Tus (Str_Util.Strip (Attrs(I).Value.Image,
                                            Str_Util.Both));
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
      Error (Xnode, "Assignment must be ""${0}"" if not Regex expression");
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
      when Condif | Repeat | Read | Call | Eval | Set | Chdir =>
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
      when Condif | Repeat | Eval | Set =>
        Text.Append ("Variable: " );
        Text.Append (Node.Assign(Node.Assign'First).Name.Image & " ");
      when others =>
        null;
    end case;
    if Node.Compute then
      Text.Append ("Compute ");
    end if;
    case Node.Kind is
      when Set | Eval | Condif =>
        if Node.Ifunset then
          Text.Append ("IfUnset ");
        end if;
      when others =>
        null;
    end case;
    if Node.Kind = Parse then
     Text.Append ("Expression: " & Node.Expression.Image & " ");
    end if;
    if Node.Regexp then
      Text.Append ("Regexp ");
      if Node.Kind /= Condif and then Node.Kind /= Repeat then
        Text.Append ("Assign: " );
        for I in Node.Assign'Range loop
          exit when Node.Assign(I).Value.Kind = Any_Def.None_Kind;
          Text.Append (Node.Assign(I).Name.Image & "="
                     & Node.Assign(I).Value.Str.Image);
        end loop;
      end if;
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
  -- Recursive insertion of a node
  procedure Insert_Node (Xnode : in Xml_Parser.Element_Type;
                         Current_Timeout : in Integer) is
    Name : constant String := Ctx.Get_Name (Xnode);
    Node, Nop_Node : Node_Rec;
    Default_Timeout : Integer;
    Xchild : Xml_Parser.Node_Type;
    In_Chats : Boolean;
    Dummy_Node : Boolean;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
    Next_Is_Script : Boolean;
  begin
    -- Fill new node
    Debug.Logger.Log_Debug ("Getting node " & Name);
    -- Propagate default timeout from father
    Default_Timeout := Current_Timeout;
    -- Default flags: Dummy is for "repeat" and "error"
    -- Next_Is_Script is for the "read" of select, "default", "timeout",
    --  "if", "else"
    Dummy_Node := False;
    Next_Is_Script := False;
    In_Chats := False;

    if Name = "chats" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms);
      In_Chats := True;
    elsif Name = "version" then
      Version := Ctx.Get_Attribute (Xnode, 1).Value;
      return;
    elsif Name = "chat" then
      -- Chat is a Read. Get name, timeout and default timeout
      Node.Kind := Read;
      Node.Name := As.U.Tus (Ctx.Get_Attribute (Xnode, "Name"));
      if Node.Name.Is_Null then
        Error (Xnode, "Empty chat name");
      end if;
      -- This is the overall timeout of the chat script
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms);
      -- Set default timeout for children
      Default_Timeout := Get_Timeout (Xnode, Infinite_Ms,
                                      "InputDefaultTimeoutMs");
    elsif Name = "select" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Current_Timeout);
    elsif Name = "expect" then
      -- The expect of a select => Read without timeout
      Node.Kind := Read;
      Next_Is_Script := True;
    elsif Name = "default" then
      -- The default of a select
      Node.Kind := Default;
      Next_Is_Script := True;
    elsif Name = "timeout" then
      -- The timeout of a select
      Node.Kind := Timeout;
      Next_Is_Script := True;

    elsif Name = "cond" then
      Node.Kind := Cond;
    elsif Name = "if"
    or else Name = "elsif" then
      Node.Kind := Condif;
      Next_Is_Script := True;
    elsif Name = "else" then
      Node.Kind := Condelse;
      Next_Is_Script := True;

    elsif Name = "repeat" then
      Node.Kind := Repeat;
      -- Move to "while" to get Variable name and text
      Xchild := Ctx.Get_Child (Xnode, 1);
    elsif Name = "while" then
      Node.Kind := Repeat;
      -- There is no "while" node: the criteria is attached to the Repeat
      Dummy_Node := True;
      Next_Is_Script := True;

    elsif Name = "read" then
      Node.Kind := Read;
      Node.Timeout := Get_Timeout (Xnode, Current_Timeout);
      -- Get text
    elsif Name = "skip" then
      Node.Kind := Skip;
      Node.Timeout := Get_Timeout (Xnode, Current_Timeout);
    elsif Name = "wait" then
      Node.Kind := Wait;
      -- Delay is mandatory
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms, "DelayMs");
      if Node.Timeout < 0 then
        Error (Xnode, "Invalid delay");
      end if;
    elsif Name = "send" then
      Node.Kind := Send;
    elsif Name = "call" then
      Node.Kind := Call;
      -- Move to "command" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
    elsif Name = "eval" then
      Node.Kind := Eval;
      -- Get_Attribute OnlyIfNotSset
    elsif Name = "set" then
      Node.Kind := Set;
    elsif Name = "assign" then
      Node.Kind := Set;
      -- Move to "statement" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
    elsif Name = "parse" then
      Node.Kind := Parse;
      -- Get_Attribute Compute
    elsif Name = "chdir" then
      Node.Kind := Chdir;
      -- Move to "dir" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
    elsif Name = "log" then
      Node.Kind := Log;
    elsif Name = "error" then
      -- Begin of error handling block
      Node := Chats.Read;
      Dummy_Node := True;
      Next_Is_Script := True;
    elsif Name = "close" then
      Node.Kind := Close;
    else
      Error (Xnode, "Unexpected node " & Name);
    end if;

    -- Insert current node
    if not Dummy_Node then
      if Name = "chats" then
        -- Insert root
        Chats.Insert_Father (Node);
      else
        Chats.Insert_Child (Node, False);
      end if;

      if Debug.Logger.Debug_On then
        Dummy := Dump (Node, 1);
      end if;
    end if;

    -- Now insert entries of Select, Cond or Repeat
    if not Dummy_Node
    and then (Node.Kind = Selec
              or else Node.Kind = Cond
              or else Node.Kind = Repeat) then
      -- Insert each entry
      Debug.Logger.Log_Debug ("  Inserting entries of "
                             & Mixed_Str (Node.Kind'Img));
      for I in 1 .. Ctx.Get_Nb_Children (Xnode) loop
        if In_Chats then
          -- Chats is made of version then (expect, script) pairs:
          -- insert "expect"
          if I = 1 or else I rem 2 = 0 then
            Debug.Logger.Log_Debug ("    Inserting chat entry of "
                                  & Mixed_Str (Node.Kind'Img));
            Xchild := Ctx.Get_Child (Xnode, I);
            Insert_Node (Xchild, Default_Timeout);
          end if;
        else
          -- Select is made of (expect/default/timeout, script) pairs:
          --   insert "expect/default/timeout"
          -- Cond is made of (if/elsif/else, script) pairs:
          --   insert "if/elsif/else"
          -- Repeat is made of a (while, script) pair: insert "while"
          if I rem 2 = 1 then
            Debug.Logger.Log_Debug ("    Inserting entry of "
                                  & Mixed_Str (Node.Kind'Img));
            Xchild := Ctx.Get_Child (Xnode, I);
            Insert_Node (Xchild, Default_Timeout);
          end if;
        end if;
      end loop;
      Debug.Logger.Log_Debug ("  End of entries of "
                            & Mixed_Str (Node.Kind'Img));
    elsif Node.Kind = Set or else Node.Kind = Parse or else Node.Kind = Call
    or else Node.Kind = Eval or else Node.Kind = Chdir then
      -- Assign is an expression then a handler (error+script)
      -- Call, Eval and Chdir are a command or target, then an opt handler
      -- (error+script)
      -- Insert error handler if any
      if Ctx.Get_Nb_Children (Xnode) = 3 then
        Debug.Logger.Log_Debug ("    Inserting error handler of "
                              & Mixed_Str (Node.Kind'Img));
        Xchild := Ctx.Get_Child (Xnode, 2);
        Insert_Node (Xchild, Default_Timeout);
      end if;
    end if;

    -- Go to next instruction
    if Next_Is_Script then
      -- For the read of a Selec ("select" or "chats"), for the "if" and
      --  the "else", for the "while", for the "error",
      -- next Xml node is a "script".
      -- Jump in it if not empty, else insert a Nop node
      -- In all cases except error, there is no next instruction
      if Ctx.Get_Nb_Children (Ctx.Get_Brother (Xnode)) /= 0 then
        Debug.Logger.Log_Debug ("  Inserting child of "
                               & Mixed_Str (Node.Kind'Img));
        Xchild := Ctx.Get_Child (Ctx.Get_Brother (Xnode), 1);
        Insert_Node (Xchild, Default_Timeout);
      else
        Debug.Logger.Log_Debug ("  Inserting Nop child of "
                              & Mixed_Str (Node.Kind'Img));
        Chats.Insert_Child (Nop_Node, False);
        Chats.Move_Father;
      end if;
    elsif Ctx.Has_Brother (Xnode) then
      -- Normal (non script) instruction : jump to Xml brother if any
      Xchild := Ctx.Get_Brother (Xnode);
      -- Insert next statement
      Debug.Logger.Log_Debug ("  Inserting next of "
                            & Mixed_Str (Node.Kind'Img));
      Insert_Node (Xchild, Default_Timeout);
    end if;

    -- Move back to father
    if not Dummy_Node and then Chats.Has_Father then
      Chats.Move_Father;
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
    Insert_Node (Xnode, Infinite_Ms);
    Chats.Move_Root;
    Variables.Reset;

    -- Dump
    Debug.Logger.Log_Debug ("Dump:");
    if Debug.Logger.Debug_On then
      Chats.Iterate (Dump'Access);
    end if;

    -- Clean up
    Ctx.Clean;
  end Parse;

  function Get_Version return String is
  begin
    return Version.Image;
  end Get_Version;
end Tree;

