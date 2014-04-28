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

  -- Get attribute if set, else return default
  function Get_Attribute (Xnode   : Xml_Parser.Element_Type;
                          Name    : String) return String is
  begin
    return Ctx.Get_Attribute (Xnode, Name);
  end Get_Attribute;

  -- Get TimeoutMs or Name if set, else default
  function Get_Timeout (Xnode : Xml_Parser.Element_Type;
                        Default_Timeout : Integer;
                        Name : String := "TimeoutMs") return Integer is
    Val : As.U.Asu_Us;
  begin
    Val := As.U.Tus (Get_Attribute (Xnode, Name));
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

  -- Get IfUnset Trilean
  function Get_Ifunset (Xnode : Xml_Parser.Element_Type)
                       return Trilean.Trilean is
    Txt : constant As.U.Asu_Us := As.U.Tus (Get_Attribute (Xnode, "IfUnset"));
  begin
    if Txt.Image = "error" then
      return Trilean.Other;
    elsif Txt.Image = "false" then
      return Trilean.False;
    elsif Txt.Image = "true" then
      return Trilean.True;
    else
      raise Constraint_Error;
    end if;
  end Get_Ifunset;

  -- Get text (PCDATA) of a node
  -- Get other attributes (Regexp, Assign)
  -- Check consistency
  -- Allow empty text or not
  -- Look for text in child (command) of current node or in current node
  procedure Get_Text (Xnode : in Xml_Parser.Element_Type;
                      Node : in out Node_Rec;
                      Empty_Allowed : in Boolean;
                      Command : in Boolean := False) is
    Text_Node : Xml_Parser.Element_Type;
    Tnode : Xml_Parser.Text_Type;
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Xnode);
    Assign : As.U.Asu_Us;
  begin
    -- Current node hosts the attributes
    if Command then
      -- A child "command" of current node hosts the text
      Text_Node := Ctx.Get_Child (Xnode, 1);
    else
      -- Current node hosts the text
      Text_Node := Xnode;
    end if;
    -- Get Text from child
    if Ctx.Get_Nb_Children (Text_Node) = 0 then
      -- No child => Empty text
      Node.Critext.Set_Null;
    else
      -- Text child
      Tnode := Ctx.Get_Child (Text_Node, 1);
      Node.Critext := Ctx.Get_Text (Tnode);
      -- No Line feed accepted
      if Str_Util.Locate (Node.Critext.Image, Line_Feed) /= 0 then
        Error (Xnode, "Invalid line-feed character in text");
        raise Parse_Error;
      end if;
    end if;

    -- Empty text forbidden
    if not Empty_Allowed and then Node.Critext.Is_Null then
      Error (Xnode, "Empty text");
      raise Parse_Error;
    end if;

    -- Get Regexp, Assign, Variable and NewLine attributes
    for I in Attrs'Range loop
      if Attrs(I).Name.Image = "Regexp" then
        if Attrs(I).Value.Image = "true" then
          Node.Regexp := True;
        end if;
      elsif Attrs(I).Name.Image = "Assign"
      or else Attrs(I).Name.Image = "Variable" then
        Assign := Attrs(I).Value;
      elsif Attrs(I).Name.Image = "NewLine"
      and then Attrs(I).Value.Image = "true" then
        Node.Critext.Append (Line_Feed);
      elsif Attrs(I).Name.Image = "Expr" then
        -- For parse, store the expression to parse
        Node.Regexp := True;
        Node.Expression := Attrs(I).Value;
      end if;
    end loop;
    if Node.Kind /= Eval
    and then Node.Kind /= Set
    and then Node.Kind /= Condif
    and then Node.Kind /= Repeat
    and then not Node.Regexp
    and then not Assign.Is_Null then
      Error (Xnode, "Assignment is only allowed with Regex expressions");
    end if;

    -- Check expansion and maybe Regexp
    Matcher.Check (Node, Assign);
  exception
    when Matcher.Match_Error =>
      Error (Xnode, "Invalid expresssion");
  end Get_Text;

  -- For dump of a node with some text before
  function Dump (Init_Str : String;
                 Node : Node_Rec;
                 Level : Natural) return Boolean is
    Tab : constant String (1 .. 2 * Level) := (others => ' ');
    Text : As.U.Asu_Us;
    use type Any_Def.Any_Kind_List, Trilean.Trilean;
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
      when Set | Eval =>
        if Node.Ifunset = Trilean.True then
          Text.Append ("IfUnset ");
        end if;
      when Condif =>
        if Node.Ifunset /= Trilean.Other then
          Text.Append ("IfUnset: " & Mixed_Str (Node.Ifunset'Img) & " ");
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
    procedure Init_Next (N : in out Node_Rec) is
    begin
      null;
    end Init_Next;
    procedure Link_Next (Node : in out Node_Rec) is
    begin
      Chats.Move_Child (False);
      Chats.Move_Father;
      Chats.Replace (Node);
    end Link_Next;
  begin
    -- Fill new node
    Debug.Logger.Log_Debug ("Getting node " & Name);
    -- Init Node's next
    Init_Next (Node);
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
      Next_Is_Script := True;
      Node.Name := As.U.Tus (Get_Attribute (Xnode, "Name"));
      if Node.Name.Is_Null then
        Error (Xnode, "Empty chat name");
      end if;
      -- This is the overall timeout of the chat script
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms);
      -- Get expect text
      Get_Text (Xnode, Node, True);
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
      -- Get expect text
      Get_Text (Xnode, Node, True);
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
      Get_Text (Xnode, Node, True);
      Node.Ifunset := Get_Ifunset (Xnode);
      Next_Is_Script := True;
    elsif Name = "else" then
      Node.Kind := Condelse;
      Next_Is_Script := True;

    elsif Name = "repeat" then
      Node.Kind := Repeat;
      -- Move to "while" to get Variable name and text
      Xchild := Ctx.Get_Child (Xnode, 1);
      Get_Text (Xchild, Node, True);
      Node.Ifunset := Get_Ifunset (Xchild);
    elsif Name = "while" then
      Node.Kind := Repeat;
      -- There is no "while" node: the criteria is attached to the Repeat
      Dummy_Node := True;
      Next_Is_Script := True;

    elsif Name = "read" then
      Node.Kind := Read;
      Node.Timeout := Get_Timeout (Xnode, Current_Timeout);
      -- Get text
      Get_Text (Xnode, Node, True);
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
      -- Get text
      Get_Text (Xnode, Node, True);
    elsif Name = "call" then
      Node.Kind := Call;
      -- Move to "command" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
      Get_Text (Xchild, Node, False);
    elsif Name = "eval" then
      Node.Kind := Eval;
      -- Get text of child "command" and current attributes
      Get_Text (Xnode, Node, False, Command => True);
      -- Get_Attribute OnlyIfNotSset
      Node.Ifunset := Trilean.Boo2Tri (
               Get_Attribute (Xnode, "OnlyIfNotSet") = "true");
    elsif Name = "set" then
      Node.Kind := Set;
      -- Get text
      Get_Text (Xnode, Node, True);
      -- Get_Attributes Compute and OnlyIfNotSset
      Node.Compute := Get_Attribute (Xnode, "Compute") = "true";
      Node.Ifunset := Trilean.Boo2Tri (
               Get_Attribute (Xnode, "OnlyIfNotSet") = "true");
    elsif Name = "assign" then
      Node.Kind := Set;
      -- Move to "statement" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
      Get_Text (Xchild, Node, True);
      -- Get_Attributes Compute and OnlyIfNotSset
      Node.Compute := Get_Attribute (Xchild, "Compute") = "true";
      Node.Ifunset := Trilean.Boo2Tri (
               Get_Attribute (Xchild, "OnlyIfNotSet") = "true");
    elsif Name = "parse" then
      Node.Kind := Parse;
      -- Get text, the regexp
      Get_Text (Xnode, Node, True);
      -- Get_Attribute Compute
      Node.Compute := Get_Attribute (Xnode, "Compute") = "true";
    elsif Name = "chdir" then
      Node.Kind := Chdir;
      -- Move to "dir" to get text
      Xchild := Ctx.Get_Child (Xnode, 1);
      Get_Text (Xchild, Node, True);
    elsif Name = "log" then
      Node.Kind := Log;
      -- Get text
      Get_Text (Xnode, Node, False);
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
        if not Dummy_Node then
          Link_Next (Node);
        end if;
      else
        Debug.Logger.Log_Debug ("  Inserting Nop child of "
                              & Mixed_Str (Node.Kind'Img));
        Init_Next (Nop_Node);
        Chats.Insert_Child (Nop_Node, False);
        Chats.Move_Father;
        if not Dummy_Node then
          Link_Next (Node);
        end if;
      end if;
    elsif Ctx.Has_Brother (Xnode) then
      -- Normal (non script) instruction : jump to Xml brother if any
      Xchild := Ctx.Get_Brother (Xnode);
      -- Insert next statement
      Debug.Logger.Log_Debug ("  Inserting next of "
                            & Mixed_Str (Node.Kind'Img));
      Insert_Node (Xchild, Default_Timeout);
      Link_Next (Node);
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
    Debug.Logger.Log_Debug ("Updating Next:");
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

  -------------
  -- Utility --
  -------------
  function Get_Version return String is
  begin
    return Version.Image;
  end Get_Version;
end Tree;

