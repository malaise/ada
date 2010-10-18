with Basic_Proc, Xml_Parser, String_Mng, Text_Line, Mixed_Str, Any_Def;
with Debug, Matcher, Variables;
package body Tree is

  -- "Global" variables
  Ctx : Xml_Parser.Ctx_Type;
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

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
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Xnode);
  begin
    for I in Attrs'Range loop
      if Asu_Ts (Attrs(I).Name) = Name then
        if Asu_Ts (Attrs(I).Value) = "None" then
          return Infinite_Ms;
        else
          return Integer'Value (Asu_Ts (Attrs(I).Value));
        end if;
      end if;
    end loop;
    return Default_Timeout;
  exception
    when Constraint_Error =>
      Error (Xnode, "Invalid timeout value for " & Name);
      raise Parse_Error;
  end Get_Timeout;

  -- Get text (PCDATA) of a node
  -- Get other attributes (Regexp, Assign)
  -- Check consistency
  procedure Get_Text (Xnode : in Xml_Parser.Element_Type;
                      Node : in out Node_Rec;
                      Empty_Allowed : in Boolean) is
    Tnode : Xml_Parser.Text_Type;
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Xnode);
    Assign : Asu_Us;
  begin
    -- Get Text from child
    if Ctx.Get_Nb_Children (Xnode) = 0 then
      -- No child => Empty text
      Node.Text := Asu_Null;
    else
      -- Text child
      Tnode := Ctx.Get_Child (Xnode, 1);
      Node.Text := Ctx.Get_Text (Tnode);
      -- No Line feed accepted
      if String_Mng.Locate (Asu_Ts (Node.Text), Line_Feed) /= 0 then
        Error (Xnode, "Invalid line-feed character in text");
        raise Parse_Error;
      end if;
    end if;

    -- Empty text forbidden
    if not Empty_Allowed and then Asu_Is_Null (Node.Text) then
      Error (Xnode, "Empty text");
      raise Parse_Error;
    end if;

    -- Get Regexp, Assign, Variable and NewLine attributes
    for I in Attrs'Range loop
      if Asu_Ts (Attrs(I).Name) = "Regexp" then
        if Asu_Ts (Attrs(I).Value) = "true" then
          Node.Regexp := True;
        end if;
      elsif Asu_Ts (Attrs(I).Name) = "Assign"
      or else Asu_Ts (Attrs(I).Name) = "Variable" then
        Assign := Attrs(I).Value;
      elsif Asu_Ts (Attrs(I).Name) = "NewLine" then
        Asu.Append (Node.Text, Line_Feed);
      end if;
    end loop;
    if Node.Kind /= Eval
    and then Node.Kind /= Set
    and then Node.Kind /= Cond
    and then Node.Kind /= Repeat
    and then not Node.Regexp
    and then not Asu_Is_Null (Assign) then
      Error (Xnode, "Assignment is only allowed with Regex expressions");
    end if;

    -- Check expansion and maybe Regexp
    Matcher.Check (Node, Assign);
  exception
    when Matcher.Match_Error =>
      Error (Xnode, "Invalid expresssion");
  end Get_Text;

  -- For dump of our tree
  function Dump (Node : Node_Rec; Level : Natural) return Boolean is
    Tab : constant String (1 .. 2 * Level) := (others => ' ');
    use type Any_Def.Any_Kind_List;
  begin
    Basic_Proc.Put_Error (Tab & Mixed_Str (Node.Kind'Img) & ": " );
    if not Asu_Is_Null (Node.Name) then
      Basic_Proc.Put_Error (Asu_Ts (Node.Name) & " ");
    end if;
    case Node.Kind is
      when Cond | Repeat | Read | Call | Eval | Set =>
        Basic_Proc.Put_Error ("Text: >" & Asu_Ts (Node.Text) & "< ");
      when Send =>
        Basic_Proc.Put_Error ("Text: >" &
          String_Mng.Replace (Asu_Ts (Node.Text), Line_Feed, "[LF]") &  "< ");
      when others =>
        null;
    end case;
    case Node.Kind is
      when Selec | Read | Skip | Wait =>
        Basic_Proc.Put_Error ("Timeout: " & Node.Timeout'Img & " ");
      when Cond | Repeat | Eval | Set =>
        Basic_Proc.Put_Error ("Variable: " );
          Basic_Proc.Put_Error (Asu_Ts (Node.Assign(Node.Assign'First).Name)
                              & " ");
      when others =>
        null;
    end case;
    if Node.Regexp then
      Basic_Proc.Put_Error ("Regexp ");
      if Node.Kind /= Cond and then Node.Kind /= Repeat then
        Basic_Proc.Put_Error ("Assign: " );
        for I in Node.Assign'Range loop
          exit when Node.Assign(I).Value.Kind = Any_Def.None_Kind;
          Basic_Proc.Put_Error (Asu_Ts (Node.Assign(I).Name) & "="
                              & Asu_Ts (Node.Assign(I).Value.Str));
        end loop;
      end if;
    end if;
    Basic_Proc.New_Line_Error;
    return True;
  end Dump;

  -----------------------
  -- Building own tree --
  -----------------------
  -- Recursive insertion of a node
  procedure Insert_Node (Xnode : in Xml_Parser.Element_Type;
                         Timeout : in Integer) is
    Name : constant String := Ctx.Get_Name (Xnode);
    Node, Nop_Node : Node_Rec;
    Default_Timeout : Integer;
    Child : Xml_Parser.Node_Type;
    Dummy : Boolean;
    Next_Is_Script : Boolean;
    procedure Init_Next (N : in out Node_Rec) is
    begin
      N.Next := new Position_Access'(Position_Access(Tree_Mng.No_Position));
    end Init_Next;
    procedure Link_Next (Node : in out Node_Rec) is
      Child_Pos : Tree_Mng.Position_Access;
    begin
      Chats.Move_Child (False);
      Child_Pos := Chats.Get_Position;
      Chats.Move_Father;
      Node.Next.all := Position_Access(Child_Pos);
      Chats.Replace (Node);
    end Link_Next;
  begin
    -- Fill new node
    Debug.Log ("Getting node " & Name);
    -- Init Node's next
    Init_Next (Node);
    -- Propagate default timeout from father
    Default_Timeout := Timeout;
    -- Default flags: Dummy is for "if" and "else"
    -- Next_Is_Script is for the "read" of select, "default", "if", "else"
    Dummy := False;
    Next_Is_Script := False;

    if Name = "chats" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms);
    elsif Name = "chat" then
      -- Chat is a Read. Get name, timeout and default timeout
      Node.Kind := Read;
      Next_Is_Script := True;
      for I in 1 .. Ctx.Get_Nb_Attributes (Xnode) loop
        if Asu_Ts (Ctx.Get_Attribute (Xnode, I).Name) = "Name" then
          Node.Name := Ctx.Get_Attribute (Xnode, I).Value;
          exit;
        end if;
      end loop;
      if Asu_Is_Null (Node.Name) then
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
      Node.Timeout := Get_Timeout (Xnode, Timeout);
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

    elsif Name = "cond" then
      Node.Kind := Cond;
      -- Move to "if" to get Varable name and text
      Child := Ctx.Get_Child (Xnode, 1);
      Get_Text (Child, Node, True);
      Child := Xml_Parser.No_Node;
    elsif Name = "if" then
      Node.Kind := Cond;
      Dummy := True;
      Next_Is_Script := True;
    elsif Name = "else" then
      Node.Kind := Cond;
      Dummy := True;
      Next_Is_Script := True;

    elsif Name = "repeat" then
      Node.Kind := Repeat;
      -- Move to "while" to get Variable name and text
      Child := Ctx.Get_Child (Xnode, 1);
      Get_Text (Child, Node, True);
      Child := Xml_Parser.No_Node;
    elsif Name = "while" then
      Node.Kind := Repeat;
      Dummy := True;
      Next_Is_Script := True;

    elsif Name = "read" then
      Node.Kind := Read;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      -- Get text
      Get_Text (Xnode, Node, True);
    elsif Name = "skip" then
      Node.Kind := Skip;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
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
      -- See if Newline
      if Asu_Ts (Ctx.Get_Attribute (Xnode, 1).Value) = "true" then
        Asu.Append (Node.Text, Line_Feed);
      end if;
    elsif Name = "call" then
      Node.Kind := Call;
      -- Get text
      Get_Text (Xnode, Node, False);
    elsif Name = "eval" then
      Node.Kind := Eval;
      -- Get text
      Get_Text (Xnode, Node, False);
    elsif Name = "set" then
      Node.Kind := Set;
      -- Get text
      Get_Text (Xnode, Node, True);
    elsif Name = "close" then
      Node.Kind := Close;
    else
      Error (Xnode, "Unexpected node " & Name);
    end if;

    -- Insert current node
    if not Dummy then
      if Name = "chats" then
        -- Insert root
        Chats.Insert_Father (Node);
      else
        Chats.Insert_Child (Node, False);
      end if;

      if Debug.Is_On then
        Dummy := Dump (Node, 1);
        Dummy := False;
      end if;
    end if;

    -- Now insert entries of Select, Cond or Repeat
    if not Dummy
    and then (Node.Kind = Selec
              or else Node.Kind = Cond
              or else Node.Kind = Repeat) then
      -- Insert each entry
      Debug.Log ("  Inserting entries of " & Mixed_Str (Node.Kind'Img));
      for I in 1 .. Ctx.Get_Nb_Children (Xnode) loop
        -- Chats and select are made of (expect, script) pairs: insert "expect"
        -- Cond is made of (if/else, script) pairs: insert "if/else"
        if I rem 2 = 1 then
          Debug.Log ("    Inserting entry of " & Mixed_Str (Node.Kind'Img));
          Child := Ctx.Get_Child (Xnode, I);
          Insert_Node (Child, Default_Timeout);
        end if;
      end loop;
      Debug.Log ("  End of entries of " & Mixed_Str (Node.Kind'Img));
      -- Child may already be set for other kinds
      Child := Xml_Parser.No_Node;
    end if;

    -- Go to next instruction
    if Next_Is_Script then
      -- For the read of a Selec ("select" or "chats"), for the "if" and
      --  the "else" and for the "while" next Xml node is a "script".
      -- Jump in it if not empty, else insert a Nop node
      -- In both cases, there is no next instruction
      if Ctx.Get_Nb_Children (Ctx.Get_Brother (Xnode)) /= 0 then
        Debug.Log ("  Inserting child of " & Mixed_Str (Node.Kind'Img));
        Child := Ctx.Get_Child (Ctx.Get_Brother (Xnode), 1);
        Insert_Node (Child, Default_Timeout);
        Link_Next (Node);
      else
        Debug.Log ("  Inserting Nop child of " & Mixed_Str (Node.Kind'Img));
        Init_Next (Nop_Node);
        Chats.Insert_Child (Nop_Node, False);
        Chats.Move_Father;
        Link_Next (Node);
      end if;
    elsif Ctx.Has_Brother (Xnode) then
      -- Normal (non script) instruction : jump to Xml brother if any
      Child := Ctx.Get_Brother (Xnode);
      -- Insert next statement
      Debug.Log ("  Inserting next of " & Mixed_Str (Node.Kind'Img));
      Insert_Node (Child, Default_Timeout);
      Link_Next (Node);
    end if;

    -- Move back to father
    if not Dummy and then Chats.Has_Father then
      Chats.Move_Father;
    end if;
  end Insert_Node;

  -- Recursive update of Next for the leafs
  function Update_Next (Next : in Position_Access) return Boolean is
    Node, Ref_Node : Node_Rec;
    -- The Next comming from parent
    Pnext : Position_Access;
    -- The Next of all intermediate children (not the following statement)
    Inext : Position_Access;
    -- Are we a leaf (Next not yet set)
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    if Next = No_Position then
      -- We are root, everything will ultimately arrive here
      Debug.Log ("On root");
      Pnext := Position_Access (Chats.Get_Position);
    else
      Pnext := Next;
    end if;

    -- If not set, update current Next to the one comming from parent
    Chats.Read (Node);
    if Node.Next.all = No_Position then
      Node.Next.all := Pnext;
      Chats.Replace (Node);
      if Debug.Is_On then
        Chats.Save_Position;
        Set_Position (Pnext);
        Chats.Read (Ref_Node);
        Chats.Restore_Position;
        Debug.Log ("Updating Next of ", False);
        Dummy := Dump (Node, 0);
        Debug.Log ("  to ", False);
        Dummy := Dump (Ref_Node, 0);
      end if;
    else
      -- Don't change current Next if already set
      if Debug.Is_On then
        Chats.Save_Position;
        Set_Position (Node.Next.all);
        Chats.Read (Ref_Node);
        Chats.Restore_Position;
        Debug.Log ("Next is OK for ", False);
        Dummy := Dump (Node, 0);
        Debug.Log ("  to ", False);
        Dummy := Dump (Ref_Node, 0);
      end if;
    end if;

    -- Set Onext depending on kind of multiplexor
    if Node.Kind = Selec
    or else Node.Kind = Cond then
      -- For leaf children of entries of a Selec, Next is the next of Selec
      -- For leaf children of if/else of a Cond, Next is the next of Cond
      Inext := Node.Next.all;
    elsif Node.Kind = Repeat then
      -- For leaf child of a Repeat, Next is the Repeat
      Inext := Position_Access(Chats.Get_Position);
    end if;

    -- Iterate on all children
    if Chats.Children_Number = 1 then
      -- Single child = next instruction
      Chats.Move_Child (True);
      Debug.Log ("Updating single child");
      Dummy := Update_Next (Pnext);
    elsif Chats.Children_Number > 1 then
      -- Selec, Cond or Repeat
      Chats.Move_Child (True);
      loop
        if Node.Next.all /= Position_Access(Chats.Get_Position) then
          -- Any intermediate child
          Debug.Log ("Updating intermediate child");
          exit when not Update_Next (Inext);
        else
          -- Last child of a Selec/Cond/Repeat, any single child
          Debug.Log ("Updating last child");
          exit when not Update_Next (Pnext);
        end if;
      end loop;
    end if;

    -- Move to brother if any
    if Chats.Has_Brother (False) then
      Debug.Log ("Moving to brother");
      Chats.Move_Brother (False) ;
      return True;
    else
      -- No more brother => back to father
      if Chats.Has_Father then
        Debug.Log ("Moving up");
        Chats.Move_Father;
      end if;
      return False;
    end if;
  end Update_Next;

  -------------------------------
  -- Parse file and build tree --
  -------------------------------
  procedure Parse (File_Name : in Asu_Us) is
    Ok : Boolean;
    Xnode : Xml_Parser.Element_Type;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    begin
      Ctx.Parse (Asu_Ts (File_Name), Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("XML ERROR: " & Ctx.Get_Parse_Error_Message);
        raise Parse_Error;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error ("XML ERROR: Cannot access to file "
          & Asu_Ts (File_Name) & ".");
        raise Parse_Error;
    end;
    Debug.Log ("File " & Asu_Ts (File_Name) & " parsed OK.");

    -- Build Tree
    Debug.Log ("Building tree:");
    Xnode := Ctx.Get_Root_Element;
    Insert_Node (Xnode, Infinite_Ms);
    Chats.Move_Root;
    Debug.Log ("Updating Next:");
    Dummy := Update_Next (No_Position);
    Chats.Move_Root;
    Variables.Reset;

    -- Dump
    Debug.Log ("Dump:");
    if Debug.Is_On then
      Chats.Iterate (Dump'Access);
    end if;

    -- Clean up
    Ctx.Clean;
  end Parse;

  -------------
  -- Utility --
  -------------
  procedure Set_Position (Position : in Position_Access) is
  begin
    Chats.Set_Position (Tree_Mng.Position_Access(Position));
  end Set_Position;

end Tree;

