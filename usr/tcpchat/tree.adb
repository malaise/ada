with Basic_Proc, Xml_Parser, String_Mng, Environ, Text_Line, Mixed_Str;
with Debug;
package body Tree is

  -- "Global" variables
  Ctx : Xml_Parser.Ctx_Type;

  ----------------------
  -- Common utilities --
  ----------------------
  -- Log an error
  procedure Error (Xnode : in Xml_Parser.Node_Type;
                   Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("XML ERROR at line "
      & Natural'Image (Ctx.Get_Line_No (Xnode)) & ": " & Msg);
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
        return Integer'Value (Asu_Ts (Attrs(I).Value));
      end if;
    end loop;
    return Default_Timeout;
  exception
    when Constraint_Error =>
      Error (Xnode, "Invalid timeout value for " & Name & ".");
      raise Parse_Error;
  end Get_Timeout;

  -- Expand ENV variables in text
  function Get_Text (Xnode : Xml_Parser.Element_Type) return Asu_Us is
    Tnode : Xml_Parser.Text_Type;
    Text : Asu_Us;
  begin
    if Ctx.Get_Nb_Children (Xnode) = 0 then
      -- Empty text => no child
      return Asu_Null;
    end if;
    Tnode := Ctx.Get_Child (Xnode, 1);
    Text := Ctx.Get_Text (Tnode);
    return Asu_Tus (String_Mng.Eval_Variables (
              Asu_Ts (Text), "${", "}", Environ.Getenv_If_Set'Access,
              Muliple_Passes => False,
              No_Check_Stop => False,
              Skip_Backslashed => True));
  exception
    when Environ.Name_Error =>
      Error (Xnode, "Unknown variable");
      raise Parse_Error;
    when String_Mng.Inv_Delimiter | String_Mng.Delimiter_Mismatch =>
      Error (Xnode, "Invalide variable reference");
      raise Parse_Error;
  end Get_Text;

  -- For dump of our tree
  function Dump (Node : Node_Rec; Level : Natural) return Boolean is
    Tab : constant String (1 .. 2 * Level) := (others => ' ');
  begin
    Basic_Proc.Put_Error (Tab & Mixed_Str (Node.Kind'Img) & ": " );
    if not Asu_Is_Null (Node.Name) then
      Basic_Proc.Put_Error (Asu_Ts (Node.Name) & " ");
    end if;
    case Node.Kind is
      when Read | Send | Call | Exec =>
        Basic_Proc.Put_Error ("Text: >" & Asu_Ts (Node.Text) & "< ");
      when others =>
        null;
    end case;
    case Node.Kind is
      when Selec | Read | Skip | Wait =>
        Basic_Proc.Put_Error ("Timeout: " & Node.Timeout'Img);
      when others =>
        null;
    end case;
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
    Node : Node_Rec;
    Default_Timeout : Integer;
    Child : Xml_Parser.Node_Type := Xml_Parser.No_Node;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    -- Insert current node
    Debug.Log ("Getting node " & Name);
    -- Propagate default timeout from father
    Default_Timeout := Timeout;
    if Name = "chats" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms);
      -- Insert root
      Chats.Insert_Father (Node);
    elsif Name = "chat" then
      -- Chat is a Read. Get name, timeout and default timeout
      Node.Kind := Read;
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
      Node.Text := Get_Text (Xnode);
      Chats.Insert_Child (Node, False);
      -- Set default timeout for children
      Default_Timeout := Get_Timeout (Xnode, Infinite_Ms,
                                      "InputDefaultTimeoutMs");
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Brother (Xnode), 1);
    elsif Name = "select" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      Chats.Insert_Child (Node, False);
    elsif Name = "expect" then
      -- The expect of a select => Read without timeout
      Node.Kind := Read;
      -- Get expect text
      Node.Text := Get_Text (Xnode);
      Chats.Insert_Child (Node, False);
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Brother (Xnode), 1);
    elsif Name = "default" then
      -- The default of a select
      Node.Kind := Default;
      Chats.Insert_Child (Node, False);
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Brother (Xnode), 1);
    elsif Name = "read" then
      Node.Kind := Read;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      -- Get text
      Node.Text := Get_Text (Xnode);
      Chats.Insert_Child (Node, False);
    elsif Name = "skip" then
      Node.Kind := Skip;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      Chats.Insert_Child (Node, False);
    elsif Name = "wait" then
      Node.Kind := Wait;
      -- Delay is mandatory
      Node.Timeout := Get_Timeout (Xnode, Infinite_Ms, "DelayMs");
      Chats.Insert_Child (Node, False);
    elsif Name = "send" then
      Node.Kind := Send;
      -- Get text
      Node.Text := Get_Text (Xnode);
      -- See if Newline
      if Asu_Ts (Ctx.Get_Attribute (Xnode, 1).Value) = "true" then
        Asu.Append (Node.Text, Text_Line.Line_Feed_Str);
      end if;
      Chats.Insert_Child (Node, False);
    elsif Name = "call" then
      Node.Kind := Call;
      -- Get text
      Node.Text := Get_Text (Xnode);
      Chats.Insert_Child (Node, False);
    elsif Name = "exec" then
      Node.Kind := Exec;
      -- Get text
      Node.Text := Get_Text (Xnode);
      Chats.Insert_Child (Node, False);
    elsif Name = "close" then
      Node.Kind := Close;
      Chats.Insert_Child (Node, False);
    else
      Error (Xnode, "Unexpected node " & Name);
    end if;

    if Debug.Is_On then
      Dummy := Dump (Node, 1);
    end if;

    -- Now insert entries of chats/select
    if Node.Kind = Selec then
      -- Insert each entry
      Debug.Log ("  Inserting entries of select");
      for I in 1 .. Ctx.Get_Nb_Children (Xnode) loop
        -- Chats and select are made of (expect, script) pairs
        -- Only insert "expect"
        if I rem 2 = 1 then
          Child := Ctx.Get_Child (Xnode, I);
          Debug.Log ("    Inserting entry of select");
          Insert_Node (Child, Default_Timeout);
        end if;
      end loop;
      Debug.Log ("  End of entries of select");
      -- Check unicity of entries of select
      -- @@@
      -- Child may already be set for other kinds
      Child := Xml_Parser.No_Node;
    end if;

    -- See if there is a following statement
    declare
      use type Xml_Parser.Node_Type;
    begin
      if Child = Xml_Parser.No_Node
      and then Ctx.Has_Brother (Xnode) then
        Child := Ctx.Get_Brother (Xnode);
      end if;
      if Child /= Xml_Parser.No_Node then
        Debug.Log ("  Inserting next statement");
        Insert_Node (Child, Default_Timeout);
      end if;
    end;

    if Node.Kind = Selec then
      -- Update Next of Selec
      -- @@@
      null;
    end if;

    -- Move back to father
    if Chats.Has_Father then
      Chats.Move_Father;
    end if;
  end Insert_Node;

  -------------------------------
  -- Parse file and build tree --
  -------------------------------
  procedure Parse (File_Name : in Asu_Us) is
    Ok : Boolean;
    Xnode : Xml_Parser.Element_Type;
  begin
    begin
      Ctx.Parse (Asu_Ts (File_Name), Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("XML ERROR: "
             & Ctx.Get_Parse_Error_Message & ".");
        raise Parse_Error;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error ("XML ERROR: Cannot access to file "
          & Asu_Ts (File_Name));
        raise Parse_Error;
    end;
    Debug.Log ("File " & Asu_Ts (File_Name) & " parsed OK.");

    -- Build Tree
    Xnode := Ctx.Get_Root_Element;
    Insert_Node (Xnode, Infinite_Ms);
    Chats.Move_Root;

    -- Dump
    Debug.Log ("Dump:");
    if Debug.Is_On then
      Chats.Iterate (Dump'Access);
    end if;

    -- Clean up
    Ctx.Clean;
  end Parse;

end Tree;

