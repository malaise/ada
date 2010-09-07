with Basic_Proc, Xml_Parser, String_Mng, Environ;
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
  function Expand (Xnode : Xml_Parser.Text_Type;
                   Text : Asu_Us) return Asu_Us is
  begin
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
  end Expand;

  -------------
  -- Parsing --
  -------------
  -- Recursive insertion of a node
  procedure Insert_Node (Xnode : in Xml_Parser.Element_Type;
                         Timeout : in Integer) is
    Name : constant String := Ctx.Get_Name (Xnode);
    Node : Node_Rec;
    Default_Timeout : Integer;
    Child : Xml_Parser.Element_Type;
    Text : Xml_Parser.Text_Type;
  begin
    -- Insert current node
    if Name = "chats" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      -- Insert root
      Chats.Insert_Father (Node);
      -- For children
      Default_Timeout := Infinite_Ms;
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
      Text := Ctx.Get_Child (Ctx.Get_Child (Xnode, 1), 1);
      Node.Text := Expand (Text, Ctx.Get_Text (Text));
      Chats.Insert_Child (Node);
      -- For children
      Default_Timeout := Get_Timeout (Xnode, Timeout, "InputDefaultTimeoutMs");
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Child (Xnode, 2), 1);
    elsif Name = "select" then
      Node.Kind := Selec;
      Node.Timeout := Get_Timeout (Xnode, Timeout);
      Chats.Insert_Child (Node);
      -- For children
      Default_Timeout := Timeout;
    elsif Name = "expect" then
      -- The expect of a select => Read
      Node.Kind := Read;
      -- Get expect text
      Text := Ctx.Get_Child (Ctx.Get_Child (Xnode, 1), 1);
      Node.Text := Expand (Text, Ctx.Get_Text (Text));
      Chats.Insert_Child (Node);
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Child (Xnode, 2), 1);
    elsif Name = "default" then
      -- The default of a select
      Node.Kind := Default;
      Chats.Insert_Child (Node);
      -- Move to script first entry
      Child := Ctx.Get_Child (Ctx.Get_Child (Xnode, 2), 1);
    elsif Name = "read" then
      -- @@@
      null;
    else
      Error (Xnode, "Unexpected node " & Name);
    end if;

    -- Now insert children of chats/select
    -- @@@
    --  or child of other node
    Insert_Node (Child, Default_Timeout);
    -- Update Next of Selec
  end Insert_Node;


  -- Parse file and build tree
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

    -- Clean up
    Ctx.Clean;
  end Parse;


  -- Kind of node
  -- type Node_Kind is (Selec, Read, Default, Skip, Wait, Send, Call, Exec, Close);

  -- Infinite timeout
  -- Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Node
  -- type Node_Rec;

  -- type Node_Access is access all Node_Rec;

  -- type Node_Rec is record
  --   Kind : Node_Kind := Close;
    -- For chat (kind Read)
  --   Name : Asu_Us;
    -- For Read, Send, Call or Exec
  --   Text : Asu_Us;
    -- For Selec, Read, Skip, Wait
  --   Timeout : Integer := Infinite_Ms;
    -- Next node
  --   Next : Node_Access := null;
  -- end record;

end Tree;

