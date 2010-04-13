with Basic_Proc, Xml_Parser.Generator, Sys_Calls, Argument;
procedure T_Xml_Gen is
  Dscr : Xml_Parser.Generator.Ctx_Type;
  Dtd_Name : constant String := "variables.dtd";
  Node : Xml_Parser.Node_Type;
  New_Node : Xml_Parser.Node_Type;
  Node_1, Path_Node, Fail_Node, Tail_Node : Xml_Parser.Node_Type;
  Ok : Boolean;

  procedure Warning (Ctx : in  Xml_Parser.Ctx_Type; Msg : in String) is
  pragma Unreferenced (Ctx);
  begin
    Basic_Proc.Put_Line_Error (Msg);
  end Warning;
  Warnings : Xml_Parser.Warning_Callback_Access := null;

  use Xml_Parser, Xml_Parser.Generator;
begin
  -- Show warnings if "-w"
  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter (1) = "-w" then
    Warnings := Warning'Unrestricted_Access;
  end if;

  -- Symlink Dtd from Data to current
  if not Sys_Calls.File_Found (Dtd_Name) then
    Sys_Calls.Link ("data/" & Dtd_Name, "./" & Dtd_Name, False);
  end if;

  -- Generate Tree
  Dscr.Set_Version (1, 1);
  Node := Dscr.Get_Prologue;
  Dscr.Add_Comment (Node,
     " Definition of variables for test of program ""comp_vars"" ", New_Node);
  Dscr.Add_Doctype (Node, "Variables", False, "", "variables.dtd", "",
               New_Node);
  Dscr.Add_Comment (Node, " After Doctype ", New_Node);

  Node := Dscr.Get_Root_Element;
  Dscr.Set_Name (Node, "Variables");
  Dscr.Add_Child (Node, " Below root ", Xml_Parser.Comment, New_Node);

  Dscr.Add_Brother (New_Node, "Var", Xml_Parser.Element, Node_1);
  Dscr.Add_Attribute (Node_1, "Name", "V1");
  Dscr.Add_Attribute (Node_1, "Type", "Int");
  Dscr.Add_Child (Node_1, "5", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, Fail_Node);
  Dscr.Add_Attribute (Fail_Node, "Name", "Fail");
  Dscr.Add_Attribute (Fail_Node, "Type", "Str");
  Dscr.Add_Child (Fail_Node, "${SET_ME}", Xml_Parser.Text, New_Node);
  Node := Fail_Node;

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "V2");
  Dscr.Add_Attribute (New_Node, "Type", "Int");
  Dscr.Add_Child (New_Node, "5*${V1}*${V1}", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "V3");
  Dscr.Add_Attribute (New_Node, "Type", "Str");
  Dscr.Add_Child (New_Node, "${V1}*${V2}", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "V4");
  Dscr.Add_Attribute (New_Node, "Type", "Str");
  Dscr.Set_Put_Empty (New_Node, True);
  Node := New_Node;

  -- Add a Copy from Node_1 as brother
  Dscr.Copy (Node_1, Node, Child => False, Next => True);
  New_Node := Dscr.Get_Brother (Node);
  --  Adapt
  Dscr.Del_Attributes (New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "V2");
  Dscr.Add_Attribute (New_Node, "Type", "Int");
  New_Node := Dscr.Get_Child (New_Node, 1);
  Dscr.Set_Text (New_Node, "${V3}*2");
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, Path_Node);
  Dscr.Add_Attribute (Path_Node, "Name", "PATH");
  Dscr.Add_Child (Path_Node, "${PATH}:/usr/local/bin", Xml_Parser.Text,
             New_Node);

  -- Swap Fail and Path
  Dscr.Swap (Fail_Node, Path_Node);

  Dscr.Add_Brother (Fail_Node,
       " Comp_var will report error if variable SET_ME is not set ",
       Xml_Parser.Comment, New_Node, Next => False);

  -- Add a comment in the tail
  -- Tail indicator is an empty element
  Dscr.Add_Brother (Fail_Node, "", Xml_Parser.Element, Tail_Node);
  Dscr.Add_Child (Tail_Node, " A comment in tail ",
                  Xml_Parser.Comment, New_Node);

  -- Check tree
  Dscr.Check (Ok, Warnings);
  if not Ok then
    Basic_Proc.Put_Line_Error (Dscr.Get_Parse_Error_Message);
    Basic_Proc.Set_Error_Exit_Code;
    Dscr.Clean;
    return;
  end if;

  -- Display Tree
  Dscr.Put (Xml_Parser.Generator.Stdout);
end T_Xml_Gen;

