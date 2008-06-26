with Basic_Proc, Xml_Parser.Generator;
procedure T_Xml_Gen is
  Dscr : Xml_Parser.Generator.Ctx_Type;
  Node : Xml_Parser.Node_Type;
  New_Node : Xml_Parser.Node_Type;
  Ok : Boolean;
  use Xml_Parser, Xml_Parser.Generator;
begin
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

  Dscr.Add_Brother (New_Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "V1");
  Dscr.Add_Attribute (New_Node, "Type", "Int");
  Dscr.Add_Child (New_Node, "5", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "PATH");
  Dscr.Add_Child (New_Node, "${PATH}:/usr/local/bin", Xml_Parser.Text,
             New_Node);
  Node := Dscr.Get_Parent (New_Node);

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
  Dscr.Add_Attribute (New_Node, "Name", "V2");
  Dscr.Add_Attribute (New_Node, "Type", "Int");
  Dscr.Add_Child (New_Node, "${V3}*2", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  Dscr.Add_Brother (Node,
       " Comp_var will report error if variable SET_ME is not set ",
       Xml_Parser.Comment, New_Node);

  Dscr.Add_Brother (New_Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "Fail");
  Dscr.Add_Attribute (New_Node, "Type", "Str");
  Dscr.Add_Child (New_Node, "${SET_ME}", Xml_Parser.Text, New_Node);
  Node := Dscr.Get_Parent (New_Node);

  -- Check tree
  Dscr.Check (Ok);
  if not Ok then
    Basic_Proc.Put_Line_Error ("Error in tree "
          & Dscr.Get_Parse_Error_Message);
    Basic_Proc.Set_Error_Exit_Code;
    Dscr.Clean;
    return;
  end if;

  -- Display Tree
  Dscr.Put (Xml_Parser.Generator.Stdout);
end T_Xml_Gen;

