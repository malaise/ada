with Xml_Parser.Generator;
procedure T_Xml_Gen is
  Dscr : Xml_Parser.Ctx_Type;
  Node : Xml_Parser.Node_Type;
  New_Node : Xml_Parser.Node_Type;
  use Xml_Parser, Xml_Parser.Generator;
begin
  Set_Version (Dscr, 1, 1);
  Node := Get_Prologue (Dscr);
  Add_Comment (Dscr, Node,
     " Definition of variables for test of program ""comp_vars"" ", New_Node);
  Add_Doctype (Dscr, Node, "Variables", False, "", "variables.dtd", "",
               New_Node);
  Add_Comment (Dscr, Node, " After Doctype ", New_Node);

  Node := Get_Root_Element (Dscr);
  Set_Name (Dscr, Node, "Variables");
  Add_Child (Dscr, Node, " Below root ", Xml_Parser.Comment, New_Node);

  Add_Brother (Dscr, New_Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "V1");
  Add_Attribute (Dscr, New_Node, "Type", "Int");
  Add_Child (Dscr, New_Node, "5", Xml_Parser.Text, New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Add_Brother (Dscr, Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "PATH");
  Add_Child (Dscr, New_Node, "${PATH}:/usr/local/bin", Xml_Parser.Text,
             New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Add_Brother (Dscr, Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "V2");
  Add_Attribute (Dscr, New_Node, "Type", "Int");
  Add_Child (Dscr, New_Node, "5*${V1}*${V1}", Xml_Parser.Text, New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Add_Brother (Dscr, Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "V3");
  Add_Attribute (Dscr, New_Node, "Type", "Str");
  Add_Child (Dscr, New_Node, "${V1}*${V2}", Xml_Parser.Text, New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Add_Brother (Dscr, Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "V2");
  Add_Attribute (Dscr, New_Node, "Type", "Int");
  Add_Child (Dscr, New_Node, "${V3}*2", Xml_Parser.Text, New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Add_Brother (Dscr, Node,
   " Comp_var will report error if variable SET_ME is not set ",
   Xml_Parser.Comment, New_Node);

  Add_Brother (Dscr, New_Node, "Var", Xml_Parser.Element, New_Node);
  Add_Attribute (Dscr, New_Node, "Name", "Fail");
  Add_Attribute (Dscr, New_Node, "Type", "Str");
  Add_Child (Dscr, New_Node, "${SET_ME}", Xml_Parser.Text, New_Node);
  Node := Get_Parent (Dscr, New_Node);

  Put (Dscr, Xml_Parser.Generator.Stdout);
end T_Xml_Gen;

