with Xml_Parser.Generator;
procedure T_Xml_Gen is
  Dscr : Xml_Parser.Generator.Xml_Dscr_Type;
begin
  Dscr.Reset (1, 1, "Variables");
  Dscr.Add_Comment (
     " Definition of variable for test of program ""comp_vars"" ");
  Dscr.Set_Doctype ("Variables", False, "", "variables.dtd", "");
  Dscr.Add_Comment (" After Doctype ");

  Dscr.Add_Child (" Below root ", Xml_Parser.Generator.Comment);

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "V1");
  Dscr.Add_Attribute ("Type", "Int");
  Dscr.Add_Child ("5", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "PATH");
  Dscr.Add_Attribute ("Type", "Str");
  Dscr.Add_Child ("${PATH}:/usr/local/bin", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "V2");
  Dscr.Add_Attribute ("Type", "Int");
  Dscr.Add_Child ("5*${V1}*${V1}", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "V3");
  Dscr.Add_Attribute ("Type", "Str");
  Dscr.Add_Child ("${V1}*${V2}", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "V2");
  Dscr.Add_Attribute ("Type", "Int");
  Dscr.Add_Child ("${V3}*2", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Add_Brother (
   " Comp_var will report error if variable SET_ME is not set ",
   Xml_Parser.Generator.Comment);

  Dscr.Add_Brother ("Var", Xml_Parser.Generator.Element);
  Dscr.Add_Attribute ("Name", "Fail");
  Dscr.Add_Attribute ("Type", "Str");
  Dscr.Add_Child ("${SET_ME}", Xml_Parser.Generator.Text);
  Dscr.Move_Father;

  Dscr.Put (Xml_Parser.Generator.Stdout);
end T_Xml_Gen;

