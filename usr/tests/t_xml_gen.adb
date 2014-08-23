with Basic_Proc, Xml_Parser.Generator, Sys_Calls, Argument, Mixed_Str;
procedure T_Xml_Gen is
  Dscr : Xml_Parser.Generator.Ctx_Type;
  Dtd_Name : constant String := "variables.dtd";
  Node : Xml_Parser.Node_Type;
  New_Node : Xml_Parser.Node_Type;
  Node_1, Path_Node, Fail_Node : Xml_Parser.Node_Type;
  Ok : Boolean;

  Abort_Error : exception;

  procedure Warning (Ctx : in  Xml_Parser.Ctx_Type; Msg : in String) is
  pragma Unreferenced (Ctx);
  begin
    Basic_Proc.Put_Line_Error (Msg);
  end Warning;
  Warnings : Xml_Parser.Warning_Callback_Access := null;

  procedure Clean is
    use type Sys_Calls.File_Kind_List;
  begin
    Dscr.Clean;
    if Sys_Calls.File_Stat (Dtd_Name).Kind = Sys_Calls.Link then
      Sys_Calls.Unlink (Dtd_Name);
    end if;
  end Clean;

  -- Check that an invalid name (for kind) raises Invalid_Argument
  procedure Check_Invalid_Node (Node : in Xml_Parser.Element_Type;
                                Name : in String;
                                Kind : in Xml_Parser.Node_Kind_List) is
    Tmp_Node : Xml_Parser.Node_Type;
  begin
    Dscr.Add_Child (Node, Name, Kind, Tmp_Node);
    Basic_Proc.Put_Line_Error ("Failed to detect invalid name for "
                             & Mixed_Str (Kind'Img));
    raise Abort_Error;
  exception
    when Xml_Parser.Generator.Invalid_Argument =>
      -- Ok, error detected
      null;
  end Check_Invalid_Node;

  -- Check that an invalid attribute name or value raises Invalid_Argument
  procedure Check_Invalid_Attribute (Node : in out Xml_Parser.Element_Type;
                                     Name, Value : in String) is
  begin
    Dscr.Add_Attribute (Node, Name, Value);
    Basic_Proc.Put_Line_Error ("Failed to detect invalid att name/value for "
                             & Name & "/" & Value);
    raise Abort_Error;
  exception
    when Xml_Parser.Generator.Invalid_Argument =>
      -- Ok, error detected
      null;
  end Check_Invalid_Attribute;

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
  ----------------
  -- XML
  Dscr.Set_Version (1, 1);
  begin
    Dscr.Set_Encoding ("UTF[8]");
    Basic_Proc.Put_Line_Error ("Check failed to detect invalid encoding");
    Basic_Proc.Set_Error_Exit_Code;
    Clean;
    return;
  exception
    when Xml_Parser.Generator.Invalid_Argument =>
      -- Ok, error detected
      null;
  end;
  Dscr.Set_Encoding ("utf-8");
  Node := Dscr.Get_Prologue;

  -- Prologue
  Dscr.Add_Comment (Node,
     " Definition of variables for test of program ""comp_vars"" ", New_Node);
  -- Insert a invalid then a valid Public doctype
  Dscr.Add_Doctype (Node, "Variables", True, "DocId", "variables.dtd", "",
                    New_Node);
  Dscr.Delete_Node (New_Node, New_Node);
  begin
    Dscr.Add_Doctype (Node, "Variables", True, "Doc{Id", "variables.dtd", "",
                      New_Node);
    Basic_Proc.Put_Line_Error ("Check failed to detect invalid DocId name");
    Basic_Proc.Set_Error_Exit_Code;
    Clean;
    return;
  exception
    when Xml_Parser.Generator.Invalid_Argument =>
      -- Ok, error detected
      null;
  end;
  -- Overwrite  with the final system doctype
  Dscr.Add_Doctype (Node, "Variables", False, "", "variables.dtd", "",
               New_Node);
  Dscr.Add_Comment (Node, " After Doctype ", New_Node);

  -- Elements
  Node := Dscr.Get_Root_Element;

  -- Check invalid Pi, Comment, Element name, Text
  Check_Invalid_Node (Node, "P[i]", Xml_Parser.Pi);
  Check_Invalid_Node (Node, "Pi Dat?>a", Xml_Parser.Pi);
  Check_Invalid_Node (Node, "Comment Dat--a", Xml_Parser.Comment);
  Check_Invalid_Node (Node, "Text Dat<a", Xml_Parser.Text);
  Check_Invalid_Node (Node, "Text &ref", Xml_Parser.Text);
  -- Check valid text with CDATA and reference
  Dscr.Add_Child (Node, "Text<![CDATA[Cdata<]]> and &ref;", Xml_Parser.Text,
                  New_Node);
  Dscr.Delete_Node (New_Node, New_Node);
  -- Check invalid attribute name and content
  Check_Invalid_Attribute (Node, "Att[Name", "AttValue");
  Check_Invalid_Attribute (Node, "AttName", "Att<Value");
  Check_Invalid_Attribute (Node, "AttName", "Att&Value");
  Check_Invalid_Attribute (Node, "AttName", "Att&#x4g;");
  -- Check a valid attribute with references
  Dscr.Add_Attribute (Node, "AttName", "&Future;=&#x4f;");
  Dscr.Del_Attribute (Node, "AttName");

  -- Fill Elements section
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
  Dscr.Set_Put_Empty (New_Node, True);
  Node := New_Node;

  -- Add a Copy from Node_1 as brother
  Dscr.Copy (Node_1, Node, New_Node => New_Node, Child => False, Next => True);
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

  -- Add an invalid entry
  Dscr.Add_Brother (Fail_Node, "Var1", Xml_Parser.Element, New_Node);
  -- Check tree
  Dscr.Check (Ok, Warn_Cb => Warnings);
  if Ok then
    Basic_Proc.Put_Line_Error ("Check failed to detect invalid element name");
    Basic_Proc.Set_Error_Exit_Code;
    Clean;
    return;
  end if;
  -- Del invalid entry
  Dscr.Delete_Node (New_Node, New_Node);

  -- Add a (valid) entry with empty attribute value and a text child
  Dscr.Add_Brother (Fail_Node, "Var", Xml_Parser.Element, New_Node);
  Dscr.Add_Attribute (New_Node, "Name", "Attr");
  Dscr.Add_Attribute (New_Node, "Type", "");
  Dscr.Add_Child (New_Node,
      "Text with a &Reference; and "
    & "<![CDATA[a Cdata < section]]> ; quite complex.",
                  Xml_Parser.Text, Node_1);
  -- Delete it
  Dscr.Delete_Node (New_Node, New_Node);

  -- Add a comment in the tail
  -- Tail indicator is an empty element
  Node := Dscr.Get_Tail;
  Dscr.Add_Child (Node, " A comment in tail ",
                  Xml_Parser.Comment, New_Node);

  -- Check tree
  Dscr.Check (Ok, Warn_Cb => Warnings);
  if not Ok then
    Basic_Proc.Put_Line_Error (Dscr.Get_Parse_Error_Message);
    Basic_Proc.Set_Error_Exit_Code;
    Clean;
    return;
  end if;

  -- Display Tree
  Dscr.Put (Xml_Parser.Generator.Stdout);

  Clean;

exception
  when Abort_Error =>
    Basic_Proc.Set_Error_Exit_Code;
    Clean;
end T_Xml_Gen;

