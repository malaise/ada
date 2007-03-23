with Ada.Text_Io, Ada.Strings.Unbounded;
with Argument, Xml_Parser, Normal;
procedure T_Xml is
  Prologue, Root : Xml_Parser.Element_Type;
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  procedure Put_Attributes (Elt : in Xml_Parser.Element_Type) is
    Attrs : Xml_Parser.Attributes_Array := Xml_Parser.Get_Attributes (Elt);
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      Ada.Text_Io.Put (" " & Asu.To_String (Attrs(I).Name
                     & "=" & Attrs(I).Value));
    end loop;
  end Put_Attributes;

  procedure Put_Element (Elt : in Xml_Parser.Element_Type;
                         Level : in Natural) is
    Children : Xml_Parser.Nodes_Array := Xml_Parser.Get_Children (Elt);
    use type Xml_Parser.Node_Kind_List;
  begin
    Ada.Text_Io.Put (Normal (Xml_Parser.Get_Line_No (Elt), 5, True, '0'));
    for I in 1 .. Level + 1 loop
      Ada.Text_Io.Put (" ");
    end loop;
    Ada.Text_Io.Put (Asu.To_String(Xml_Parser.Get_Name (Elt)) & " :" );
    Put_Attributes (Elt);
    Ada.Text_Io.New_Line;
    for I in Children'Range loop
      if I rem 2 = 0 then
        -- Test the individual get
        Children(I) := Xml_Parser.Get_Child (Elt, I);
      end if;
      if Children(I).Kind = Xml_Parser.Element then
        -- Recursive put child
        Put_Element (Children(I), Level + 1);
      else
        -- Specific put text
        Ada.Text_Io.Put (Normal (Xml_Parser.Get_Line_No (Children(I)),
                                 5, True, '0'));
        for I in 1 .. Level + 1 loop
          Ada.Text_Io.Put (" ");
        end loop;
        Ada.Text_Io.Put_Line ("  =>" & Xml_Parser.Get_Text (Children(I))
                            & "<=");
      end if;
    end loop;
  end Put_Element;

begin
  Xml_Parser.Parse (Argument.Get_Parameter, Prologue, Root);
  Ada.Text_Io.Put_Line ("Prologue:");
  Put_Element (Prologue, 0);
  Ada.Text_Io.Put_Line ("Elements tree:");
  Put_Element (Root, 0);
  Xml_Parser.Clean (Prologue, Root);
exception
  when Xml_Parser.Parse_Error =>
    Ada.Text_Io.Put_Line (Xml_Parser.Get_Parse_Error_Message);
    raise;
end T_Xml;

