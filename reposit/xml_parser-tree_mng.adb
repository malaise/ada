separate (Xml_Parser)
package body Tree_Mng is

  -- Insert an element
  procedure Add_Element (Name : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Kind := Element;
    Cell.Name := Name;
    Cell.Line_No := Line;
    Cell.Nb_Attributes := 0;
    Cell.Value := Asu_Null;
    if My_Tree.Is_Empty (Tree) then
      -- Insert root
      My_Tree.Insert_Father (Tree, Cell);
    else
      -- Insert as child of current
      My_Tree.Insert_Child (Tree, Cell, False);
    end if;
  end Add_Element;

  -- Move to father of current element
  procedure Move_Up is
  begin
    My_Tree.Move_Father (Tree);
  end Move_Up;

  procedure Add_Attribute (Name, Value : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Kind := Attribute;
    Cell.Name := Name;
    Cell.Line_No := Line;
    Cell.Value := Value;
    -- Insert as attribute of current and remain current
    My_Tree.Insert_Child (Tree, Cell, False);
    My_Tree.Move_Father (Tree);
    -- Increment number of attributes
    My_Tree.Read (Tree, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Tree, Cell);
  end Add_Attribute;

  function Attribute_Exists (Name : Asu_Us) return Boolean is
    Nb : Trees.Child_Range;
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    Nb := My_Tree.Children_Number (Tree);
    if Nb = 0 then
      return False;
    end if;
    for I in 1 .. Nb loop
      -- Move to first child of to brother
      if I = 1 then
        My_Tree.Move_Child (Tree);
      else
        My_Tree.Move_Brother (Tree, False);
      end if;
      My_Tree.Read (Tree, Cell);
      -- As soon as not an attribute, there will be no more attributes
      exit when Cell.Kind /= Attribute;
      if Cell.Kind = Attribute and then Cell.Name = Name then
        -- Found
        My_Tree.Move_Father (Tree);
        return True;
      end if;
    end loop;
    -- Not found
    My_Tree.Move_Father (Tree);
    return False;
  end Attribute_Exists;

  procedure Add_Text (Text : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Kind := Xml_Parser.Text;
    Cell.Name := Text;
    Cell.Line_No := Line;
    Cell.Value := Asu_Null;
    -- Insert as attribute of current and remain current
    My_Tree.Insert_Child (Tree, Cell, False);
    My_Tree.Move_Father (Tree);
  end Add_Text;

end Tree_Mng;

