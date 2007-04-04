separate (Xml_Parser)
package body Tree_Mng is

  -- Check if an attribute already exists for current
  --  element of the prologue or the tree
  -- Return its index or 0 if not
  procedure Find_Attribute (In_Tree : in out My_Tree.Tree_Type;
                            Name : in Asu_Us;
                            Index : out Natural;
                            Value : out Asu_Us) is
    Nb : Trees.Child_Range;
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    Index := 0;
    Nb := My_Tree.Children_Number (In_Tree);
    if Nb = 0 then
      return;
    end if;
    for I in 1 .. Nb loop
      -- Move to first child of to brother
      if I = 1 then
        My_Tree.Move_Child (In_Tree);
      else
        My_Tree.Move_Brother (In_Tree, False);
      end if;
      My_Tree.Read (In_Tree, Cell);
      -- As soon as not an attribute, there will be no more attributes
      exit when Cell.Kind /= Attribute;
      if Cell.Kind = Attribute and then Cell.Name = Name then
        -- Found
        Index := I;
        Value := Cell.Value;
        My_Tree.Move_Father (In_Tree);
        return;
      end if;
    end loop;
    -- Not found
    My_Tree.Move_Father (In_Tree);
    return;
  end Find_Attribute;

  -- Append an attribute to the list of attributes of current
  --  element. Remain on current element
  procedure Insert_Attribute (In_Tree : in out My_Tree.Tree_Type;
                              Attr : in My_Tree_Cell) is
    Nb_Attrs : Natural;
    use type Asu_Us;
  begin
    -- Optim, if current cell has only attributes or only elements, append
    Nb_Attrs := My_Tree.Read (In_Tree).Nb_Attributes;
    if My_Tree.Children_Number (In_Tree) = Nb_Attrs then
      -- Only attributes
      My_Tree.Insert_Child (In_Tree, Attr, False);
      My_Tree.Move_Father (In_Tree);
      return;
    elsif Nb_Attrs = 0 then
      -- Only elements
      My_Tree.Insert_Child (In_Tree, Attr, True);
      My_Tree.Move_Father (In_Tree);
      return;
    end if;

    -- No optim possible, add as brother of last attribute
    for I in 1 .. Nb_Attrs loop
      -- Move to first child of to brother
      if I = 1 then
        My_Tree.Move_Child (In_Tree);
      else
        My_Tree.Move_Brother (In_Tree, False);
      end if;
    end loop;
    My_Tree.Insert_Brother (In_Tree, Attr, False);
    -- Done
    My_Tree.Move_Father (In_Tree);
    return;
  end Insert_Attribute;

  -- Insert an element
  procedure Add_Element (Name : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
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
    Cell.Line_No := Line;
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Value;
    -- Insert as attribute of current and remain current
    Insert_Attribute (Tree, Cell);
    -- Increment number of attributes
    My_Tree.Read (Tree, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Tree, Cell);
  end Add_Attribute;

  function Attribute_Exists (Name : Asu_Us) return Boolean is
    Index : Natural;
    Value : Asu_Us;
  begin
    Find_Attribute (Tree, Name, Index, Value);
    return Index /= 0;
  end Attribute_Exists;

  -- Get an attribute (if it exists, otherwise "")
  function Get_Attribute (Name : Asu_Us) return Asu_Us is
    Index : Natural;
    Value : Asu_Us;
  begin
    Find_Attribute (Tree, Name, Index, Value);
    if Index /= 0 then
      return Value;
    else
      return Asu_Null;
    end if;
  end Get_Attribute;

  procedure Add_Text (Text : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Xml_Parser.Text;
    Cell.Nb_Attributes := 0;
    Cell.Name := Text;
    Cell.Value := Asu_Null;
    -- Insert as attribute of current and remain current
    My_Tree.Insert_Child (Tree, Cell, False);
    My_Tree.Move_Father (Tree);
  end Add_Text;

  --------------
  -- PROLOGUE --
  --------------
  -- Initialise an empty prologue tree
  procedure Init_Prologue is
    Cell : My_Tree_Cell;
  begin
    -- Tree must be empty
    if not My_Tree.Is_Empty (Prologue) then
      raise Constraint_Error;
    end if;
    Cell.Line_No := 1;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Null;
    Cell.Value := Asu_Null;
      -- Insert root
    My_Tree.Insert_Father (Prologue, Cell);
  end Init_Prologue;

  -- Set xml directive
  procedure Set_Xml (Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu.To_Unbounded_String ("xml");
    Cell.Value := Asu_Null;
      -- Update root
    My_Tree.Replace (Prologue, Cell);
  end Set_Xml;

  procedure Add_Xml_Attribute (Name, Value : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Value;
    -- Insert as attribute of Root and remain root
    Insert_Attribute (Prologue, Cell);
    -- Increment number of attributes
    My_Tree.Read (Prologue, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Prologue, Cell);
  end Add_Xml_Attribute;

  function Xml_Existst return Boolean is
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    My_Tree.Read (Prologue, Cell);
    return Cell.Name /= "";
  end Xml_Existst;

  procedure Find_Xml_Attribute (Name : in Asu_Us;
                                Index : out Natural;
                                Value : out Asu_Us) is
  begin
    Find_Attribute (Prologue, Name, Index, Value);
  end Find_Xml_Attribute;

  -- Add a processing instruction
  procedure Add_Pi (Name, Text : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    -- Insert the Element child of root
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Asu_Null;
    My_Tree.Insert_Child (Prologue, Cell, False);
    -- Insert the text child of element
    Cell.Kind := Xml_Parser.Text;
    Cell.Name := Text;
    My_Tree.Insert_Child (Prologue, Cell, False);
    -- Move back to root
    My_Tree.Move_Root (Prologue);
  end Add_Pi;

end Tree_Mng;

