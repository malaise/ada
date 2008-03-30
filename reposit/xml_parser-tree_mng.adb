separate (Xml_Parser)
package body Tree_Mng is

  -- Check if an attribute already exists for current
  --  element of the prologue or the tree
  -- Return it and its index, or Asu_Null and 0 if not
  procedure Find_Attribute (In_Tree : in out My_Tree.Tree_Type;
                            Name : in Asu_Us;
                            Index : out Natural;
                            Value : out Asu_Us) is
    Nb : Trees.Child_Range;
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    Index := 0;
    Value := Asu_Null;
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
      -- Only elements or texts or comments
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

  procedure Nb_Attributes (In_Tree : in out My_Tree.Tree_Type;
                           Number : out Trees.Child_Range) is
    Cell : My_Tree_Cell;
  begin
    My_Tree.Read (In_Tree, Cell);
    Number := Cell.Nb_Attributes;
  end Nb_Attributes;

  -- Insert an element
  procedure Add_Element (Elements : in out My_Tree.Tree_Type;
                         Name : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Asu_Null;
    if My_Tree.Is_Empty (Elements) then
      -- Insert root
      My_Tree.Insert_Father (Elements, Cell);
    else
      -- Insert as child of current
      My_Tree.Insert_Child (Elements, Cell, False);
    end if;
  end Add_Element;

  -- Move to father of current element
  procedure Move_Up (Elements : in out My_Tree.Tree_Type) is
  begin
    My_Tree.Move_Father (Elements);
  end Move_Up;

  procedure Add_Attribute (Elements : in out My_Tree.Tree_Type;
                           Name, Value : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Value;
    -- Insert as attribute of current and remain current
    Insert_Attribute (Elements, Cell);
    -- Increment number of attributes
    My_Tree.Read (Elements, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Elements, Cell);
  end Add_Attribute;

  procedure Attribute_Exists (Elements : in out My_Tree.Tree_Type;
                              Name : in Asu_Us;
                              Exists : out Boolean) is
    Index : Natural;
    Value : Asu_Us;
  begin
    Find_Attribute (Elements, Name, Index, Value);
    Exists := Index /= 0;
  end Attribute_Exists;

  -- Get an attribute (if it exists, otherwise "")
  procedure Get_Attribute (Elements : in out My_Tree.Tree_Type;
                           Name : in Asu_Us;
                           Value : out Asu_Us) is
    Index : Natural;
  begin
    Find_Attribute (Elements, Name, Index, Value);
  end Get_Attribute;

  procedure Add_Text (Elements : in out My_Tree.Tree_Type;
                      Text : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Xml_Parser.Text;
    Cell.Nb_Attributes := 0;
    Cell.Name := Text;
    Cell.Value := Asu_Null;
    -- Insert as attribute of current and remain current
    My_Tree.Insert_Child (Elements, Cell, False);
    My_Tree.Move_Father (Elements);
  end Add_Text;

  --------------
  -- PROLOGUE --
  --------------
  -- Initialise an empty prologue tree
  procedure Init_Prologue (Prologue : in out My_Tree.Tree_Type) is
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
  procedure Set_Xml (Prologue : in out My_Tree.Tree_Type;
                     Line : in Positive) is
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

  procedure Add_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                               Name, Value : in Asu_Us; Line : in Positive) is
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

  procedure Xml_Existst (Prologue : in out My_Tree.Tree_Type;
                         Exists : out Boolean) is
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    My_Tree.Read (Prologue, Cell);
    Exists := Cell.Name /= "";
  end Xml_Existst;

  procedure Find_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                                Name : in Asu_Us;
                                Index : out Natural;
                                Value : out Asu_Us) is
  begin
    Find_Attribute (Prologue, Name, Index, Value);
  end Find_Xml_Attribute;

  procedure Get_Nb_Xml_Attributes (Prologue : in out My_Tree.Tree_Type;
                                   Number : out Natural) is
  begin
    Nb_Attributes (Prologue, Number);
  end Get_Nb_Xml_Attributes;

  -- Add a processing instruction
  procedure Add_Pi (Prologue : in out My_Tree.Tree_Type;
                    Name, Text : in Asu_Us; Line : in Positive) is
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

  -- Is a tree (elements or prologue) empty
  function Is_Empty (Tree : My_Tree.Tree_Type) return Boolean is
  begin
    return My_Tree.Is_Empty (Tree);
  end Is_Empty;

  -- Add a comment to current cell (of elements or prologue)
  -- remain on current cell
  procedure Add_Comment (Tree : in out My_Tree.Tree_Type;
                         Comment : in Asu_Us; Line : in Positive) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Xml_Parser.Comment;
    Cell.Nb_Attributes := 0;
    Cell.Name := Comment;
    Cell.Value := Asu_Null;
    -- Insert as attribute of current and remain current
    My_Tree.Insert_Child (Tree, Cell, False);
    My_Tree.Move_Father (Tree);
  end Add_Comment;
end Tree_Mng;

