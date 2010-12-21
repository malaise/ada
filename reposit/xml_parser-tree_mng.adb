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
  begin
    Index := 0;
    Value := Asu_Null;
    Nb := In_Tree.Children_Number;
    if Nb = 0 then
      return;
    end if;
    for I in 1 .. Nb loop
      -- Move to first child or to brother
      if I = 1 then
        In_Tree.Move_Child;
      else
        In_Tree.Move_Brother (False);
      end if;
      In_Tree.Read (Cell);
      -- As soon as not an attribute, there will be no more attributes
      exit when Cell.Kind /= Attribute;
      if Cell.Kind = Attribute and then Cell.Name = Name then
        -- Found
        Index := I;
        Value := Cell.Value;
        In_Tree.Move_Father;
        return;
      end if;
    end loop;
    -- Not found
    In_Tree.Move_Father;
    return;
  end Find_Attribute;

  -- Append an attribute to the list of attributes of current
  --  element. Remain on current element
  procedure Insert_Attribute (In_Tree : in out My_Tree.Tree_Type;
                              Attr : in My_Tree_Cell) is
    Nb_Attrs : Natural;
  begin
    -- Optim, if current cell has only attributes or only elements, append
    Nb_Attrs := In_Tree.Read.Nb_Attributes;
    if In_Tree.Children_Number = Nb_Attrs then
      -- Only attributes
      In_Tree.Insert_Child (Attr, False);
      In_Tree.Move_Father;
      return;
    elsif Nb_Attrs = 0 then
      -- Only elements or texts or comments
      In_Tree.Insert_Child (Attr, True);
      In_Tree.Move_Father;
      return;
    end if;

    -- No optim possible, add as brother of last attribute
    for I in 1 .. Nb_Attrs loop
      -- Move to first child of to brother
      if I = 1 then
        In_Tree.Move_Child;
      else
        In_Tree.Move_Brother (False);
      end if;
    end loop;
    In_Tree.Insert_Brother (Attr, False);
    -- Done
    In_Tree.Move_Father;
    return;
  end Insert_Attribute;

  procedure Nb_Attributes (In_Tree : in out My_Tree.Tree_Type;
                           Number : out Trees.Child_Range) is
    Cell : My_Tree_Cell;
  begin
    In_Tree.Read (Cell);
    Number := Cell.Nb_Attributes;
  end Nb_Attributes;

  -- Insert an element
  procedure Add_Element (Elements : in out My_Tree.Tree_Type;
                         Name : in Asu_Us; Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Asu_Null;
    if Elements.Is_Empty then
      -- Insert root
      Elements.Insert_Father (Cell);
    else
      -- Insert as child of current
      Elements.Insert_Child (Cell, False);
    end if;
  end Add_Element;

  -- Add specific tuning to element (xml:space=preserve)
  -- Use the Value field of the element
  procedure Add_Tuning (Elements : in out My_Tree.Tree_Type;
                        Tuning : in String) is
    Cell : My_Tree_Cell;
  begin
    Elements.Read (Cell);
    Cell.Value.Append (Tuning & " ");
    Elements.Replace (Cell);
  end Add_Tuning;

  -- Get specific tuning to element (xml:space=preserve)
  function Get_Tuning (Elements : My_Tree.Tree_Type) return String is
    Cell : My_Tree_Cell;
  begin
    Elements.Read (Cell);
    return Cell.Value.Image;
  end Get_Tuning;

  -- Set Put_Empty to False
  procedure Set_Put_Empty (Elements : in out My_Tree.Tree_Type;
                           Put_Empty : in Boolean) is
    Cell : My_Tree_Cell;
  begin
    Elements.Read (Cell);
    Cell.Put_Empty := Put_Empty;
    Elements.Replace (Cell);
  end Set_Put_Empty;

  -- Set Is_Mixed
  procedure Set_Is_Mixed (Elements : in out My_Tree.Tree_Type;
                          Is_Mixed : in Boolean) is
    Cell : My_Tree_Cell;
  begin
    Elements.Read (Cell);
    Cell.Is_Mixed := Is_Mixed;
    Elements.Replace (Cell);
  end Set_Is_Mixed;

  -- Move to root of current tree
  procedure Move_Root (Elements : in out My_Tree.Tree_Type) is
  begin
    if not Elements.Is_Empty then
      Elements.Move_Root;
    end if;
  end Move_Root;

  procedure Add_Attribute (Elements : in out My_Tree.Tree_Type;
                           Name, Value : in Asu_Us; Line : in Natural) is
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
    Elements.Read (Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    Elements.Replace (Cell);
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

  --------------
  -- PROLOGUE --
  --------------
  -- Initialise an empty prologue tree
  procedure Init_Prologue (Prologue : in out My_Tree.Tree_Type) is
    Cell : My_Tree_Cell;
  begin
    -- Tree must be empty
    if not Prologue.Is_Empty then
      raise Constraint_Error;
    end if;
    Cell.Line_No := 1;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Null;
    Cell.Value := Asu_Null;
      -- Insert root
    Prologue.Insert_Father (Cell);
  end Init_Prologue;

  -- Set xml directive
  procedure Set_Xml (Prologue : in out My_Tree.Tree_Type;
                     Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Tus ("xml");
    Cell.Value := Asu_Null;
    -- Update root
    Prologue.Replace (Cell);
  end Set_Xml;

  procedure Add_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                               Name, Value : in Asu_Us; Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Value;
    -- Insert as attribute
    Insert_Attribute (Prologue, Cell);
    -- Increment number of attributes
    Prologue.Read (Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    Prologue.Replace (Cell);
  end Add_Xml_Attribute;

  -- Sets or overwrites a xml attribute at a given index
  procedure Set_Xml_Attribute (Prologue : in out My_Tree.Tree_Type;
                  Name : in Asu_Us; Index : in Positive; Value : in Asu_Us) is
    Pro_Cell, Cell, Tmp_Cell : My_Tree_Cell;
  begin
    Cell.Line_No := 0;
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Value;
    -- Read root and check
    Prologue.Read (Pro_Cell);
    if Index > Pro_Cell.Nb_Attributes + 1 then
       Trace ("Inserting/replacing XML attribute " & Name.Image
            & " at index " & Index'Img & " while having "
            & Pro_Cell.Nb_Attributes'Img & " attributes");
      raise Internal_Error;
    end if;
    if Pro_Cell.Nb_Attributes = 0 then
       -- No Attribute
       Prologue.Insert_Child (Cell);
       Prologue.Move_Root;
       return;
    end if;
    -- Move to attribute before Index
    Prologue.Move_Child;
    for I in 1 .. Index - 2 loop
      Prologue.Move_Brother (False);
    end loop;
    if Index = Pro_Cell.Nb_Attributes + 1 then
      -- This is the insertion of last attribute
      Prologue.Insert_Brother (Cell, False);
      Pro_Cell.Nb_Attributes := Pro_Cell.Nb_Attributes + 1;
    else
      -- Read next attribute and check name
      Prologue.Move_Brother (False);
      Prologue.Read (Tmp_Cell);
      if Tmp_Cell.Name = Name then
        -- This attribute matches, overwrite it
        Prologue.Replace (Cell);
      else
        -- Attribute must be inserted before current
        Prologue.Insert_Brother (Cell, True);
        Pro_Cell.Nb_Attributes := Pro_Cell.Nb_Attributes + 1;
      end if;
    end if;
    -- Done
    Prologue.Move_Root;
    Prologue.Replace (Pro_Cell);
  end Set_Xml_Attribute;

  procedure Xml_Existst (Prologue : in out My_Tree.Tree_Type;
                         Exists : out Boolean) is
   Cell : My_Tree_Cell;
  begin
    if Prologue.Is_Empty then
      Exists := False;
    else
      Prologue.Read (Cell);
      Exists := not Cell.Name.Is_Null;
    end if;
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
  procedure Add_Pi (Tree : in out My_Tree.Tree_Type;
                    Name, Text : in Asu_Us; Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    -- Insert the Element child of root
    Cell.Line_No := Line;
    Cell.Kind := Pi;
    Cell.Nb_Attributes := 0;
    Cell.Name := Name;
    Cell.Value := Text;
    Tree.Insert_Child (Cell, False);
  end Add_Pi;

  -- Is a tree (elements or prologue) empty
  function Is_Empty (Tree : My_Tree.Tree_Type) return Boolean is
  begin
    return Tree.Is_Empty;
  end Is_Empty;

  -- Add a comment to current cell (of elements or prologue)
  -- remain on current cell
  procedure Add_Text (Tree : in out My_Tree.Tree_Type;
                      Text : in Asu_Us; Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Xml_Parser.Text;
    Cell.Nb_Attributes := 0;
    Cell.Name := Text;
    Cell.Value := Asu_Null;
    -- Insert as child of current and remain current
    Tree.Insert_Child (Cell, False);
  end Add_Text;

  -- Add a comment to current cell (of elements or prologue)
  -- remain on current cell
  procedure Add_Comment (Tree : in out My_Tree.Tree_Type;
                         Comment : in Asu_Us; Line : in Natural) is
    Cell : My_Tree_Cell;
  begin
    Cell.Line_No := Line;
    Cell.Kind := Xml_Parser.Comment;
    Cell.Nb_Attributes := 0;
    Cell.Name := Comment;
    Cell.Value := Asu_Null;
    -- Insert child
    Tree.Insert_Child (Cell, False);
  end Add_Comment;

  -- Build the Node_Update associated to current Node
  procedure Build_Update (Tree   : in out My_Tree.Tree_Type;
                          Update : in out Node_Update;
                          Creation : in Boolean) is
    Cell, Attr : My_Tree_Cell;
  begin
    Tree.Read (Cell);
    Update.Line_No := Cell.Line_No;
    Update.Name := Cell.Name;
    Update.Value := Cell.Value;
    Update.Creation := Creation;
    Update.Is_Mixed := Cell.Is_Mixed;
    case Cell.Kind is
      when Element =>
        Update.Kind := Element;
      when Text =>
        Update.Kind := Text;
      when Pi =>
        Update.Kind := Pi;
      when Comment =>
        Update.Kind := Comment;
      when Attribute =>
        Trace ("Building a node update from an attribute " & Cell.Name.Image);
        raise Internal_Error;
    end case;
    Deallocate (Update.Attributes);

    -- Case of deletion, no build of attribtues
    if not Creation then
      if Cell.Kind /= Element then
        Trace ("Building deletion but not of element " & Cell.Name.Image);
        raise Internal_Error;
      end if;
      return;
    end if;

    -- Children and attributes are only for elements
    if Update.Kind /= Element then
      return;
    end if;

    -- Create and fill attributes
    Update.Attributes := new Attributes_Array (1 .. Cell.Nb_Attributes);
    for I in 1 .. Cell.Nb_Attributes loop
      -- Move to first child or to brother
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      Tree.Read (Attr);
      Update.Attributes(I).Name := Attr.Name;
      Update.Attributes(I).Value := Attr.Value;
      Update.Attributes(I).Unparsed := Attr.Unparsed;
    end loop;
    if Update.Attributes'Length /= 0 then
      Tree.Move_Father;
    end if;
  end Build_Update;

end Tree_Mng;

