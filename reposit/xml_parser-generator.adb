-- Generates a Xml file (or stdout), or string from a tree
with Ada.Characters.Latin_1;
with Integer_Image, Text_Line, Sys_Calls, Trees;
package body Xml_Parser.Generator is

  -- Version incremented at each significant change
  Minor_Version : constant String := "1";
  function Version return String is
  begin
    return "V" & Major_Version & "." & Minor_Version;
  end Version;

  -- Name validity check
  function Is_Valid_In_Name (Char : Character) return Boolean is
  begin
    if Character'Pos (Char) > 127 then
      -- Simple test not possible for non ASCII character
      -- May be a valid byte within a UTF-8 sequence
      return True;
    end if;
    -- Nmtoken validity check
    return  ('a' <= Char and then Char >= 'z')
    or else ('A' <= Char and then Char >= 'Z')
    or else ('0' <= Char and then Char >= '9')
    or else Char = ':'
    or else Char = '_'
    or else Char = '-'
    or else Char = '.';
  end Is_Valid_In_Name;
  procedure Check_Name (Name : in String) is
  begin
    if Name = "" then
      raise Invalid_Argument;
    end if;
    for I in Name'Range loop
      if not Is_Valid_In_Name (Name(I)) then
        raise Invalid_Argument;
      end if;
    end loop;
  end Check_Name;

  -- Detect separator
  function Is_Separator (Char : Character) return Boolean is
  begin
    return  Char = Ada.Characters.Latin_1.Space
    or else Char = Ada.Characters.Latin_1.Lf
    or else Char = Ada.Characters.Latin_1.Cr
    or else Char = Ada.Characters.Latin_1.Ht;
  end Is_Separator;

  Xml_Name : constant As.U.Asu_Us := As.U.Tus ("xml");
  -- Init prologue and empty root element if needed
  procedure Init_Ctx (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    if Ctx.Status = Clean
    or else Ctx.Status = Unparsed then
      Ctx.Magic := Get_Magic;
      -- Init prologue: a xml node
      Cell.Kind := Element;
      Cell.Nb_Attributes := 0;
      Cell.Name := Xml_Name;
      Cell.Value.Set_Null;
      Ctx.Prologue.Insert_Father (Cell);
      -- Init elements: an empty root element
      Cell.Name.Set_Null;
      Ctx.Elements.Insert_Father (Cell);
      if Ctx.Status = Unparsed then
        Ctx.Unparsed_List.Delete_List;
      end if;
    end if;
    -- Reset to init (in case of error...)
    Ctx.Status := Init;
  end Init_Ctx;

  -- Move to Node in tree
  procedure Move_To (Ctx  : in out Ctx_Type;
                     Node : in Node_Type;
                     Tree : out Tree_Acc) is
  begin
    -- Init context if needed
    Init_Ctx (Ctx);
    Tree := Get_Tree (Ctx, Node);
    Tree.Set_Position (Node.Tree_Access);
  end Move_To;

  -------------------------
  -- PROLOGUE OPERATIONS --
  -------------------------
  Version_Name : constant As.U.Asu_Us := As.U.Tus ("version");
  Encoding_Name : constant As.U.Asu_Us := As.U.Tus ("encoding");
  Standalone_Name : constant As.U.Asu_Us := As.U.Tus ("standalone");
  procedure Set_Version (Ctx   : in out Ctx_Type;
                         Major : in Major_Range;
                         Minor : in Minor_Range) is
    Cell : My_Tree_Cell;
    Vers : My_Tree_Cell;
  begin
    Init_Ctx (Ctx);
    -- Init version attribute
    Vers.Kind := Attribute;
    Vers.Name := Version_Name;
    Vers.Value := As.U.Tus (Integer_Image (Major)
                          & "." & Integer_Image (Minor));

    -- Read Xml
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Read (Cell);
    if Cell.Nb_Attributes = 0 then
      Cell.Nb_Attributes := 1;
      Ctx.Prologue.Replace (Cell);
      -- Insert version as first attribute
      Ctx.Prologue.Insert_Child (Vers);
    else
       -- Overwrite first attribute
       Ctx.Prologue.Move_Child;
       Ctx.Prologue.Replace (Vers);
    end if;
  end Set_Version;

  procedure Init_Version (Ctx : in out  Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    Init_Ctx (Ctx);
    -- Read Xml
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Read (Cell);
    if Cell.Nb_Attributes = 0 then
      Set_Version (Ctx, 1, 0);
    end if;
  end Init_Version;

  procedure Set_Encoding (Ctx : in out Ctx_Type; Encoding : in String) is
    Cell : My_Tree_Cell;
    Encoding_Cell : My_Tree_Cell;
    use type As.U.Asu_Us;
  begin
    Check_Name (Encoding);
    -- Read Xml
    Init_Version (Ctx);
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Read (Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    Ctx.Prologue.Replace (Cell);
    -- Set this attribute
    Encoding_Cell.Kind := Attribute;
    Encoding_Cell.Name := Encoding_Name;
    Encoding_Cell.Value := As.U.Tus (Encoding);
    -- Add this attribute as brother of version
    Ctx.Prologue.Move_Child;
    if Ctx.Prologue.Has_Brother (False) then
      Ctx.Prologue.Move_Brother (False);
      Ctx.Prologue.Read (Cell);
      if Cell.Kind = Attribute and then Cell.Name = Encoding_Name then
        -- Change encoding
        Ctx.Prologue.Replace (Encoding_Cell);
        return;
      else
        -- Back to version
        Ctx.Prologue.Move_Brother (True);
      end if;
    end if;
    -- Insert encoding as brother of version
    Ctx.Prologue.Insert_Brother (Encoding_Cell, False);
  end Set_Encoding;

  procedure Set_Standalone (Ctx : in out Ctx_Type;
                            Standalone : in Boolean) is
    Cell : My_Tree_Cell;
    Standalone_Cell : My_Tree_Cell;
    use type As.U.Asu_Us;
  begin
    -- Read Xml
    Init_Version (Ctx);
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Read (Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    Ctx.Prologue.Replace (Cell);
    -- Set this attribute
    Standalone_Cell.Kind := Attribute;
    Standalone_Cell.Name := Standalone_Name;
    if Standalone then
      Standalone_Cell.Value := As.U.Tus ("yes");
    else
      Standalone_Cell.Value := As.U.Tus ("no");
    end if;
    -- Add this attribute as brother of version or encoding
    -- Move to Version
    Ctx.Prologue.Move_Child;
    if not Ctx.Prologue.Has_Brother (False) then
      -- No more brother (only version)
      Ctx.Prologue.Insert_Brother (Standalone_Cell, False);
      return;
    end if;
    -- Move 2nd child
    Ctx.Prologue.Move_Brother (False);
    Ctx.Prologue.Read (Cell);
    if Cell.Kind /= Attribute then
      -- 2nd child is no attribute
      Ctx.Prologue.Insert_Brother (Standalone_Cell, True);
      return;
    end if;
    if Cell.Name = Standalone_Name then
      -- Change standalone as 2nd attribute
      Ctx.Prologue.Replace (Standalone_Cell);
      return;
    end if;
    -- 2nd attribute is encoding
    if not Ctx.Prologue.Has_Brother (False) then
      -- No more brother (only version and encoding)
      Ctx.Prologue.Insert_Brother (Standalone_Cell, False);
      return;
    end if;
    Ctx.Prologue.Move_Brother (False);
    Ctx.Prologue.Read (Cell);
    if Cell.Kind = Attribute and then Cell.Name = Standalone_Name then
      -- 3rd child is standalone
      Ctx.Prologue.Replace (Standalone_Cell);
    else
      -- 3rd child is not attribute
      Ctx.Prologue.Insert_Brother (Standalone_Cell, True);
    end if;
  end Set_Standalone;

  -- Clear Xml information
  procedure Clear_Xml (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    -- Clear all Xml attributes
    Init_Ctx (Ctx);
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Read (Cell);
    Cell.Nb_Attributes := 0;
    Ctx.Prologue.Replace  (Cell);
    loop
      Ctx.Prologue.Move_Child (True);
      Ctx.Prologue.Read (Cell);
      exit when Cell.Kind /= Attribute;
      Ctx.Prologue.Delete_Current;
    end loop;
  end Clear_Xml;

  -- Internal op to insert child of prologue
  procedure Insert_Cell (Tree : in Tree_Acc;
                         Cell : in My_Tree_Cell;
                         Append_Next : in Boolean) is
    Father : My_Tree_Cell;
  begin
    Tree.Read (Father);
    if Append_Next then
      if not Tree.Has_Father then
        -- Current is Root of prologue, append as last child
        Tree.Insert_Child (Cell, False);
      else
        -- Current is a child
        Tree.Insert_Brother (Cell, False);
      end if;
    elsif Tree.Has_Father then
      -- Current is a child
      Tree.Insert_Brother (Cell, True);
    elsif Tree.Children_Number = Father.Nb_Attributes then
      -- No child (only attributes), append as last child
      Tree.Insert_Child (Cell, False);
    elsif Father.Nb_Attributes = 0 then
      -- No attribute, insert as first child
      Tree.Insert_Child (Cell, True);
    else
      -- Insert after attributes
      Tree.Move_Child (False);
      for I in 1 .. Father.Nb_Attributes - 1 loop
        Tree.Move_Brother (False);
      end loop;
      Tree.Insert_Brother (Cell, False);
    end if;
  end Insert_Cell;

  -- Add the DOCTYPE directive
  procedure Add_Doctype (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Name     : in String;
                         Public   : in Boolean;
                         Pub_Id   : in String;
                         File     : in String;
                         Int_Def  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    if not Node.In_Prologue then
      raise Invalid_Node;
    end if;
    if not Ctx.Doctype.Name.Is_Null then
      raise Doctype_Already_Set;
    end if;
    Check_Name (Name);
    if not Public and then Pub_Id /= "" then
      raise Invalid_Argument;
    end if;
    -- Move to node
    Move_To (Ctx, Node, Tree);
    -- Add this child to prologue
    Cell.Kind := Text;
    Cell.Nb_Attributes := 0;
    Insert_Cell (Tree, Cell, Append_Next);

    -- Store all Doctype info
    Ctx.Doctype.Line_No := 0;
    Ctx.Doctype.Name    := As.U.Tus (Name);
    Ctx.Doctype.Public  := Public;
    Ctx.Doctype.Pub_Id  := As.U.Tus (Pub_Id);
    Ctx.Doctype.File    := As.U.Tus (File);
    Ctx.Doctype.Int_Def := As.U.Tus (Int_Def);
    New_Node := (Kind => Text,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => Tree.Get_Position);
  end Add_Doctype;

  -- Set the Dtd file towards which comformance shall be checked
  procedure Set_Dtd_File (Ctx      : in out Ctx_Type;
                          Dtd_File : in String) is
  begin
    Ctx.Dtd_File := As.U.Tus (Dtd_File);
  end Set_Dtd_File;

  -- Internal: Split PI into Target (-> Name) and text (-> Value)
  procedure Set_Pi (Cell : in out My_Tree_Cell;
                    Pi : in String) is
    Sep1, Sep2, Sep3 : Natural;
  begin
    -- Locate first non separator, then first separator,
    --  then first non separator
    Sep1 := 0;
    Sep2 := 0;
    Sep3 := 0;
    for I in Pi'Range loop
      if Is_Separator (Pi(I)) then
        if Sep1 /= 0 and then Sep2 = 0 then
          -- First separator
          Sep2 := I - 1;
        end if;
      else
        if Sep1 = 0 then
          Sep1 := I;
        elsif Sep2 /= 0 and then Sep3 = 0 then
          -- First non separator after separators
          Sep3 := I;
          exit;
        end if;
      end if;
    end loop;
    if Sep1 = 0 then
      -- All separators
      Cell.Name.Set_Null;
      Cell.Value.Set_Null;
    elsif Sep2 = 0 then
      -- No separator after first word
      Cell.Name := As.U.Tus (Pi (Sep1 .. Pi'Last));
      Cell.Value.Set_Null;
    elsif Sep3 = 0 then
      -- A target then only separators
      Cell.Name := As.U.Tus (Pi (Sep1 .. Sep2));
      Cell.Value.Set_Null;
    else
      -- A target then separators then ...
      Cell.Name := As.U.Tus (Pi(Sep1 .. Sep2));
      Cell.Value := As.U.Tus (Pi(Sep3 .. Pi'Last));
   end if;
  end Set_Pi;

  -- Add a processing instruction in the prologue
  procedure Add_Pi (Ctx  : in out Ctx_Type;
                    Node : in Node_Type;
                    Pi   : in String;
                    New_Node : out Node_Type;
                    Append_Next : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    if not Node.In_Prologue then
      raise Invalid_Node;
    end if;
    -- Move to node
    Move_To (Ctx, Node, Tree);
    -- Add this child to prologue
    Cell.Kind := Xml_Parser.Pi;
    Cell.Nb_Attributes := 0;
    Set_Pi (Cell, Pi);
    Check_Name (Cell.Name.Image);
    Insert_Cell (Tree, Cell, Append_Next);
    -- Done
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => Tree.Get_Position);
  end Add_Pi;

  -- Add a comment in prologue
  procedure Add_Comment (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Comment  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    if not Node.In_Prologue then
      raise Invalid_Node;
    end if;
    -- Move to node
    Move_To (Ctx, Node, Tree);
    -- Add this comment to prologue
    Cell.Kind := Xml_Parser.Comment;
    Cell.Nb_Attributes := 0;
    Cell.Name := As.U.Tus (Comment);
    Insert_Cell (Tree, Cell, Append_Next);
    -- Done
    New_Node := (Kind => Xml_Parser.Comment,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => Tree.Get_Position);
  end Add_Comment;

  -- Clear the whole prologue
  procedure Clear_Prologue (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    Init_Ctx (Ctx);
    -- Delete prologue
    Ctx.Prologue.Move_Root;
    Ctx.Prologue.Delete_Tree;
    -- Clear doctype
    Ctx.Doctype.Line_No := 0;
    Ctx.Doctype.Name.Set_Null;
    -- Init prologue: a xml node
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Xml_Name;
    Cell.Value.Set_Null;
    Ctx.Prologue.Insert_Father (Cell);
  end Clear_Prologue;

  ----------------------
  -- TREE OF ELEMENTS --
  ----------------------

  procedure Move_To_Element (Ctx  : in out Ctx_Type;
                             Node : in Node_Type;
                             Tree : out Tree_Acc) is
  begin
    if Node.In_Prologue then
      raise Invalid_Node;
    end if;
    Move_To (Ctx, Node, Tree);
  end Move_To_Element;

  -- Internal op
  function Internal_Kind_Of (Kind : Node_Kind_List) return Internal_Kind_List is
  begin
    case Kind is
      when Element => return Element;
      when Text    => return Text;
      when Pi      => return Pi;
      when Comment => return Comment;
    end case;
  end Internal_Kind_Of;

  -- Set (change) the name of an element
  -- May raise Invalid_Argument if invalid name
  procedure Set_Name (Ctx     : in out Ctx_Type;
                      Element : in out Element_Type;
                      Name    : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Element, Tree);
    -- Update name
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Name);
    Tree.Replace (Cell);
  end Set_Name;

  -- Add an attribute to current element
  -- May raise No_Element if current element is text
  procedure Add_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
    Nb_Attributes : Natural;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Element, Tree);
    -- Increment Nb_Attributes
    Tree.Read (Cell);
    Nb_Attributes := Cell.Nb_Attributes;
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    Tree.Replace (Cell);
    -- Add this attribute
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := As.U.Tus (Name);
    Cell.Value := As.U.Tus (Value);
    if Nb_Attributes = 0 then
      -- As first child
      Tree.Insert_Child (Cell);
    else
      -- Insert after current attributes
      Tree.Move_Child;
      for I in 1 .. Nb_Attributes - 1 loop
        Tree.Move_Brother (False);
      end loop;
      Tree.Insert_Brother (Cell, False);
    end if;
    Tree.Move_Father;
  end Add_Attribute;

 -- Set all the attributes of an element
  -- May raise Invalid_Argument if a name is invalid
  procedure Set_Attributes (Ctx        : in out Ctx_Type;
                            Element    : in out Element_Type;
                            Attributes : in Attributes_Array) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Delete all attibutes of this element
    Del_Attributes (Ctx, Element);
    -- Move to node
    Move_To_Element (Ctx, Element, Tree);
    -- Set Nb_Attributes
    Tree.Read (Cell);
    Cell.Nb_Attributes := Attributes'Length;
    Tree.Replace (Cell);
    -- Add these attributes
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    for I in reverse Attributes'Range loop
      Cell.Name := Attributes(I).Name;
      Cell.Value := Attributes(I).Value;
      Tree.Insert_Child (Cell);
      Tree.Move_Father;
    end loop;
  end Set_Attributes;

  -- Delete the attributes of an element
  procedure Del_Attributes (Ctx     : in out Ctx_Type;
                            Element : in out Element_Type) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Element, Tree);
    -- Reset Nb_Attributes
    Tree.Read (Cell);
    Cell.Nb_Attributes := 0;
    Tree.Replace (Cell);
    -- Del all attributes
    loop
      -- No more child?
      exit when Tree.Children_Number = 0;
      Tree.Move_Child;
      Tree.Read (Cell);
      if Cell.Kind /= Attribute then
        -- A child element (or comment or text), no more attribute
        Tree.Move_Father;
        exit;
      end if;
      -- Delete this attribute (move up)
      Tree.Delete_Current;
    end loop;
  end Del_Attributes;

  -- Insert a child element, text or comment, and move to it
  procedure Add_Child (Ctx      : in out Ctx_Type;
                       Element  : in Element_Type;
                       Name     : in String;
                       Kind     : in Node_Kind_List;
                       New_Node : out Node_Type;
                       Append   : in Boolean := True) is
    Tree : Tree_Acc;
    Father, Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Element, Tree);
    Tree.Read (Father);
    -- Add this child
    Cell.Kind := Internal_Kind_Of (Kind);
    Cell.Nb_Attributes := 0;
    if Kind = Pi then
      Set_Pi (Cell, Name);
    else
      Cell.Name := As.U.Tus (Name);
    end if;
    if Append or else Father.Nb_Attributes = 0 then
      Tree.Insert_Child (Cell, not Append);
    else
      -- Insert after attributes
      Tree.Move_Child;
      for I in 1 .. Father.Nb_Attributes - 1 loop
        Tree.Move_Brother (False);
      end loop;
      Tree.Insert_Brother (Cell, False);
    end if;
    New_Node := (Kind => Kind,
                 Magic => Ctx.Magic,
                 In_Prologue => False,
                 Tree_Access => Tree.Get_Position);
  end Add_Child;

  -- Insert a brother element, text or comment, and move to it
  procedure Add_Brother (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Name     : in String;
                         Kind     : in Node_Kind_List;
                         New_Node : out Node_Type;
                         Next     : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Node, Tree);
     -- Add this brother
    Cell.Kind := Internal_Kind_Of (Kind);
    Cell.Nb_Attributes := 0;
    if Kind = Pi then
      Set_Pi (Cell, Name);
    else
      Cell.Name := As.U.Tus (Name);
    end if;
    Tree.Insert_Brother (Cell, not Next);
    New_Node := (Kind => Kind,
                 Magic => Ctx.Magic,
                 In_Prologue => False,
                 Tree_Access => Tree.Get_Position);
  end Add_Brother;

  -- Swap two elements (and their children)
  procedure Swap (Ctx  : in out Ctx_Type;
                  Elt1 : in out Element_Type;
                  Elt2 : in out Element_Type) is
    Tree : Tree_Acc;
  begin
    -- Move to node1, must be an element, save pos
    Move_To_Element (Ctx, Elt1, Tree);
    Tree.Save_Position;
    -- Move to node2, must be an element
    Move_To_Element (Ctx, Elt2, Tree);
    -- Swap
    Tree.Swap_Saved;
  exception
    when Trees.Is_Ancestor =>
      raise Invalid_Node;
  end Swap;

  --  Copy Src element as Next (or prev) Child (or brother)
  --  of Dst
  procedure  Copy (Ctx   : in out Ctx_Type;
                  Src   : in out Element_Type;
                  Dst   : in out Element_Type;
                  Child : in Boolean := True;
                  Next  : in Boolean := True) is
    Tree : Tree_Acc;
  begin
    -- Move to Src, must be an element, save pos
    Move_To_Element (Ctx, Src, Tree);
    Tree.Save_Position;
    -- Move to Dst, must be an element
    Move_To_Element (Ctx, Dst, Tree);
    -- Copy Src below or beside Dst
    Tree.Copy_Saved (Child, not Next);
  exception
    when Trees.Is_Ancestor =>
      raise Invalid_Node;
  end Copy;

  -- Set the Put_Empty tag on the element
  procedure Set_Put_Empty (Ctx        : in out Ctx_Type;
                           Element    : in out Element_Type;
                           Put_Empty  : in Boolean) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Element, Tree);
    -- Update name
    Tree.Read (Cell);
    Cell.Put_Empty := Put_Empty;
    Tree.Replace (Cell);
  end Set_Put_Empty;

  -- Set the PITarget and data of a Pi
  -- Content must have the form "<PITarget> [ <spaces> <Pi_Data> ]"
  procedure Set_Pi (Ctx     : in out Ctx_Type;
                    Pi    : in out Pi_Type;
                    Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be a Pi
    Move_To_Element (Ctx, Pi, Tree);
    -- Update Pi
    Tree.Read (Cell);
    Set_Pi (Cell, Content);
    Tree.Replace (Cell);
  end Set_Pi;

  -- Set the text of a Text element
  procedure Set_Text (Ctx     : in out Ctx_Type;
                      Text    : in out Text_Type;
                      Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be a text
    Move_To_Element (Ctx, Text, Tree);
    -- Update Text
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Content);
    Tree.Replace (Cell);
  end Set_Text;

  -- Set the text of a Comment
  procedure Set_Comment (Ctx     : in out Ctx_Type;
                         Comment : in out Comment_Type;
                         Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be a comment
    Move_To_Element (Ctx, Comment, Tree);
    -- Update Text
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Content);
    Tree.Replace (Cell);
  end Set_Comment;

  -- Delete current node and its children
  -- May raise Invalid_Node if being Prologue or Element root
  procedure Delete_Node (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         New_Node : out Node_Type) is
    Tree : Tree_Acc;
  begin
    -- Move to node, check not root
    Move_To (Ctx, Node, Tree);
    if not Tree.Has_Father then
      raise Invalid_Node;
    end if;
    -- Clean doctype info if node is the doctype (text of prologue)
    if Node.In_Prologue and then Node.Kind = Text then
      Ctx.Doctype.Name.Set_Null;
    end if;
    Tree.Delete_Tree;
    -- Father is an element
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 In_Prologue => Node.In_Prologue,
                 Tree_Access => Tree.Get_Position);
  end Delete_Node;


  -- Delete all children of current element
  procedure Delete_Children (Ctx     : in out Ctx_Type;
                             Element : in out Element_Type) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node
    Move_To (Ctx, Element, Tree);
    -- Clean doctype info if node is the prologue
    if Element.In_Prologue and then not Tree.Has_Father then
      Ctx.Doctype.Name.Set_Null;
    end if;
    -- Delete all children
    Tree.Read (Cell);
    for I in 1 .. Tree.Children_Number - Cell.Nb_Attributes loop
      Tree.Move_Child (True);
      Tree.Delete_Tree;
    end loop;
  end Delete_Children;

  --------------------------------------------------------------------------------------
  -- GENERATION --
  ----------------
  type Flow_Dscr (Use_File : Boolean) is record
    case Use_File is
      when True => File : Text_Line.File_Type;
      when False => Us : As.U.Asu_Us;
    end case;
  end record;

  -- Internal procedure to generate the output
  procedure Generate (Ctx       : in Ctx_Type;
                      Format    : in Format_Kind_List;
                      Width     : in Natural;
                      Namespace : in Boolean;
                      Flow      : in out Flow_Dscr);

  -- Put in a file the indented or raw XML flow.
  -- Raises Text_Line exceptions
  procedure Put (Ctx       : in out Ctx_Type;
                 File_Name : in String;
                 Format    : in Format_Kind_List := Default_Format;
                 Width     : in Natural := Default_Width;
                 Namespace : in Boolean := False) is
    Flow : Flow_Dscr(Use_File => True);
    Fd : Sys_Calls.File_Desc;
    use type Sys_Calls.File_Desc;
    procedure Close is
    begin
      if Text_Line.Is_Open (Flow.File) then
        Text_Line.Close (Flow.File);
      end if;
      if Fd /= Sys_Calls.Stdout then
        Sys_Calls.Close (Fd);
      end if;
    exception
      when others => null;
    end Close;
  begin
    -- Open flow
    if File_Name /= Stdout then
      begin
        Fd := Sys_Calls.Create (File_Name);
      exception
        when Sys_Calls.Name_Error =>
          raise File_Error;
      end;
    else
      Fd := Sys_Calls.Stdout;
    end if;
    Text_Line.Open (Flow.File, Text_Line.Out_File, Fd);
    -- Put generated text
    Generate (Ctx, Format, Width, Namespace, Flow);
    -- Close flow
    Close;
  exception
    when others =>
      Close;
      raise;
  end Put;

  -- Dumps in a string then raw XML flow (no CR no space)
  function Set (Ctx       : Ctx_Type;
                Format    : Format_Kind_List := Default_Format;
                Width     : Natural := Default_Width;
                Namespace : Boolean := False) return String is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Width, Namespace, Flow);
    return Flow.Us.Image;
  end Set;

  procedure Set (Ctx       : in Ctx_Type;
                 Str       : out As.U.Asu_Us;
                 Format    : in Format_Kind_List := Default_Format;
                 Width     : in Natural := Default_Width;
                 Namespace : in Boolean := False) is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Width, Namespace, Flow);
    Str := Flow.Us;
  end Set;

  ---------------------
  -- Put the Xml way --
  ---------------------
  -- Internal procedures to put Str or New_Line on the flow
  procedure Put (Flow : in out Flow_Dscr; Str : in String) is
  begin
    if Flow.Use_File then
      Text_Line.Put (Flow.File, Str);
    else
      Flow.Us.Append (Str);
    end if;
  end Put;
  procedure New_Line (Flow : in out Flow_Dscr) is
  begin
    if Flow.Use_File then
      Text_Line.Put (Flow.File, Text_Line.Line_Feed_Str);
    else
      Flow.Us.Append (Text_Line.Line_Feed_Str);
    end if;
  end New_Line;

  -- Replace any sequence of separators by a space
  function Normalize (Str : String) return String is
    Res : As.U.Asu_Us;
    Prev_Is_Space : Boolean := False;
    Char : Character;
  begin
    for I in Str'Range loop
      Char := Str(I);
      if Char = Ada.Characters.Latin_1.Ht
      or else Char = Ada.Characters.Latin_1.Cr
      or else Char = Ada.Characters.Latin_1.Lf
      or else Char = Ada.Characters.Latin_1.Space then
        -- A space
        if not Prev_Is_Space then
          -- Add this new space
          Res.Append (Ada.Characters.Latin_1.Space);
          Prev_Is_Space := True;
        end if;
      else
        -- Not a space
        Res.Append (Char);
        Prev_Is_Space := False;
      end if;
    end loop;
    return Res.Image;
  end Normalize;

  -- Set the name of an element or attribute, with or without namespace
  -- If no Use_Namespace, just copy Name
  -- Else Expand Name and Namespace
  procedure Set_Name (Result : out As.U.Asu_Us;
                      Use_Namespace : in Boolean;
                      Name, Namespace : in As.U.Asu_Us) is
  begin
    if Use_Namespace then
      Result := Expand_Name (Name, Namespace);
    else
      Result := Name;
    end if;
  end Set_Name;

  -- Put the attributes
  procedure Put_Attributes (Flow         : in out Flow_Dscr;
                            Format       : in Format_Kind_List;
                            Width        : in Natural;
                            Namespace    : in Boolean;
                            Attributes   : in Attributes_Array;
                            Level        : in Natural;
                            Offset       : in Positive;
                            Has_Children : in Boolean) is
    Att_Name : As.U.Asu_Us;
    Pad : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    Cur_Col : Natural;
    Att_Width : Positive;
  begin
    -- Put each attribute
    Cur_Col := Pad'Length;
    for I in Attributes'Range loop
      Set_Name (Att_Name, Namespace,
                Attributes(I).Name, Attributes(I).Namespace);
      -- Needed width is ' Name="Value"'
      Att_Width := Att_Name.Length
                 + Attributes(I).Value.Length + 4;
      -- For last attribute, a ">" (if children) or a "/>" will be added
      if I = Attributes'Last then
        if Has_Children then
          Att_Width := Att_Width + 1;
        else
          Att_Width := Att_Width + 2;
        end if;
      end if;
      -- New line and Indent if needed
      -- Never new line for first
      -- New line if One_Per_Line of Fill_Width and no more width
      if I /= Attributes'First
      and then (Format = One_Per_Line
         or else (Format = Fill_Width
                  and then Width /= Infinite_Width
                  and then Cur_Col + Att_Width > Width) ) then
        New_Line (Flow);
        Put (Flow, Pad);
        Cur_Col := Pad'Length;
      end if;
      Put (Flow, " " & Att_Name.Image & "="""
               & Attributes(I).Value.Image & """");
      Cur_Col := Cur_Col + Att_Width;
    end loop;
  end Put_Attributes;

  -- Put the attributes of current element
  -- Move to first child of element if any, otherwise remain on element
  procedure Put_Attributes (Flow         : in out Flow_Dscr;
                            Format       : in Format_Kind_List;
                            Width        : in Natural;
                            Namespace    : in Boolean;
                            Element      : in out My_Tree.Tree_Type;
                            Level        : in Natural;
                            Offset       : in Positive;
                            Has_Children : in Boolean) is
    Cell : My_Tree_Cell;
    Nb_Attributes : Natural;
  begin
    -- Read number of attribtues
    Element.Read (Cell);
    Nb_Attributes := Cell.Nb_Attributes;
    if Nb_Attributes = 0 then
      -- No attribute. Move to first child if any
      if Element.Children_Number /= 0 then
        Element.Move_Child (True);
      end if;
      return;
    end if;
    declare
      Attributes : Attributes_Array (1 .. Nb_Attributes);
    begin
      -- Read each attribute
      for I in 1 .. Nb_Attributes loop
        if I = 1 then
           Element.Move_Child (True);
        else
          Element.Move_Brother (False);
        end if;
        -- Read attribute, needed width is ' Name="Value"'
        Element.Read (Cell);
        Attributes(I).Name := Cell.Name;
        Attributes(I).Namespace := Cell.Namespace;
        Attributes(I).Value := Cell.Value;
      end loop;
      Put_Attributes (Flow, Format, Width, Namespace,
                      Attributes, Level, Offset, Has_Children);
    end;
    -- Move to next brother if any, otherwise move to father
    if Element.Has_Brother (False) then
      Element.Move_Brother (False);
    else
      Element.Move_Father;
    end if;
  end Put_Attributes;

  -- Put a comment at proper indent
  procedure Put_Comment (Flow    : in out Flow_Dscr;
                         Comment : in String) is
  begin
    Put (Flow, "<!--" & Comment & "-->");
  end Put_Comment;

  -- Put a DOCTYPE directive
  procedure Put_Doctype (Flow    : in out Flow_Dscr;
                         Doctype : in Doctype_Type;
                         Format  : in Format_Kind_List) is
  begin
    -- Put DOCTYPE, name
    Put (Flow, "<!DOCTYPE " & Doctype.Name.Image);
    if Doctype.Name.Is_Null then
      raise Internal_Error;
    end if;
    -- Public or System external reference
    if Doctype.Public then
      -- Public and public Id
      Put (Flow, " PUBLIC """ & Doctype.Pub_Id.Image & """");
    elsif not Doctype.File.Is_Null then
      Put (Flow, " SYSTEM");
    end if;
    -- Public or System URI
    if not Doctype.File.Is_Null then
      Put (Flow, " """  & Doctype.File.Image & """");
    end if;
    -- Internal definition
    if not Doctype.Int_Def.Is_Null then
      if Format /= Raw then
        Put (Flow, " [" & Doctype.Int_Def.Image & "]");
      else
        Put (Flow, "[" & Normalize (Doctype.Int_Def.Image) & "]");
      end if;
    end if;
    Put (Flow, ">");
  end Put_Doctype;

  -- Put an element (and its attributes and children)
  Prologue_Level : constant := -1;
  procedure Put_Element (Flow      : in out Flow_Dscr;
                         Format    : in Format_Kind_List;
                         Width     : in Natural;
                         Namespace : in Boolean;
                         Ctx       : in Ctx_Type;
                         Element   : in out My_Tree.Tree_Type;
                         Level     : in Integer;
                         In_Mixed  : in Boolean) is
    Cell : constant My_Tree_Cell := Element.Read;
    Cell_Ref : constant My_Tree.Position_Access := Element.Get_Position;
    Nb_Children : constant Trees.Child_Range := Element.Children_Number;
    Child : My_Tree_Cell;
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Indent1 : constant String := Indent & "  ";
    Is_Mixed : constant Boolean := Cell.Is_Mixed;
    Elt_Name : As.U.Asu_Us;
    Xml_Attr_Format : Format_Kind_List;
    In_Tail : Boolean;
    Closed : Boolean := False;
    -- Terminate tag after children
    procedure Close (Force_No_Indent : in Boolean := False) is
    begin
      if not In_Tail and then not Closed then
        if not Is_Mixed and then Format /= Raw and then not Force_No_Indent then
          -- Indent the end of this non-mixed element
          Put (Flow, Indent);
        end if;
        Put (Flow, "</" & Elt_Name.Image & ">");
        if not In_Mixed and then Format /= Raw then
          -- End of this element not in mixed
          New_Line (Flow);
        end if;
      end if;
      Closed := True;
    end Close;
    use type My_Tree.Position_Access;
  begin
    if Level = Prologue_Level then
      -- A prologue
      -- The Xml directive:  Put it if it has attributes
      if Cell.Nb_Attributes = 0 then
        -- No Xml Directive: move to first child as if after putting attributes
        if Element.Children_Number /= 0 then
          Element.Move_Child (True);
        end if;
      else
        -- The Xml directive
        -- Even if one attr per line request, Xml directive attributes
        --  are all on the same line
        if Format = One_Per_Line then
          Xml_Attr_Format := Fill_Width;
        else
          Xml_Attr_Format := Format;
        end if;
        -- Put the xml directive with attributes if any
        Put (Flow, "<?" & Cell.Name.Image);
        Put_Attributes (Flow, Xml_Attr_Format, Width, Namespace, Element, 0,
                        2 + Cell.Name.Length, False);
        Put (Flow, "?>");
        if Format /= Raw then
          New_Line (Flow);
        end if;
      end if;
      -- Any child of prologue?
      if Element.Get_Position = Cell_Ref then
        -- No Child (Put_Attributes moved back to current): return
        if Format /= Raw and then Cell.Nb_Attributes /= 0 then
          -- Skip one line between prologue and root if there is
          --  a xml directive
          New_Line (Flow);
        end if;
        return;
      end if;

      -- Put prologue children: DOCTYPE, PIs and comments
      -- Put_Attributes remained on first child
      loop
        Child := Element.Read;
        case Child.Kind is
          when Attribute =>
            -- Impossibe
            raise Internal_Error;
          when Xml_Parser.Element =>
            -- No child element of prologue
            raise Constraint_Error;
          when Xml_Parser.Pi =>
            -- Put xml or PI
            Put (Flow, "<?" & Child.Name.Image);
            if not Child.Value.Is_Null then
              Put (Flow, " " & Child.Value.Image);
            end if;
            Put (Flow, "?>");
          when Text =>
            Put_Doctype (Flow, Ctx.Doctype, Format);
          when Comment =>
            Put_Comment (Flow, Child.Name.Image);
        end case;
        if Format /= Raw then
          New_Line (Flow);
        end if;
        -- Next child or done
        exit when not Element.Has_Brother (False);
        Element.Move_Brother (False);
      end loop;
      -- End of prologue and its children
      Element.Move_Father;
      if Format /= Raw then
        -- Skip one line between prologue and root
        New_Line (Flow);
      end if;
      return;
    end if;

    -- In Elements or in tail
    In_Tail := Level = 1 and then Cell.Name.Is_Null;

    if not In_Tail then
      -- Put element, attributes and children recursively
      if not In_Mixed and then Format /= Raw then
        -- Indent before the start of this element
        Put (Flow, Indent);
      end if;
      Set_Name (Elt_Name, Namespace, Cell.Name, Cell.Namespace);
      Put (Flow, "<" & Elt_Name.Image);
    end if;

    -- Put attributes and move to first child (if any)
    Put_Attributes (Flow, Format, Width, Namespace, Element, Level,
                    1 + Cell.Name.Length,
                    Has_Children => Nb_Children > Cell.Nb_Attributes);

    -- Any child?
    if Element.Get_Position = Cell_Ref then
      -- No Child (Put_Attributes moved back to current): return
      if not In_Tail then
        if Cell.Put_Empty then
          -- EmptyElementTag now
          Put (Flow, "/>");
          if not In_Mixed and then Format /= Raw then
            -- New line after the end of this element
            New_Line (Flow);
          end if;
        else
          -- Finish STag now and close (add ETag)
          Put (Flow, ">");
          if Is_Mixed then
            -- No indentation between STag and ETag
            Close (Force_No_Indent => True);
          else
            New_Line (Flow);
            Close (Force_No_Indent => False);
          end if;
        end if;
      end if;
      -- Terminate now
      return;
    else
      -- End Father for child or not (just a tail)
      Child := Element.Read;
      if not In_Tail then
        if not Child.Name.Is_Null then
          -- There is a significant child
          Put (Flow, ">");
          if not Is_Mixed and then Format /= Raw then
            -- New line after the start of this non-mixed element
            New_Line (Flow);
          end if;
        else
          -- Closing root with a tail
          Put (Flow, "/>");
          if Format /= Raw then
            -- New line after the end of root and before tail
            New_Line (Flow);
          end if;
          Closed := True;
        end if;
      end if;
    end if;

    -- Put children
    for I in 1 .. Nb_Children loop
      Child := Element.Read;
      case Child.Kind is
        when Attribute =>
          -- Impossibe
          raise Internal_Error;
        when Xml_Parser.Element =>
          if In_Tail then
            -- Impossibe
            raise Internal_Error;
          end if;
          if Level = 0 and then Child.Name.Is_Null then
            -- Start of Tail
            Close;
          end if;
          -- Recursive dump child
          Put_Element (Flow, Format, Width, Namespace, Ctx, Element,
                         Level + 1, Cell.Is_Mixed and then not In_Tail);
        when Text =>
          if In_Tail then
            -- Impossibe
            raise Internal_Error;
          end if;
          -- Specific put text
          Put (Flow, Child.Name.Image);
          if not Is_Mixed and then Format /= Raw then
            -- New line after the end of this text
            New_Line (Flow);
          end if;
        when Pi =>
          -- Put PI
          if not In_Tail and then not Is_Mixed and then Format /= Raw then
            Put (Flow, Indent1);
          end if;
          Put (Flow, "<?" & Child.Name.Image);
          if not Child.Value.Is_Null then
            Put (Flow, " " & Child.Value.Image);
          end if;
          Put (Flow, "?>");
          if not Is_Mixed and then Format /= Raw then
            -- New line after the end of this PI
            New_Line (Flow);
          end if;
        when Comment =>
          -- Comment
          if not In_Tail and then not Is_Mixed and then Format /= Raw then
            Put (Flow, Indent1);
          end if;
          Put_Comment (Flow, Child.Name.Image);
          if not Is_Mixed and then Format /= Raw then
            -- New line after the end of this comment
            New_Line (Flow);
          end if;
        end case;
        -- Next child or done
        exit when not Element.Has_Brother (False);
        Element.Move_Brother (False);
      end loop;
      Close;
      -- End of this element
      Element.Move_Father;
  end Put_Element;

  -- Internal procedure to generate the output
  procedure Generate (Ctx       : in Ctx_Type;
                      Format    : in Format_Kind_List;
                      Width     : in Natural;
                      Namespace : in Boolean;
                      Flow      : in out Flow_Dscr) is
  begin
    if Ctx.Status /= Init and then Ctx.Status /= Parsed_Elements then
      raise Status_Error;
    end if;
    -- Put prologue if any
    Ctx.Prologue.Move_Root;
    Put_Element (Flow, Format, Width, Namespace, Ctx, Ctx.Prologue.all,
                 Prologue_Level, False);
    -- Put Elements
    Ctx.Elements.Move_Root;
    Put_Element (Flow, Format, Width, Namespace, Ctx, Ctx.Elements.all,
                 0, False);
    if Format /= Raw then
      New_Line (Flow);
    end if;
  end Generate;

  -- Put a node update image in a string
  function Image (Ctx       : Xml_Parser.Ctx_Type;
                  Update    : Node_Update;
                  Format    : Format_Kind_List := Default_Format;
                  Width     : Natural := Default_Width;
                  Namespace : Boolean := False) return String is
    Elt_Name : As.U.Asu_Us;
    Flow : Flow_Dscr(Use_File => False);
    Indent : constant String (1 .. 2 * Update.Level) := (others => ' ');
    Xml_Attr_Format : Format_Kind_List;
  begin
    if Update.Stage = Prologue then
      if Update.Level = 0 then
        -- The XML directive
        -- Put the xml directive if it has attributes
        if Update.Attributes /= null
        and then Update.Attributes'Length /= 0 then
          -- Even if one attr per line request, Xml directive attributes
          --  are all on the same line
          if Format = One_Per_Line then
            Xml_Attr_Format := Fill_Width;
          else
            Xml_Attr_Format := Format;
          end if;
          Put (Flow, "<?" & Update.Name.Image);
          Put_Attributes (Flow, Xml_Attr_Format, Width, Namespace,
                          Update.Attributes.all, 0, 2 + Update.Name.Length,
                          False);
          Put (Flow, "?>");
          if Format /= Raw then
            New_Line (Flow);
          end if;
        end if;
      else
        -- A child of prologue (PI, comment or doctype)
        case Update.Kind is
          when Xml_Parser.Element =>
            -- Only one element in the prologue => the xml
            raise Constraint_Error;
          when Xml_Parser.Pi =>
            -- Put PI
            Put (Flow, "<?" & Update.Name.Image);
            if not Update.Value.Is_Null then
              Put (Flow, " " & Update.Value.Image);
            end if;
            Put (Flow, "?>");
          when Text =>
            Put_Doctype (Flow, Ctx.Doctype, Format);
          when Comment =>
            Put_Comment (Flow, Update.Name.Image);
        end case;
        if Format /= Raw then
          New_Line (Flow);
        end if;
      end if;
      return Flow.Us.Image;
    end if;

    -- In elements tree
    if Format /= Raw
    and then Update.Stage = Elements
    and then Update.Creation
    and then Update.Level = 0 then
      -- Creation of root, separate from prologue
      New_Line (Flow);
    end if;

    if Format /= Raw then
      if Update.Creation
      and then not Update.In_Mixed
      and then Update.Kind /= Xml_Parser.Text
      and then (Update.Kind /= Xml_Parser.Element
              or else not Update.Name.Is_Null) then
        -- Creation of a new node: Indent if not within mixed, if not text
        --  and if not at beginning of tail
        Put (Flow, Indent);
     elsif not Update.Creation and then not Update.Is_Mixed then
       -- Closure of an element: Indent if not mixed
        Put (Flow, Indent);
      end if;
    end if;
    case Update.Kind is
      when Xml_Parser.Element =>
        Set_Name (Elt_Name, Namespace, Update.Name, Update.Namespace);
        if Update.Creation and then not Update.Name.Is_Null then
          -- Put element and attributes
          Put (Flow, "<" & Elt_Name.Image);
          if Update.Attributes /= null then
            Put_Attributes (Flow, Format, Width, Namespace,
                            Update.Attributes.all, Update.Level,
                            1 + Update.Name.Length, Update.Has_Children);
          end if;
          -- Any child?
          if not Update.Has_Children then
            -- No child, terminate tag now
            Put (Flow, "/>");
          else
            -- Children to come: New_Line id not mixed and done
            Put (Flow, ">");
            if Format /= Raw and then not Update.Is_Mixed then
              New_Line (Flow);
            end if;
            return Flow.Us.Image;
          end if;
        elsif not Update.Name.Is_Null then
          -- End of element with children
          Put (Flow, "</" & Elt_Name.Image & ">");
        end if;
      when Text =>
        -- Put text
        Put (Flow, Update.Name.Image);
      when Pi =>
        -- Put PI
        Put (Flow, "<?" & Update.Name.Image);
        if not Update.Value.Is_Null then
          Put (Flow, " " & Update.Value.Image);
        end if;
        Put (Flow, "?>");
      when Comment =>
        -- Comment
        Put_Comment (Flow, Update.Name.Image);
    end case;
    if Format /= Raw and then not Update.In_Mixed then
      -- End of element
      New_Line (Flow);
    end if;
    return Flow.Us.Image;
  end Image;

  procedure Set_Image (Ctx    : in Xml_Parser.Ctx_Type;
                       Update : in Node_Update;
                       Str    : out As.U.Asu_Us;
                       Format : in Format_Kind_List := Default_Format;
                       Width  : in Natural := Default_Width;
                       Namespace : in Boolean := False) is
  begin
    Str := As.U.Tus (Image (Ctx, Update, Format, Width, Namespace));
  end Set_Image;

end Xml_Parser.Generator;

