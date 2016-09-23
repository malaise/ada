-- Generates a Xml file (or stdout), or string from a tree
with Aski, Images, Text_Line, Sys_Calls, Str_Util;
package body Xml_Parser.Generator is

  -- Version incremented at each significant change
  Minor_Version : constant String := "2";
  function Version return String is
  begin
    return "V" & Major_Version & "." & Minor_Version;
  end Version;

  -- Common delimiter definitions
  Cdata_Start : constant String := "<![CDATA[";
  Cdata_Stop  : constant String := "]]>";
  Start_Tag   : constant String := "<";
  Ampersand   : constant String := "&";
  Quotation   : constant String := """";
  Apostrophe  : constant String := "'";

  -- Process pure text (outside CDATA)
  procedure Process_Text (
      Text : in out As.U.Asu_Us;
      Process : access procedure (Text : in out As.U.Asu_Us)) is
    Start, Stop : Natural;
    Slice : As.U.Asu_Us;
    Len : Natural;
  begin
    -- Look for "<![CDATA[ ... ]]>"
    Stop := 0;
    loop
      Start := Str_Util.Locate (Text.Image, Cdata_Start, Stop + 1);
      if Start = 0 then
        -- Check CharData after last CData section (the CharData if no Cdata)
        Slice := Text.Uslice (Stop + 1, Text.Length);
        Process (Slice);
        Text.Replace (Stop + 1, Text.Length, Slice.Image);
        exit;
      end if;
      -- Check CharData between CData sections
      Slice := Text.Uslice (Stop + 1, Start - 1);
      Len := Slice.Length;
      Process (Slice);
      Text.Replace (Stop + 1, Start - 1, Slice.Image);
      Start := Start + Slice.Length - Len;
      Stop := Str_Util.Locate (Text.Image, Cdata_Stop, Start + 1);
      if Stop = 0 then
        -- Unterminated Cdata section
        raise Invalid_Argument;
      end if;
    end loop;
  end Process_Text;

  -- Check validity of Name
  procedure Check_Name (Name : in String) is
  begin
    if not Name_Ok (As.U.Tus (Name)) then
      raise Invalid_Argument;
    end if;
  end Check_Name;

  -- Check validity of Num (deci or hexa)
  procedure Check_Num (Num : in String) is
  begin
    if Num(Num'First) = 'x' then
      -- Hexa num
      if Num'Length = 1 then
        raise Invalid_Argument;
      end if;
      for I in Positive'Succ (Num'First) .. Num'Last loop
        if Num(I) not in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' then
          raise Invalid_Argument;
        end if;
      end loop;
    else
      -- Deci num
      for N of Num loop
        if N not in '0' .. '9' then
          raise Invalid_Argument;
        end if;
      end loop;
    end if;
  end Check_Num;

  -- Check a reference (name or num)
  procedure Check_Ref (Ref : in String) is
  begin
    if Ref(Ref'First) = '#' then
      Check_Num (Ref(Positive'Succ(Ref'First) .. Ref'Last));
    else
      Check_Name (Ref);
    end if;
  end Check_Ref;

  -- Locate and check references
  procedure Check_Refs (Text : in String) is
    Start, Stop : Natural;
  begin
    -- Valid reference to Entity (&Name;) or char (&#Num;)
    -- Num in decimal ([0-9]+)or hexa ([0-9a-fA-F]+)
    Stop := Text'First - 1;
    loop
      -- Locate next "&...;"
      Start := Str_Util.Locate (Text, Ampersand, Stop + 1);
      exit when Start = 0;
      Stop := Str_Util.Locate (Text, ";", Start + 1);
      if Stop = 0 or else Stop = Start + 1 then
        -- Unterminated or empty reference
        raise Invalid_Argument;
      end if;
      Check_Ref (Text(Start + 1 .. Stop - 1));
    end loop;
  end Check_Refs;

  -- Check validity of Text (including CData and references)
  procedure Check_Text (Txt : in String) is
    -- Find CDATA sections, and between them:
    -- - check that there is no Start_Tag nor Cdata_Stop sequence
    -- - check references (name or num)

    -- Check a CharData block (between CData sections)
    procedure Check_Chardata (Text : in out As.U.Asu_Us) is
    begin
      if Str_Util.Locate (Text.Image, Start_Tag) /= 0 then
        raise Invalid_Argument;
      end if;
      if Str_Util.Locate (Text.Image, Cdata_Stop) /= 0 then
        raise Invalid_Argument;
      end if;
      Check_Refs (Text.Image);
    end Check_Chardata;

    Text : As.U.Asu_Us := As.U.Tus (Txt);
  begin
    Process_Text (Text, Check_Chardata'Access);
  end Check_Text;

  -- Detect separator
  function Is_Separator (Char : Character) return Boolean is
  begin
    return  Char = Aski.Spc
    or else Char = Aski.Lf
    or else Char = Aski.Cr
    or else Char = Aski.Ht;
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
      -- Init tail: an empty root element
      Cell.Name.Set_Null;
      Ctx.Tail.Insert_Father (Cell);
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
    Vers.Value := As.U.Tus (Images.Integer_Image (Major)
                          & "." & Images.Integer_Image (Minor));

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
    if not Is_Valid_Encoding (As.U.Tus (Encoding)) then
      raise Invalid_Argument;
    end if;
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
    Ctx.Prologue.Replace (Cell);
    -- Delete all attributes
    if Ctx.Prologue.Children_Number /= 0 then
      loop
        Ctx.Prologue.Move_Child (True);
        Ctx.Prologue.Read (Cell);
        exit when Cell.Kind /= Attribute;
        Ctx.Prologue.Delete_Current;
      end loop;
    end if;
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
    Check_Name (Name);
    if not Is_Valid_Pubid (As.U.Tus (Pub_Id)) then
      raise Invalid_Argument;
    end if;
    if Str_Util.Locate (File, "'") /= 0
    and then Str_Util.Locate (File, """") /= 0 then
      raise Invalid_Argument;
    end if;
    if not Public and then Pub_Id /= "" then
      raise Invalid_Argument;
    end if;
    if Node.Branch /= Prologue_Br then
      raise Invalid_Node;
    end if;
    if not Ctx.Doctype.Name.Is_Null then
      raise Doctype_Already_Set;
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
                 Branch => Prologue_Br,
                 Tree_Access => Tree.Get_Position);
  end Add_Doctype;

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
   -- Checks
   if not Name_Ok (Cell.Name) then
     raise Invalid_Argument;
   end if;
   if Str_Util.Locate (Cell.Value.Image, "?>") /= 0 then
     raise Invalid_Argument;
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
    if Node.Branch /= Prologue_Br then
      raise Invalid_Node;
    end if;
    -- Add this child to prologue
    Cell.Kind := Xml_Parser.Pi;
    Cell.Nb_Attributes := 0;
    Set_Pi (Cell, Pi);
    -- Move to node
    Move_To (Ctx, Node, Tree);
    Insert_Cell (Tree, Cell, Append_Next);
    -- Done
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 Branch => Prologue_Br,
                 Tree_Access => Tree.Get_Position);
  end Add_Pi;

  -- Internal: Check validity of a comment
  procedure Check_Comment (Comment  : in String) is
  begin
    if Str_Util.Locate (Comment, "--") /= 0 then
      raise Invalid_Argument;
    end if;
  end Check_Comment;

  -- Add a comment in prologue
  procedure Add_Comment (Ctx      : in out Ctx_Type;
                         Node     : in Node_Type;
                         Comment  : in String;
                         New_Node : out Node_Type;
                         Append_Next : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    Check_Comment (Comment);
    if Node.Branch /= Prologue_Br then
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
                 Branch => Prologue_Br,
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

  ------------------------------
  -- TREE OF ELEMENTS or TAIL --
  ------------------------------
  -- Move to a node in Elements or Tail
  procedure Move_To_Node_Elementail (Ctx  : in out Ctx_Type;
                                     Node : in Node_Type;
                                     Tree : out Tree_Acc) is
  begin
    if Node.Branch = Prologue_Br then
      raise Invalid_Node;
    end if;
    Move_To (Ctx, Node, Tree);
  end Move_To_Node_Elementail;

  -- INTERNAL op
  function Internal_Kind_Of (Kind : Node_Kind_List) return Internal_Kind_List is
  begin
    return (case Kind is
              when Element => Element,
              when Text    => Text,
              when Pi      => Pi,
              when Comment => Comment);
  end Internal_Kind_Of;

  -- INTERNAL: Set kind and name of a cell; Check
  procedure Set (Cell : in out My_Tree_Cell;
                 Kind : in Node_Kind_List;
                 Name : in String) is
  begin
    Cell.Kind := Internal_Kind_Of (Kind);
    Cell.Nb_Attributes := 0;
    -- Default for all but Pi. Set_Pi will overwrite
    Cell.Name := As.U.Tus (Name);
    case Kind is
      when Element =>
        Check_Name (Name);
      when Text =>
        Check_Text (Name);
      when Pi =>
        Set_Pi (Cell, Name);
      when Comment =>
        Check_Comment (Name);
    end case;
  end Set;

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
    -- Set the Cell, check name
    Set (Cell, Kind, Name);
    -- Move to Element, must be in Elements or Tail
    Move_To_Node_Elementail (Ctx, Element, Tree);
    Tree.Read (Father);
    -- Forbid inserting Element or Text in tail
    if Element.Branch = Tail_Br
    and then (Kind = Xml_Parser.Element or else Kind = Xml_Parser.Text) then
      raise Invalid_Node;
    end if;
    -- Insert
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
                 Branch => Element.Branch,
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
    Brother, Cell : My_Tree_Cell;
  begin
    -- Set the Cell
    Set (Cell, Kind, Name);
    -- Move to Node, must be in Elements or Tail
    Move_To_Node_Elementail (Ctx, Node, Tree);
    Tree.Read (Brother);
    -- Forbid inserting Element or Text in tail
    if Node.Branch = Tail_Br
    and then (Kind = Element or else Kind = Text) then
      raise Invalid_Node;
    end if;
    -- Forbid inserting a brother to root (of elements or tail)
    if not Tree.Has_Father then
      raise Invalid_Node;
    end if;
    -- Insert
    Tree.Insert_Brother (Cell, not Next);
    New_Node := (Kind => Kind,
                 Magic => Ctx.Magic,
                 Branch => Node.Branch,
                 Tree_Access => Tree.Get_Position);
  end Add_Brother;

  -- Set the PITarget and data of a Pi
  -- Content must have the form "<PITarget> [ <spaces> <Pi_Data> ]"
  procedure Set_Pi (Ctx     : in out Ctx_Type;
                    Pi    : in out Pi_Type;
                    Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Check Pi info
    Set_Pi (Cell, Content);
    -- Move to node, must be in Elements or Tail
    Move_To_Node_Elementail (Ctx, Pi, Tree);
    -- Update Pi
    Tree.Read (Cell);
    Set_Pi (Cell, Content);
    Tree.Replace (Cell);
  end Set_Pi;

  -- Set the text of a Comment
  procedure Set_Comment (Ctx     : in out Ctx_Type;
                         Comment : in out Comment_Type;
                         Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    Check_Comment (Content);
    -- Move to node, must be in Elements or Tail
    Move_To_Node_Elementail (Ctx, Comment, Tree);
    -- Update Text
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Content);
    Tree.Replace (Cell);
  end Set_Comment;

  ----------------------
  -- TREE OF ELEMENTS --
  ----------------------
  -- Move to a node in Elements
  procedure Move_To_Node_Element (Ctx  : in out Ctx_Type;
                                  Node : in Node_Type;
                                  Tree : out Tree_Acc) is
  begin
    if Node.Branch /= Elements_Br then
      raise Invalid_Node;
    end if;
    Move_To (Ctx, Node, Tree);
  end Move_To_Node_Element;

  -- Set (change) the name of an element
  -- May raise Invalid_Argument if invalid name
  procedure Set_Name (Ctx     : in out Ctx_Type;
                      Element : in out Element_Type;
                      Name    : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    Check_Name (Name);
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    -- Update name
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Name);
    Tree.Replace (Cell);
  end Set_Name;

  -- INTERNAL: Check validity of an attribute value
  -- Any char except '<'
  -- No mix of '"' and '''
  -- Valid references
  procedure Check_Attribute (Val : in String) is
  begin
    if Val = "" then
      return;
    end if;
    if Str_Util.Locate (Val, "<") /= 0 then
      raise Invalid_Argument;
    end if;
    if Str_Util.Locate (Val, "'") /= 0
    and then Str_Util.Locate (Val, """") /= 0 then
      raise Invalid_Argument;
    end if;
    Check_Refs (Val);
  end Check_Attribute;

 -- Set all the attributes of an element
  -- May raise Invalid_Argument if a name is invalid
  procedure Set_Attributes (Ctx        : in out Ctx_Type;
                            Element    : in out Element_Type;
                            Attributes : in Attributes_Array) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Check input attributes
    for Attr of Attributes loop
      Check_Name (Attr.Name.Image);
      Check_Attribute (Attr.Name.Image);
    end loop;
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    -- Delete all attibutes of this element
    Del_Attributes (Ctx, Element);
    -- Set Nb_Attributes
    Tree.Read (Cell);
    Cell.Nb_Attributes := Attributes'Length;
    Tree.Replace (Cell);
    -- Add these attributes
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    for Attr of reverse Attributes loop
      Check_Name (Attr.Name.Image);
      Cell.Name := Attr.Name;
      Cell.Value := Attr.Value;
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
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    -- Reset Nb_Attributes
    Tree.Read (Cell);
    Cell.Nb_Attributes := 0;
    Tree.Replace (Cell);
    -- Del all attributes
    -- As lng as child
    while Tree.Children_Number /= 0 loop
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

  -- Add an attribute to current element
  -- May raise No_Element if current element is text
  procedure Add_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
    Nb_Attributes : Natural;
  begin
    Check_Name (Name);
    Check_Attribute (Value);
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
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
  end Add_Attribute;

 -- Set the value of an attribute of current element
  -- May raise Invalid_Argument if a name is invalid
  -- May raise May raise Attribute_Not_Found
  procedure Set_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name, Value : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
    use type As.U.Asu_Us;
  begin
    Check_Name (Name);
    Check_Attribute (Value);
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    Tree.Read (Cell);
    -- Look for attribute with this Name
    for I in 1 .. Cell.Nb_Attributes loop
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      Tree.Read (Cell);
      if Cell.Kind /= Attribute then
        raise Internal_Error;
      end if;
      if Cell.Name = As.U.Tus (Name) then
        Cell.Value := As.U.Tus (Value);
        Tree.Replace (Cell);
        return;
      end if;
    end loop;
    raise Attribute_Not_Found;
  end Set_Attribute;

 -- Delete an attribute of current element
  -- May raise Invalid_Argument if a name is invalid
  -- May raise May raise Attribute_Not_Found
  procedure Del_Attribute (Ctx     : in out Ctx_Type;
                           Element : in out Element_Type;
                           Name : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
    use type As.U.Asu_Us;
  begin
    Check_Name (Name);
    -- Move to Element, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    Tree.Read (Cell);
    -- Look for attribute with this Name
    for I in 1 .. Cell.Nb_Attributes loop
      if I = 1 then
        Tree.Move_Child;
      else
        Tree.Move_Brother (False);
      end if;
      Tree.Read (Cell);
      if Cell.Kind /= Attribute then
        raise Internal_Error;
      end if;
      if Cell.Name = As.U.Tus (Name) then
        Tree.Delete_Current;
        Tree.Read (Cell);
        Cell.Nb_Attributes := Cell.Nb_Attributes - 1;
        Tree.Replace (Cell);
        return;
      end if;
    end loop;
    raise Attribute_Not_Found;
  end Del_Attribute;

  -- Swap two elements (and their children)
  procedure Swap (Ctx  : in out Ctx_Type;
                  Elt1 : in out Element_Type;
                  Elt2 : in out Element_Type) is
    Tree : Tree_Acc;
  begin
    if Elt1.Branch /= Elements_Br
    or else Elt1.Branch /= Elements_Br then
      raise Invalid_Node;
    end if;
    -- Move to node1, must be in Elements, save pos
    Move_To_Node_Element (Ctx, Elt1, Tree);
    Tree.Save_Position;
    -- Move to node2, must be in Elements
    Move_To_Node_Element (Ctx, Elt2, Tree);
    -- Swap
    Tree.Swap_Saved;
  exception
    when Trees.Is_Ancestor =>
      raise Invalid_Node;
  end Swap;

  --  Copy Src element as Next (or prev) Child (or brother)
  --  of Dst
  procedure Copy (Ctx      : in out Ctx_Type;
                  Src      : in Element_Type;
                  Dst      : in Element_Type;
                  New_Node : out Node_Type;
                  Child    : in Boolean := True;
                  Next     : in Boolean := True) is
    Tree : Tree_Acc;
  begin
    -- Move to Src, must be in Elements, save pos
    Move_To_Node_Element (Ctx, Src, Tree);
    Tree.Save_Position;
    -- Move to Dst, must be in Elements
    Move_To_Node_Element (Ctx, Dst, Tree);
    -- Copy Src below or beside Dst
    Tree.Copy_Saved (Child, not Next);
    New_Node := (Kind => Src.Kind,
                 Magic => Ctx.Magic,
                 Branch => Elements_Br,
                 Tree_Access => Tree.Get_Position);
  exception
    when Trees.Is_Ancestor =>
      raise Invalid_Node;
  end Copy;

  -- Set the Empty_Info of the element to Tag_Empty
  procedure Set_Tag_Empty (Ctx        : in out Ctx_Type;
                           Element    : in out Element_Type;
                           Tag_Empty  : in Boolean) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    if Element.Branch /= Elements_Br then
      raise Invalid_Node;
    end if;
    -- Move to node, must be in Elements
    Move_To_Node_Element (Ctx, Element, Tree);
    -- Update name
    Tree.Read (Cell);
    Cell.Empty_Info := (if Tag_Empty then Xml_Parser.Tag_Empty else Not_Empty);
    Tree.Replace (Cell);
  end Set_Tag_Empty;

  -- Set the text of a Text element
  procedure Set_Text (Ctx     : in out Ctx_Type;
                      Text    : in out Text_Type;
                      Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    Check_Text (Content);
    -- Move to node, must be in Elements
    Move_To_Node_Element (Ctx, Text, Tree);
    -- Update Text
    Tree.Read (Cell);
    Cell.Name := As.U.Tus (Content);
    Tree.Replace (Cell);
  end Set_Text;

  -----------------------
  -- COMMON OPERATIONS --
  -----------------------
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
    if Node.Branch = Prologue_Br and then Node.Kind = Text then
      Ctx.Doctype.Name.Set_Null;
    end if;
    Tree.Delete_Tree;
    -- Father is an element
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 Branch => Node.Branch,
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
    if Element.Branch = Prologue_Br and then not Tree.Has_Father then
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
  -- CONVERSIONS --
  -----------------

  -- Local: Convert pure text (no CDATA) to XML and reverse
  procedure Text2Xml (Text : in out As.U.Asu_Us) is
  begin
    Text := As.U.Tus (Str_Util.Substit (Text.Image, Ampersand, "&amp;"));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, Start_Tag, "&lt;"));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, Cdata_Stop, "]]&gt;"));
  end Text2Xml;
  procedure Xml2Text (Text : in out As.U.Asu_Us) is
  begin
    -- Convert ampersand at the end
    Text := As.U.Tus (Str_Util.Substit (Text.Image, "]]&gt;", Cdata_Stop));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, "&lt;", Start_Tag));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, "&amp;", Ampersand));
  end Xml2Text;

  -- Convert a Text for XML
  -- Outside CDATA sections, replace "&" by "&amp;", "<" by "&lt;"
  --  and "]]>" by "]]&gt;"
  function Text2Xml (Str : String) return String is
    -- Find CDATA sections, and between them, replace
    Text : As.U.Asu_Us := As.U.Tus (Str);
  begin
    Process_Text (Text, Text2Xml'Access);
    return Text.Image;
  end Text2Xml;

  -- Convert Text from Xml
  -- Outside CDATA sections, replace "&amp;" by "&", "&lt;" by "<"
  --  and "&gt;" by ">"
  function Xml2Text (Str : String) return String is
    -- Find CDATA sections, and between them, replace
    Text : As.U.Asu_Us := As.U.Tus (Str);
  begin
    Process_Text (Text, Xml2Text'Access);
    return Text.Image;
  end Xml2Text;

  -- Convert an attribute value for Xml
  -- Replace any "'" by "&apos;" and any """" by "&quot;"
  function Attr2Xml (Str : String) return String is
    Text : As.U.Asu_Us := As.U.Tus (Str);
  begin
    Text := As.U.Tus (Str_Util.Substit (Text.Image, Quotation, "&quot;"));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, Apostrophe, "&apos;"));
    return Text.Image;
  end Attr2Xml;

  -- Convert an attribute value from Xml
  -- Replace any "&apos;" by "'" and any "&quot;" by """"
  function Xml2Attr (Str : String) return String is
    Text : As.U.Asu_Us := As.U.Tus (Str);
  begin
    Text := As.U.Tus (Str_Util.Substit (Text.Image, "&quot;", Quotation));
    Text := As.U.Tus (Str_Util.Substit (Text.Image, "&apos;", Apostrophe));
    return Text.Image;
  end Xml2Attr;

  -- Convert the content of a context, after it has been checked with expand
  --  so that it can be put/set as a valid Xml flow:
  -- Apply Text2Xml to each Text node and Attr2Xml to each attribute value.
  -- Iterator
  function To_Xml (Cell  : in out My_Tree_Cell;
                   Dummy_Level : Natural) return Trees.Iteration_Policy is
  begin
    case Cell.Kind is
      when Text =>
        Cell.Name := As.U.Tus (Text2Xml (Cell.Name.Image));
      when Attribute =>
        Cell.Value := As.U.Tus (Attr2Xml (Cell.Value.Image));
      when others =>
        null;
    end case;
    return Trees.Go_On;
  end To_Xml;

  procedure Tree2Xml (Ctx : in out Ctx_Type) is
  begin
    Init_Ctx (Ctx);
    Ctx.Elements.Iterate (To_Xml'Access);
  end Tree2Xml;

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
                      Format    : in Format_Definition;
                      Namespace : in Boolean;
                      Flow      : in out Flow_Dscr);

  -- Put in a file the indented or raw XML flow.
  -- Raises Text_Line exceptions
  procedure Put (Ctx       : in out Ctx_Type;
                 File_Name : in String;
                 Format    : in Format_Definition := Default_Format;
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
    Generate (Ctx, Format, Namespace, Flow);
    -- Close flow
    Close;
  exception
    when others =>
      Close;
      raise;
  end Put;

  -- Dumps in a string then raw XML flow (no CR no space)
  function Put (Ctx       : Ctx_Type;
                Format    : Format_Definition := Default_Format;
                Namespace : Boolean := False) return String is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Namespace, Flow);
    return Flow.Us.Image;
  end Put;

  procedure Put (Ctx       : in Ctx_Type;
                 Str       : out As.U.Asu_Us;
                 Format    : in Format_Definition := Default_Format;
                 Namespace : in Boolean := False) is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Namespace, Flow);
    Str := Flow.Us;
  end Put;

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
  begin
    for Char of Str loop
      if Is_Separator (Char) then
        -- A space
        if not Prev_Is_Space then
          -- Add this new space
          Res.Append (Aski.Spc);
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

  -- Do we indent the children of the current element
  function Do_Indent_Child (Format       : Format_Definition;
                            Stage        : Stage_List;
                            Normalize    : Boolean;
                            Is_Mixed     : Boolean;
                            Empty_Info   : Empty_Info_List;
                            Has_Children : Boolean) return Boolean is
  begin
    -- No indent in raw mode
    if Format.Kind = Raw then return False; end if;
    -- Always indent if not in Elements
    if Stage /= Elements then return True; end if;
    -- Only indent if Normalize
    if not Normalize then return False; end if;
    -- No indent if it is micxed, if it is defined as empty in Dtd,
    --  or if it has not child and Split_No_child is not forced
    if Is_Mixed
        or else Empty_Info = Def_Empty
        or else (not Has_Children and then not Format.Split_No_Child) then
      return False;
    end if;
    -- Othewise indent
    return True;
  end Do_Indent_Child;

  -- Do we indent the current element
  function Do_Indent_Parent (Format       : Format_Definition;
                             Stage        : Stage_List;
                             Normalize    : Boolean;
                             In_Mixed     : Boolean) return Boolean is
  begin
    -- No indent in raw mode
    if Format.Kind = Raw then return False; end if;
    -- Always indent if not in Elements
    if Stage /= Elements then return True; end if;
    -- Only indent if Normalize
    if not Normalize then return False; end if;
    -- No indent if in mixed
    if In_Mixed then return False; end if;
    -- Othewise indent
    return True;
  end Do_Indent_Parent;

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
                            Format       : in Format_Definition;
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
        Att_Width := Att_Width + (if Has_Children then 1 else 2);
      end if;
      -- New line and Indent if needed
      -- Never new line for first
      -- New line if One_Per_Line of Fill_Width and no more width
      if I /= Attributes'First
      and then (Format.Kind = One_Per_Line
         or else (Format.Kind = Fill_Width
                  and then Format.Width /= Infinite_Width
                  and then Cur_Col + Att_Width > Format.Width) ) then
        New_Line (Flow);
        Put (Flow, Pad);
        Cur_Col := Pad'Length;
      end if;
      Put (Flow, " " & Att_Name.Image & "=");
      -- Use '"' by default, use ''' if File contains '"'
      if Str_Util.Locate (Attributes(I).Value.Image, """") = 0 then
        Put (Flow, """" & Attributes(I).Value.Image & """");
      else
        Put (Flow, "'" & Attributes(I).Value.Image & "'");
      end if;
      Cur_Col := Cur_Col + Att_Width;
    end loop;
  end Put_Attributes;

  -- Put the attributes of current element
  -- Move to first child of element if any, otherwise remain on element
  procedure Put_Attributes (Flow         : in out Flow_Dscr;
                            Format       : in Format_Definition;
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
      Put_Attributes (Flow, Format, Namespace,
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
                         Format  : in Format_Definition) is
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
      -- Use '"' by default, use ''' if File contains '"'
      if Str_Util.Locate (Doctype.File.Image, """") = 0 then
        Put (Flow, " """  & Doctype.File.Image & """");
      else
        Put (Flow, " '"  & Doctype.File.Image & "'");
      end if;
    end if;
    -- Internal definition
    if not Doctype.Int_Def.Is_Null then
      if Format.Kind /= Raw then
        Put (Flow, " [" & Doctype.Int_Def.Image & "]");
      else
        Put (Flow, "[" & Normalize (Doctype.Int_Def.Image) & "]");
      end if;
    end if;
    Put (Flow, ">");
  end Put_Doctype;

  -- Put an element (and its attributes and children)
  procedure Put_Element (Flow      : in out Flow_Dscr;
                         Format    : in Format_Definition;
                         Namespace : in Boolean;
                         Ctx       : in Ctx_Type;
                         Stage     : in Stage_List;
                         Tree      : in out My_Tree.Tree_Type;
                         Level     : in Integer;
                         In_Mixed  : in Boolean) is
    Cell : constant My_Tree_Cell := Tree.Read;
    Cell_Ref : constant My_Tree.Position_Access := Tree.Get_Position;
    Nb_Cell_Children : constant Trees.Child_Range := Tree.Children_Number;
    Child : My_Tree_Cell;
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Indent1 : constant String := Indent & "  ";
    Is_Mixed : constant Boolean := Cell.Is_Mixed;
    Empty_Info : constant Empty_Info_List := Cell.Empty_Info;
    Nb_Children : constant Child_Range
                := Nb_Cell_Children - Cell.Nb_Attributes;
    Elt_Name : As.U.Asu_Us;
    Xml_Attr_Format : Format_Definition;
    Closed : Boolean := False;
    -- Do we indent these outputs
    function Do_Indent_Child return Boolean is
    begin
      return Do_Indent_Child (Format, Stage, Ctx.Normalize, Is_Mixed,
                              Empty_Info, Nb_Children /= 0);
    end Do_Indent_Child;
    function Do_Indent_Parent return Boolean is
    begin
      return Do_Indent_Parent (Format, Stage, Ctx.Normalize, In_Mixed);
    end Do_Indent_Parent;
    -- Terminate tag after children
    procedure Close is
    begin
      if not Closed then
        if Do_Indent_Child then
          -- Indent the end of this non-mixed element
          Put (Flow, Indent);
        end if;
        Put (Flow, "</" & Elt_Name.Image & ">");
        if Do_Indent_Parent then
          -- End of this element
          New_Line (Flow);
        end if;
      end if;
      Closed := True;
    end Close;
    use type My_Tree.Position_Access;
  begin
    if Stage = Prologue then
      -- A prologue
      -- The Xml directive:  Put it if it has attributes
      if Cell.Nb_Attributes = 0 then
        -- No Xml Directive: move to first child as if after putting attributes
        if Tree.Children_Number /= 0 then
          Tree.Move_Child (True);
        end if;
      else
        -- The Xml directive
        -- Even if one attr per line is requested, Xml directive attributes
        --  are all on the same line
        Xml_Attr_Format := Format;
        Xml_Attr_Format.Kind := Fill_Width;
        -- Put the xml directive with attributes if any
        Put (Flow, "<?" & Cell.Name.Image);
        Put_Attributes (Flow, Xml_Attr_Format, Namespace, Tree, 0,
                        2 + Cell.Name.Length, False);
        Put (Flow, "?>");
        if Format.Kind /= Raw then
          New_Line (Flow);
        end if;
      end if;
      -- Any child of prologue?
      if Tree.Get_Position = Cell_Ref then
        -- No Child (Put_Attributes moved back to current): return
        if Format.Kind /= Raw and then Cell.Nb_Attributes /= 0 then
          -- Skip one line between prologue and root if there is
          --  a xml directive
          New_Line (Flow);
        end if;
        return;
      end if;

      -- Put prologue children: DOCTYPE, PIs and comments
      -- Put_Attributes remained on first child
      loop
        Child := Tree.Read;
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
        if Format.Kind /= Raw then
          New_Line (Flow);
        end if;
        -- Next child or done
        exit when not Tree.Has_Brother (False);
        Tree.Move_Brother (False);
      end loop;
      -- End of prologue and its children
      Tree.Move_Father;
      if Format.Kind /= Raw then
        -- Skip one line between prologue and root
        New_Line (Flow);
      end if;
      -- End of processing of Prologue
      return;
    end if;

    -- Elements and tail
    if Stage = Elements then
      -- Put element, attributes and children recursively
      if Do_Indent_Parent then
        -- Indent before the start of this element
        Put (Flow, Indent);
      end if;
      Set_Name (Elt_Name, Namespace, Cell.Name, Cell.Namespace);
      Put (Flow, "<" & Elt_Name.Image);
    end if;

    -- Put attributes and move to first child (if any)
    Put_Attributes (Flow, Format, Namespace, Tree, Level,
                    1 + Cell.Name.Length,
                    Has_Children => Nb_Children /= 0);

    -- Any child?
    if Nb_Children = 0 then
      -- No Child (Put_Attributes moved back to current): return
      if Stage = Elements then
        if Cell.Empty_Info = Tag_Empty then
          -- EmptyElementTag now
          Put (Flow, "/>");
          if Do_Indent_Parent then
            -- New line after the end of this element
            New_Line (Flow);
          end if;
        else
          -- Finish STag now and close (add ETag)
          Put (Flow, ">");
          if Do_Indent_Child then
            New_Line (Flow);
          end if;
          Close;
        end if;
        -- Newline after root with no child
        if Level = 0 and then Format.Kind /= Raw
                     and then not Do_Indent_Parent then
          New_Line (Flow);
        end if;
      end if;
      -- Terminate now
      return;
    else
      -- End Father for child
      if Stage = Elements then
        Put (Flow, ">");
        if Do_Indent_Child then
          -- New line after the start of this non-mixed element
          New_Line (Flow);
        end if;
      end if;
    end if;

    -- Put children
    for I in 1 .. Nb_Children loop
      Child := Tree.Read;
      case Child.Kind is
        when Attribute =>
          -- Impossibe
          raise Internal_Error;
        when Xml_Parser.Element =>
          if Stage = Tail then
            -- Impossibe
            raise Internal_Error;
          end if;
          -- Recursive dump child
          Put_Element (Flow, Format, Namespace, Ctx, Stage, Tree,
                         Level + 1, Cell.Is_Mixed and then Stage = Elements);
        when Text =>
          if Stage = Tail then
            -- Impossibe
            raise Internal_Error;
          end if;
          -- Specific put text
          Put (Flow, Child.Name.Image);
          if Do_Indent_Child then
            -- New line after the end of this text
            New_Line (Flow);
          end if;
        when Pi =>
          -- Put PI
          if Stage = Elements and then Do_Indent_Child then
            Put (Flow, Indent1);
          end if;
          Put (Flow, "<?" & Child.Name.Image);
          if not Child.Value.Is_Null then
            Put (Flow, " " & Child.Value.Image);
          end if;
          Put (Flow, "?>");
          if Do_Indent_Child then
            -- New line after the end of this PI
            New_Line (Flow);
          end if;
        when Comment =>
          -- Comment
          if Stage = Elements and then Do_Indent_Child then
            Put (Flow, Indent1);
          end if;
          Put_Comment (Flow, Child.Name.Image);
          if Do_Indent_Child then
            -- New line after the end of this comment
            New_Line (Flow);
          end if;
      end case;
      -- Next child or done
      exit when not Tree.Has_Brother (False);
      Tree.Move_Brother (False);
    end loop;
    -- Close parent element after its children
    if Stage = Elements then
      Close;
      -- Newline after root with children
      if Level = 0 and then Format.Kind /= Raw
                   and then not Do_Indent_Parent then
        New_Line (Flow);
      end if;
    end if;
    -- End of this element
    Tree.Move_Father;
  end Put_Element;

  -- Internal procedure to generate the output
  procedure Generate (Ctx       : in Ctx_Type;
                      Format    : in Format_Definition;
                      Namespace : in Boolean;
                      Flow      : in out Flow_Dscr) is
  begin
    if Ctx.Status /= Init and then Ctx.Status /= Parsed_Elements then
      raise Status_Error;
    end if;
    -- Put prologue if any
    Ctx.Prologue.Move_Root;
    Put_Element (Flow, Format, Namespace, Ctx,
                 Prologue, Ctx.Prologue.all, 0, False);
    -- Put Elements
    Ctx.Elements.Move_Root;
    Put_Element (Flow, Format, Namespace, Ctx,
                 Elements, Ctx.Elements.all, 0, False);
    -- Put tail
    Ctx.Tail.Move_Root;
    Put_Element (Flow, Format, Namespace, Ctx,
                 Tail, Ctx.Tail.all, 0, False);
    if Format.Kind /= Raw then
      New_Line (Flow);
    end if;

  end Generate;

  -- Put a node update image in a string
  function Image (Ctx       : Xml_Parser.Ctx_Type;
                  Update    : Node_Update;
                  Format    : Format_Definition := Default_Format;
                  Namespace : Boolean := False) return String is
    -- Do we indent these outputs
    function Do_Indent_Child return Boolean is
    begin
      return Do_Indent_Child (Format, Update.Stage, Ctx.Normalize,
                              Update.Is_Mixed, Update.Empty_Info,
                              Update.Has_Children);
    end Do_Indent_Child;
    function Do_Indent_Parent return Boolean is
    begin
      return Do_Indent_Parent (Format, Update.Stage, Ctx.Normalize,
                               Update.In_Mixed);
    end Do_Indent_Parent;
    Elt_Name : As.U.Asu_Us;
    Flow : Flow_Dscr(Use_File => False);
    Indent : constant String (1 .. 2 * Update.Level) := (others => ' ');
    Xml_Attr_Format : Format_Definition;
  begin
    if Update.Stage = Prologue then
      if Update.Level = 0 then
        -- The XML directive
        -- Put the xml directive if it has attributes
        if Update.Attributes /= null
        and then Update.Attributes'Length /= 0 then
          -- Even if one attr per line request, Xml directive attributes
          --  are all on the same line
          Xml_Attr_Format := Format;
          Xml_Attr_Format.Kind := Fill_Width;
          Put (Flow, "<?" & Update.Name.Image);
          Put_Attributes (Flow, Xml_Attr_Format, Namespace,
                          Update.Attributes.all, 0, 2 + Update.Name.Length,
                          False);
          Put (Flow, "?>");
          if Format.Kind /= Raw then
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
        if Format.Kind /= Raw then
          New_Line (Flow);
        end if;
      end if;
      return Flow.Us.Image;
    end if;

    -- In elements tree
    if Update.Stage = Elements then
      if Format.Kind /= Raw
      and then Update.Creation
      and then Update.Level = 0 then
        -- Creation of root, separate from prologue
        New_Line (Flow);
      end if;

      if Update.Creation and then Do_Indent_Parent
      and then Update.Kind /= Xml_Parser.Text
      and then (Update.Kind /= Xml_Parser.Element
                or else not Update.Name.Is_Null) then
        -- Creation of a new node: Indent if not within mixed, if not text
        --  and if not in tail
        Put (Flow, Indent);
      elsif not Update.Creation and then Do_Indent_Child then
        -- Closure of an element: Indent if not mixed
        Put (Flow, Indent);
      end if;
    end if;

    -- In Elements or Tail
    case Update.Kind is
      when Xml_Parser.Element =>
        Set_Name (Elt_Name, Namespace, Update.Name, Update.Namespace);
        if Update.Creation then
          -- Put element and attributes
          Put (Flow, "<" & Elt_Name.Image);
          if Update.Attributes /= null then
            Put_Attributes (Flow, Format, Namespace,
                            Update.Attributes.all, Update.Level,
                            1 + Update.Name.Length, Update.Has_Children);
          end if;
          -- Any child?
          if not Update.Has_Children then
            -- No child, terminate tag now
            if Update.Empty_Info = Tag_Empty then
              -- <Elt/>
              Put (Flow, "/>");
            elsif not Do_Indent_Child then
              -- <Elt></Elt>
              Put (Flow, "></" & Elt_Name.Image & ">");
            else
              -- <Elt>
              -- </Elt>
              Put (Flow, ">");
              New_Line (Flow);
              Put (Flow, Indent & "</" & Elt_Name.Image & ">");
            end if;
          else
            -- Children to come: New_Line if not mixed and done
            Put (Flow, ">");
            if Do_Indent_Child then
              New_Line (Flow);
            end if;
            return Flow.Us.Image;
          end if;
        else
          -- End of element with children
          Put (Flow, "</" & Elt_Name.Image & ">");
          if Format.Kind /= Raw and then Update.Stage = Elements
          and then Update.Level = 0 and then not Do_Indent_Parent then
            -- End of root while end of element will not appen New_Line
            New_Line (Flow);
          end if;
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
    if Do_Indent_Parent then
      -- End of element
      New_Line (Flow);
    end if;
    return Flow.Us.Image;
  end Image;

  procedure Image (Ctx    : in Xml_Parser.Ctx_Type;
                   Update : in Node_Update;
                   Str    : out As.U.Asu_Us;
                   Format : in Format_Definition := Default_Format;
                   Namespace : in Boolean := False) is
  begin
    Str := As.U.Tus (Image (Ctx, Update, Format, Namespace));
  end Image;

end Xml_Parser.Generator;

