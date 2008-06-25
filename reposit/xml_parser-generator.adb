-- Generates a Xml file (or stdout), or string from a tree
with Ada.Characters.Latin_1;
with Int_Image, Text_Line, Sys_Calls;
package body Xml_Parser.Generator is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;

  -- Image for xml version
  function Vers_Image is new Int_Image (Natural);

  -- Name validity simple check
  function Is_Letter (Char : Character) return Boolean is
  begin
    return (Char >= 'a' and then Char <= 'z')
    or else (Char >= 'A' and then Char <= 'Z');
  end Is_Letter;
  function Is_Digit (Char : Character) return Boolean is
  begin
    return Char >= '0' and then Char <= '9';
  end Is_Digit;
  function Is_Valid_In_Name (Char : Character) return Boolean is
  begin
    return Is_Letter (Char)
           or else Is_Digit (Char)
           or else Char = '_'
           or else Char = ':'
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

  Xml_Name : constant Asu_Us := Asu_Tus ("xml");
  -- Init prologue and empty root element if needed
  procedure Init_Ctx (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    if Ctx.Status = Error then
      Clean (Ctx);
    end if;
    if Ctx.Status = Clean then
      Ctx.Magic := Get_Magic;
      -- Init prologue: a xml node
      Cell.Kind := Element;
      Cell.Nb_Attributes := 0;
      Cell.Name := Xml_Name;
      Cell.Value := Asu_Null;
      My_Tree.Insert_Father (Ctx.Prologue.all, Cell);
      -- Init elements: an empty root element
      Cell.Name := Asu_Null;
      My_Tree.Insert_Father (Ctx.Elements.all, Cell);
      Ctx.Status := Init;
    end if;
  end Init_Ctx;

  -- Move to Node in tree
  procedure Move_To (Ctx  : in out Ctx_Type;
                     Node : in Node_Type;
                     Tree : out Tree_Acc) is
  begin
    -- Init context if needed
    Init_Ctx (Ctx);
    Tree := Get_Tree (Ctx, Node);
    My_Tree.Set_Position (Tree.all, Node.Tree_Access);
  end Move_To;

  -------------------------
  -- PROLOGUE OPERATIONS --
  -------------------------
  Version_Name : constant Asu_Us := Asu_Tus ("version");
  Encoding_Name : constant Asu_Us := Asu_Tus ("encoding");
  Standalone_Name : constant Asu_Us := Asu_Tus ("standalone");
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
    Vers.Value := Asu_Tus (Vers_Image (Major) & "." & Vers_Image (Minor));

    -- Read Xml
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    if Cell.Nb_Attributes = 0 then
      Cell.Nb_Attributes := 1;
      My_Tree.Replace (Ctx.Prologue.all, Cell);
      -- Insert version as first attribute
      My_Tree.Insert_Child (Ctx.Prologue.all, Vers);
    else
       -- Overwrite first attribute
       My_Tree.Move_Child (Ctx.Prologue.all);
       My_Tree.Replace (Ctx.Prologue.all, Vers);
    end if;
  end Set_Version;

  procedure Init_Version (Ctx : in out  Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    Init_Ctx (Ctx);
    -- Read Xml
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    if Cell.Nb_Attributes = 0 then
      Set_Version (Ctx, 1, 0);
    end if;
  end Init_Version;

  procedure Set_Encoding (Ctx : in out Ctx_Type; Encoding : in String) is
    Cell : My_Tree_Cell;
    Encoding_Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    Check_Name (Encoding);
    -- Read Xml
    Init_Version (Ctx);
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Ctx.Prologue.all, Cell);
    -- Set this attribute
    Encoding_Cell.Kind := Attribute;
    Encoding_Cell.Name := Encoding_Name;
    Encoding_Cell.Value := Asu_Tus (Encoding);
    -- Add this attribute as brother of version
    My_Tree.Move_Child (Ctx.Prologue.all);
    if My_Tree.Has_Brother (Ctx.Prologue.all, False) then
      My_Tree.Move_Brother (Ctx.Prologue.all, False);
      My_Tree.Read (Ctx.Prologue.all, Cell);
      if Cell.Kind = Attribute and then Cell.Name = Encoding_Name then
        -- Change encoding
        My_Tree.Replace (Ctx.Prologue.all, Encoding_Cell);
        return;
      else
        -- Back to version
        My_Tree.Move_Brother (Ctx.Prologue.all, True);
      end if;
    end if;
    -- Insert encoding as brother of version
    My_Tree.Insert_Brother (Ctx.Prologue.all, Encoding_Cell, False);
  end Set_Encoding;

  procedure Set_Standalone (Ctx : in out Ctx_Type;
                            Standalone : in Boolean) is
    Cell : My_Tree_Cell;
    Standalone_Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    -- Read Xml
    Init_Version (Ctx);
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Ctx.Prologue.all, Cell);
    -- Set this attribute
    Standalone_Cell.Kind := Attribute;
    Standalone_Cell.Name := Standalone_Name;
    if Standalone then
      Standalone_Cell.Value := Asu_Tus ("yes");
    else
      Standalone_Cell.Value := Asu_Tus ("no");
    end if;
    -- Add this attribute as brother of version or encoding
    -- Move to Version
    My_Tree.Move_Child (Ctx.Prologue.all);
    if not My_Tree.Has_Brother (Ctx.Prologue.all, False) then
      -- No more brother (only version)
      My_Tree.Insert_Brother (Ctx.Prologue.all, Standalone_Cell, False);
      return;
    end if;
    -- Move 2nd child
    My_Tree.Move_Brother (Ctx.Prologue.all, False);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    if Cell.Kind /= Attribute then
      -- 2nd child is no attribute
      My_Tree.Insert_Brother (Ctx.Prologue.all, Standalone_Cell, True);
      return;
    end if;
    if Cell.Name = Standalone_Name then
      -- Change standalone as 2nd attribute
      My_Tree.Replace (Ctx.Prologue.all, Standalone_Cell);
      return;
    end if;
    -- 2nd attribute is encoding
    if not My_Tree.Has_Brother (Ctx.Prologue.all, False) then
      -- No more brother (only version and encoding)
      My_Tree.Insert_Brother (Ctx.Prologue.all, Standalone_Cell, False);
      return;
    end if;
    My_Tree.Move_Brother (Ctx.Prologue.all, False);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    if Cell.Kind = Attribute and then Cell.Name = Standalone_Name then
      -- 3rd child is standalone
      My_Tree.Replace (Ctx.Prologue.all, Standalone_Cell);
    else
      -- 3rd child is not attribute
      My_Tree.Insert_Brother (Ctx.Prologue.all, Standalone_Cell, True);
    end if;
  end Set_Standalone;

  -- Clear Xml information
  procedure Clear_Xml (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    -- Clear all Xml attributes
    Init_Ctx (Ctx);
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Read (Ctx.Prologue.all, Cell);
    Cell.Nb_Attributes := 0;
    My_Tree.Replace  (Ctx.Prologue.all, Cell);
    loop
      My_Tree.Move_Child (Ctx.Prologue.all, True);
      My_Tree.Read (Ctx.Prologue.all, Cell);
      exit when Cell.Kind /= Attribute;
      My_Tree.Delete_Current (Ctx.Prologue.all);
    end loop;
  end Clear_Xml;

  -- Internal op to insert child of prologue
  procedure Insert_Cell (Tree : in Tree_Acc;
                         Cell : in My_Tree_Cell;
                         Append_Next : in Boolean) is
    Father : My_Tree_Cell;
  begin
    My_Tree.Read (Tree.all, Father);
    if Append_Next then
      if not My_Tree.Has_Father (Tree.all) then
        -- Current is Root of prologue, append as last child
        My_Tree.Insert_Child (Tree.all, Cell, False);
      else
        -- Current is a child
        My_Tree.Insert_Brother (Tree.all, Cell, False);
      end if;
    elsif My_Tree.Has_Father (Tree.all) then
      -- Current is a child
      My_Tree.Insert_Brother (Tree.all, Cell, True);
    elsif My_Tree.Children_Number (Tree.all) = Father.Nb_Attributes then
      -- No child (only attributes), append as last child
      My_Tree.Insert_Child (Tree.all, Cell, False);
    elsif Father.Nb_Attributes = 0 then
      -- No attribute, insert as first child
      My_Tree.Insert_Child (Tree.all, Cell, True);
    else
      -- Insert after attributes
      My_Tree.Move_Child (Tree.all, False);
      for I in 1 .. Father.Nb_Attributes - 1 loop
        My_Tree.Move_Brother (Tree.all, False);
      end loop;
      My_Tree.Insert_Brother (Tree.all, Cell, False);
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
    use type Asu_Us;
  begin
    if not Node.In_Prologue then
      raise Invalid_Node;
    end if;
    if Ctx.Doctype.Name /= Asu_Null then
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
    Ctx.Doctype.Name    := Asu_Tus (Name);
    Ctx.Doctype.Public  := Public;
    Ctx.Doctype.Pub_Id  := Asu_Tus (Pub_Id);
    Ctx.Doctype.File    := Asu_Tus (File);
    Ctx.Doctype.Int_Def := Asu_Tus (Int_Def);
    New_Node := (Kind => Text,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
  end Add_Doctype;

  -- Add a processing instruction in the prologue
  procedure Add_Pi (Ctx  : in out Ctx_Type;
                    Node : in Node_Type;
                    Name : in String; Value : in String;
                    New_Node : out Node_Type;
                    Append_Next : in Boolean := True) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    if not Node.In_Prologue then
      raise Invalid_Node;
    end if;
    Check_Name (Name);
    -- Move to node
    Move_To (Ctx, Node, Tree);
    -- Add this child to prologue
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    Cell.Value := Asu_Tus (Value);
    Insert_Cell (Tree, Cell, Append_Next);
    -- Done
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
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
    Cell.Name := Asu_Tus (Comment);
    Insert_Cell (Tree, Cell, Append_Next);
    -- Done
    New_Node := (Kind => Xml_Parser.Comment,
                 Magic => Ctx.Magic,
                 In_Prologue => True,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
  end Add_Comment;

  -- Clear the whole prologue
  procedure Clear_Prologue (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    Init_Ctx (Ctx);
    -- Delete prologue
    My_Tree.Move_Root (Ctx.Prologue.all);
    My_Tree.Delete_Tree (Ctx.Prologue.all);
    -- Clear doctype
    Ctx.Doctype.Line_No := 0;
    Ctx.Doctype.Name := Asu_Null;
    -- Init prologue: a xml node
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Xml_Name;
    Cell.Value := Asu_Null;
    My_Tree.Insert_Father (Ctx.Prologue.all, Cell);
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
  -- May raise No_Element if no father (at root or tree is empty)
  procedure Move_Father (Ctx      : in out Ctx_Type) is
  begin
    if not My_Tree.Has_Father (Ctx.Elements.all) then
      raise No_Element;
    end if;
    My_Tree.Move_Father (Ctx.Elements.all);
  end Move_Father;

  -- Internal op
  function Internal_Kind_Of (Kind : Node_Kind_List) return Internal_Kind_List is
  begin
    case Kind is
      when Element => return Element;
      when Text    => return Text;
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
    My_Tree.Read (Tree.all, Cell);
    Cell.Name := Asu_Tus (Name);
    My_Tree.Replace (Tree.all, Cell);
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
    My_Tree.Read (Tree.all, Cell);
    Nb_Attributes := Cell.Nb_Attributes;
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Tree.all, Cell);
    -- Add this attribute
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    Cell.Value := Asu_Tus (Value);
    if Nb_Attributes = 0 then
      -- As first child
      My_Tree.Insert_Child (Tree.all, Cell);
    else
      -- Insert after current attributes
      My_Tree.Move_Child (Tree.all);
      for I in 1 .. Nb_Attributes - 1 loop
        My_Tree.Move_Brother (Tree.all, False);
      end loop;
      My_Tree.Insert_Brother (Tree.all, Cell, False);
    end if;
    My_Tree.Move_Father (Tree.all);
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
    My_Tree.Read (Tree.all, Cell);
    Cell.Nb_Attributes := Attributes'Length;
    My_Tree.Replace (Tree.all, Cell);
    -- Add these attributes
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    for I in reverse Attributes'Range loop
      Cell.Name := Attributes(I).Name;
      Cell.Value := Attributes(I).Value;
      My_Tree.Insert_Child (Tree.all, Cell);
      My_Tree.Move_Father (Tree.all);
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
    My_Tree.Read (Tree.all, Cell);
    Cell.Nb_Attributes := 0;
    My_Tree.Replace (Tree.all, Cell);
    -- Del all attributes
    loop
      -- No more child?
      exit when My_Tree.Children_Number (Tree.all) = 0;
      My_Tree.Move_Child (Tree.all);
      My_Tree.Read (Tree.all, Cell);
      if Cell.Kind /= Attribute then
        -- A child element (or comment or text), no more attribute
        My_Tree.Move_Father (Tree.all);
        exit;
      end if;
      -- Delete this attribute (move up)
      My_Tree.Delete_Current (Tree.all);
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
    My_Tree.Read (Tree.all, Father);
    -- Add this child
    Cell.Kind := Internal_Kind_Of (Kind);
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    if Append or else Father.Nb_Attributes = 0 then
      My_Tree.Insert_Child (Tree.all, Cell, not Append);
    else
      -- Insert after attributes
      My_Tree.Move_Child (Tree.all);
      for I in 1 .. Father.Nb_Attributes - 1 loop
        My_Tree.Move_Brother (Tree.all, False);
      end loop;
      My_Tree.Insert_Brother (Tree.all, Cell, False);
    end if;
    New_Node := (Kind => Kind,
                 Magic => Ctx.Magic,
                 In_Prologue => False,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
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
    Cell.Name := Asu_Tus (Name);
    My_Tree.Insert_Brother (Tree.all, Cell, not Next);
    New_Node := (Kind => Kind,
                 Magic => Ctx.Magic,
                 In_Prologue => False,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
  end Add_Brother;

  -- Set the text of a Text element
  procedure Set_Text (Ctx     : in out Ctx_Type;
                      Text    : in out Text_Type;
                      Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Text, Tree);
    -- Update Text
    My_Tree.Read (Tree.all, Cell);
    Cell.Name := Asu_Tus (Content);
    My_Tree.Replace (Tree.all, Cell);
  end Set_Text;

  -- Set the text of a Comment
  procedure Set_Comment (Ctx     : in out Ctx_Type;
                         Comment : in out Comment_Type;
                         Content : in String) is
    Tree : Tree_Acc;
    Cell : My_Tree_Cell;
  begin
    -- Move to node, must be an element
    Move_To_Element (Ctx, Comment, Tree);
    -- Update Text
    My_Tree.Read (Tree.all, Cell);
    Cell.Name := Asu_Tus (Content);
    My_Tree.Replace (Tree.all, Cell);
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
    if not My_Tree.Has_Father (Tree.all) then
      raise Invalid_Node;
    end if;
    -- Clean doctype info if node is the doctype (text of prologue)
    if Node.In_Prologue and then Node.Kind = Text then
      Ctx.Doctype.Name := Asu_Null;
    end if;
    My_Tree.Delete_Current (Tree.all);
    -- Father is an element
    New_Node := (Kind => Element,
                 Magic => Ctx.Magic,
                 In_Prologue => Node.In_Prologue,
                 Tree_Access => My_Tree.Get_Position (Tree.all));
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
    if Element.In_Prologue and then not My_Tree.Has_Father (Tree.all) then
      Ctx.Doctype.Name := Asu_Null;
    end if;
    -- Delete all children
    My_Tree.Read (Tree.all, Cell);
    for I in 1 .. My_Tree.Children_Number (Tree.all) - Cell.Nb_Attributes loop
      My_Tree.Move_Child (Tree.all, True);
      My_Tree.Delete_Current (Tree.all);
    end loop;
  end Delete_Children;

  --------------------------------------------------------------------------------------
  -- GENERATION --
  ----------------
  type Flow_Dscr (Use_File : Boolean) is record
    case Use_File is
      when True => File : Text_Line.File_Type;
      when False => Us : Asu_Us;
    end case;
  end record;

  ----------------
  -- GENERATION --
  ----------------
  -- Internal procedure to generate the output
  procedure Generate (Ctx    : in Ctx_Type;
                      Format : in Format_Kind_List;
                      Width  : in Natural;
                      Flow   : in out Flow_Dscr);

  -- Put in a file the indented or raw XML flow.
  -- Raises Text_Line exceptions
  procedure Put (Ctx       : in out Ctx_Type;
                 File_Name : in String;
                 Format    : in Format_Kind_List := Default_Format;
                 Width     : in Natural := Default_Width) is
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
    Generate (Ctx, Format, Width, Flow);
    -- Close flow
    Close;
  exception
    when others =>
      Close;
      raise;
  end Put;

  -- Dumps in a string then raw XML flow (no CR no space)
  function Set (Ctx   : Ctx_Type;
                Format : Format_Kind_List := Default_Format;
                Width  : Natural := Default_Width) return String is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Width, Flow);
    return Asu_Ts (Flow.Us);
  end Set;

  procedure Set (Ctx : in Ctx_Type;
                 Str : out Ada.Strings.Unbounded.Unbounded_String;
                 Format : in Format_Kind_List := Default_Format;
                 Width  : in Natural := Default_Width) is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Ctx, Format, Width, Flow);
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
      Asu.Append (Flow.Us, Str);
    end if;
  end Put;
  procedure New_Line (Flow : in out Flow_Dscr) is
  begin
    if Flow.Use_File then
      Text_Line.Put (Flow.File, Text_Line.Line_Feed & "");
    else
      Asu.Append (Flow.Us, Text_Line.Line_Feed);
    end if;
  end New_Line;

  -- Replace any sequence of separators by a space
  function Normalize (Str : String) return String is
    Res : Asu_Us;
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
          Asu.Append (Res, Ada.Characters.Latin_1.Space);
          Prev_Is_Space := True;
        end if;
      else
        -- Not a space
        Asu.Append (Res, Char);
        Prev_Is_Space := False;
      end if;
    end loop;
    return Asu_Ts (Res);
  end Normalize;

  -- Put the attributes of current element
  -- Move to first child of element if any, otherwise remain on element
  procedure Put_Attributes (Flow         : in out Flow_Dscr;
                            Format       : in Format_Kind_List;
                            Width        : in Natural;
                            Element      : in out My_Tree.Tree_Type;
                            Level        : in Natural;
                            Offset       : in Positive;
                            Has_Children : in Boolean) is
    Cell : My_Tree_Cell;
    Nb_Attributes : Natural;
    Pad : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    Cur_Col : Natural;
    Att_Width : Positive;
    use type Asu_Us;
  begin
    -- Read number of attribtues
    My_Tree.Read (Element, Cell);
    Nb_Attributes := Cell.Nb_Attributes;
    if Nb_Attributes = 0 then
      -- No attribute. Move to first child if any
      if My_Tree.Children_Number (Element) /= 0 then
        My_Tree.Move_Child (Element, True);
      end if;
      return;
    end if;
    -- Put each attribute
    Cur_Col := Pad'Length;
    for I in 1 .. Nb_Attributes loop
      if I = 1 then
         My_Tree.Move_Child (Element, True);
      else
        My_Tree.Move_Brother (Element, False);
      end if;
      -- Read attribute, needed width is ' Name="Value"'
      My_Tree.Read (Element, Cell);
      Att_Width := Asu.Length (Cell.Name) + Asu.Length (Cell.Value) + 4;
      -- For last attribute, a ">" (if children) or a "/>" will be added
      if I = Nb_Attributes then
        if Has_Children then
          Att_Width := Att_Width + 1;
        else
          Att_Width := Att_Width + 2;
        end if;
      end if;
      -- New line and Indent if needed
      -- Never new line for first
      -- New line if One_Per_Line of Fill_Width and no more width
      if I /= 1
      and then (Format = One_Per_Line
         or else (Format = Fill_Width
                  and then Cur_Col + Att_Width > Width) ) then
        New_Line (Flow);
        Put (Flow, Pad);
        Cur_Col := Pad'Length;
      end if;
      Put (Flow, " " & Asu_Ts (Cell.Name & "=""" & Cell.Value) & """");
      Cur_Col := Cur_Col + Att_Width;
    end loop;
    -- Move to next brother if any, otherwise move to father
    if My_Tree.Has_Brother (Element, False) then
      My_Tree.Move_Brother (Element, False);
    else
      My_Tree.Move_Father (Element);
    end if;
  end Put_Attributes;

  -- Put a comment at propoer indent
  procedure Put_Comment (Flow    : in out Flow_Dscr;
                         Comment : in String) is
  begin
    Put (Flow, "<!--" & Comment & "-->");
  end Put_Comment;

  -- Put an element (and its attributes and children)
  Prologue_Level : constant := -1;
  procedure Put_Element (Flow    : in out Flow_Dscr;
                         Format  : in Format_Kind_List;
                         Width   : in Natural;
                         Ctx     : in Ctx_Type;
                         Element : in out My_Tree.Tree_Type;
                         Level   : in Integer) is
    Cell : constant My_Tree_Cell := My_Tree.Read (Element);
    Cell_Ref : constant My_Tree.Position_Access
             := My_Tree.Get_Position (Element);
    Nb_Children : constant Trees.Child_Range
                := My_Tree.Children_Number (Element);
    Child : My_Tree_Cell;
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Indent1 : constant String := Indent & "  ";
    Doctype_Name, Doctype_File : Asu_Us;
    Prev_Is_Text : Boolean;
    Xml_Attr_Format : Format_Kind_List;
    use type Asu_Us, My_Tree.Position_Access;
  begin
    if Level = Prologue_Level then
      -- A prologue
      -- Even if one attr per line request, Xml directive attributes
      --  are all on the same line
      if Format = One_Per_Line then
        Xml_Attr_Format := Fill_Width;
      else
        Xml_Attr_Format := Format;
      end if;
      -- Put the xml directive with attributes if any
      Put (Flow, "<?" & Asu_Ts (Cell.Name));
      Put_Attributes (Flow, Xml_Attr_Format, Width, Element, 0,
                      2 + Asu.Length (Cell.Name), False);
      Put (Flow, "?>");
      if Format /= Raw then
        New_Line (Flow);
      end if;
      -- Any child of prologue?
      if My_Tree.Get_Position (Element) = Cell_Ref then
        -- No Child (Put_Attributes moved back to current): return
        return;
      end if;

      -- Put prologue children: DOCTYPE, PIs and comments
      -- Put_Attributes remained on first child
      loop
        Child := My_Tree.Read (Element);
        case Child.Kind is
          when Attribute =>
            -- Impossibe
            raise Internal_Error;
          when Xml_Parser.Element =>
            -- Put PI
            Put (Flow, "<?" & Asu_Ts (Child.Name));
            if Child.Value /= Asu_Null then
              Put (Flow, " " & Asu_Ts (Child.Value));
            end if;
            Put (Flow, "?>");
          when Text =>
            -- Put DOCTYPE, name
            Put (Flow, "<!DOCTYPE " & Asu_Ts (Ctx.Doctype.Name));
            if Ctx.Doctype.Name = Asu_Null then
              raise Internal_Error;
            end if;
            -- Public or System external reference
            if Ctx.Doctype.Public then
              -- Public and public Id
              Put (Flow, " PUBLIC """ & Asu_Ts (Ctx.Doctype.Pub_Id) & """");
            else
              Put (Flow, " SYSTEM");
            end if;
            -- Public or System URI
            Put (Flow, " """  & Asu_Ts (Ctx.Doctype.File) & """");
            -- Internal definition
            if Ctx.Doctype.Int_Def /= Asu_Null then
              if Format /= Raw then
                Put (Flow, " [" & Asu_Ts (Ctx.Doctype.Int_Def) & "] ");
              else
                Put (Flow, "[" & Normalize (Asu_Ts (Ctx.Doctype.Int_Def))
                               & "]");
              end if;
            end if;
            Put (Flow, ">");
          when Comment =>
            Put_Comment (Flow, Asu_Ts (Child.Name));
        end case;
        if Format /= Raw then
          New_Line (Flow);
        end if;
        -- Next child or done
        exit when not My_Tree.Has_Brother (Element, False);
        My_Tree.Move_Brother (Element, False);
      end loop;
      -- End of prologue and its children
      My_Tree.Move_Father (Element);
      if Format /= Raw then
        New_Line (Flow);
      end if;
      return;
    end if;

    -- An element
    -- Put element, attributes and children recursively
    if Format /= Raw then
      Put (Flow, Indent);
    end if;
    Put (Flow, "<" & Asu_Ts(Cell.Name));
    Put_Attributes (Flow, Format, Width, Element, Level,
                    1 + Asu.Length (Cell.Name),
                    Has_Children => My_Tree.Children_Number (Element)
                                      > Cell.Nb_Attributes);
    -- Any child
    if My_Tree.Get_Position (Element) = Cell_Ref then
      -- No child, terminate tag now
      Put (Flow, "/>");
      return;
    end if;

    -- Put children
    Put (Flow, ">");
    Prev_Is_Text := False;
    for I in 1 .. Nb_Children loop
      Child := My_Tree.Read (Element);
      case Child.Kind is
        when Attribute =>
          -- Impossibe
          raise Internal_Error;
        when Xml_Parser.Element =>
          -- Recursive dump child
          if I = 1 or else not Prev_Is_Text then
            -- Father did not New_Line because of possible text
            --  or prev was not text and did not New_Line because
            --  of possible text
            if Format /= Raw then
              New_Line (Flow);
            end if;
            Put_Element (Flow, Format, Width, Ctx, Element, Level + 1);
          elsif I = 1 then
            -- First Child
            Put_Element (Flow, Format, Width, Ctx, Element, Level + 1);
          else
            -- Child element following text
            --  we bet that it has no child itself, so no New_Line nor Indent
            Put_Element (Flow, Format, Width, Ctx, Element, 0);
          end if;
          if not My_Tree.Has_Brother (Element, False)
          and then Format /= Raw then
            -- Last child
            New_Line (Flow);
          end if;
          Prev_Is_Text := False;
        when Text =>
          -- Specific put text
          Put (Flow, Asu_Ts (Child.Name));
          Prev_Is_Text := True;
        when Comment =>
          -- Comment
          if Format /= Raw then
            if (I = 1 or else not Prev_Is_Text) then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              New_Line (Flow);
            end if;
            Put (Flow, Indent1);
          end if;
          Put_Comment (Flow, Asu_Ts (Child.Name));
          if not My_Tree.Has_Brother (Element, False)
          and then Format /= Raw then
            -- Last child
            New_Line (Flow);
          end if;
          Prev_Is_Text := False;
        end case;
        -- Next child or done
        exit when not My_Tree.Has_Brother (Element, False);
        My_Tree.Move_Brother (Element, False);
      end loop;
      -- Terminate tag after children
      if not Prev_Is_Text and then Format /= Raw then
        Put (Flow, Indent);
      end if;
      Put (Flow, "</" & Asu_Ts (Cell.Name) & ">");

      -- End of this element
      My_Tree.Move_Father (Element);
  end Put_Element;

  -- Internal procedure to generate the output
  procedure Generate (Ctx   : in Ctx_Type;
                      Format : in Format_Kind_List;
                      Width  : in Natural;
                      Flow   : in out Flow_Dscr) is
  begin
    if Ctx.Status = Error
    or else Ctx.Status = Error
    or else Ctx.Status = Parsed_Prologue then
      raise Status_Error;
    end if;
    -- Put prologue if any
    My_Tree.Move_Root (Ctx.Prologue.all);
    Put_Element (Flow, Format, Width, Ctx, Ctx.Prologue.all, Prologue_Level);
    -- Put Elements
    My_Tree.Move_Root (Ctx.Elements.all);
    Put_Element (Flow, Format, Width, Ctx, Ctx.Elements.all, 0);
    if Format /= Raw then
      New_Line (Flow);
      New_Line (Flow);
    end if;
  end Generate;

end Xml_Parser.Generator;

