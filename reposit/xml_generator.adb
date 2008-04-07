-- with Ada.Strings.Unbounded, Ada.Finalization;
-- with Trees;
-- Generates a Xml file (or stdout), or string from a tree
with Int_Image, Text_Line, Sys_Calls;
package body Xml_Generator is

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

  -- Reset a XML descriptor,
  -- If Major is not 0, then set xml version (only 1.0 and 1.1 are OK)
  -- Set root element
  -- The xml version stored when no xml directive
  No_Xml : constant String := "None";
  procedure Reset (Dscr      : in out Xml_Dscr_Type;
                   Major : in Natural; Minor : in Natural;
                   Root_Name : in String) is
    Cell : My_Tree_Cell;
  begin
    -- Check values
    if Major /= 0
    and then (Major /= 1 or else (Minor /= 0 and then Minor /= 1)) then
      raise Invalid_Argument;
    end if;
    Check_Name (Root_Name);
    -- Clean Dscr
    Finalize (Dscr);
    -- Init prologue: a xml node with a version attribute
    Cell.Kind := Element;
    Cell.Nb_Attributes := 1;
    Cell.Name := Asu_Tus ("xml");
    Cell.Value := Asu_Null;
    My_Tree.Insert_Father (Dscr.Prologue.all, Cell);
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus ("version");
    if Major = 0 then
      Cell.Value := Asu_Tus (No_Xml);
    else
      Cell.Value := Asu_Tus (Vers_Image (Major) & "." & Vers_Image (Minor));
    end if;
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
    -- Init root element: an element
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Root_Name);
    Cell.Value := Asu_Null;
    My_Tree.Insert_Father (Dscr.Elements.all, Cell);
  end Reset;

  -- Raise Status_Error id Dscr not (re)set
  procedure Check_Set  (Dscr : in Xml_Dscr_Type) is
  begin
    if My_Tree.Is_Empty (Dscr.Elements.all) then
      raise Status_Error;
    end if;
  end Check_Set;

  -------------------------
  -- PROLOGUE OPERATIONS --
  -------------------------
  procedure Set_Encoding (Dscr : in out Xml_Dscr_Type; Encoding : in String) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    My_Tree.Read (Dscr.Prologue.all, Cell);
    if My_Tree.Children_Number (Dscr.Prologue.all) /= Cell.Nb_Attributes then
      raise Has_Children;
    end if;
    Check_Name (Encoding);
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Dscr.Prologue.all, Cell);
    -- Add this attribute to prologue
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus ("encoding");
    Cell.Value := Asu_Tus (Encoding);
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
  end Set_Encoding;

  procedure Set_Standalone (Dscr : in out Xml_Dscr_Type;
                            Standalone : in Boolean) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    -- Check no child and increment Nb_Attributes
    My_Tree.Read (Dscr.Prologue.all, Cell);
    if My_Tree.Children_Number (Dscr.Prologue.all) /= Cell.Nb_Attributes then
      raise Has_Children;
    end if;
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Dscr.Prologue.all, Cell);
    -- Add this attribute to prologue
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus ("standalone");
    if Standalone then
      Cell.Value := Asu_Tus ("yes");
    else
      Cell.Value := Asu_Tus ("no");
    end if;
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
  end Set_Standalone;

  -- Set the DOCTYPE text
  procedure Set_Doctype (Dscr : in out Xml_Dscr_Type;
                         Name : in String; Text : in String) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    if Dscr.Doctype_Set then
      raise Has_Children;
    end if;
    Check_Name (Name);
    -- Add this child to prologue
    Cell.Kind := Xml_Generator.Text;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    Cell.Value := Asu_Tus (Text);
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
    Dscr.Doctype_Set := True;
  end Set_Doctype;

  -- Add a processing instruction in the prologue
  procedure Add_Pi (Dscr : in out Xml_Dscr_Type;
                    Name : in String; Value : in String) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    Check_Name (Name);
    -- Add this child to prologue
    My_Tree.Move_Root (Dscr.Prologue.all);
    Cell.Kind := Element;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    Cell.Value := Asu_Tus (Value);
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
  end Add_Pi;

  -- Add a comment in the prologue
  procedure Add_Comment (Dscr : in out Xml_Dscr_Type;
                         Text : in String) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    -- Add this comment to prologue
    My_Tree.Move_Root (Dscr.Prologue.all);
    Cell.Kind := Comment;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Text);
    My_Tree.Insert_Child (Dscr.Prologue.all, Cell);
    My_Tree.Move_Root (Dscr.Prologue.all);
  end Add_Comment;

  ----------------------
  -- TREE OF ELEMENTS --
  ----------------------
  -- May raise No_Element if no father (at root or tree is empty)
  procedure Move_Father (Dscr : in out Xml_Dscr_Type) is
  begin
    Check_Set (Dscr);
    if not My_Tree.Has_Father (Dscr.Elements.all) then
      raise No_Element;
    end if;
    My_Tree.Move_Father (Dscr.Elements.all);
  end Move_Father;

  -- Raise No_Element if current is not an elemen
  procedure Check_Element (Dscr : in Xml_Dscr_Type) is
    Cell : My_Tree_Cell;
  begin
    My_Tree.Read (Dscr.Elements.all, Cell);
    if Cell.Kind /= Element then
      raise No_Element;
    end if;
  end Check_Element;

  -- Add an attribute to current element
  -- May raise No_Element if current element is text
  procedure Add_Attribute (Dscr  : in out Xml_Dscr_Type;
                           Name, Value : in String) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    Check_Element (Dscr);
    -- Check no child and increment Nb_Attributes
    My_Tree.Read (Dscr.Elements.all, Cell);
    if My_Tree.Children_Number (Dscr.Elements.all) /= Cell.Nb_Attributes then
      raise Has_Children;
    end if;
    Cell.Nb_Attributes := Cell.Nb_Attributes + 1;
    My_Tree.Replace (Dscr.Elements.all, Cell);
    -- Add this attribute to prologue
    Cell.Kind := Attribute;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    Cell.Value := Asu_Tus (Value);
    My_Tree.Insert_Child (Dscr.Elements.all, Cell);
    My_Tree.Move_Father (Dscr.Elements.all);
  end Add_Attribute;

  -- Insert a child element, text or comment, and move to it
  -- May raise No_Element if current element is text
  procedure Add_Child (Dscr : in out Xml_Dscr_Type;
                       Name : in String;
                       Kind : in Kind_List) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
    Check_Element (Dscr);
    -- Add this child
    Cell.Kind := Kind;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    My_Tree.Insert_Child (Dscr.Elements.all, Cell);
  end Add_Child;

  -- Insert a brother element, text or comment, and move to it
  procedure Add_Brother (Dscr : in out Xml_Dscr_Type;
                         Name : in String;
                         Kind : in Kind_List) is
    Cell : My_Tree_Cell;
  begin
    Check_Set (Dscr);
     -- Add this brother
    Cell.Kind := Kind;
    Cell.Nb_Attributes := 0;
    Cell.Name := Asu_Tus (Name);
    My_Tree.Insert_Brother (Dscr.Elements.all, Cell);
  end Add_Brother;

  procedure Finalize (Dscr : in out Xml_Dscr_Type) is
  begin
    -- Clean prologue
     if not My_Tree.Is_Empty (Dscr.Prologue.all) then
      My_Tree.Move_Root (Dscr.Prologue.all);
      My_Tree.Delete_Tree (Dscr.Prologue.all);
    end if;
    -- Clean element tree
    if not My_Tree.Is_Empty (Dscr.Elements.all) then
      My_Tree.Move_Root (Dscr.Elements.all);
      My_Tree.Delete_Tree (Dscr.Elements.all);
    end if;
    -- Clean xml
    Dscr.Xml_Set := False;
    -- Clean doctype
    Dscr.Doctype_Set := False;
  end Finalize;

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
  procedure Generate (Dscr : in Xml_Dscr_Type;
                      Format : in Boolean;
                      Flow : in out Flow_Dscr);

  -- Put in a file the indented or raw XML flow.
  -- Raises Text_Line exceptions
  procedure Put (Dscr : in out Xml_Dscr_Type;
                 Format : in Boolean;
                 File_Name : in String := "") is
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
    if File_Name /= "" then
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
    Generate (Dscr, Format, Flow);
    -- Close flow
    Close;
  exception
    when others =>
      Close;
      raise;
  end Put;

  -- Dumps in a string then raw XML flow (no CR no space)
  function Set (Dscr : Xml_Dscr_Type; Format : Boolean) return String is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Dscr, Format, Flow);
    return Asu_Ts (Flow.Us);
  end Set;

  procedure Set (Dscr : in Xml_Dscr_Type;
                 Format : in Boolean;
                 Str : out Ada.Strings.Unbounded.Unbounded_String) is
    Flow : Flow_Dscr(Use_File => False);
  begin
    -- Compute generated text
    Generate (Dscr, Format, Flow);
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

  -- Put the attributes of current element
  -- Move to first child of element if any, otherwise remain on element
  procedure Put_Attributes (Flow : in out Flow_Dscr;
                            Format : in Boolean;
                            Element : in out My_Tree.Tree_Type;
                            Level : in Natural;
                            Offset : in Positive;
                            One_Per_Line : in Boolean := True) is
    Cell : My_Tree_Cell;
    Nb_Attributes : Natural;
    Indent : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    use type Asu_Us;
  begin
    -- Read number of attribtues
    My_Tree.Read (Element, Cell);
    Nb_Attributes := Cell.Nb_Attributes;
    if Nb_Attributes = 0 then
      -- No attribute. Move to first child if any
      if My_Tree.Children_Number (Element) /= 0 then
        My_Tree.Move_Child (Element, False);
      end if;
      return;
    end if;
    -- Put each attribute
    for I in 1 .. Nb_Attributes loop
      if I = 1 then
         My_Tree.Move_Child (Element, False);
      else
        My_Tree.Move_Brother (Element);
      end if;
      if I /= 1 and then One_Per_Line and then Format then
        -- Indent
        Put (Flow, Indent);
      end if;
      My_Tree.Read (Element, Cell);
      Put (Flow, " " & Asu_Ts (Cell.Name & "=""" & Cell.Value) & """");
      if I /= Nb_Attributes and then One_Per_Line and then Format then
        New_Line (Flow);
      end if;
    end loop;
    -- Move to next brother if any, otherwise move to father
    if My_Tree.Has_Brother (Element) then
      My_Tree.Move_Brother (Element);
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
  procedure Put_Element (Flow : in out Flow_Dscr;
                         Format : in Boolean;
                         Element : in out My_Tree.Tree_Type;
                         Level : in Integer) is
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
    use type Asu_Us, My_Tree.Position_Access;
  begin
    if Level = Prologue_Level then
      -- A prologue
      -- Read first attribute of prologue: the xml version
      My_Tree.Move_Child (Element);
      My_Tree.Read (Element, Child);
      My_Tree.Move_Father (Element);
      -- Put the xml directive with attributes if any
      if Asu_Ts (Child.Value) /= No_Xml then
        Put (Flow, "<?" & Asu_Ts (Cell.Name));
        Put_Attributes (Flow, Format, Element, 0,
                        2 + Asu.Length (Cell.Name), False);
        Put (Flow, "?>");
        if Format then
          New_Line (Flow);
        end if;
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
          when Xml_Generator.Element =>
            -- Put PI
            Put (Flow, "<?" & Asu_Ts (Child.Name));
            if Child.Value /= Asu_Null then
              Put (Flow, " " & Asu_Ts (Child.Value));
            end if;
            Put (Flow, "?>");
          when Text =>
            -- Put DOCTYPE, name then text if any
            Put (Flow, "<!DOCTYPE " & Asu_Ts (Child.Name));
            if Child.Value /= Asu_Null then
              Put (Flow, " " & Asu_Ts (Child.Value));
            end if;
            Put (Flow, ">");
          when Comment =>
            Put_Comment (Flow, Asu_Ts (Child.Name));
        end case;
        if Format then
          New_Line (Flow);
        end if;
        -- Next child or done
        exit when not My_Tree.Has_Brother (Element);
        My_Tree.Move_Brother (Element);
      end loop;
      -- End of prologue and its children
      My_Tree.Move_Father (Element);
      if Format then
        New_Line (Flow);
      end if;
      return;
    end if;

    -- An element
    -- Put element, attributes and children recursively
    if Format then
      Put (Flow, Indent);
    end if;
    Put (Flow, "<" & Asu_Ts(Cell.Name));
    Put_Attributes (Flow, Format, Element, Level, 1 + Asu.Length (Cell.Name));
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
        when Xml_Generator.Element =>
          -- Recursive dump child
          if I = 1 or else not Prev_Is_Text then
            -- Father did not New_Line because of possible text
            --  or prev was not text and did not New_Line because
            --  of possible text
            if Format then
              New_Line (Flow);
            end if;
            Put_Element (Flow, Format, Element, Level + 1);
          elsif I = 1 then
            -- First Child
            Put_Element (Flow, Format, Element, Level + 1);
          else
            -- Child element following text
            --  we bet that it has no child itself, so no New_Line nor Indent
            Put_Element (Flow, Format, Element, 0);
          end if;
          if not My_Tree.Has_Brother (Element) and then Format then
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
          if Format then
            if (I = 1 or else not Prev_Is_Text) then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              New_Line (Flow);
            end if;
            Put (Flow, Indent1);
          end if;
          Put_Comment (Flow, Asu_Ts (Child.Name));
          if not My_Tree.Has_Brother (Element) and then Format then
            -- Last child
            New_Line (Flow);
          end if;
          Prev_Is_Text := False;
        end case;
        -- Next child or done
        exit when not My_Tree.Has_Brother (Element);
        My_Tree.Move_Brother (Element);
      end loop;
      -- Terminate tag after children
      if not Prev_Is_Text and then Format then
        Put (Flow, Indent);
      end if;
      Put (Flow, "</" & Asu_Ts (Cell.Name) & ">");

      -- End of this element
      My_Tree.Move_Father (Element);
  end Put_Element;

  -- Internal procedure to generate the output
  procedure Generate (Dscr : in Xml_Dscr_Type;
                      Format : in Boolean;
                      Flow : in out Flow_Dscr) is
  begin
    Check_Set (Dscr);
    -- Put prologue if any
    Put_Element (Flow, Format, Dscr.Prologue.all, Prologue_Level);
    -- Put Elements
    My_Tree.Move_Root (Dscr.Elements.all);
    Put_Element (Flow, Format, Dscr.Elements.all, 0);
    if Format then
      New_Line (Flow);
    end if;
  end Generate;
end Xml_Generator;

