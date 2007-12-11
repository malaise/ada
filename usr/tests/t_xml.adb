with Ada.Text_Io, Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Xml_Parser, Normal, Basic_Proc;
procedure T_Xml is
  Ctx : Xml_Parser.Ctx_Type;
  Prologue, Root : Xml_Parser.Element_Type;
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  Dump : Boolean := False;
  Arg_Index : Natural := 1;
  Parse_Ok : Boolean;
  

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name
        & " [ -dump ] <xml_file>");
  end Usage;

  -------------------
  -- Dump xml tree --
  -------------------
  procedure Dump_Line (Node : in Xml_Parser.Node_Type) is
  begin
    Ada.Text_Io.Put (Normal (Ctx.Get_Line_No (Node), 5, True, '0'));
  end Dump_Line;

  procedure Dump_Attributes (Elt : in Xml_Parser.Element_Type) is
    Attrs : Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      Ada.Text_Io.Put (" " & Asu.To_String (Attrs(I).Name
                     & "=" & Attrs(I).Value));
    end loop;
  end Dump_Attributes;

  procedure Dump_Element (Elt : in Xml_Parser.Element_Type;
                          Level : in Natural) is
    Children : Xml_Parser.Nodes_Array := Ctx.Get_Children (Elt);
    Indent : constant String (1 .. Level + 1) := (others => ' ');
    use type Xml_Parser.Node_Kind_List;
  begin
    Dump_Line (Elt);
    Ada.Text_Io.Put (Indent);
    Ada.Text_Io.Put (Asu.To_String(Ctx.Get_Name (Elt)));
    if Ctx.Get_Nb_Attributes (Elt) /= 0 then
      Ada.Text_Io.Put (" :" );
    end if;
    Dump_Attributes (Elt);
    Ada.Text_Io.New_Line;
    for I in Children'Range loop
      if I rem 2 = 0 then
        -- Test the individual get
        Children(I) := Ctx.Get_Child (Elt, I);
      end if;
      if Children(I).Kind = Xml_Parser.Element then
        -- Recursive dump child
        Dump_Element (Children(I), Level + 1);
      else
        -- Specific put text
        Dump_Line (Children(I));
        Ada.Text_Io.Put (Indent);
        Ada.Text_Io.Put_Line (" =>" & Ctx.Get_Text (Children(I))
                            & "<=");
      end if;
    end loop;
  end Dump_Element;

  ---------------------
  -- Put the Xml way --
  ---------------------
  procedure Put_Attributes (Elt : in Xml_Parser.Element_Type;
                            Level : in Natural;
                            Offset : in Positive) is
    Attrs : Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    Indent : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      if I /= 1 then
        -- Indent
        Ada.Text_Io.Put (Indent);
      end if;
      Ada.Text_Io.Put (" " & Asu.To_String (Attrs(I).Name
                     & "=""" & Attrs(I).Value) & """");
      if I /= Attrs'Last then
        Ada.Text_Io.New_Line;
      end if;
    end loop;
  end Put_Attributes;

  Prologue_Level : constant := -1;
  Prologue_Instruction_Level : constant := -2;
  procedure Put_Element (Elt : in Xml_Parser.Element_Type;
                         Level : in Integer) is
    Name : constant String := Asu.To_String(Ctx.Get_Name (Elt));
    Children : Xml_Parser.Nodes_Array := Ctx.Get_Children (Elt);
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Prev_Is_Text : Boolean;
    use type Xml_Parser.Node_Kind_List;
  begin
    if Level = Prologue_Level then
      if Name /= "" then
        -- A prologue
        -- Put the xml directive with attributes
        Ada.Text_Io.Put ("<?" & Name);
        Put_Attributes (Elt, 0, 2 + Name'Length);
        Ada.Text_Io.Put_Line ("?>");
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Put PIs
            Put_Element (Children(I), Prologue_Instruction_Level);
          end if;
        end loop;
      end if;
      return;
    elsif Level = Prologue_Instruction_Level then
      -- A PI of the prologue: 0 child or 1 text child
      -- Put PI directive
      Ada.Text_Io.Put ("<?" & Name);
      if Children'Length = 1 then
        Ada.Text_Io.Put (" " & Ctx.Get_Text (Children(1)));
      elsif Children'Length /= 0 then
        Basic_Proc.Put_Line_Error ("Error. Prologue processing instruction "
            & Name & " has several text children.");
        raise Program_Error;
      end if;
      Ada.Text_Io.Put_Line ("?>");
      return;
    else
      -- Put element, attributes and children recursively
      Ada.Text_Io.Put (Indent);
      Ada.Text_Io.Put ("<" & Name);
      Put_Attributes (Elt, Level, 1 + Name'Length);
      if Children'Length = 0 then
        -- No child, terminate tag now
        Ada.Text_Io.Put ("/>");
      else
        Ada.Text_Io.Put (">");
        Prev_Is_Text := False;
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Recursive dump child
            if I = 1 or else not Prev_Is_Text then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              Ada.Text_Io.New_Line;
            end if;
            Put_Element (Children(I), Level + 1);
            if I = Children'Last then
              Ada.Text_Io.New_Line;
            end if;
            Prev_Is_Text := False;
          else
            -- Specific put text
            Ada.Text_Io.Put (Ctx.Get_Text (Children(I)));
            Prev_Is_Text := True;
          end if;
        end loop;
        -- Terminate tag after children
        if not Prev_Is_Text then
          Ada.Text_Io.Put (Indent);
        end if;
        Ada.Text_Io.Put ("</" & Name & ">");
      end if;
    end if;
  end Put_Element;

begin
  if Argument.Get_Nbre_Arg = 2
  and then Argument.Get_Parameter = "-dump" then
    Dump := True;
    Arg_Index := 2;
  elsif Argument.Get_Nbre_Arg /= 1 then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Argument.Get_Parameter = "-h"
  or else Argument.Get_Parameter = "--help" then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Parse
  Ctx.Parse (Argument.Get_Parameter(Arg_Index), Parse_Ok);
  if not Parse_Ok then
    Basic_Proc.Put_Line_Error (Xml_Parser.Get_Parse_Error_Message (Ctx));
    Basic_Proc.Set_Error_Exit_Code;
    Xml_Parser.Clean (Ctx);
    return;
  end if;
  Prologue := Ctx.Get_Prologue;
  Root := Ctx.Get_Root_Element;

  -- Dump / put
  if Dump then
    Ada.Text_Io.Put_Line ("Prologue:");
    Dump_Element (Prologue, 0);
    Ada.Text_Io.Put_Line ("Elements tree:");
    Dump_Element (Root, 0);
  else
    Put_Element (Prologue, Prologue_Level);
    Ada.Text_Io.New_Line;
    Put_Element (Root, 0);
  end if;
  Ctx.Clean;
exception
  when Error:others =>
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end T_Xml;

