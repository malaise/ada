with Ada.Text_Io, Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Xml_Parser, Normal, Basic_Proc;
procedure Xml_Checker is
  Ctx : Xml_Parser.Ctx_Type;
  Prologue, Root : Xml_Parser.Element_Type;
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  type Output_Kind_List is (Xml, Dump, Silent);
  Output_Kind : Output_Kind_List := Xml;
  Arg_Index : Natural;
  Parse_Ok : Boolean;
  Arg_Error : exception;

  Doctype_Internal : Boolean := False;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name
        & " [ -d | --dump | -s | --silent ] [ <xml_file> ]");
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
                            Offset : in Positive;
                            One_Per_Line : in Boolean := True) is
    Attrs : Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    Indent : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      if I /= 1 and then One_Per_Line then
        -- Indent
        Ada.Text_Io.Put (Indent);
      end if;
      Ada.Text_Io.Put (" " & Asu.To_String (Attrs(I).Name
                     & "=""" & Attrs(I).Value) & """");
      if I /= Attrs'Last and then One_Per_Line then
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
    Indent1 : constant String := Indent & "  ";
    Doctype_Name, Doctype_File : Asu_Us;
    Prev_Is_Text : Boolean;
    use type Xml_Parser.Node_Kind_List, Asu_Us;
  begin
    if Level = Prologue_Level then
      if Name /= "" then
        -- A prologue
        -- Put the xml directive with attributes
        Ada.Text_Io.Put ("<?" & Name);
        Put_Attributes (Elt, 0, 2 + Name'Length, False);
        Ada.Text_Io.Put_Line ("?>");
        -- Put DOCTYPE if any
        Ctx.Get_Doctype (Doctype_Name, Doctype_File, Doctype_Internal);
        if Doctype_Name /= Asu_Null then
          Ada.Text_Io.Put ("<!DOCTYPE " & Asu.To_String (Doctype_Name));
          if Doctype_File /= Asu_Null then
            Ada.Text_Io.Put (" SYSTEM """ & Asu.To_String (Doctype_File)
                           & """");
          end if;
          Ada.Text_Io.Put_Line (">");
        end if;
        -- Put prologue PIs and comments
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Put PIs
            Put_Element (Children(I), Prologue_Instruction_Level);
          elsif Children(I).Kind = Xml_Parser.Comment then
            -- Put Comments of prologue
            Ada.Text_Io.Put_Line ("<!--" & Ctx.Get_Comment (Children(I))
                                & "-->");
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
              Put_Element (Children(I), Level + 1);
            elsif I = 1 then
              -- First Child
              Put_Element (Children(I), Level + 1);
            else
              -- Child element following text
              --  we bet that it has no child itself, so no New_Line nor Indent
              Put_Element (Children(I), 0);
            end if;
            if I = Children'Last then
              Ada.Text_Io.New_Line;
            end if;
            Prev_Is_Text := False;
          elsif Children(I).Kind = Xml_Parser.Text then
            -- Specific put text
            Ada.Text_Io.Put (Ctx.Get_Text (Children(I)));
            Prev_Is_Text := True;
          else
            -- Comment
            if I = 1 or else not Prev_Is_Text then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              Ada.Text_Io.New_Line;
            end if;
            Ada.Text_Io.Put (Indent1);
            Ada.Text_Io.Put ("<!--" & Ctx.Get_Comment (Children(I)) & "-->");
            if I = Children'Last then
              Ada.Text_Io.New_Line;
            end if;
            Prev_Is_Text := False;
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

  function Get_File_Name return String is
  begin
    if Argument.Get_Nbre_Arg = Arg_Index then
      return Argument.Get_Parameter(Arg_Index);
    else
      return "";
    end if;
  end Get_File_Name;

begin
  -- Parse options
  Arg_Index := 1;
  -- At most one option, and possible one file
  if Argument.Get_Nbre_Arg > 2 then
    raise Arg_Error;
  end if;
  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter = "-d"
    or else Argument.Get_Parameter = "--dump") then
    Output_Kind := Dump;
    Arg_Index := 2;
  elsif Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter = "-s"
    or else Argument.Get_Parameter = "--silent") then
    Output_Kind := Silent;
    Arg_Index := 2;
  elsif Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help") then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  if Argument.Get_Nbre_Arg > Arg_Index then
    -- 2 args but first is not a known option
    raise Arg_Error;
  end if;

  -- Parse file provided as arg or stdin
  Ctx.Parse (Get_File_Name, Parse_Ok, Comments => Output_Kind = Xml);
  if not Parse_Ok then
    Basic_Proc.Put_Line_Error ("Error in file " & Get_File_Name & ": "
                             & Xml_Parser.Get_Parse_Error_Message (Ctx));
    Basic_Proc.Set_Error_Exit_Code;
    Xml_Parser.Clean (Ctx);
    return;
  end if;
  Prologue := Ctx.Get_Prologue;
  Root := Ctx.Get_Root_Element;

  -- Dump / put
  if Output_Kind = Xml then
    Put_Element (Prologue, Prologue_Level);
    Ada.Text_Io.New_Line;
    Put_Element (Root, 0);
    Ada.Text_Io.New_Line;
  elsif Output_Kind = Dump then
    Ada.Text_Io.Put_Line ("Prologue:");
    Dump_Element (Prologue, 0);
    Ada.Text_Io.Put_Line ("Elements tree:");
    Dump_Element (Root, 0);
  end if;
  Ctx.Clean;

  if Output_Kind = Xml and then Doctype_Internal then
    Ada.Text_Io.Flush;
    Basic_Proc.Put_Line_Error (
     "Warning, Doctype internal subset is parsed but not displayed.");
  end if;
exception
  when Xml_Parser.File_Error =>
    Basic_Proc.Put_Line_Error ("Error reading file "
      & Get_File_Name & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Arg_Error =>
    Basic_Proc.Put_Line_Error ("Error, invalid arguments.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end Xml_Checker;

