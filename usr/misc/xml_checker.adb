with Ada.Text_Io, Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Xml_Parser, Normal, Basic_Proc, Xml_Generator;
procedure Xml_Checker is
  Ctx : Xml_Parser.Ctx_Type;
  Prologue, Root : Xml_Parser.Element_Type;
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  type Output_Kind_List is (Xml, Dump, Silent);
  Output_Kind : Output_Kind_List := Xml;
  Arg_Index : Natural;
  Parse_Ok : Boolean;
  Arg_Error : exception;

  Dscr : Xml_Generator.Xml_Dscr_Type;

  Internal_Error : exception;

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

  -- Copy Ctx prologue in Dscr
  procedure Copy_Prologue (Prologue : in Xml_Parser.Element_Type;
                           Root : in Xml_Parser.Element_Type) is
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Prologue);
    Children : constant Xml_Parser.Nodes_Array
             := Ctx.Get_Children (Prologue);
    Doctype_Name, Doctype_Id, Doctype_File, Doctype_Internal : Asu_Us;
    Doctype_Public : Boolean;
    Root_Name : constant String := Ctx.Get_Name (Root);
    Text : Xml_Parser.Text_Type;
    use type Asu_Us;
  begin
    -- Reset Dscr, provide version and Root name
    if Attrs'Length = 0 then
      -- No xml directive
      Dscr.Reset (0, 1, Root_Name);
    elsif Asu_Ts (Attrs(1).Value) = "1.0" then
      Dscr.Reset (1, 0, Root_Name);
    else
      Dscr.Reset (1, 1, Root_Name);
    end if;

    -- Add Xml attributes
    for I in 2 .. Attrs'Last loop
      if Asu_Ts (Attrs(I).Name) = "encoding" then
        Dscr.Set_Encoding (Asu_Ts (Attrs(I).Value));
      elsif Asu_Ts (Attrs(I).Name) = "standalone" then
        if Asu_Ts (Attrs(I).Value) = "yes" then
          Dscr.Set_Standalone (True);
        elsif Asu_Ts (Attrs(I).Value) = "no" then
          Dscr.Set_Standalone (False);
        else
          raise Internal_Error;
        end if;
      else
        raise Internal_Error;
      end if;
    end loop;

    -- Fill prologue with PIs, comments, doctype
    for I in Children'Range loop
      case Children(I).Kind is
        when Xml_Parser.Element =>
          -- A PI: Element with one Text child
          Text := Ctx.Get_Child (Children(I), 1);
          Dscr.Add_Pi (Asu_Ts (Ctx.Get_Name (Children(I))),
                       Asu_Ts (Ctx.Get_Text (Text)));
        when Xml_Parser.Comment =>
          Dscr.Add_Comment (Ctx.Get_Comment (Children(I)) );
        when Xml_Parser.Text =>
          -- The doctype: (empty text);
          Ctx.Get_Doctype (Doctype_Name, Doctype_Public, Doctype_Id,
                           Doctype_File, Doctype_Internal);
          if Doctype_File /= Asu_Null then
            -- A PUBLIC <id> <uri> or SYSTEM <uri>
            if Doctype_Public then
              Doctype_File := "PUBLIC """ & Doctype_Id
                            & """ """ & Doctype_File & """";
            else
              Doctype_File := "SYSTEM """ & Doctype_File & """";
            end if;
            if Doctype_Internal /= Asu_Null then
              Doctype_File := Doctype_File & " ";
            end if;
          end if;
          Dscr.Set_Doctype (Asu_Ts (Doctype_Name),
              Asu_Ts (Doctype_File & Doctype_Internal));
      end case;
    end loop;
  end Copy_Prologue;

  -- Copy Ctx element children in Dscr
  procedure Copy_Element (Element : in Xml_Parser.Element_Type) is
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Element);
    Nb_Children : constant Natural := Ctx.Get_Nb_Children (Element);
    Child : Xml_Parser.Node_Type;
  begin
    -- Add attributes
    for I in Attrs'Range loop
      Dscr.Add_Attribute (Asu_Ts (Attrs(I).Name), Asu_Ts (Attrs(I).Value));
    end loop;

    -- Copy children
    for I in 1 .. Nb_Children loop
      Child := Ctx.Get_Child (Element, I);
      case Child.Kind is
        when Xml_Parser.Element =>
          Dscr.Add_Child (Ctx.Get_Name (Child), Xml_Generator.Element);
          -- Recursively
          Copy_Element (Child);
          Dscr.Move_Father;
        when Xml_Parser.Text =>
          Dscr.Add_Child (Ctx.Get_Text (Child), Xml_Generator.Text);
          Dscr.Move_Father;
        when Xml_Parser.Comment =>
          Dscr.Add_Child (Ctx.Get_Comment (Child), Xml_Generator.Comment);
          Dscr.Move_Father;
      end case;
    end loop;
  end Copy_Element;
  
  -- Current file name
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
    Copy_Prologue (Prologue, Root);
    Copy_Element (Root);
    Dscr.Put (True);
    Ada.Text_Io.New_Line;
  elsif Output_Kind = Dump then
    Ada.Text_Io.Put_Line ("Prologue:");
    Dump_Element (Prologue, 0);
    Ada.Text_Io.Put_Line ("Elements tree:");
    Dump_Element (Root, 0);
  end if;
  Ctx.Clean;

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

