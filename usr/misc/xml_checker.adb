with Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Argument_Parser, Xml_Parser, Normal, Basic_Proc, Xml_Generator,
     Text_Line, Sys_Calls;
procedure Xml_Checker is
  -- Current version
  Version : constant String := "V2.2";

  -- Ada.Strings.Unbounded and Ada.Exceptions re-definitions
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;
  Asu_Null :  constant Asu_Us := Asu.Null_Unbounded_String;

  procedure Ae_Re (E : in Ada.Exceptions.Exception_Id;
                   M : in String := "")
            renames Ada.Exceptions.Raise_Exception;

  -- Xml Parser context, elements and parsing parameters
  Ctx : Xml_Parser.Ctx_Type;
  Expand : Boolean;

  -- Argument error
  Arg_Error : exception;
  -- Error on Xml directive!
  Internal_Error : exception;
  -- Abort loop of arguments
  Abort_Error : exception;

  -- Kind of ouput: None, dump or Xml_Generator
  type Output_Kind_List is (None, Dump, Gen);
  Output_Kind : Output_Kind_List;

  -- Xml_Generator descriptor and format
  Gen_Dscr : Xml_Generator.Xml_Dscr_Type;
  Format : Xml_Generator.Format_Kind_List;
  Width  : Positive;

  -- Flow of dump
  Out_Flow : Text_Line.File_Type;

  -- Program help
  procedure Usage is
    procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;
  begin
    Ple ("Usage: " & Argument.Get_Program_Name & "[ { <option> } ] [ { <file> } ]");
    Ple (" <option> ::= <silent> | <dump> | <raw> | <width> | <one> | <expand>");
    Ple ("            | <help> | <version>");
    Ple (" <silent>  ::= -s | --silent    -- No output, only exit code");
    Ple (" <dump>    ::= -d | --dump      -- Dump expanded Xml tree");
    Ple (" <raw>     ::= -r | --raw       -- Put all on one line");
    Ple (" <width>   ::= -w <Width> | --width=<Width>");
    Ple ("                                -- Put attributes up to Width");
    Ple (" <one>     ::= -1 | --one       -- Put one attribute per line");
    Ple (" <expand>  ::= -e | --expand    -- Expand general entities");
    Ple (" <help>    ::= -h | --help      -- Put this help");
    Ple (" <version> ::= -v | --version   -- Put versions");
    Ple ("Always expands general entities in dump.");
    Ple ("All options except expand are exclusive.");
    Ple ("Default is -w" & Xml_Generator.Default_Width'Img & " on stdin.");
  end Usage;

  -- The argument keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   1 => ('s', Asu_Tus ("silent"), False, False),
   2 => ('d', Asu_Tus ("dump"), False, False),
   3 => ('r', Asu_Tus ("raw"), False, False),
   4 => ('w', Asu_Tus ("width"), False, True),
   5 => ('1', Asu_Tus ("one"), False, False),
   6 => ('h', Asu_Tus ("help"), False, False),
   7 => ('v', Asu_Tus ("version"), False, False),
   8 => ('e', Asu_Tus ("expand"), False, False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -------------------
  -- Dump xml tree --
  -------------------
  procedure Dump_Line (Node : in Xml_Parser.Node_Type) is
  begin
    Out_Flow.Put (Normal (Ctx.Get_Line_No (Node), 5, True, '0'));
  end Dump_Line;

  procedure Dump_Attributes (Elt : in Xml_Parser.Element_Type) is
    Attrs : Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      Out_Flow.Put (" " & Asu.To_String (Attrs(I).Name
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
    Out_Flow.Put (Indent);
    Out_Flow.Put (Asu.To_String(Ctx.Get_Name (Elt)));
    if Ctx.Get_Nb_Attributes (Elt) /= 0 then
      Out_Flow.Put (" :" );
    end if;
    Dump_Attributes (Elt);
    Out_Flow.New_Line;
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
        Out_Flow.Put (Indent);
        Out_Flow.Put_Line (" =>" & Ctx.Get_Text (Children(I)) & "<=");
      end if;
    end loop;
  end Dump_Element;

  ----------------------
  -- Put the xml text --
  ----------------------
  -- Copy Ctx prologue in Gen_Dscr
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
      Gen_Dscr.Reset (0, 1, Root_Name);
    elsif Asu_Ts (Attrs(1).Value) = "1.0" then
      Gen_Dscr.Reset (1, 0, Root_Name);
    else
      Gen_Dscr.Reset (1, 1, Root_Name);
    end if;

    -- Add Xml attributes
    for I in 2 .. Attrs'Last loop
      if Asu_Ts (Attrs(I).Name) = "encoding" then
        Gen_Dscr.Set_Encoding (Asu_Ts (Attrs(I).Value));
      elsif Asu_Ts (Attrs(I).Name) = "standalone" then
        if Asu_Ts (Attrs(I).Value) = "yes" then
          Gen_Dscr.Set_Standalone (True);
        elsif Asu_Ts (Attrs(I).Value) = "no" then
          Gen_Dscr.Set_Standalone (False);
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
          Gen_Dscr.Add_Pi (Asu_Ts (Ctx.Get_Name (Children(I))),
                       Asu_Ts (Ctx.Get_Text (Text)));
        when Xml_Parser.Comment =>
          Gen_Dscr.Add_Comment (Ctx.Get_Comment (Children(I)) );
        when Xml_Parser.Text =>
          -- The doctype: (empty text);
          Ctx.Get_Doctype (Doctype_Name, Doctype_Public, Doctype_Id,
                           Doctype_File, Doctype_Internal);
          Gen_Dscr.Set_Doctype (Asu_Ts (Doctype_Name), Doctype_Public,
                                Asu_Ts (Doctype_Id), Asu_Ts (Doctype_File),
                                Asu_Ts (Doctype_Internal));
      end case;
    end loop;
  end Copy_Prologue;

  -- Copy Ctx element children in Gen_Dscr
  procedure Copy_Element (Element : in Xml_Parser.Element_Type) is
    Attrs : constant Xml_Parser.Attributes_Array
          := Ctx.Get_Attributes (Element);
    Nb_Children : constant Natural := Ctx.Get_Nb_Children (Element);
    Child : Xml_Parser.Node_Type;
  begin
    -- Add attributes
    for I in Attrs'Range loop
      Gen_Dscr.Add_Attribute (Asu_Ts (Attrs(I).Name), Asu_Ts (Attrs(I).Value));
    end loop;

    -- Copy children
    for I in 1 .. Nb_Children loop
      Child := Ctx.Get_Child (Element, I);
      case Child.Kind is
        when Xml_Parser.Element =>
          Gen_Dscr.Add_Child (Ctx.Get_Name (Child), Xml_Generator.Element);
          -- Recursively
          Copy_Element (Child);
          Gen_Dscr.Move_Father;
        when Xml_Parser.Text =>
          Gen_Dscr.Add_Child (Ctx.Get_Text (Child), Xml_Generator.Text);
          Gen_Dscr.Move_Father;
        when Xml_Parser.Comment =>
          Gen_Dscr.Add_Child (Ctx.Get_Comment (Child), Xml_Generator.Comment);
          Gen_Dscr.Move_Father;
      end case;
    end loop;
  end Copy_Element;

  -- Return a file name
  function Get_File_Name (Occurence : in Natural;
                          For_Message : in Boolean) return String is
  begin
    if Occurence = 0 then
      if For_Message then
        return "Stdin";
      else
        return Xml_Parser.Stdin;
      end if;
    else
      return Arg_Dscr.Get_Option (No_Key_Index, Occurence);
    end if;
  end Get_File_Name;

  -- Parse a file provided as arg or stdin
  -- Retrieve comments and don't expand General Entities if output is Xml
  procedure Do_One (Index : in Natural;
                    Expand : in Boolean) is
    -- Parsing elements and charactericstics
    Prologue, Root : Xml_Parser.Element_Type;
    Parse_Ok : Boolean;
  begin
    Ctx.Parse (Get_File_Name (Index, False),
               Parse_Ok, Comments => Output_Kind = Gen,
               Expand_Entities => Expand or else Output_Kind = Dump);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error ("Error in file "
                               & Get_File_Name (Index, True) & ": "
                               & Xml_Parser.Get_Parse_Error_Message (Ctx));
      Basic_Proc.Set_Error_Exit_Code;
      Xml_Parser.Clean (Ctx);
      return;
    end if;
    Prologue := Ctx.Get_Prologue;
    Root := Ctx.Get_Root_Element;

    -- Dump / put
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) > 1 then
      -- Title except if stdin or if only one file
      Out_Flow.Put_Line (Get_File_Name (Index, True) & ":");
      Out_Flow.Flush;
    end if;
    if Output_Kind = Dump then
      Out_Flow.Put_Line ("Prologue:");
      Dump_Element (Prologue, 0);
      Out_Flow.Put_Line ("Elements tree:");
      Dump_Element (Root, 0);
      Out_Flow.Flush;
    elsif Output_Kind = Gen then
      Copy_Prologue (Prologue, Root);
      Copy_Element (Root);
      Gen_Dscr.Put (Xml_Generator.Stdout, Format, Width);
    end if;
    Ctx.Clean;
  exception
    when Xml_Parser.File_Error =>
      Basic_Proc.Put_Line_Error ("Error reading file "
        & Get_File_Name (Index, True) & ".");
      raise Abort_Error;
  end Do_One;

  -- Close output flow
  procedure Close is
  begin
    if Out_Flow.Is_Open then
      Out_Flow.Close;
    end if;
  end Close;

begin
  -- Open output flow
  Out_Flow.Open (Text_Line.Out_File, Sys_Calls.Stdout);

  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Basic_Proc.Put_Line_Error ("Error: " & Arg_Dscr.Get_Error & ".");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  -- Any path/file spec must be after options
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Ae_Re (Arg_Error'Identity, "File name(s) must appear after option(s)");
  end if;

  -- Process help and version options
  if Arg_Dscr.Is_Set (6) then
    -- No file nor other option
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 0
    or else Arg_Dscr.Get_Number_Keys > 1 then
      Ae_Re (Arg_Error'Identity, "Too many options");
    end if;
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Arg_Dscr.Is_Set (7) then
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 0
    or else Arg_Dscr.Get_Number_Keys > 1 then
      Ae_Re (Arg_Error'Identity, "Too many options");
    end if;
    Out_Flow.Put_Line ("Xml_Checker version: " & Version);
    Out_Flow.Put_Line ("Parser version:      " & Xml_Parser.Version);
    Out_Flow.Put_Line ("Generator version:   " & Xml_Generator.Version);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Default behavior
  Output_Kind := Gen;
  Width := Xml_Generator.Default_Width;
  Format := Xml_Generator.Default_Format;
  Expand := False;
  -- Get Expand option
  if Arg_Dscr.Is_Set (8) then
    Expand := True;
  end if;

  -- Only 1 option (or at most 2 if Expand)
  if (not Expand and then Arg_Dscr.Get_Number_Keys > 1)
  or else (Expand and then Arg_Dscr.Get_Number_Keys > 2) then
    Ae_Re (Arg_Error'Identity, "Too many options");
  end if;

  -- Get format info
  if Arg_Dscr.Is_Set (1) then
    -- Silent
    Output_Kind := None;
  elsif Arg_Dscr.Is_Set (2) then
    -- Dump
    Output_Kind := Dump;
  elsif Arg_Dscr.Is_Set (3) then
    Format := Xml_Generator.Raw;
  elsif Arg_Dscr.Is_Set (4) then
    -- -w <Width>
    Format := Xml_Generator.Fill_Width;
    if Arg_Dscr.Get_Option (4) = "" then
      Ae_Re (Arg_Error'Identity, "Width value is mandatory with -w");
    end if;
    begin
      Width := Positive'Value (Arg_Dscr.Get_Option (4));
    exception
      when others =>
        Ae_Re (Arg_Error'Identity, "Invalid Width value "
             & Arg_Dscr.Get_Option (4));
    end;
  elsif Arg_Dscr.Is_Set (5) then
    Format := Xml_Generator.One_Per_Line;
  end if;

  -- Process arguments
  if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) = 0 then
    Do_One (0, Expand);
  else
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (No_Key_Index) loop
      Do_One (I, Expand);
      if I /= Arg_Dscr.Get_Nb_Occurences (No_Key_Index) then
        Out_Flow.New_Line;
        Out_Flow.Flush;
      end if;
    end loop;
  end if;

  Close;

exception
  when Error:Arg_Error =>
    -- Argument error
    Basic_Proc.Put_Line_Error ("Error "
         & Ada.Exceptions.Exception_Message(Error) & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Abort_Error =>
    -- Error already put while parsing file
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    -- Unexpected or internal error
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end Xml_Checker;

