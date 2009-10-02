with Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Argument_Parser, Xml_Parser.Generator, Normal, Basic_Proc,
     Text_Line, Sys_Calls, Parser;
procedure Xml_Checker is
  -- Current version
  Version : constant String := "V7.0";

  -- Ada.Strings.Unbounded and Ada.Exceptions re-definitions
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;
  function Asu_Tus (Str : String) return Asu_Us renames Asu.To_Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;

  procedure Ae_Re (E : in Ada.Exceptions.Exception_Id;
                   M : in String := "")
            renames Ada.Exceptions.Raise_Exception;

  -- Xml Parser context, elements and parsing parameters
  Ctx : Xml_Parser.Generator.Ctx_Type;
  Expand : Boolean;

  -- Argument error
  Arg_Error : exception;
  -- Abort loop of arguments
  Abort_Error : exception;

  -- Kind of ouput: None, dump or Xml_Generator
  type Output_Kind_List is (None, Dump, Gen, No_Comment);
  Output_Kind : Output_Kind_List;

  -- Warning detection
  Warnings : Boolean;

  -- Xml_Generator descriptor and format
  Format : Xml_Parser.Generator.Format_Kind_List;
  Width  : Positive;

  -- Flow of dump
  Out_Flow : Text_Line.File_Type;

  -- Dtd options
  Use_Dtd : Boolean;
  Dtd_File : Asu_Us;

  -- Maximum of options allowed
  Max_Opt : Natural;

  -- String list of unparsed entities
  Unparsed_Entities : Asu_Us;

  -- Program help
  procedure Usage is
    procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;
  begin
    Ple ("Usage: " & Argument.Get_Program_Name & "[ { <option> } ] [ { <file> } ]");
    Ple (" <option> ::= <silent> | <dump> | <raw> | <no_comment> | <warnings>");
    Ple ("            |  <width> | <one> | <expand> | <check_dtd> | <tree>");
    Ple ("            | <help> | <version>");
    Ple (" <silent>     ::= -s | --silent     -- No output, only exit code");
    Ple (" <dump>       ::= -d | --dump       -- Dump expanded Xml tree");
    Ple (" <raw>        ::= -r | --raw        -- Put all on one line");
    Ple (" <no_comment> ::= -n | --no_comment -- Skip comments");
    Ple (" <warnings>   ::= -w | --warnings   -- Check for warnings");
    Ple (" <width>      ::= -W <Width> | --width=<Width>");
    Ple ("                                    -- Put attributes up to Width");
    Ple (" <one>        ::= -1 | --one        -- Put one attribute per line");
    Ple (" <expand>     ::= -e | --expand     -- Expand general entities and");
    Ple ("                                    --  attributes with default");
    Ple (" <check_dtd>  ::= -c [ <Dtd> ] | --check_dtd=[<Dtd>]");
    Ple ("                                    -- Check vs a specific dtd or none");
    Ple (" <tree>       ::= -t | --tree       -- Build tree then dump it");
    Ple (" <help>       ::= -h | --help       -- Put this help");
    Ple (" <version>    ::= -v | --version    -- Put versions");
    Ple ("Always expands general entities in dump.");
    Ple ("All options except expand, no_comment, warnings and check_dtd are exclusive.");
    Ple ("No_comment not allowed on silent, dump and raw modes.");
    Ple ("Empty Dtd leads to skip check of comformance to DTD.");
    Ple ("Default is -w" & Xml_Parser.Generator.Default_Width'Img
                         & " on stdout.");
    Ple ("Building the tree is not recommended for big files.");
    Ple ("Please also consider increasing the process stack size (ulimit -s) to");
    Ple ("  avoid stack overflow and Storage_Error.");
  end Usage;

  -- The argument keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => ('s', Asu_Tus ("silent"), False, False),
    2 => ('d', Asu_Tus ("dump"), False, False),
    3 => ('r', Asu_Tus ("raw"), False, False),
    4 => ('W', Asu_Tus ("width"), False, True),
    5 => ('1', Asu_Tus ("one"), False, False),
    6 => ('h', Asu_Tus ("help"), False, False),
    7 => ('v', Asu_Tus ("version"), False, False),
    8 => ('e', Asu_Tus ("expand"), False, False),
    9 => ('n', Asu_Tus ("no_comment"), False, False),
   10 => ('c', Asu_Tus ("check_dtd"), False, True),
   11 => ('t', Asu_Tus ("tree"), False, False),
   12 => ('w', Asu_Tus ("warnings"), False, False)
   );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -------------------
  -- Dump xml tree --
  -------------------
  procedure Dump_Line (Node : in Xml_Parser.Node_Type) is
  begin
    Out_Flow.Put (Normal (Ctx.Get_Line_No (Node), 8, True, '0'));
  end Dump_Line;

  procedure Dump_Attributes (Elt : in Xml_Parser.Element_Type) is
    Attrs : constant Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      if not Attrs(I).Unparsed then
        Out_Flow.Put (" " & Asu.To_String (Attrs(I).Name
                       & "=" & Attrs(I).Value));
      else
        Out_Flow.Put (" " & Asu.To_String (Attrs(I).Name
                       & "=U=" & Attrs(I).Value));
        -- Maybe several entities here
        Asu.Append (Unparsed_Entities, Asu.To_String (Attrs(I).Value) & ' ');
      end if;
    end loop;
  end Dump_Attributes;

  procedure Dump_Element (Elt : in Xml_Parser.Element_Type;
                          Level : in Natural) is
    Children : Xml_Parser.Nodes_Array := Ctx.Get_Children (Elt);
    Indent : constant String (1 .. Level + 1) := (others => ' ');
    In_Tail : Boolean;
    use type Xml_Parser.Node_Kind_List, Asu_Us;
  begin
    In_Tail := Level = 1 and then String'(Ctx.Get_Name (Elt)) = "";
    if not In_Tail then
       -- Not the tail
      Dump_Line (Elt);
      Out_Flow.Put (Indent);
      Out_Flow.Put (Asu.To_String(Ctx.Get_Name (Elt)));
      if Ctx.Get_Nb_Attributes (Elt) /= 0 then
        Out_Flow.Put (" :" );
      end if;
      Dump_Attributes (Elt);
      Out_Flow.New_Line;
    end if;

    for I in Children'Range loop
      if I rem 2 = 0 then
        -- Test the individual get
        Children(I) := Ctx.Get_Child (Elt, I);
      end if;
      case Children(I).Kind is
        when Xml_Parser.Element =>
          -- Recursive dump child
          if not In_Tail then
            Dump_Element (Children(I), Level + 1);
          else
            Dump_Element (Children(I), 0);
          end if;
        when Xml_Parser.Text =>
          -- Put text
          Dump_Line (Children(I));
          if not In_Tail then
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put_Line (" =>" & Ctx.Get_Text (Children(I)) & "<=");
        when Xml_Parser.Pi =>
          -- Put Pi
          Dump_Line (Children(I));
          if not In_Tail then
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put (" <?" & Ctx.Get_Target (Children(I)));
          if Asu.Length (Ctx.Get_Pi (Children(I))) /= 0 then
            Out_Flow.Put (" " & Ctx.Get_Pi (Children(I)));
          end if;
          Out_Flow.Put_Line ("?>");
        when Xml_Parser.Comment =>
          -- Put Comment
          Dump_Line (Children(I));
          if not In_Tail then
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put_Line (" <!--" & Ctx.Get_Comment (Children(I)) & "-->");
      end case;
    end loop;
  end Dump_Element;

  -- Dump the unparsed entities characterisics
  procedure Dump_Unparsed_Entities is
    -- Iterator on string list of unparsed entities
    Iter : Parser.Iterator;
    Info : Xml_Parser.Unparsed_Entity_Info_Rec;
  begin
    Iter.Set (Asu_Ts (Unparsed_Entities));
    loop
      declare
        Entity : constant String := Iter.Next_Word;
      begin
        exit when Entity = "";
        Ctx.Get_Unparsed_Entity_Info (Entity, Info);
        Out_Flow.Put_Line ("Entity: " & Entity
                         & ", System_Id=" & Asu_Ts (Info.Entity_System_Id)
                         & ", Public_Id=" & Asu_Ts (Info.Entity_System_Id));
        Out_Flow.Put_Line (" Notation: " & Asu_Ts (Info.Notation_Name)
                         & ", System_Id=" & Asu_Ts (Info.Notation_System_Id)
                         & ", Public_Id=" & Asu_Ts (Info.Notation_Public_Id));
      end;
    end loop;
    Iter.Del;
  end Dump_Unparsed_Entities;

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

  -- Callback for "on the flow" display
  In_Prologue : Boolean := False;
  procedure Callback (Ctx  : in Xml_Parser.Ctx_Type;
                      Node : in Xml_Parser.Node_Update) is
    Indent : constant String (1 .. Node.Level + 1) := (others => ' ');
    use type Xml_Parser.Node_Kind_List, Xml_Parser.Attributes_Access, Asu_Us;
  begin
    if Output_Kind = None then
      return;
    elsif Output_Kind /= Dump then
      -- Use the Image of Xml_Parser.Generator
      Out_Flow.Put (Xml_Parser.Generator.Image (Ctx, Node, Format, Width));
      return;
    end if;
    -- Dump mode
    if not Node.Creation then
      return;
    end if;
    if not In_Prologue and then Node.In_Prologue then
      -- Entering prologue (new file)
      Out_Flow.Put_Line ("Prologue:");
    elsif In_Prologue and then not Node.In_Prologue then
      -- Leaving prologue
      Out_Flow.Put_Line ("Elements tree:");
    end if;
    In_Prologue := Node.In_Prologue;
    Out_Flow.Put (Normal (Node.Line_No, 8, True, '0'));
    if Node.Prev_Is_Text then
      Out_Flow.Put (" T");
    else
      Out_Flow.Put (" F");
    end if;
    Out_Flow.Put (Indent);
    case Node.Kind is
      when Xml_Parser.Element =>
        Out_Flow.Put (Asu.To_String(Node.Name));
      when Xml_Parser.Pi =>
        Out_Flow.Put ("<?" & Asu.To_String(Node.Name));
        if Asu.Length (Node.Value) /= 0 then
          Out_Flow.Put (" " & Asu.To_String(Node.Value));
        end if;
        Out_Flow.Put ("?>");
      when Xml_Parser.Comment =>
        Out_Flow.Put ("<!--" & Asu.To_String(Node.Name) & "-->");
      when Xml_Parser.Text =>
        Out_Flow.Put ("=>" & Asu.To_String(Node.Name) & "<=");
    end case;
    if Node.Attributes /= null then
      Out_Flow.Put (" :");
      for I in  Node.Attributes.all'Range loop
        if not Node.Attributes(I).Unparsed then
          Out_Flow.Put (" " & Asu.To_String (Node.Attributes(I).Name
                      & "=" & Node.Attributes(I).Value));
        else
          Out_Flow.Put (" " & Asu.To_String (Node.Attributes(I).Name
                      & "=U=" & Node.Attributes(I).Value));
          Asu.Append (Unparsed_Entities,
                      Asu.To_String (Node.Attributes(I).Value) & ' ');
        end if;
      end loop;
    end if;
    Out_Flow.New_Line;
  end Callback;
  Callback_Acc : Xml_Parser.Parse_Callback_Access;

  -- Parse a file provided as arg or stdin
  -- Retrieve comments and don't expand General Entities if output is Xml
  procedure Do_One (Index : in Natural;
                    Expand : in Boolean) is
    -- Parsing elements and charactericstics
    Prologue, Root : Xml_Parser.Element_Type;
    Parse_Ok : Boolean;
    use type Xml_Parser.Generator.Format_Kind_List,
             Xml_Parser.Parse_Callback_Access;
  begin
    Ctx.Parse (Get_File_Name (Index, False),
               Parse_Ok,
               Comments =>
                  (Output_Kind = Gen
                     and then Format /= Xml_Parser.Generator.Raw)
                  or else Output_Kind = Dump,
               Warnings => Warnings,
               Expand => Expand or else Output_Kind = Dump,
               Use_Dtd => Use_Dtd,
               Dtd_File => Asu_Ts (Dtd_File),
               Callback => Callback_Acc);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error ("Error in file "
                               & Get_File_Name (Index, True) & ": "
                               & Ctx.Get_Parse_Error_Message);
      Basic_Proc.Set_Error_Exit_Code;
      Ctx.Clean;
      return;
    end if;

    -- Done in callback mode
    if Callback_Acc /= null then
      if Output_Kind = Dump then
        Out_Flow.Put_Line ("Unparsed entities:");
        Dump_Unparsed_Entities;
      end if;
      if Output_Kind /= Dump
      and then Output_Kind /= None
      and then Format /= Xml_Parser.Generator.Raw then
         -- Last Line feeds
         Out_Flow.New_Line;
         Out_Flow.New_Line;
      end if;
      Ctx.Clean;
      return;
    end if;

    -- Dump / put
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) > 1 then
      -- Title except if stdin or if only one file
      Out_Flow.Put_Line (Get_File_Name (Index, True) & ":");
      Out_Flow.Flush;
    end if;
    if Output_Kind = Dump then
      Prologue := Ctx.Get_Prologue;
      Root := Ctx.Get_Root_Element;
      Out_Flow.Put_Line ("Prologue:");
      Dump_Element (Prologue, 0);
      Out_Flow.Put_Line ("Elements tree:");
      Dump_Element (Root, 0);
      Out_Flow.Put_Line ("Unparsed entities:");
      Dump_Unparsed_Entities;
      Out_Flow.Flush;
    elsif Output_Kind = Gen or else Output_Kind = No_Comment then
      Xml_Parser.Generator.Put (Ctx, Xml_Parser.Generator.Stdout,
                                Format, Width);
      -- Ctx.Put (Xml_Parser.Generator.Stdout, Format, Width);
    end if;
    Ctx.Clean;
  exception
    when Xml_Parser.File_Error =>
      Basic_Proc.Put_Line_Error ("IO Error reading file "
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

  use type Xml_Parser.Generator.Format_Kind_List;
  use type Asu_Us;
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
    Out_Flow.Put_Line ("Parser version:      " & Xml_Parser.Version);
    Out_Flow.Put_Line ("Generator version:   " & Xml_Parser.Generator.Version);
    Out_Flow.Put_Line ("Xml_Checker version: " & Version);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Default behavior
  Output_Kind := Gen;
  Width := Xml_Parser.Generator.Default_Width;
  Format := Xml_Parser.Generator.Default_Format;
  Warnings := False;
  Expand := False;
  Use_Dtd := True;
  Dtd_File := Asu_Null;
  Callback_Acc := null;
  -- Get Expand option and chek max of options
  -- Only one option, one more if Expand, one more if no_comment
  --  one more if check_dtd, one more if ontheflow, one more if warnings
  Max_Opt := 1;
  if Arg_Dscr.Is_Set (8) then
    Expand := True;
    Max_Opt := 2;
  end if;
  if Arg_Dscr.Is_Set (9) then
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (10) then
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (11) then
    Max_Opt := Max_Opt + 1;
  else
    Callback_Acc := Callback'Unrestricted_Access;
  end if;
  if Arg_Dscr.Get_Number_Keys > Max_Opt then
    Ae_Re (Arg_Error'Identity, "Too many options");
  end if;
  if Arg_Dscr.Is_Set (12) then
    Max_Opt := Max_Opt + 1;
  end if;

  -- Get format info
  if Arg_Dscr.Is_Set (1) then
    -- Silent
    Output_Kind := None;
  elsif Arg_Dscr.Is_Set (2) then
    -- Dump
    Output_Kind := Dump;
  elsif Arg_Dscr.Is_Set (3) then
    Format := Xml_Parser.Generator.Raw;
  end if;

  if Output_Kind = Gen then
    -- Options only significant in normal mode
    if Arg_Dscr.Is_Set (4) then
      -- -w <Width>
      Format := Xml_Parser.Generator.Fill_Width;
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
      Format := Xml_Parser.Generator.One_Per_Line;
    end if;
  end if;

  if Arg_Dscr.Is_Set (9) then
    if Output_Kind = Gen and then Format /= Xml_Parser.Generator.Raw then
      Output_Kind := No_Comment;
    else
      Ae_Re (Arg_Error'Identity, "Incompatible options");
    end if;
  end if;

  if Arg_Dscr.Is_Set (10) then
    Dtd_File := Asu_Tus (Arg_Dscr.Get_Option (10));
    if Dtd_File = Asu_Null then
      -- If option set with empty dtd => no check
      Use_Dtd := False;
    end if;
  end if;

  if Arg_Dscr.Is_Set (12) then
    Warnings := True;
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
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    -- Unexpected or internal error
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end Xml_Checker;

