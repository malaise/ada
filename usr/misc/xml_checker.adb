with Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Argument_Parser, Xml_Parser.Generator, Normal, Basic_Proc,
     Text_Line, Sys_Calls;
procedure Xml_Checker is
  -- Current version
  Version : constant String := "V4.0";

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
  -- Error on Xml directive!
  Internal_Error : exception;
  -- Abort loop of arguments
  Abort_Error : exception;

  -- Kind of ouput: None, dump or Xml_Generator
  type Output_Kind_List is (None, Dump, Gen, No_Comment);
  Output_Kind : Output_Kind_List;

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

  -- Program help
  procedure Usage is
    procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;
  begin
    Ple ("Usage: " & Argument.Get_Program_Name & "[ { <option> } ] [ { <file> } ]");
    Ple (" <option> ::= <silent> | <dump> | <raw> | <no_comment> | <width> | <one>");
    Ple ("            | <expand> | <check_dtd> | <help> | <version>");
    Ple (" <silent>     ::= -s | --silent     -- No output, only exit code");
    Ple (" <dump>       ::= -d | --dump       -- Dump expanded Xml tree");
    Ple (" <raw>        ::= -r | --raw        -- Put all on one line");
    Ple (" <no_comment> ::= -n | --no_comment -- Skip comments");
    Ple (" <width>      ::= -w <Width> | --width=<Width>");
    Ple ("                                    -- Put attributes up to Width");
    Ple (" <one>        ::= -1 | --one        -- Put one attribute per line");
    Ple (" <expand>     ::= -e | --expand     -- Expand general entities and");
    Ple ("                                    --  attributes with default");
    Ple (" <check_dtd>  ::= -c <Dtd> | --check_dtd=<Dtd>");
    Ple ("                                    -- Check vs a specific dtd or none");
    Ple (" <help>       ::= -h | --help       -- Put this help");
    Ple (" <version>    ::= -v | --version    -- Put versions");
    Ple ("Always expands general entities in dump.");
    Ple ("All options except expand, no_comment and check_dtd are exclusive.");
    Ple ("No_comment not allowed on silent, dump and raw modes.");
    Ple ("Empty Dtd leads to skip check of comformance to DTD.");
    Ple ("Default is -w" & Xml_Parser.Generator.Default_Width'Img
                         & " on stdout.");
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
    8 => ('e', Asu_Tus ("expand"), False, False),
    9 => ('n', Asu_Tus ("no_comment"), False, False),
   10 => ('c', Asu_Tus ("check_dtd"), False, True)
   );
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
    use type Xml_Parser.Generator.Format_Kind_List;
  begin
    Ctx.Parse (Get_File_Name (Index, False),
               Parse_Ok,
               Comments => Output_Kind = Gen
                 and then Format /= Xml_Parser.Generator.Raw,
               Expand => Expand or else Output_Kind = Dump,
               Use_Dtd => Use_Dtd,
               Dtd_File => Asu_Ts (Dtd_File));
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error ("Error in file "
                               & Get_File_Name (Index, True) & ": "
                               & Ctx.Get_Parse_Error_Message);
      Basic_Proc.Set_Error_Exit_Code;
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
  Expand := False;
  Use_Dtd := True;
  Dtd_File := Asu_Null;
  -- Get Expand option and chek max of options
  -- Only 1 option, one more if Expand, one more if no_comment
  --  one more if check_dtd
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
  if Arg_Dscr.Get_Number_Keys > Max_Opt then
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
      Ae_Re (Arg_Error'Identity, "Too many options");
    end if;
  end if;

  if Arg_Dscr.Is_Set (10) then
    Dtd_File := Asu_Tus (Arg_Dscr.Get_Option (10));
    if Dtd_File = Asu_Null then
      -- If option set with empty dtd => no check
      Use_Dtd := False;
    end if;
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

