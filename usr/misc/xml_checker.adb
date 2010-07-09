with Ada.Exceptions;
with As.U; use As.U;
with Argument, Argument_Parser, Xml_Parser.Generator, Normal, Basic_Proc,
     Text_Line, Sys_Calls, Parser;
procedure Xml_Checker is
  -- Current version
  Version : constant String := "V12.0";

  procedure Ae_Re (E : in Ada.Exceptions.Exception_Id;
                   M : in String := "")
            renames Ada.Exceptions.Raise_Exception;

  -- Xml Parser context, elements and parsing parameters
  Ctx : Xml_Parser.Generator.Ctx_Type;

  -- "Keep" options
  Keep_Comments, Keep_Expand, Keep_Cdata : Boolean;
  Cdata_Policy : Xml_Parser.Cdata_Policy_List;

  -- Are these options set
  Keep_Expand_Set : Boolean := False;
  Keep_Comments_Set : Boolean := False;
  Keep_Cdata_Set : Boolean := False;

  -- Argument error
  Arg_Error : exception;
  -- Abort loop of arguments
  Abort_Error : exception;

  -- Kind of ouput: None, dump, Xml_Generator or Canonical
  type Output_Kind_List is (None, Dump, Gen, Canon);
  Output_Kind : Output_Kind_List;

  -- Xml_Generator descriptor and format
  Format : Xml_Parser.Generator.Format_Kind_List;
  Width  : Natural;

  -- Normalize text
  Normalize : Boolean;

  -- Flow of dump
  Out_Flow : Text_Line.File_Type;

  -- Dtd options
  Use_Dtd : Boolean;
  Dtd_File : Asu_Us;

  -- Warning detection
  procedure Warning (Ctx : in  Xml_Parser.Ctx_Type; Msg : in String) is
  pragma Unreferenced (Ctx);
  begin
    Basic_Proc.Put_Line_Error (Msg);
  end Warning;
  Warnings : Xml_Parser.Warning_Callback_Access;

  -- Maximum of options allowed
  Max_Opt : Natural;

  -- String list of unparsed entities
  Unparsed_Entities : Asu_Us;

  -- Program help
  procedure Usage is
    procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;
  begin
    Ple ("Usage: " & Argument.Get_Program_Name & "[ { <option> } ] [ { <file> } ]");
    Ple (" <option> ::= <silent> | <dump> | <raw> | <warnings> |  <width> | <one>");
    Ple ("            | <keep> | <check_dtd> | <tree> | <canonical>");
    Ple ("            | <help> | <version>");
    Ple (" <silent>     ::= -s | --silent     -- No output, only exit code");
    Ple (" <dump>       ::= -d | --dump       -- Dump expanded Xml tree");
    Ple (" <raw>        ::= -r | --raw        -- Put all on one line");
    Ple (" <warnings>   ::= -w | --warnings   -- Check for warnings");
    Ple (" <width>      ::= -W <Width> | --width=<Width>");
    Ple ("                                    -- Put attributes up to Width");
    Ple (" <one>        ::= -1 | --one        -- Put one attribute per line");
    Ple (" <keep>       ::= -k [e|c|d|a]  |  --keep[=[expanded|comments|cdata|all]]");
    Ple ("                                    -- Keep un-expanded general entities");
    Ple ("                                    --  and attributes with default");
    Ple ("                                    -- Keep comments");
    Ple ("                                    -- Keep CDATA sections");
    Ple ("                                    -- Keep all (default)");
    Ple (" <check_dtd>  ::= -c [ <Dtd> ] | --check_dtd=[<Dtd>]");
    Ple ("                                    -- Check vs a specific dtd or none");
    Ple (" <tree>       ::= -t | --tree       -- Build tree then dump it");
    Ple (" <canonical>  ::= -C | --canonical  -- Canonicalize xml");
    Ple (" <help>       ::= -h | --help       -- Put this help");
    Ple (" <version>    ::= -v | --version    -- Put versions");
    Ple ("Always expands general entities in dump.");
    Ple ("All options except keep, check_dtd, warnings and tree are exclusive.");
    Ple ("Keep not allowed on Dump mode, Dump => keep all.");
    Ple ("Canonical only allows dtd, warnings and comment options.");
    Ple ("Empty Dtd leads to skip check of comformance to DTD.");
    Ple ("Default is -W" & Xml_Parser.Generator.Default_Width'Img
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
    8 => ('k', Asu_Tus ("keep"), True, True),
    9 => ('c', Asu_Tus ("check_dtd"), False, True),
   10 => ('t', Asu_Tus ("tree"), False, False),
   11 => ('w', Asu_Tus ("warnings"), False, False),
   12 => ('C', Asu_Tus ("canonical"), False, False)
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
    In_Mixed : Boolean;
    In_Tail : Boolean;
    use type Xml_Parser.Node_Kind_List, Asu_Us;
    procedure Put_Mixed (N : in Xml_Parser.Node_Type;
                         Inm : in Boolean) is
    begin
      if N.Kind = Xml_Parser.Element
      and then Ctx.Get_Is_Mixed (N) then
        Out_Flow.Put (" M");
      else
        Out_Flow.Put (" -");
      end if;
      -- Get Father's Is_Mixed
      if Inm then
        Out_Flow.Put (" M");
      else
        Out_Flow.Put (" -");
      end if;
    end Put_Mixed;

  begin
    In_Tail := Level = 1 and then String'(Ctx.Get_Name (Elt)) = "";
    if In_Tail then
      Out_Flow.Put_Line ("Tail:");
    else
       -- Not the tail
      Dump_Line (Elt);
      if Ctx.Is_Root (Elt) then
        In_Mixed := False;
      else
        In_Mixed := Ctx.Get_Is_Mixed (Ctx.Get_Parent (Elt));
      end if;
      Put_Mixed (Elt, In_Mixed);
      Out_Flow.Put (Indent);
      Out_Flow.Put (Asu.To_String(Ctx.Get_Name (Elt)));
      if Ctx.Get_Nb_Attributes (Elt) /= 0 then
        Out_Flow.Put (" :" );
      end if;
      Dump_Attributes (Elt);
      Out_Flow.New_Line;
    end if;

    In_Mixed := Ctx.Get_Is_Mixed (Elt);
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
            Put_Mixed (Children(I), In_Mixed);
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put_Line (" =>" & Ctx.Get_Text (Children(I)) & "<=");
        when Xml_Parser.Pi =>
          -- Put Pi
          Dump_Line (Children(I));
          if not In_Tail then
            Put_Mixed (Children(I), In_Mixed);
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
            Put_Mixed (Children(I), In_Mixed);
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

  -- To store if Cb is in prologue/elements/tail for dump mode
  -- And for Canon_Callback filtering algorithms
  Stage : Xml_Parser.Stage_List;

  -- To skip empty line before root if no prologue at all
  type Cb_Status_List is (Init, Skip, Done);
  Cb_Status : Cb_Status_List;

  -- Callback for "on the flow" display
  procedure Callback (Ctx  : in Xml_Parser.Ctx_Type;
                      Node : in Xml_Parser.Node_Update) is
    Str : Asu_Us;
    Indent : constant String (1 .. Node.Level + 1) := (others => ' ');
    use type Xml_Parser.Node_Kind_List, Xml_Parser.Attributes_Access,
             Xml_Parser.Stage_List, Asu_Us;
  begin
    if Output_Kind = None then
      return;
    elsif Output_Kind /= Dump then
      -- Use the Image of Xml_Parser.Generator
      Str := Asu_Tus (Xml_Parser.Generator.Image (Ctx, Node, Format, Width));
      if Cb_Status = Init then
        if Asu_Is_Null (Str) then
          -- Dummy Xml node when no xml directive, we will need to skip
          --  the leading Line_Feed of root if any
          Cb_Status := Skip;
          return;
        else
          -- Valid Xml directive
          Cb_Status := Done;
        end if;
      end if;
      if Cb_Status = Skip
      and then Asu.Length (Str) >= 1
      and then Asu.Element (Str, 1) = Text_Line.Line_Feed_Char then
        -- Leading Line_Feed (of root when no prologue): remove it
        Asu.Delete (Str, 1, 1);
      end if;
      Cb_Status := Done;
      Out_Flow.Put (Asu_Ts (Str));
      return;
    end if;

    -- Dump mode
    if not Node.Creation then
      return;
    end if;
    if Node.Stage /= Stage then
      Stage := Node.Stage;
      -- New Stage
      case Stage is
        when Xml_Parser.Prologue =>
          -- Entering prologue (new file)
          Out_Flow.Put_Line ("Prologue:");
        when Xml_Parser.Elements =>
          -- Entering elements
          Out_Flow.Put_Line ("Elements:");
        when Xml_Parser.Tail =>
          -- Entering tail
          Out_Flow.Put_Line ("Tail:");
      end case;
    end if;
    Out_Flow.Put (Normal (Node.Line_No, 8, True, '0'));
    if Node.Is_Mixed then
      Out_Flow.Put (" M");
    else
      Out_Flow.Put (" -");
    end if;
    if Node.In_Mixed then
      Out_Flow.Put (" M");
    else
      Out_Flow.Put (" -");
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
    if Node.Attributes /= null
    and then Node.Attributes.all'Length /= 0 then
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

  -- Callback for canonical processing
  procedure Canon_Callback (Ctx  : in Xml_Parser.Ctx_Type;
                            Node : in Xml_Parser.Node_Update) is separate;
  Callback_Acc : Xml_Parser.Parse_Callback_Access;

  -- Parse a file provided as arg or stdin
  -- Retrieve comments and don't expand General Entities if output is Xml
  procedure Do_One (Index : in Natural) is
    -- Parsing elements and charactericstics
    Prologue, Root : Xml_Parser.Element_Type;
    Parse_Ok : Boolean;
    use type Xml_Parser.Generator.Format_Kind_List,
             Xml_Parser.Parse_Callback_Access;
  begin

    -- Title except if stdin or if only one file
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) > 1 then
      Out_Flow.Put_Line (Get_File_Name (Index, True) & ":");
      Out_Flow.Flush;
    end if;

    Stage := Xml_Parser.Elements;
    Cb_Status := Init;
    Ctx.Parse (Get_File_Name (Index, False),
               Parse_Ok,
               Comments => Keep_Comments,
               Expand => not Keep_Expand,
               Cdata  => Cdata_Policy,
               Normalize => Normalize,
               Use_Dtd => Use_Dtd,
               Dtd_File => Asu_Ts (Dtd_File),
               Warn_Cb => Warnings,
               Parse_Cb => Callback_Acc);
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
      and then Output_Kind /= Canon
      and then Format /= Xml_Parser.Generator.Raw then
        -- Last Line feed
        Out_Flow.New_Line;
      end if;
      Ctx.Clean;
      return;
    end if;

    -- Dump / put
    if Output_Kind = Dump then
      Prologue := Ctx.Get_Prologue;
      Root := Ctx.Get_Root_Element;
      Out_Flow.Put_Line ("Prologue:");
      Dump_Element (Prologue, 0);
      Out_Flow.Put_Line ("Elements:");
      Dump_Element (Root, 0);
      Out_Flow.Put_Line ("Unparsed entities:");
      Dump_Unparsed_Entities;
      Out_Flow.Flush;
    elsif Output_Kind = Gen then
      Ctx.Put (Xml_Parser.Generator.Stdout, Format, Width);
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
  Warnings := null;
  Keep_Expand := False;
  Keep_Comments := False;
  Keep_Cdata := False;
  Normalize := True;
  Use_Dtd := True;
  Dtd_File := Asu_Null;
  Callback_Acc := null;
  -- Get options and check max of options
  -- Only one option, one more for each keep, one more if check_dtd,
  --  one more if ontheflow, one more if warnings
  Max_Opt := 1;
  if Arg_Dscr.Is_Set (8) then
    -- One or several Keep options
    Max_Opt := Max_Opt + Arg_Dscr.Get_Nb_Occurences (8);
  end if;
  if Arg_Dscr.Is_Set (9) then
    -- Check dtd
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (10) then
    -- Tree mode
    Max_Opt := Max_Opt + 1;
  else
    Callback_Acc := Callback'Unrestricted_Access;
  end if;
  if Arg_Dscr.Is_Set (11) then
    -- Put warnings
    Max_Opt := Max_Opt + 1;
    Warnings := Warning'Unrestricted_Access;
  end if;
  if Arg_Dscr.Get_Number_Keys > Max_Opt then
    Ae_Re (Arg_Error'Identity, "Too many options");
  end if;

  -- Get format info
  if Arg_Dscr.Is_Set (1) then
    -- Silent
    Output_Kind := None;
  elsif Arg_Dscr.Is_Set (2) then
    -- Dump: keep all
    Output_Kind := Dump;
    Keep_Expand := True;
    Keep_Comments := True;
    Keep_Cdata := True;
  elsif Arg_Dscr.Is_Set (3) then
    Format := Xml_Parser.Generator.Raw;
  elsif Arg_Dscr.Is_Set (12) then
    Output_Kind := Canon;
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
        Width := Natural'Value (Arg_Dscr.Get_Option (4));
      exception
        when others =>
          Ae_Re (Arg_Error'Identity, "Invalid Width value "
               & Arg_Dscr.Get_Option (4));
      end;
    elsif Arg_Dscr.Is_Set (5) then
      Format := Xml_Parser.Generator.One_Per_Line;
    end if;
  end if;

  if Arg_Dscr.Is_Set (8) then
    if Output_Kind = Dump then
      Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" and ""dump"" options");
    end if;
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (8) loop
      -- Parse Keep options
      declare
        Opt : constant String := Arg_Dscr.Get_Option (8, I);
        Chr : constant Boolean := Arg_Dscr.Is_Char (8, I);
      begin
        if Opt = ""
        or else (Chr and then Opt = "a")
        or else (not Chr and then Opt = "all") then
          if Keep_Expand_Set or else Keep_Comments_Set or else Keep_Cdata_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          Keep_Expand := True;
          Keep_Expand_Set := True;
          Keep_Comments := True;
          Keep_Comments_Set := True;
          Keep_Cdata := True;
          Keep_Cdata_Set := True;
        elsif (Chr and then Opt = "e")
        or else (not Chr and then Opt = "expanded") then
          if Keep_Expand_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          Keep_Expand := True;
          Keep_Expand_Set := True;
        elsif (Chr and then Opt = "c")
        or else (not Chr and then Opt = "comments") then
          if Keep_Comments_Set or else Keep_Cdata_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          Keep_Comments := True;
          Keep_Comments_Set := True;
        elsif (Chr and then Opt = "d")
        or else (not Chr and then Opt = "cdata") then
          if Keep_Cdata_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          Keep_Cdata := True;
          Keep_Cdata_Set := True;
        else
          Ae_Re (Arg_Error'Identity, "Invalid ""keep"" option");
        end if;
      end;
    end loop;
  end if;
  if Keep_Cdata then
    Cdata_Policy := Xml_Parser.Keep_Cdata_Section;
  else
    Cdata_Policy := Xml_Parser.Remove_Cdata_Markers;
  end if;

  if Arg_Dscr.Is_Set (9) then
    -- Check dtd file
    Dtd_File := Asu_Tus (Arg_Dscr.Get_Option (9));
    if Asu_Is_Null (Dtd_File) then
      -- If option set with empty dtd => no check
      Use_Dtd := False;
    end if;
  end if;

  if Arg_Dscr.Is_Set (12) then
    if Arg_Dscr.Is_Set (10) or else Keep_Expand_Set or else Keep_Cdata_Set then
      Ae_Re (Arg_Error'Identity, "Incompatible options");
    end if;
    Output_Kind := Canon;
    Format := Xml_Parser.Generator.Fill_Width;
    Width := Xml_Parser.Generator.Infinite_Width;
    Normalize := False;
    Callback_Acc := Canon_Callback'Unrestricted_Access;
  end if;

  -- Process other arguments
  if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) = 0 then
    Do_One (0);
  else
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (No_Key_Index) loop
      Do_One (I);
      if I /= Arg_Dscr.Get_Nb_Occurences (No_Key_Index)
      and then Output_Kind /= None then
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

