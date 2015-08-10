-- Check/Format/Canonify a XML file or flow
with Ada.Exceptions;
with As.U.Utils, Argument, Argument_Parser, Xml_Parser.Generator, Normal,
     Basic_Proc, Text_Line, Sys_Calls, Parser, Bloc_Io, Str_Util, Trilean,
     Trace.Loggers, Mixed_Str;
procedure Xml_Checker is
  -- Current version
  Version : constant String := "V22.0";

  procedure Ae_Re (E : in Ada.Exceptions.Exception_Id;
                   M : in String := "")
            renames Ada.Exceptions.Raise_Exception;

  -- Xml Parser context, elements and parsing parameters
  Ctx, Ctxc : aliased Xml_Parser.Generator.Ctx_Type;
  type Ctx_Access is access all Xml_Parser.Generator.Ctx_Type;
  Ctxa : Ctx_Access;

  -- Expand general entities and attributes with default
  Expand : Boolean;

  -- "Keep" options
  Keep_Comments, Keep_Cdata : Boolean;
  Cdata_Policy : Xml_Parser.Cdata_Policy_List;

  -- Are these options set
  Keep_Comments_Set : Boolean := False;
  Keep_Cdata_Set : Boolean := False;

  -- Argument error
  Arg_Error : exception;
  -- Abort loop of arguments
  Abort_Error : exception;

  -- Kind of ouput: None, progress bar, dump, Xml_Generator or Canonical
  type Output_Kind_List is (None, Progress, Dump, Gen, Canon);
  Output_Kind : Output_Kind_List;

  -- Xml_Generator descriptor and format
  Format : Xml_Parser.Generator.Format_Kind_List;
  Width  : Natural;

  -- Normalize text
  Normalize : Boolean;

  -- Flow of dump or canonicalization
  Out_Flow : Text_Line.File_Type;

  -- Dtd options
  Use_Dtd : Boolean;
  Dtd_File : As.U.Asu_Us;

  -- Use Namespaces
  Namespace : Boolean;

  -- Total Nb of lines of current file (when progress)
  Lines : Natural;
  -- Progress factor (Nb of signs)
  Progress_Factor : constant := 50;

  -- Warning detection
  procedure Warning (Unused_Ctx : in  Xml_Parser.Ctx_Type; Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error (Msg);
  end Warning;
  Warnings : Xml_Parser.Warning_Callback_Access;

  -- Update Is_Mixed in tree
  Update_Mix : Boolean;

  -- Maximum of options allowed
  Max_Opt : Natural;

  -- String list of unparsed entities
  Unparsed_Entities : As.U.Asu_Us;

  -- The argument keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"),      False),
   02 => (False, 'v', As.U.Tus ("version"),   False),
   03 => (False, 'C', As.U.Tus ("canonical"), False),
   04 => (False, 'c', As.U.Tus ("copytree"),  False),
   05 => (False, 'D', As.U.Tus ("dump"),      False),
   06 => (True,  'd', As.U.Tus ("dtd"),       False, True, As.U.Tus ("Dtd")),
   07 => (False, 'E', As.U.Tus ("expand"),    False),
   08 => (True,  'k', As.U.Tus ("keep"),      True,  True, As.U.Tus ("c|d|n|a")),
   09 => (False, 'm', As.U.Tus ("mixed"),     False),
   10 => (False, 'n', As.U.Tus ("namespace"), False),
   11 => (False, Argument_Parser.No_Key_Char, As.U.Tus ("no-normalize"), False),
   12 => (False, '1', As.U.Tus ("one"),       False),
   13 => (False, 'p', As.U.Tus ("progress"),  False),
   14 => (False, 'r', As.U.Tus ("raw"),       False),
   15 => (False, 's', As.U.Tus ("silent"),    False),
   16 => (False, 't', As.U.Tus ("tree"),      False),
   17 => (False, 'V', As.U.Tus ("validate"),  False),
   18 => (True,  'W', As.U.Tus ("width"),     False, True, As.U.Tus ("Width")),
   19 => (False, 'w', As.U.Tus ("warnings"),  False)
   );
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  Names : constant As.U.Utils.Asu_Array (Keys'Range) := (
    06 => As.U.Tus ("check_dtd"),
    09 => As.U.Tus ("update_mix"),
    11 => As.U.Tus ("normalize"),
    others => As.U.Asu_Null);

  Helps : constant As.U.Utils.Asu_Array (Keys'Range) := (
    01 => As.U.Tus ("Put this help"),
    02 => As.U.Tus ("Put versions"),
    03 => As.U.Tus ("Canonicalize xml"),
    04 => As.U.Tus ("Copy tree then dump the copy"),
    05 => As.U.Tus ("Dump expanded Xml tree"),
    06 => As.U.Tus ("Use a specific dtd or skip doctype"),
    07 => As.U.Tus ("Expand general entities"),
    08 => As.U.Tus ("Keep comments and Cdata"),
    09 => As.U.Tus ("Update Mixed tag in tree"),
    10 => As.U.Tus ("Put Namespace^Suffix"),
    11 => As.U.Tus ("Do not normalize attributes and text"),
    12 => As.U.Tus ("Put one attribute per line"),
    13 => As.U.Tus ("Only show a progress bar"),
    14 => As.U.Tus ("Put all on one line"),
    15 => As.U.Tus ("No output, only exit code"),
    16 => As.U.Tus ("Build tree then dump it"),
    17 => As.U.Tus ("Process for validation (-E -k n)"),
    18 => As.U.Tus ("Put attributes up to Width"),
    19 => As.U.Tus ("Check for warnings")
    );

  -- Program help
  procedure Usage (Full : in Boolean) is
    procedure Pl (Str : in String) renames Basic_Proc.Put_Error;
    procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;
    Tab : constant String (1 .. 35) := (others => ' ');
    Ustr, Tstr : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Ple ("Usage: " & Argument.Get_Program_Name & "[ { <option> } ] [ { <file> } ]");
    if not Full then
      Ple ("Use option '-h' or '--help' for help.");
      return;
    end if;
    Ple (" <option> ::= <silent> | <progress> | <dump> | <raw> | <width> | <one> |");
    Ple ("            | <expand> | <keep> | <namespace> | <canonical> | <normalize>");
    Ple ("            | <check_dtd> | <validate> | <tree> | <update_mix> | <copytree>");
    Ple ("            | <warnings>");
    Ple ("            | <help> | <version>");
    for I in Keys'Range loop
      if Names(I).Is_Null then
        Ustr := "<" & Keys(I).Key_String & ">";
      else
        Ustr := "<" & Names(I) & ">";
      end if;
      Ustr := As.U.Tus (" " & Str_Util.Procuste (Ustr.Image, 13) & " ::= ");
      if I /= 08 then
        Ustr := Ustr & Argument_Parser.Image (Keys(I));
      else
        -- For Keep we replace "c|d|n|a" by "comments|cdata|none|all>"
        declare
          Str : constant String := Argument_Parser.Image (Keys(I));
        begin
          Tstr := As.U.Tus (Str(Str'First .. Str'Last - 8))
                & "comments|cdata|none|all>";
          Ustr := Ustr & Tstr;
        end;
      end if;
      if Ustr.Length <= Tab'Length then
        Pl (Str_Util.Procuste (Ustr.Image, Tab'Length));
      else
        Ple (Ustr.Image);
        Pl (Tab);
      end if;
      Ple (" : " & Helps(I).Image);
      if I = 07 then
        Ple (Tab & " :  and attributes with default");
      elsif I = 08 then
        Ple (Tab & " : Keep CDATA sections unchanged");
        Ple (Tab & " : Keep none (remove comments and CDATA");
        Ple (Tab & " :  markers)");
        Ple (Tab & " : Keep all (default)");
      end if;
    end loop;

    Ple ("Empty Dtd can be used to force skipping validation versus dtd.");
    Ple ("All options except expand, keep, dtd, warnings, namespace and tree are");
    Ple (" mutually exclusive.");
    Ple ("Keep, expand and namespace are not allowed on Dump mode, Dump => keep all.");
    Ple ("Canonical is implemented with a callback (no tree). It only allows options dtd,");
    Ple ("  warnings and keep-comments (it expands, removes CDATA markers, ignores");
    Ple ("  namespace, does not normalize and, by default, removes comments).");
    Ple ("Namespace requires no expand.");
    Ple ("Default format is -W" & Xml_Parser.Generator.Default_Width'Img
                         & " on stdout.");
    Ple ("Building the tree is not recommended for big files and forbidden in canonical.");
    Ple ("Progress bar requires the callback mode (no tree).");
    Ple ("For big files, consider using the callback mode or increasing the process stack");
    Ple ("  size (ulimit -s) to avoid stack overflow and Storage_Error.");
  end Usage;


  -------------------
  -- Dump xml tree --
  -------------------
  procedure Dump_Line (Actx : in out Xml_Parser.Generator.Ctx_Type;
                       Node : in Xml_Parser.Node_Type) is
  begin
    Out_Flow.Put (Normal (Actx.Get_Line_No (Node), 8, True, '0'));
  end Dump_Line;

  procedure Dump_Attributes (Actx : in out Xml_Parser.Generator.Ctx_Type;
                             Elt : in Xml_Parser.Element_Type) is
    Attrs : constant Xml_Parser.Attributes_Array := Actx.Get_Attributes (Elt);
  begin
    for I in Attrs'Range loop
      if not Attrs(I).Unparsed then
        Out_Flow.Put (" " & Attrs(I).Name.Image & "=" & Attrs(I).Value.Image);
      else
        Out_Flow.Put (" " & Attrs(I).Name.Image & "=U=" & Attrs(I).Value.Image);
        -- Maybe several entities here
        Unparsed_Entities.Append (Attrs(I).Value.Image & ' ');
      end if;
    end loop;
  end Dump_Attributes;

  procedure Dump_Element (Actx : in out Xml_Parser.Generator.Ctx_Type;
                          Elt : in Xml_Parser.Element_Type;
                          Level : in Natural;
                          Show_It : in Boolean) is
    Children : Xml_Parser.Nodes_Array := Actx.Get_Children (Elt);
    Indent : constant String (1 .. Level + 1) := (others => ' ');
    In_Mixed : Boolean;
    use type Xml_Parser.Node_Kind_List;
    procedure Put_Mixed (N : in Xml_Parser.Node_Type;
                         Inm : in Boolean) is
    begin
      if N.Kind = Xml_Parser.Element
      and then Actx.Get_Is_Mixed (N) then
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
      -- Empty info
      if N.Kind = Xml_Parser.Element then
        case Actx.Get_Empty_Info (N) is
          when Xml_Parser.Tag_Empty =>
            Out_Flow.Put (" T");
          when Xml_Parser.Def_Empty =>
            Out_Flow.Put (" D");
          when Xml_Parser.Not_Empty =>
            Out_Flow.Put (" N");
        end case;
      else
        Out_Flow.Put (" -");
      end if;
    end Put_Mixed;

  begin
    if Show_It then
       -- Not the tail
      Dump_Line (Actx, Elt);
      if Actx.Is_Root (Elt) then
        In_Mixed := False;
      else
        In_Mixed := Actx.Get_Is_Mixed (Actx.Get_Parent (Elt));
      end if;
      Put_Mixed (Elt, In_Mixed);
      Out_Flow.Put (Indent);
      Out_Flow.Put (Actx.Get_Name (Elt).Image);
      if Actx.Get_Nb_Attributes (Elt) /= 0 then
        Out_Flow.Put (" :" );
      end if;
      Dump_Attributes (Actx, Elt);
      Out_Flow.New_Line;
    end if;

    In_Mixed := Actx.Get_Is_Mixed (Elt);
    for I in Children'Range loop
      if I rem 2 = 0 then
        -- Test the individual get
        Children(I) := Actx.Get_Child (Elt, I);
      end if;
      case Children(I).Kind is
        when Xml_Parser.Element =>
          -- Recursive dump child
          Dump_Element (Actx, Children(I), Level + 1, True);
        when Xml_Parser.Text =>
          -- Put text
          Dump_Line (Actx, Children(I));
          Put_Mixed (Children(I), In_Mixed);
          Out_Flow.Put (Indent);
          Out_Flow.Put_Line (" =>" & Actx.Get_Text (Children(I)) & "<=");
        when Xml_Parser.Pi =>
          -- Put Pi
          Dump_Line (Actx, Children(I));
          Put_Mixed (Children(I), In_Mixed);
          if Show_It then
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put (" <?" & Actx.Get_Target (Children(I)));
          if Actx.Get_Pi (Children(I)).Length /= 0 then
            Out_Flow.Put (" " & Actx.Get_Pi (Children(I)));
          end if;
          Out_Flow.Put_Line ("?>");
        when Xml_Parser.Comment =>
          -- Put Comment
          Dump_Line (Actx, Children(I));
          Put_Mixed (Children(I), In_Mixed);
          if Show_It then
            Out_Flow.Put (Indent);
          end if;
          Out_Flow.Put_Line (" <!--" & Actx.Get_Comment (Children(I)) & "-->");
      end case;
    end loop;
  end Dump_Element;

  -- Dump the unparsed entities characterisics
  procedure Dump_Unparsed_Entities
            (Actx : in out Xml_Parser.Generator.Ctx_Type) is
    -- Iterator on string list of unparsed entities
    Iter : Parser.Iterator;
    Info : Xml_Parser.Unparsed_Entity_Info_Rec;
  begin
    Iter.Set (Unparsed_Entities.Image);
    loop
      declare
        Entity : constant String := Iter.Next_Word;
      begin
        exit when Entity = "";
        Actx.Get_Unparsed_Entity_Info (Entity, Info);
        Out_Flow.Put_Line ("Entity: " & Entity
                         & ", System_Id=" & Info.Entity_System_Id.Image
                         & ", Public_Id=" & Info.Entity_System_Id.Image);
        Out_Flow.Put_Line (" Notation: " & Info.Notation_Name.Image
                         & ", System_Id=" & Info.Notation_System_Id.Image
                         & ", Public_Id=" & Info.Notation_Public_Id.Image);
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

  -- To Store Previous progress
  Prev_Progress : Natural;

  -- To store if Cb is in prologue/elements/tail for dump mode
  -- And for Canon_Callback filtering algorithms
  Stage : Xml_Parser.Stage_List;

  -- To skip empty line before root if no prologue at all
  type Cb_Status_List is (Init, Skip, Done);
  Cb_Status : Cb_Status_List;

  -- Callback for "on the flow" display
  procedure Callback (Ctx  : in Xml_Parser.Ctx_Type;
                      Node : in Xml_Parser.Node_Update) is
    Str : As.U.Asu_Us;
    Curr_Progress : Natural;
    Indent : constant String (1 .. Node.Level + 1) := (others => ' ');
    use type Xml_Parser.Node_Kind_List, Xml_Parser.Attributes_Access,
             Xml_Parser.Stage_List;
  begin
    if Output_Kind = None then
      return;
    elsif Output_Kind = Progress then
      if Lines > 1 and then Node.Line_No /= 0 then
        -- Update progress
        Curr_Progress := Node.Line_No * Progress_Factor / Lines;
        if Curr_Progress /= Prev_Progress
        and then Curr_Progress < Progress_Factor then
          for I in Prev_Progress + 1 .. Curr_Progress loop
            Out_Flow.Put ("=");
          end loop;
          Prev_Progress := Curr_Progress;
          Out_Flow.Flush;
        end if;
      end if;
      return;
    elsif Output_Kind /= Dump then
      -- Use the Image of Xml_Parser.Generator
      Str := As.U.Tus (Xml_Parser.Generator.Image (Ctx, Node, Format,
                                                   Width, Namespace));
      if Cb_Status = Init then
        if Str.Is_Null then
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
      and then Str.Length >= 1
      and then Str.Element (1) = Text_Line.Line_Feed_Char then
        -- Leading Line_Feed (of root when no prologue): remove it
        Str.Delete (1, 1);
      end if;
      Cb_Status := Done;
      Out_Flow.Put (Str.Image);
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
    if Node.Kind = Xml_Parser.Element then
      case Node.Empty_Info is
        when Xml_Parser.Tag_Empty =>
          Out_Flow.Put (" T");
        when Xml_Parser.Def_Empty =>
          Out_Flow.Put (" D");
        when Xml_Parser.Not_Empty =>
          Out_Flow.Put (" N");
      end case;
    else
      Out_Flow.Put (" -");
    end if;

    Out_Flow.Put (Indent);
    case Node.Kind is
      when Xml_Parser.Element =>
        Out_Flow.Put (Node.Name.Image);
      when Xml_Parser.Pi =>
        Out_Flow.Put ("<?" & Node.Name.Image);
        if Node.Value.Length /= 0 then
          Out_Flow.Put (" " & Node.Value.Image);
        end if;
        Out_Flow.Put ("?>");
      when Xml_Parser.Comment =>
        Out_Flow.Put ("<!--" & Node.Name.Image & "-->");
      when Xml_Parser.Text =>
        Out_Flow.Put ("=>" & Node.Name.Image & "<=");
    end case;
    if Node.Attributes /= null
    and then Node.Attributes.all'Length /= 0 then
      Out_Flow.Put (" :");
      for I in  Node.Attributes.all'Range loop
        if not Node.Attributes(I).Unparsed then
          Out_Flow.Put (" " & Node.Attributes(I).Name.Image
                      & "=" & Node.Attributes(I).Value.Image);
        else
          Out_Flow.Put (" " & Node.Attributes(I).Name.Image
                      & "=U=" & Node.Attributes(I).Value.Image);
          Unparsed_Entities.Append (Node.Attributes(I).Value.Image & ' ');
        end if;
      end loop;
    end if;
    Out_Flow.New_Line;
  end Callback;

  -- Callback for canonical processing
  procedure Canon_Callback (Ctx  : in Xml_Parser.Ctx_Type;
                            Node : in Xml_Parser.Node_Update) is separate;
  Callback_Acc : Xml_Parser.Parse_Callback_Access;

  -- Copytree (no callback)
  Copy_Tree : Boolean := False;

  -- Count number of lines of file
  procedure Count_Lines (File_Name : in String) is
    package Size_Io is new Bloc_Io(Character);
    use type Size_Io.Count;
    Size_Flow : Size_Io.File_Type;
    Bloc_Size : constant Size_Io.Count := 1024 * 1024;
    Bloc : Size_Io.Element_Array (1 .. Bloc_Size);
    File_Size, Nb_Read, Nb_To_Read : Size_Io.Count;
  begin
    Size_Flow.Open (Size_Io.In_File, File_Name);
    File_Size := Size_Flow.Size;
    Nb_Read := 0;
    Lines := 0;
    Nb_To_Read := Bloc_Size;
    loop
      -- Compute Nb to read (Bloc_Size or last chars) and read
      if Nb_Read + Bloc_Size > File_Size then
        Nb_To_Read := File_Size - Nb_Read;
      end if;
      Size_Flow.Read (Bloc(1 .. Nb_To_Read));
      -- Count Line Feeds
      for I in 1 .. Nb_To_Read loop
        if Bloc(I) = Text_Line.Line_Feed_Char then
          Lines := Lines + 1;
        end if;
      end loop;
      -- Update count of read and exit when done
      Nb_Read := Nb_Read + Nb_To_Read;
      exit when Nb_Read = File_Size;
    end loop;
    Size_Flow.Close;
  exception
    when others =>
      Lines := 0;
  end Count_Lines;

  -- Copy prologue of Ctx into Ctxc
  Copylog : Trace.Loggers.Logger;
  procedure Copy_Prologue (Node : in Xml_Parser.Node_Type) is
    Attrs : constant Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Node);
    Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Node);
    -- The prologue node
    Prologue : constant Xml_Parser.Node_Type := Ctxc.Get_Prologue;
    -- Major an minor Xml version
    Dot, Major, Minor : Natural;
    -- Doctype public info
    Name, Pub_Id, File, Int_Def : As.U.Asu_Us;
    Public : Boolean;
    -- Node created
    Tmp_Node : Xml_Parser.Node_Type;

  begin
    Copylog.Init ("Copylog");
    -- Copy XML node
    for I in Attrs'Range loop
      -- The possible Xml directive
      if Attrs(I).Name.Image = "version" then
        -- Parse Major and minor
        Dot := Str_Util.Locate (Attrs(I).Value.Image, ".");
        Major := Natural'Value (Attrs(I).Value.Slice (1, Dot-1));
        Minor := Natural'Value (Attrs(I).Value.Slice (
                   Dot+1, Attrs(I).Value.Length));
        begin
          Ctxc.Set_Version (Major, Minor);
        exception
          when Xml_Parser.Generator.Invalid_Argument =>
            Copylog.Log_Error ("Error on Set_Version ("
                             & Major'Img & ", " & Minor'Img & ')');
            raise;
        end;
      elsif Attrs(I).Name.Image = "encoding" then
        begin
          Ctxc.Set_Encoding (Attrs(I).Value.Image);
        exception
          when Xml_Parser.Generator.Invalid_Argument =>
            Copylog.Log_Error ("Error on Set_Encoding ("
                             & Attrs(I).Value.Image & ')');
            raise;
        end;
      elsif Attrs(I).Name.Image = "standalone" then
        begin
          Ctxc.Set_Standalone (Attrs(I).Value.Image = "yes");
        exception
          when Xml_Parser.Generator.Invalid_Argument =>
            Copylog.Log_Error ("Error on Set_Standalone ("
                             & Attrs(I).Value.Image & ')');
            raise;
        end;
      end if;
    end loop;
    -- Copy the children
    for I in Children'Range loop
      case Children(I).Kind is
        when Xml_Parser.Element =>
          -- Xml node cannot have child of kind element
          raise Constraint_Error;
        when Xml_Parser.Pi =>
          begin
            Ctxc.Add_Pi (Prologue, Ctx.Get_Target (Children(I))
                                 & " " & Ctx.Get_Pi (Children(I)), Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Pi ("
                               & Ctx.Get_Target (Children(I))
                               & " " & Ctx.Get_Pi (Children(I)) & ')');
              raise;
          end;
        when Xml_Parser.Comment =>
          begin
            Ctxc.Add_Comment (Prologue, Ctx.Get_Comment (Children(I)),
                              Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Set_Comment ("
                               & Ctx.Get_Comment (Children(I))  & ')');
              raise;
          end;
        when Xml_Parser.Text =>
          -- Doctype
          Ctx.Get_Doctype (Name, Public, Pub_Id, File, Int_Def);
          begin
            Ctxc.Add_Doctype (Prologue, Name.Image, Public,
                              Pub_Id.Image, File.Image, Int_Def.Image,
                              Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Doctype ("
                               & Name.Image & ", " & Mixed_Str (Public'Img)
                               & ", " & Pub_Id.Image & ", " & File.Image
                               & ", [" & Int_Def.Image & "])");
              raise;
          end;
      end case;
    end loop;
  end Copy_Prologue;

  -- Copy Elements of Ctx into Ctxc recursively
  procedure Copy_Elements (Node : in Xml_Parser.Node_Type;
                           Nodec : in out Xml_Parser.Node_Type;
                           In_Elements : in Boolean) is
    -- Children of Node
    Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Node);
    -- Node created
    Tmp_Node : Xml_Parser.Node_Type;
    use type Xml_Parser.Empty_Info_List;
  begin
    if In_Elements then
      -- Copy attributes
      begin
        Ctxc.Set_Attributes (Nodec, Ctx.Get_Attributes (Node));
      exception
        when Xml_Parser.Generator.Invalid_Argument =>
          Copylog.Log_Error ("Error on Set_Attributes");
          declare
            Attr : constant Xml_Parser.Attributes_Array
                 := Ctx.Get_Attributes (Node);
          begin
            for I in Attr'Range loop
              Copylog.Log_Error ("  " & Attr(I).Name.Image & "-"
                                      & Attr(I).Value.Image);
            end loop;
          end;
          raise;
      end;
      Ctxc.Set_Tag_Empty (Nodec,
                          Ctx.Get_Empty_Info (Node) = Xml_Parser.Tag_Empty);
    end if;

    -- Copy children
    for I in Children'Range loop
      case Children(I).Kind is
        when Xml_Parser.Element =>
          begin
            Ctxc.Add_Child (Nodec, Ctx.Get_Name (Children(I)),
                            Xml_Parser.Element, Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Child Element ("
                               & Ctx.Get_Name (Children(I)) & ')');
              raise;
          end;
          Copy_Elements (Children(I), Tmp_Node, In_Elements);
        when Xml_Parser.Text =>
          begin
            Ctxc.Add_Child (Nodec, Ctx.Get_Text (Children(I)), Xml_Parser.Text,
                            Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Child Text ("
                               & Ctx.Get_Text (Children(I)) & ')');
              raise;
          end;
        when Xml_Parser.Pi =>
          begin
            Ctxc.Add_Child (Nodec, Ctx.Get_Target (Children(I)) & " "
                                 & Ctx.Get_Pi (Children(I)),
                            Xml_Parser.Pi, Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Pi Element ("
                               & Ctx.Get_Target (Children(I)) & " "
                               & Ctx.Get_Pi (Children(I)) & ')');
              raise;
          end;
        when Xml_Parser.Comment =>
          begin
            Ctxc.Add_Child (Nodec, Ctx.Get_Comment (Children(I)),
                            Xml_Parser.Comment, Tmp_Node);
          exception
            when Xml_Parser.Generator.Invalid_Argument =>
              Copylog.Log_Error ("Error on Add_Child Comment ("
                               & Ctx.Get_Comment (Children(I)) & ')');
              raise;
          end;
      end case;
    end loop;
  end Copy_Elements;

  -- Parse a file provided as arg or stdin
  -- Retrieve comments and don't expand General Entities if output is Xml
  procedure Do_One (Index : in Natural) is
    -- Parsing elements and charactericstics
    Parse_Ok : Boolean;
    Dummy : Boolean;
    Nodec : Xml_Parser.Element_Type;
    use type Xml_Parser.Generator.Format_Kind_List,
             Xml_Parser.Parse_Callback_Access;
  begin

    -- Title except if stdin or if only one file
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) > 1
    and then Output_Kind /= None then
      Out_Flow.Put_Line (Get_File_Name (Index, True) & ":");
      Out_Flow.Flush;
    end if;

    if Output_Kind = Progress and then Index /= 0 then
      -- Count lines and prepare output
      Count_Lines (Get_File_Name (Index, False));
      Prev_Progress := 0;
      Out_Flow.Put_Line ("|-------------------------------------------------|");
      Out_Flow.Put ("|");
      Out_Flow.Flush;
      Dummy := Sys_Calls.Set_Tty_Attr (Out_Flow.Get_Fd, Sys_Calls.Char);
    end if;

    Stage := Xml_Parser.Elements;
    Cb_Status := Init;
    Ctx.Parse (Get_File_Name (Index, False),
               Parse_Ok,
               Comments => Keep_Comments,
               Cdata  => Cdata_Policy,
               Expand => Expand,
               Normalize => Normalize,
               Use_Dtd => Use_Dtd,
               Dtd_File => Dtd_File.Image,
               Namespace => Namespace,
               Warn_Cb => Warnings,
               Parse_Cb => Callback_Acc);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error ("Error: "
                               & Ctx.Get_Parse_Error_Message);
      Basic_Proc.Set_Error_Exit_Code;
      Ctx.Clean;
      return;
    end if;

    -- Callback mode
    ----------------
    if Callback_Acc /= null then
      if Output_Kind = Dump then
        Out_Flow.Put_Line ("Unparsed entities:");
        Dump_Unparsed_Entities (Ctx);
      end if;
      if Output_Kind = Progress and then Index /= 0 then
        -- Terminate progress bar
        for I in Prev_Progress + 1 .. Progress_Factor - 1 loop
          Out_Flow.Put ("=");
        end loop;
        Out_Flow.Put ("|");
        Out_Flow.Flush;
        Dummy := Sys_Calls.Set_Tty_Attr (Out_Flow.Get_Fd, Sys_Calls.Canonical);
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

    -- Tree mode
    ------------
    -- Update tags Is_Mixed in tree
    if Update_Mix then
      Ctx.Update_Is_Mixed;
    end if;

    -- Copy tree
    if Copy_Tree then
      -- Copy Ctx into Ctxc
      Ctxc.Clear_Xml;
      Copy_Prologue (Ctx.Get_Prologue);
      Nodec := Ctxc.Get_Root_Element;
      Ctxc.Set_Name (Nodec, Ctx.Get_Name (Ctx.Get_Root_Element));
      Copy_Elements (Ctx.Get_Root_Element, Nodec, True);
      -- Copy Tail
      Nodec := Ctxc.Get_Tail;
      Copy_Elements (Ctx.Get_Tail, Nodec, False);
      -- Check
      Ctxc.Check (Ok => Parse_Ok,
                  Expand => Trilean.Boo2Tri (Expand),
                  Normalize => Trilean.Boo2Tri (Normalize),
                  Use_Dtd  => Trilean.Boo2Tri (Use_Dtd),
                  Dtd_File => Dtd_File.Image,
                  Namespace => Trilean.Boo2Tri (Namespace));
      if not Parse_Ok then
        Basic_Proc.Put_Line_Error ("Error in copied tree "
                                 & Get_File_Name (Index, True) & ": "
                                 & Ctxc.Get_Parse_Error_Message);
        Basic_Proc.Set_Error_Exit_Code;
        Ctx.Clean;
        return;
      end if;
      if Update_Mix then
        Ctxc.Update_Is_Mixed;
      end if;
      Ctxa := Ctxc'Access;
    else
      Ctxa := Ctx'Access;
    end if;

    -- Dump / put
    if Output_Kind = Dump then
      Out_Flow.Put_Line ("Prologue:");
      Dump_Element (Ctxa.all, Ctxa.Get_Prologue, 0, True);
      Out_Flow.Put_Line ("Elements:");
      Dump_Element (Ctxa.all, Ctxa.Get_Root_Element, 0, True);
      if Ctxa.Get_Nb_Children (Ctxa.Get_Tail) /= 0 then
        Out_Flow.Put_Line ("Tail:");
        Dump_Element (Ctxa.all, Ctxa.Get_Tail, 0, False);
      end if;
      Out_Flow.Put_Line ("Unparsed entities:");
      Dump_Unparsed_Entities (Ctxa.all);
      Out_Flow.Flush;
    elsif Output_Kind = Gen then
      Ctxa.Put (Xml_Parser.Generator.Stdout, Format, Width, Namespace);
    end if;
    Ctx.Clean;
  exception
    when Xml_Parser.File_Error =>
      Basic_Proc.Put_Line_Error ("IO Error reading file "
        & Get_File_Name (Index, True) & ".");
      raise Abort_Error;
  end Do_One;

  use type Xml_Parser.Generator.Format_Kind_List,
           Xml_Parser.Parse_Callback_Access;
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
  if Arg_Dscr.Is_Set (01) then
    -- Help: No file nor other option
    if Arg_Dscr.Get_Nb_Occurences (No_Key_Index) /= 0
    or else Arg_Dscr.Get_Number_Keys > 1 then
      Ae_Re (Arg_Error'Identity, "Too many options");
      Usage (False);
    else
      Usage (True);
    end if;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Arg_Dscr.Is_Set (02) then
    -- Version: No file nor other option
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
  Expand := False;
  Keep_Comments := True;
  Keep_Cdata := True;
  Normalize := True;
  Use_Dtd := True;
  Dtd_File.Set_Null;
  Callback_Acc := null;
  Namespace := False;
  Update_Mix := False;
  Copy_Tree := False;
  -- Get options and check max of options
  -- Only one option, one more for keep, check_dtd, tree, warnings, copytree,
  --  validate
  Max_Opt := 1;
  if Arg_Dscr.Is_Set (04) then
    -- Copy tree
    Copy_Tree := True;
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (06) then
    -- Check dtd
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (07) then
    -- Expand entities and default of attributes
    Expand := True;
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (08) then
    -- One or several Keep options
    Max_Opt := Max_Opt + Arg_Dscr.Get_Nb_Occurences (8);
  end if;
  if Arg_Dscr.Is_Set (09) then
     -- Update Is_Mixed in tree
     if not Arg_Dscr.Is_Set (16) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""callback"" and ""update_mix"" options");
    end if;
    Update_Mix := True;
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (10) then
    -- Expand namespaces
    if Arg_Dscr.Is_Set (07) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""namespace"" and ""expand"" options");
    end if;
    Namespace := True;
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (16) then
    -- Tree mode
    Max_Opt := Max_Opt + 1;
  else
    Callback_Acc := Callback'Unrestricted_Access;
  end if;
  if Arg_Dscr.Is_Set (17) then
    -- Validate
    Max_Opt := Max_Opt + 1;
  end if;
  if Arg_Dscr.Is_Set (19) then
    -- Put warnings
    Max_Opt := Max_Opt + 1;
    Warnings := Warning'Unrestricted_Access;
  end if;

  -- Check Nb of options
  if Arg_Dscr.Get_Number_Keys > Max_Opt then
    Ae_Re (Arg_Error'Identity, "Too many or incompatible options");
  end if;

  -- Get format info
  if Arg_Dscr.Is_Set (03) then
    -- Canonical
    Output_Kind := Canon;
  elsif Arg_Dscr.Is_Set (05) then
    -- DUmp
    if Arg_Dscr.Is_Set (08) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""keep"" and ""dump"" options");
    end if;
    if Arg_Dscr.Is_Set (07) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""expand"" and ""dump"" options");
    end if;
    if Arg_Dscr.Is_Set (10) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""namespace"" and ""dump"" options");
    end if;
    if Arg_Dscr.Is_Set (17) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""validate"" and ""dump"" options");
    end if;
    -- -D: Dump, keep all
    Output_Kind := Dump;
    Keep_Comments := True;
    Keep_Cdata := True;
    Expand := False;
    Namespace := False;
  elsif Arg_Dscr.Is_Set (13) then
    -- Progress bar
    if Arg_Dscr.Is_Set (16) then
      Output_Kind := Progress;
        Ae_Re (Arg_Error'Identity,
               "Incompatible ""progress"" and ""tree"" options");
    end if;
    declare
      use type Sys_Calls.File_Desc_Kind_List;
    begin
      if Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty then
        Output_Kind := Progress;
      else
        Output_Kind := None;
      end if;
    end;
  elsif Arg_Dscr.Is_Set (14) then
    Format := Xml_Parser.Generator.Raw;
  elsif Arg_Dscr.Is_Set (15) then
    -- Silent
    Output_Kind := None;
  end if;

  if Output_Kind = Gen then
    -- Options only significant in normal mode
    if Arg_Dscr.Is_Set (12) then
      -- -1
      Format := Xml_Parser.Generator.One_Per_Line;
    elsif Arg_Dscr.Is_Set (18) then
      -- -w <Width>
      Format := Xml_Parser.Generator.Fill_Width;
      if Arg_Dscr.Get_Option (18) = "" then
        Ae_Re (Arg_Error'Identity, "Width value is mandatory with -w");
      end if;
      begin
        Width := Natural'Value (Arg_Dscr.Get_Option (18));
      exception
        when others =>
          Ae_Re (Arg_Error'Identity, "Invalid Width value "
               & Arg_Dscr.Get_Option (18));
      end;
    end if;
  end if;

  -- Specific external Dtd or skip Doctype
  if Arg_Dscr.Is_Set (06) then
    -- -d: Check dtd file
    Dtd_File := As.U.Tus (Arg_Dscr.Get_Option (06));
    if Dtd_File.Is_Null then
      -- If option set with empty dtd => no check
      Use_Dtd := False;
    end if;
  end if;

  if Arg_Dscr.Is_Set (08) then
    -- -k: keep comments|cdata|none|all
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (08) loop
      -- Parse Keep options
      Keep_Comments := False;
      Keep_Cdata := False;
      declare
        Opt : constant String := Arg_Dscr.Get_Option (08, I);
        Chr : constant Boolean := Arg_Dscr.Is_Char (08, I);
      begin
        if (Chr and then Opt = "n")
        or else (not Chr and then Opt = "none") then
          if Keep_Comments_Set or else Keep_Cdata_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          -- Keep none (remove comments and Cdata)
          Keep_Comments := False;
          Keep_Comments_Set := True;
          Keep_Cdata := False;
          Keep_Cdata_Set := True;
        elsif (Chr and then Opt = "a")
        or else (not Chr and then Opt = "all") then
          -- Keep all
          if Keep_Comments_Set or else Keep_Cdata_Set then
            Ae_Re (Arg_Error'Identity, "Incompatible ""keep"" options");
          end if;
          Keep_Comments := True;
          Keep_Comments_Set := True;
          Keep_Cdata := True;
          Keep_Cdata_Set := True;
        elsif (Chr and then Opt = "c")
        or else (not Chr and then Opt = "comments") then
          if Keep_Comments_Set then
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

  -- Canonical
  if Arg_Dscr.Is_Set (03) then
    if Arg_Dscr.Is_Set (07) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""canonical"" and ""expand"" options");
    end if;
    -- No tree
    if Arg_Dscr.Is_Set (16) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""canonical"" and ""tree"" options");
    end if;
    -- No namespace
    if Arg_Dscr.Is_Set (10) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""canonical"" and ""namespace"" options");
    end if;
    -- No no-normalized (forced)
    if Arg_Dscr.Is_Set (11) then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""canonical"" and ""no-normalize"" options");
    end if;
    -- No other keep than -k c
    if Arg_Dscr.Is_Set (08) and then Keep_Cdata_Set then
      Ae_Re (Arg_Error'Identity,
             "Incompatible ""canonical"" and ""keep cdata"" options");
    end if;
    -- Skip comments by default, don't keep others
    Keep_Comments := Keep_Comments_Set;
    Expand := True;
    Keep_Cdata := False;
    Output_Kind := Canon;
    Format := Xml_Parser.Generator.Fill_Width;
    Width := Xml_Parser.Generator.Infinite_Width;
    Normalize := False;
    Namespace := False;
    Callback_Acc := Canon_Callback'Unrestricted_Access;
  end if;

  -- Copy tree => tree mode (no standard nor Canonical Cb)
  if Copy_Tree and then Callback_Acc /= null then
    Ae_Re (Arg_Error'Identity,
      "Incompatible ""copy_tree"" and ""callback"" (or ""canonical"") options");
  end if;

  -- No normalize
  if Arg_Dscr.Is_Set (11) then
    Normalize := False;
  end if;

  -- Validate
  if Arg_Dscr.Is_Set (17) then
    if Arg_Dscr.Is_Set (03) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""canonical"" options");
    end if;
    if Arg_Dscr.Is_Set (04) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""copytree"" options");
    end if;
    if Arg_Dscr.Is_Set (07) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""expand"" options");
    end if;
    if Arg_Dscr.Is_Set (08) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""keep"" options");
    end if;
    if Arg_Dscr.Is_Set (09) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""mixed"" options");
    end if;
    if Arg_Dscr.Is_Set (10) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""namespace"" options");
    end if;
    if Arg_Dscr.Is_Set (11) then
      Ae_Re (Arg_Error'Identity,
        "Incompatible ""validate"" and ""normalize"" options");
    end if;
    Keep_Cdata := False;
    Keep_Comments := False;
    Expand := True;
  end if;

  -- Set Xml_Parser Cdata policy
  if Keep_Cdata then
    Cdata_Policy := Xml_Parser.Keep_Cdata_Section;
  else
    Cdata_Policy := Xml_Parser.Remove_Cdata_Markers;
  end if;

  -- Process files
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

exception
  when Error:Arg_Error =>
    -- Argument error
    Basic_Proc.Put_Line_Error ("Error "
         & Ada.Exceptions.Exception_Message(Error) & ".");
    Usage (False);
    Basic_Proc.Set_Error_Exit_Code;
  when Abort_Error =>
    -- Error already put while parsing file
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    -- Unexpected or internal error
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage (False);
    Basic_Proc.Set_Error_Exit_Code;
end Xml_Checker;

