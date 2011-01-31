with Ada.Exceptions;
with Basic_Proc, Directory, Sys_Calls, Text_Char, Ada_Parser, String_Mng,
     Mixed_Str;
with Debug;
package body Sourcer is

  -- Operations for Unique_list managmeent
  subtype Src_Code is String (1 .. 2);
  Src_Codes : constant array (Src_Kind_List) of Src_Code := ("US", "UB", "SU");
  procedure Set (To : out Src_Dscr; Val : in Src_Dscr) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Src_Dscr; Criteria : Src_Dscr) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Kind = Criteria.Kind
    and then Current.Unit = Criteria.Unit;
  end "=";
  function Image (Element : Src_Dscr) return String is
  begin
    return Element.Unit.Image & "#" & Src_Codes(Element.Kind);
  end Image;

  -- Dump a unit dscr
  procedure Dump (Dscr : in Src_Dscr) is
  begin
    Basic_Proc.Put_Output ("  " & Image (Dscr));
    Basic_Proc.Put_Output (", standalone: "
           & Mixed_Str (Dscr.Standalone'Img));
    Basic_Proc.Put_Line_Output (", parent: " & Dscr.Parent.Image);
    Basic_Proc.Put_Line_Output ("  withed: " & Dscr.Witheds.Image);
    Basic_Proc.Put_Line_Output ("  used  : " & Dscr.Useds.Image);
  end Dump;

  -- Report an error and raise exception
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    raise Error_Raised;
  end Error;

  -- Does a file exist
  function File_Exists (Root, Suffix : String) return Boolean is
  begin
    return Sys_Calls.File_Check (Directory.Build_File_Name ("", Root, Suffix));
  exception
    when Sys_Calls.Access_Error =>
      Error ("Error accessing file "
        & Directory.Build_File_Name ("", Root, Suffix));
      return False;
  end File_Exists;

  -- Get next significant word (reserved or identifier or delimiter)
  -- (skips comments, separators, literals)
  procedure Next_Word (Txt : in Text_Char.File_Type;
                       Ctx : in out Ada_Parser.Parsing_Context;
                       Word : out As.U.Asu_Us;
                       Lexic : out Ada_Parser.Lexical_Kind_List) is
    use type Ada_Parser.Lexical_Kind_List;
  begin
    loop
      Ada_Parser.Parse_Next (Txt, Ctx, Word, Lexic, Raise_End => True);
      exit when Lexic = Ada_Parser.Reserved_Word
      or else Lexic = Ada_Parser.Identifier
      or else Lexic = Ada_Parser.Delimiter;
    end loop;
  end Next_Word;

  -- Parse a file
  procedure Parse_File (Dir, File : in String) is
    -- The unit descriptor
    Dscr : Src_Dscr;
    -- Text_Char stuff
    Fd : Sys_Calls.File_Desc;
    Txt : Text_Char.File_Type;
    -- File root: path/prefix
    Root_File : As.U.Asu_Us;
    -- Last Minus '-' separator index in file name
    Minus : Natural;
    -- Ada_Parser stuff
    Context : Ada_Parser.Parsing_Context;
    Word : As.U.Asu_Us;
    Lexic : Ada_Parser.Lexical_Kind_List;
    -- Are we in a with / a use statement
    In_With : Boolean;
    In_Use : Boolean;
    -- Is prev delimiter a "."
    Prev_Dot : Boolean;
    use type As.U.Asu_Us, Ada_Parser.Lexical_Kind_List;
  begin
    -- Store local or full file name
    if Dir = "." then
      Dscr.File := As.U.Tus (File);
    else
      Dscr.File := As.U.Tus (Directory.Build_File_Name (Dir, File, ""));
    end if;

    -- Set Kind according to file name, check if standalone
    Root_File := As.U.Tus (Directory.Build_File_Name (Dir,
                            Directory.File_Prefix (File), ""));
    if Directory.File_Suffix (File) = ".ads" then
      Dscr.Kind := Unit_Spec;
      Dscr.Standalone := not File_Exists (Root_File.Image, "adb");
    else
      Dscr.Kind := Unit_Body;
      Dscr.Standalone := not File_Exists (Root_File.Image, "ads");
    end if;

    -- Store Unit and parent
    -- Locate last '-' if any, save parent
    Minus := String_Mng.Locate (File, "-", Forward => False);
    if Minus = 0 then
      Dscr.Parent.Set_Null;
      Dscr.Unit := As.U.Tus (Directory.File_Prefix (File));
    else
      -- '-' indicates either a child unit or a subunit
      -- Parsing "separate" will identify subunits
      -- Full unit name (parent.unit) without suffix
      Dscr.Parent := As.U.Tus (Mixed_Str (
         String_Mng.Replace (File(File'First .. Minus - 1), "-", ".")));
      Dscr.Unit := Dscr.Parent & "."
               & Directory.File_Prefix (File(Minus+1 .. File'Last));
    end if;
    Dscr.Unit := As.U.Tus (Mixed_Str (Dscr.Unit.Image));
    Dscr.Witheds.Set_Null;

    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Parsing file " & Dscr.File.Image);
    end if;

    -- Open
    begin
      Fd := Sys_Calls.Open (Dscr.File.Image, Sys_Calls.In_File);
    exception
      when Sys_Calls.Name_Error =>
        Error ("Cannot open file " & Dscr.File.Image);
    end;
    Txt.Open (Fd);

    -- Parse until start of unit
    loop
      -- Get next keywork or identifier
      Next_Word (Txt, Context, Word, Lexic);
      if Lexic = Ada_Parser.Reserved_Word then
        if Word.Image = "with" then
          In_With := True;
        elsif Word.Image = "use" then
          In_Use := True;
        elsif Word.Image = "procedure"
        or else Word.Image = "function"
        or else Word.Image = "package"
        or else Word.Image = "generic" then
          -- End of context clause of unit (or child)
          exit;
        elsif Word.Image = "separate" then
          -- End of context clause of subunit
          -- Parsing of file name must have led to child body
          if Dscr.Kind /= Unit_Body
          or else Dscr.Parent.Is_Null then
            Error ("Unexpected separate in unit");
          end if;
          Dscr.Kind := Subunit;
          exit;
        end if;
        -- Skip other keywords

      elsif Lexic = Ada_Parser.Delimiter then
        Prev_Dot := Word.Image = ".";
        if Word.Image = ";" then
          In_With := False;
          In_Use := False;
        end if;
      elsif In_With then
        -- Identifier in "with" statement, append in list of withed
        if Prev_Dot then
          Dscr.Witheds.Append (".");
        else
          Dscr.Witheds.Append (Separator);
        end if;
        Dscr.Witheds.Append (Word);
      elsif In_Use then
        -- Identifier in "use" statement, append in list of withed
        if Prev_Dot then
          Dscr.Useds.Append (".");
        else
          Dscr.Useds.Append (Separator);
        end if;
        Dscr.Useds.Append (Word);
      end if;
      -- Skip other (used) keywords
    end loop;

    -- Done: store and close
    if not Dscr.Witheds.Is_Null then
      Dscr.Witheds.Append (Separator);
    end if;
    if not Dscr.Useds.Is_Null then
      Dscr.Witheds.Append (Separator);
    end if;
    -- Drop new version of this unit if one already exists
    List.Insert_If_New (Dscr);
    Txt.Close;
    Sys_Calls.Close (Fd);

    if Debug.Is_Set then
      Dump (Dscr);
    end if;

  exception
    when Err:others =>
      Error ("Exception " & Ada.Exceptions.Exception_Name (Err)
                 & " raised while parsing file " & Dscr.File.Image);
  end Parse_File;

  -- Parse the files of a dir
  procedure Parse_Dir (Dir : in String) is
    Dir_Desc : Directory.Dir_Desc;
    File_Name : As.U.Asu_Us;
    use type Directory.File_Kind_List;
  begin
    begin
      Dir_Desc := Directory.Open (Dir);
    exception
      when Directory.Name_Error =>
        Error ("Cannot find directory " & Dir);
      when others =>
        Error ("Cannot access to directory " & Dir);
    end;
    loop
      -- Get entry
      begin
        File_Name := As.U.Tus (Directory.Next_Entry (Dir_Desc));
      exception
        when Directory.End_Error =>
          -- End of this dir
          exit;
      end;
      -- File and matches ada source?
      if Directory.File_Kind (
             Directory.Build_File_Name (Dir, File_Name.Image, "") )
         = Directory.File
      and then (Directory.File_Match (File_Name.Image, "*.ads")
        or else Directory.File_Match (File_Name.Image, "*.adb") ) then
        -- Yes, process it
        Parse_File (Dir, File_Name.Image);
      end if;
    end loop;

    -- Done
    Directory.Close (Dir_Desc);
  end Parse_Dir;

  -- Parse sources and build list
  Incl_Key : constant := 5;
  procedure Build_List (Args : in Argument_Parser.Parsed_Dscr) is
    -- The unit descriptor
    Dscr, Crit : Src_Dscr;
    -- Unique list indicators
    Moved : Boolean;
    Found : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Process local then include dirs
    Parse_Dir (".");
    for I in 1 .. Args.Get_Nb_Occurences (Incl_Key) loop
      Parse_Dir (Directory.Make_Full_Path (Args.Get_Option (Incl_Key, I)));
    end loop;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Parsing completed.");
    end if;
    if List.Is_Empty then
      return;
    end if;

    -- Sanity checks:
    --  A standalone spec or body must be standalone
    --  A body must have same path as its spec
    --  A subunit must have same path as its parent body
    List.Rewind;
    loop
      List.Read_Next (Dscr, Moved);
      Crit.Unit := Dscr.Unit;
      -- Search counterpart Spec -> Body, Body -> Spec, Subunit -> Spec
      case Dscr.Kind is
        when Unit_Spec => Crit.Kind := Unit_Body;
        when Unit_Body => Crit.Kind := Unit_Spec;
        when Subunit =>
          if not Dscr.Standalone then
            Error ("Unexpected not standalone subunit " & Image (Dscr));
          end if;
          Crit.Kind := Unit_Spec;
      end case;
      List.Search (Crit, Found);
      if Dscr.Standalone then
        if Found then
          Error ("Standalone " & Image (Dscr) & " has a counterpart");
        end if;
      elsif not Found then
        Error ("Not standalone " & Image (Dscr) & " has no counterpart");
      else
        -- Unit and counterpart must have same dirname
        begin
          List.Read (Crit);
        exception
          when Src_List_Mng.Not_In_List =>
            Error ("Unit " & Image (Dscr) & " has no counterpart");
        end;
        if Directory.Dirname (Dscr.File.Image)
        /= Directory.Dirname (Crit.File.Image) then
          Error ("Unit " & Image (Dscr)
                     & " not in same dir as its counterpart");
        end if;
      end if;

      -- Search parent of subunit or child
      if Has_Dot (Dscr.Unit) then
        -- Parent of child: unit or child
        Crit.Unit := Dscr.Parent;
        if Dscr.Kind = Subunit then
          if Has_Dot (Crit.Unit) then
            -- Subunit, either of a subunit or of a child
            Crit.Kind := Subunit;
            List.Search (Crit, Found);
            if not Found then
              Crit.Kind := Unit_Body;
            end if;
          else
            -- Subunit of a child
            Crit.Kind := Unit_Body;
          end if;
        else
          -- Child of a unit
          Crit.Kind := Unit_Spec;
        end if;
        -- Unit and parent must have same dirname
        begin
          List.Read (Crit);
        exception
          when Src_List_Mng.Not_In_List =>
            Error ("Unit " & Image (Dscr) & " has no parent");
        end;
        if Directory.Dirname (Dscr.File.Image)
        /= Directory.Dirname (Crit.File.Image) then
          Error ("Unit " & Image (Dscr)
                     & " not in same dir as its parent");
        end if;
        -- Update list of subunits of parent
        if Dscr.Kind = Subunit then
          if Crit.Subunits.Is_Null then
            Crit.Subunits := As.U.Tus (Separator & "");
          end if;
          Crit.Subunits.Append (Dscr.Unit & Separator);
          List.Insert (Crit);
          if Debug.Is_Set then
            Basic_Proc.Put_Line_Output ("Adding subunit " & Image (Dscr)
                     & " to " & Image (Crit));
          end if;
        end if;
      end if;

      exit when not Moved;
    end loop;

    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Checks completed.");
    end if;
  end Build_List;

  -- Does a unit name contain a '.'
  function Has_Dot (Unit : in As.U.Asu_Us) return Boolean is
  begin
    return String_Mng.Locate (Unit.Image, ".") /= 0;
  end Has_Dot;

  -- Get parent of Dscr (body or subunit)
  -- Return Dscr itself if it is a Unit_Spec
  function Get_Parent (Dscr : in Src_Dscr) return Src_Dscr is
    Crit : Src_Dscr;
    Found : Boolean;
  begin
    case Dscr.Kind is
      when Unit_Spec =>
        return Dscr;
      when Unit_Body =>
        if Dscr.Standalone then
          return Dscr;
        end if;
        Crit.Unit := Dscr.Unit;
        Crit.Kind := Unit_Spec;
      when Subunit =>
        Crit.Unit := Dscr.Parent;
        Crit.Kind := Unit_Body;
    end case;
    List.Search (Crit, Found);
    if not Found and then Dscr.Kind = Subunit then
      -- No body parent of the subunit: Look for parent subunit
      Crit.Kind := Subunit;
      List.Search (Crit, Found);
    end if;
    if not Found then
      Error ("Not parent for " & Image (Dscr));
    end if;
    List.Read (Crit);
    return Crit;
  end Get_Parent;

  -- Get Unit_Body of a subunit
  function Get_Body (Sub : in Src_Dscr) return Src_Dscr is
    Parent : Src_Dscr;
  begin
    if Sub.Kind /= Subunit then
      Error ("Cannot get root of " & Image (Sub));
    end if;
    -- Get parents until it is a body
    Parent := Sub;
    loop
      -- Get direct parent
      Parent := Get_Parent (Parent);
      exit when Parent.Kind = Unit_Body;
    end loop;
    return Parent;
  end Get_Body;

end Sourcer;

