with Ada.Exceptions;
with Basic_Proc, Directory, Sys_Calls, Text_Char, Ada_Parser, String_Mng,
     Mixed_Str;
with Debug;
package body Sourcer is

  use type Asu_Us;

  -- Operations for Unique_list managmeent
  subtype Src_Code is String (1 .. 2);
  Src_Codes : constant array (Src_Kind_List) of Src_Code := ("US", "UB", "SU");
  procedure Set (To : out Src_Dscr; Val : in Src_Dscr) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Src_Dscr; Criteria : Src_Dscr) return Boolean is
  begin
    return Current.Kind = Criteria.Kind
    and then Current.Unit = Criteria.Unit;
  end "=";
  function Image (Element : Src_Dscr) return String is
  begin
    return Asu_Ts (Element.Unit) & "#" & Src_Codes(Element.Kind);
  end Image;

  -- Dump a unit dscr
  procedure Dump (Dscr : in Src_Dscr) is
  begin
    Basic_Proc.Put_Output ("  " & Image (Dscr));
    Basic_Proc.Put_Output (", standalone: "
           & Mixed_Str (Dscr.Standalone'Img));
    Basic_Proc.Put_Line_Output (", parent: " & Asu_Ts (Dscr.Parent));
    Basic_Proc.Put_Line_Output ("  withed: " & Asu_Ts (Dscr.Witheds));
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
                       Word : out Asu_Us;
                       Lexic : out Ada_Parser.Lexical_Kind_List) is
    use type Ada_Parser.Lexical_Kind_List;
  begin
    loop
      Ada_Parser.Parse_Next (Txt, Word, Lexic, Raise_End => True);
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
    Root_File : Asu_Us;
    -- Last Minus '-' separator index in file name
    Minus : Natural;
    -- Ada_Parser stuff
    Word : Asu_Us;
    Lexic : Ada_Parser.Lexical_Kind_List;
    use type Ada_Parser.Lexical_Kind_List;
    -- Are we in a with statement
    In_With : Boolean;
    -- Is prev delimiter a "."
    Prev_Dot : Boolean;
  begin
    -- Store local or full file name
    if Dir = "." then
      Dscr.File := Asu_Tus (File);
    else
      Dscr.File := Asu_Tus (Directory.Build_File_Name (Dir, File, ""));
    end if;

    -- Set Kind according to file name, check if standalone
    Root_File := Asu_Tus (Directory.Build_File_Name (Dir,
                            Directory.File_Prefix (File), ""));
    if Directory.File_Suffix (File) = ".ads" then
      Dscr.Kind := Unit_Spec;
      Dscr.Standalone := not File_Exists (Asu_Ts (Root_File), "adb");
    else
      Dscr.Kind := Unit_Body;
      Dscr.Standalone := not File_Exists (Asu_Ts (Root_File), "ads");
    end if;

    -- Store Unit and parent
    -- Locate last '-' if any, save parent
    Minus := String_Mng.Locate (File, "-", Forward => False);
    if Minus = 0 then
      Dscr.Parent := Asu_Null;
      Dscr.Unit := Asu_Tus (Directory.File_Prefix (File));
    else
      -- '-' indicates either a child unit or a subunit
      -- Parsing "separate" will identify subunits
      -- Full unit name (parent.unit) without suffix
      Dscr.Parent := Asu_Tus (Mixed_Str (
         String_Mng.Replace (File(File'First .. Minus - 1), "-", ".")));
      Dscr.Unit := Dscr.Parent & "."
               & Directory.File_Prefix (File(Minus+1 .. File'Last));
    end if;
    Dscr.Unit := Asu_Tus (Mixed_Str (Asu_Ts (Dscr.Unit)));
    Dscr.Witheds := Asu_Null;

    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Parsing file " & Asu_Ts (Dscr.File));
    end if;

    -- Open
    begin
      Fd := Sys_Calls.Open (Asu_Ts (Dscr.File), Sys_Calls.In_File);
    exception
      when Sys_Calls.Name_Error =>
        Error ("Cannot open file " & Asu_Ts (Dscr.File));
    end;
    Txt.Open (Fd);

    -- Parse until start of unit
    loop
      -- Get next keywork or identifier
      Next_Word (Txt, Word, Lexic);
      if Lexic = Ada_Parser.Reserved_Word then
        if Asu_Ts (Word) = "with" then
          In_With := True;
        elsif Asu_Ts (Word) = "use" then
          In_With := True;
        elsif Asu_Ts (Word) = "procedure"
        or else Asu_Ts (Word) = "function"
        or else Asu_Ts (Word) = "package"
        or else Asu_Ts (Word) = "generic" then
          -- End of context clause of unit (or child)
          exit;
        elsif Asu_Ts (Word) = "separate" then
          -- End of context clause of subunit
          -- Parsing of file name must have led to child body
          if Dscr.Kind /= Unit_Body
          or else Dscr.Parent = Asu_Null then
            Error ("Unexpected separate in unit");
          end if;
          Dscr.Kind := Subunit;
          exit;
        end if;
        -- Skip other keywords

      elsif Lexic = Ada_Parser.Delimiter then
        Prev_Dot := Asu_Ts (Word) = ".";
      elsif In_With then
        -- Identifier in "with" statement, append in list of withed
        if Prev_Dot then
          Asu.Append (Dscr.Witheds, ".");
        else
          Asu.Append (Dscr.Witheds, Separator);
        end if;
        Asu.Append (Dscr.Witheds, Word);
      end if;
      -- Skip other (used) keywords
    end loop;

    -- Done: store and close
    if Dscr.Witheds /= Asu_Null then
      Asu.Append (Dscr.Witheds, Separator);
    end if;
    -- Drop new version of this unit if one already exists
    List.Insert (Dscr, Drop => True);
    Txt.Close;
    Sys_Calls.Close (Fd);

    if Debug.Is_Set then
      Dump (Dscr);
    end if;

  exception
    when Err:others =>
      Error ("Exception " & Ada.Exceptions.Exception_Name (Err)
                 & " raised while parsing file " & Asu_Ts (Dscr.File));
  end Parse_File;

  -- Parse the files of a dir
  procedure Parse_Dir (Dir : in String) is
    Dir_Desc : Directory.Dir_Desc;
    File_Name : Asu_Us;
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
        File_Name := Asu_Tus (Directory.Next_Entry (Dir_Desc));
      exception
        when Directory.End_Error =>
          -- End of this dir
          exit;
      end;
      -- File and matches ada source?
      if Directory.File_Kind (
             Directory.Build_File_Name (Dir, Asu_Ts (File_Name), "") )
         = Directory.File
      and then (Directory.File_Match (Asu_Ts (File_Name), "*.ads")
        or else Directory.File_Match (Asu_Ts (File_Name), "*.adb") ) then
        -- Yes, process it
        Parse_File (Dir, Asu_Ts (File_Name));
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
  begin
    -- Process local then include dirs
    Parse_Dir (".");
    for I in 1 .. Args.Get_Nb_Occurences (Incl_Key) loop
      Parse_Dir (Directory.Make_Full_Path (Args.Get_Option (Incl_Key, I)));
    end loop;
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Parsing completed.");
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
          List.Read (Crit, Crit);
        exception
          when Src_List_Mng.Not_In_List =>
            Error ("Unit " & Image (Dscr) & " has no counterpart");
        end;
        if Directory.Dirname (Asu_Ts (Dscr.File))
        /= Directory.Dirname (Asu_Ts (Crit.File)) then
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
          List.Read (Crit, Crit);
        exception
          when Src_List_Mng.Not_In_List =>
            Error ("Unit " & Image (Dscr) & " has no parent");
        end;
        if Directory.Dirname (Asu_Ts (Dscr.File))
        /= Directory.Dirname (Asu_Ts (Crit.File)) then
          Error ("Unit " & Image (Dscr)
                     & " not in same dir as its parent");
        end if;
        -- Update list of subunits of parent
        if Dscr.Kind = Subunit then
          if Crit.Subunits = Asu_Null then
            Crit.Subunits := Asu_Tus (Separator & "");
          end if;
          Asu.Append (Crit.Subunits, Dscr.Unit & Separator);
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
  function Has_Dot (Unit : in Asu_Us) return Boolean is
  begin
    return String_Mng.Locate (Asu_Ts (Unit), ".") /= 0;
  end Has_Dot;

  -- Get name of library unit parent (of a subunit)
  function Get_Root (Sub : in Src_Dscr) return Src_Dscr is
    Crit : Src_Dscr;
    Found : Boolean;
  begin
    Crit := Sub;
    loop
      -- Search parent
      Crit.Unit := Crit.Parent;
      -- Search a unit body
      Crit.Kind := Unit_Body;
      List.Search (Crit, Found);
      exit when Found;
      -- Search a subunit
      Crit.Kind := Subunit;
      List.Search (Crit, Found);
      if not Found then
        Error ("Not parent body for " & Asu_Ts (Crit.Unit));
      end if;
    end loop;
    -- Read it
    List.Read (Crit, Crit);
    return Crit;
  end Get_Root;

end Sourcer;

