with Ada.Exceptions;
with Basic_Proc, Directory, Sys_Calls, Text_Char, Ada_Parser, String_Mng,
     Mixed_Str, As.U.Utils, Parser;
with Debug, Sort;
package body Sourcer is

  -- The empty dscr
  Default_Dscr : Src_Dscr;
  Empty_Dscr : constant Src_Dscr := Default_Dscr;

  -- Is separator for iterator
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Separator;
  end Is_Sep;

  -- Operations for unique list of Src_Dscr
  subtype Src_Code is String (1 .. 2);
  Src_Codes : constant array (Src_Kind_List) of Src_Code := ("US", "UB", "SU");
  procedure Set (To : out Src_Dscr; Val : in Src_Dscr) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Src_Dscr; Criteria : Src_Dscr) return Boolean is
  begin
    return Current.Kind = Criteria.Kind
    and then
     Sort.Make_Path (Current.Path, Current.Unit) =
     Sort.Make_Path (Criteria.Path, Criteria.Unit);
  end "=";
  function Image (Element : Src_Dscr) return String is
  begin
    return Sort.Make_Path (Element.Path, Element.Unit)
         & "#" & Src_Codes(Element.Kind);
  end Image;

  -- Operations for unique list of Name_Dscr
  procedure Set (To : out Name_Dscr; Val : in Name_Dscr) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Name_Dscr; Criteria : Name_Dscr) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Unit = Criteria.Unit;
  end "=";
  function Image (Element : Name_Dscr) return String is
  begin
    return Element.Unit.Image;
  end Image;

  -- Operations for unique list of Withing_Dscr
  procedure Set (To : out Withing_Dscr; Val : in Withing_Dscr) is begin
    To := Val;
  end Set;
  function "=" (Current : Withing_Dscr; Criteria : Withing_Dscr)
               return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Unit = Criteria.Unit;
  end "=";
  function Image (Element : Withing_Dscr) return String is
  begin
    return Element.Unit.Image;
  end Image;

  -- Dump a unit dscr
  procedure Dump (Dscr : in Src_Dscr) is
  begin
    Basic_Proc.Put_Output ("  " & Image (Dscr));
    Basic_Proc.Put_Output (", standalone: "
           & Mixed_Str (Dscr.Standalone'Img));
    Basic_Proc.Put_Line_Output (", parent: " & Dscr.Parent.Image);
    Basic_Proc.Put_Line_Output ("  withed: " & Dscr.Witheds.Image);
    Basic_Proc.Put_Line_Output ("  withPa: " & Dscr.Witheds_Parents.Image);
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

  -- Add a cross reference if B is withing A then append B to A Withing_Dscr
  procedure Add_Withing (Withed, Path_Name : in As.U.Asu_Us) is
   Dscr : Withing_Dscr;
   Found : Boolean;
   use type As.U.Asu_Us;
  begin
    Dscr.Unit := Withed;
    -- See if it exists
    Withing_List.Search (Dscr, Found);
    if Found then
      Withing_List.Read (Dscr);
      Dscr.Withings.Append (Path_Name & Separator);
    else
      Dscr.Withings.Set (Separator & Path_Name & Separator);
    end if;
    Withing_List.Insert (Dscr);
  end Add_Withing;

  -- Parse a file
  procedure Parse_File (Dir, File : in String) is
    -- The unit descriptor
    Dscr : Src_Dscr;
    -- Its name descriptor
    Name : Name_Dscr;
    Found : Boolean;
    -- Full path and unit
    Full_Unit_Name : As.U.Asu_Us;
    -- Full file name
    Full_File_Name : As.U.Asu_Us;
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
    -- Iterator on withed units to find their parents
    Iterator : Parser.Iterator;
    use type As.U.Asu_Us, Ada_Parser.Lexical_Kind_List;
  begin
    -- Store file name and path
    Dscr.File := As.U.Tus (File);
    Full_File_Name := Sort.Make_Path (Dir, File);
    Dscr.Path := As.U.Tus (Directory.Dirname (Full_File_Name.Image));

    -- Set Kind according to file name, check if standalone
    Root_File := Sort.Make_Path (
          Dscr.Path.Image,
          Directory.File_Prefix (Dscr.File.Image));
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
         String_Mng.Substit (File(File'First .. Minus - 1), "-", ".")));
      Dscr.Unit := Dscr.Parent & "."
               & Directory.File_Prefix (File(Minus+1 .. File'Last));
    end if;
    Dscr.Unit := As.U.Tus (Mixed_Str (Dscr.Unit.Image));
    Dscr.Witheds.Set_Null;

    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Parsing file " & Full_File_Name.Image);
    end if;

    -- Open
    begin
      Fd := Sys_Calls.Open (Full_File_Name.Image, Sys_Calls.In_File);
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

    -- Append Path for this unit name
    if Dscr.Kind = Unit_Spec
    or else (Dscr.Kind = Unit_Body and then Dscr.Standalone) then
      -- Read name if it exists
      Name.Unit := Dscr.Unit;
      Name_List.Search (Name, Found);
      if Found then
        Name_List.Read (Name);
        Name.Paths.Append (Dscr.Path & Separator);
      else
        Name.Paths.Set (Separator & Dscr.Path & Separator);
      end if;
      Name_List.Insert (Name);
    end if;

    -- Store parents of withed units
    Full_Unit_Name := As.U.Tus (Directory.Build_File_Name
         (Dscr.Path.Image, Dscr.Unit.Image, ""));
    if not Dscr.Witheds.Is_Null then
      Dscr.Witheds.Append (Separator);
      -- Scan each withed unit and and appends its parents if any
      Iterator.Set (Dscr.Witheds.Image, Is_Sep'Access);
      loop
        declare
          -- The current withed unit
          Unit : constant String := Iterator.Next_Word;
          -- Depth of parent
          Depth : Positive;
          Dot : Natural;
          -- New parent
          Parent, Parent_Sep : As.U.Asu_Us;
        begin
          -- No more unit?
          exit when Unit = "";
          -- Insert cross reference to this withed unit
          Add_Withing (As.U.Tus (Unit), Full_Unit_Name);
          -- Look for successive '.' in its name
          Depth := 1;
          loop
            Dot := String_Mng.Locate (Unit, ".", Occurence => Depth);
            exit when Dot = 0;
            -- Append parent if new
            Parent := As.U.Tus (Unit(Unit'First .. Dot - 1));
            Parent_Sep := Separator & Parent & Separator;
            if String_Mng.Locate (Dscr.Witheds_Parents.Image,
                                  Parent_Sep.Image) = 0 then
              Dscr.Witheds_Parents.Append (Parent_Sep);
              -- Insert cross reference to this parent of withed unit
              Add_Withing (Parent, Full_Unit_Name);
            end if;
            Depth := Depth + 1;
          end loop;
        end;
      end loop;
      if not Dscr.Witheds_Parents.Is_Null then
        Dscr.Witheds_Parents.Append (Separator);
      end if;
      -- Replace @@ by @
      Dscr.Witheds_Parents := As.U.Tus (
          String_Mng.Substit (Dscr.Witheds_Parents.Image,
                              Separator & Separator,
                              Separator & ""));
    end if;

    if not Dscr.Useds.Is_Null then
      Dscr.Useds.Append (Separator);
    end if;
    -- Done: store and close
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
      Dir_Desc.Open (Dir);
    exception
      when Directory.Name_Error =>
        Error ("Cannot find directory " & Dir);
      when others =>
        Error ("Cannot access to directory " & Dir);
    end;
    loop
      -- Get entry
      begin
        File_Name := As.U.Tus (Dir_Desc.Next_Entry);
      exception
        when Directory.End_Error =>
          -- End of this dir
          exit;
      end;
      -- File and matches ada source?
      if Directory.File_Kind (Sort.Make_Path (Dir, File_Name.Image) )
         = Directory.File
      and then (Directory.File_Match (File_Name.Image, "*.ads")
        or else Directory.File_Match (File_Name.Image, "*.adb") ) then
        -- Yes, process it
        Parse_File (Dir, File_Name.Image);
      end if;
    end loop;

    -- Done
    Dir_Desc.Close;
  end Parse_Dir;

  -- Parse sources and build lists
  procedure Build_Lists is
    -- The list of paths to scan (in priority order)
    Paths : constant As.U.Utils.Asu_Ua.Unb_Array := Sort.Get_Paths;
    -- The unit descriptor
    Dscr, Crit : Src_Dscr;
    -- Unique list indicators
    Moved : Boolean;
    Found : Boolean;
    -- A withing and a name, for debug
    Withing : Withing_Dscr;
    Name : Name_Dscr;
    use type As.U.Asu_Us;
  begin
    -- Process paths one by one
    for I in 1 .. Paths.Length loop
      if Debug.Is_Set then
        Basic_Proc.Put_Line_Output ("Parsing dir " & Paths.Element (I).Image);
      end if;
      Parse_Dir (Paths.Element (I).Image);
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
      Crit.Path := Dscr.Path;
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
        Crit.Path := Dscr.Path;
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

      -- Dumpt Names (not empty) and Withings if any
      Basic_Proc.Put_Line_Output ("Names:");
      Name_List.Rewind;
      loop
        Name_List.Read_Next (Name, Moved);
        Basic_Proc.Put_Line_Output (Name.Unit.Image & " = "
                                  & Name.Paths.Image);
        exit when not Moved;
      end loop;
      Basic_Proc.Put_Line_Output ("Withings:");
      if not Withing_List.Is_Empty then
        Withing_List.Rewind;
        loop
          Withing_List.Read_Next (Withing, Moved);
          Basic_Proc.Put_Line_Output (Withing.Unit.Image & " <- "
                                    & Withing.Withings.Image);
          exit when not Moved;
        end loop;
      end if;
    end if;
  end Build_Lists;

  -- Does a unit name contain a '.'
  function Has_Dot (Unit : in As.U.Asu_Us) return Boolean is
  begin
    return String_Mng.Locate (Unit.Image, ".") /= 0;
  end Has_Dot;

  -- Get parent of Dscr (body or subunit)
  -- Return Dscr itself if it is a spec or a standalone body
  function Get_Parent (Dscr : in Src_Dscr) return Src_Dscr is
    Crit : Src_Dscr;
    Found : Boolean;
  begin
    Crit.Path := Dscr.Path;
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
      Error ("No parent for " & Image (Dscr));
    end if;
    List.Read (Crit);
    return Crit;
  end Get_Parent;

  -- Get root Unit of a path/unit
  -- Return a spec or else a standalone body
  function Get_Unit (Path, Unit : in As.U.Asu_Us) return Src_Dscr is
    Crit : Src_Dscr;
    Found : Boolean;
  begin
    -- Try to get a spec
    Crit.Path := Path;
    Crit.Unit := Unit;
    Crit.Kind := Unit_Spec;
    List.Search (Crit, Found);
    if Found then
      List.Read (Crit);
      return Crit;
    end if;
    -- Try to get a body
    Crit.Kind := Unit_Body;
    List.Search (Crit, Found);
    if Found then
      List.Read (Crit);
      -- Body must be standalone
      if Crit.Standalone then
        return Crit;
      else
        return Empty_Dscr;
      end if;
    else
      -- Try to get a subunit
      Crit.Kind := Subunit;
      List.Search (Crit, Found);
      if Found then
        List.Read (Crit);
        return Crit;
      else
        return Empty_Dscr;
      end if;
    end if;
  end Get_Unit;
  function Get_Unit (Path_Unit : in As.U.Asu_Us) return Src_Dscr is
  begin
    return Get_Unit (As.U.Tus (Directory.Dirname (Path_Unit.Image)),
                     As.U.Tus (Directory.Basename (Path_Unit.Image)));
  end Get_Unit;


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

