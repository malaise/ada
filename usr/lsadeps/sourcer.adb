with Ada.Exceptions;
with Basic_Proc, Directory, Sys_Calls, Text_Char, Ada_Parser, String_Mng,
     Mixed_Str;
package body Sourcer is
  Debug : constant Boolean := True;

  -- Unbounded string
  use type Asu_Us;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;

  -- Report an error and raise exception
  procedure Raise_Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    raise Error;
  end Raise_Error;

  -- Does a file exist
  function File_Exists (Root, Suffix : String) return Boolean is
  begin
    return Sys_Calls.File_Check (Directory.Build_File_Name ("", Root, Suffix));
  exception
    when Sys_Calls.Access_Error =>
      Raise_Error ("Error accessing file "
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
      Ada_Parser.parse_Next (Txt, Word, Lexic, Raise_End => True);
      exit when Lexic = Ada_Parser.Reserved_Word
      or else Lexic = Ada_Parser.Identifier
      or else Lexic = Ada_Parser.Delimiter;
    end loop;
  end Next_Word;

  -- Parse a file
  procedure Parse_File (Dir, File : in String; Dscr : out Src_Dscr) is
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
      Dscr.Unit := Asu_Tus (Directory.File_Prefix (File));
      Dscr.Parent := Asu_Null;
    else
      -- '-' indicates either a child unit or a subunit
      -- Parsing "separate" will identify subunits
      if Dscr.Kind = Unit_Spec then
        Dscr.Kind := Child_Spec;
      else
        Dscr.Kind := Child_Body;
      end if;
      Dscr.Unit := Asu_Tus (Directory.File_Prefix (
                                File (Minus + 1 .. File'Last)));
      Dscr.Parent := Asu_Tus (Mixed_Str (
         String_Mng.Replace (File (File'First .. Minus - 1), "-", ".")));
    end if;
    Dscr.Unit := Asu_Tus (Mixed_Str (Asu_Ts (Dscr.Unit)));
    Dscr.Witheds := Asu_Null;

    if Debug then
      Basic_Proc.Put_Line_Output ("Parsing file " & Asu_Ts (Dscr.File));
    end if;

    -- Open
    begin
      Fd := Sys_Calls.Open (Asu_Ts (Dscr.File), Sys_Calls.In_File);
    exception
      when Sys_Calls.Name_Error =>
        Raise_Error ("Cannot open file " & Asu_Ts (Dscr.File));
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
          if Dscr.Kind /= Child_Body then
            Raise_Error ("Unexpected separate in unit");
          end if;
          Dscr.Kind := Subunit;
        end if;
        -- Skip other keywords

      elsif Lexic = Ada_Parser.Delimiter then
        Prev_Dot := Asu_Ts (Word) = ".";
      elsif In_With then
        -- Identifier in "with" statement, append in list of withed
        if Prev_Dot then
          Asu.Append (Dscr.Witheds, ".");
        else
          Asu.Append (Dscr.Witheds, "@");
        end if;
        Asu.Append (Dscr.Witheds, Word);
      end if;
      -- Skip other (used) keywords
    end loop;

    -- Done, close
    if Dscr.Witheds /= Asu_Null then
      Asu.Append (Dscr.Witheds, "@");
    end if;
    Txt.Close;
    Sys_Calls.Close (Fd);

    if Debug then
      Basic_Proc.Put_Line_Output ("  unit: " & Asu_Ts (Dscr.Unit));
      Basic_Proc.Put_Line_Output ("  kind: " & Mixed_Str (Dscr.Kind'Img));
      Basic_Proc.Put_Line_Output ("  parent: " & Asu_Ts (Dscr.Parent));
      Basic_Proc.Put_Line_Output ("  Standalone: "
             & Mixed_Str (Dscr.Standalone'Img));
      Basic_Proc.Put_Line_Output ("  Withed: " & Asu_Ts (Dscr.Witheds));
    end if;
  exception
    when Error:others =>
      Raise_Error ("Exception " & Ada.Exceptions.Exception_Name (Error)
                 & " raised while parsing file " & Asu_Ts (Dscr.File));
  end Parse_File;

  -- Parse the files of a dir
  procedure Parse_Dir (Dir : in String) is
    Dir_Desc : Directory.Dir_Desc;
    File_Name : Asu_Us;
    File_Desc : Src_Dscr;
    use type Directory.File_Kind_List;
  begin
    begin
      Dir_Desc := Directory.Open (Dir);
    exception
      when Directory.Name_Error =>
        Raise_Error ("Cannot find directory " & Dir);
      when others =>
        Raise_Error ("Cannot access to directory " & Dir);
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
        Parse_File (Dir, Asu_Ts (File_Name), File_Desc);
      end if;
    end loop;
    -- Done
    Directory.Close (Dir_Desc);
  end Parse_Dir;

  -- Parse sources and build list
  Incl_Key : constant := 5;
  procedure Build_List (Args : in Argument_Parser.Parsed_Dscr) is
  begin
    -- Process local then include dirs
    Parse_Dir (".");
    for I in 1 .. Args.Get_Nb_Occurences (Incl_Key) loop
      Parse_Dir (Args.Get_Option (Incl_Key, I));
    end loop;
  end Build_List;

end Sourcer;

