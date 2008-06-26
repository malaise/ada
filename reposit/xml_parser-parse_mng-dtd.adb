with String_Mng, Regular_Expressions, Parser, Int_Image;
separate (Xml_Parser.Parse_Mng)
package body Dtd is

  -- Image of line_no without leading space
  function Line_Image is new Int_Image (Natural);

  -- Separator within Info name and list
  Info_Sep : constant Character := '#';
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Info_Sep;
  end Is_Sep;

  -- Init (clear) Dtd data
  procedure Init (Adtd : in out Dtd_Type) is
    Info : Info_Rec;
  begin
    -- No Dtd set
    Adtd.Set := False;
    -- No xml instruction found (yet)
    Adtd.Xml_Found := False;
    -- Clear entities
    Entity_Mng.Initialise (Adtd.Entity_List);
    -- Reset info list
    Info_Mng.Delete_List (Adtd.Info_List);
    -- Init with empty IDs and empty IDREFs
    Info.Name := Asu_Tus ("Idl");
    Info_Mng.Insert (Adtd.Info_List, Info);
    Info.Name := Asu_Tus ("Idr");
    Info_Mng.Insert (Adtd.Info_List, Info);
  end Init;

  -- Parse an instruction:
  -- Check xml version, append any other instruction to prologue
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type;
                               External : in Boolean) is
    Ok : Boolean;
  begin
    -- See if this is the xml directive
    Util.Check_Cdata (Ctx.Flow, 3);
    Util.Try (Ctx.Flow, "xml", Ok);
    if Ok then
      if not External then
        Util.Error (Ctx.Flow, "Invalid xml instruction in internal dtd");
      end if;
      if Adtd.Xml_Found then
        Util.Error (Ctx.Flow, "Second declaration of xml in dtd");
      end if;
      Util.Parse_Until_Str (Ctx.Flow, Util.Instruction & Util.Stop);
      Adtd.Xml_Found := True;
      Trace ("Dtd parsed instruction " & Asu_Ts(Util.Get_Curr_Str (Ctx.Flow)));
      Util.Reset_Curr_Str (Ctx.Flow);
    else
      -- Parse instruction as if in Xml
      Parse_Instruction (Ctx, Adtd);
    end if;
  end Parse_Instruction;

  -- Build the regexp: <name> -> (#<name>#) ,  . -> \.
  procedure Build_Regexp (Ctx : in out Ctx_Type; Str : in out Asu_Us) is
    -- Separators
    Seps : constant String := "|,?*+()";
    -- Result with the info sep
    Res : Asu_Us;
    -- Intermediate logic
    In_Word : Boolean := False;
    C : Character;

    function Is_Sep (C : Character) return Boolean is
    begin
      for I in Seps'Range loop
        if C = Seps(I) then
          return True;
        end if;
      end loop;
      return False;
    end Is_Sep;
  begin
    for I in 1 .. Asu.Length (Str) loop
      C := Asu.Element (Str, I);
      if Is_Sep (C) then
        if In_Word then
          -- End of word
          Asu.Append (Res, "#)");
          In_Word := False;
        end if;
      else
        if not In_Word then
          -- Start of word
          Asu.Append (Res, "(#");
          In_Word := True;
        end if;
      end if;
      Asu.Append (Res, C);
    end loop;
    if In_Word then
      Asu.Append (Res, "#)");
    end if;
    -- Remove any ','
    Res := Asu_Tus (String_Mng.Replace (Asu_Ts (Res), ",", ""));
    -- Now compile to check it
    if not Regular_Expressions.Check (Asu_Ts (Res)) then
      Trace ("Dtd regex does node compile >" & Asu_Ts (Res) & "<");
      Util.Error (Ctx.Flow, "Invalid children definition");
    end if;
    -- Done
    Str := Res;
  end Build_Regexp;

  -- Parse <!ELEMENT
  procedure Parse_Element (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Info : Info_Rec;
    Info_Name : Asu_Us;
    Found : Boolean;
    Char : Character;
    use type Asu_Us;
  begin
    -- Parse element name
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Info_Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Info_Name, In_Dtd => True);
    if not Util.Name_Ok (Info_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Asu_Ts (Info_Name));
    end if;
    Info.Name := "Elt" & Info_Sep & Info_Name;
    -- Element must not exist
    Info_Mng.Search (Adtd.Info_List, Info, Found);
    if Found then
      Util.Error (Ctx.Flow, "ELEMENT " & Asu_Ts (Info_Name)
                          & " already exists");
    end if;
    -- Parse type
    Util.Skip_Separators (Ctx.Flow);
    Util.Check_Cdata (Ctx.Flow, 5);
    Util.Try (Ctx.Flow, "EMPTY", Found);
    if Found then
      Info.List := Asu_Tus ("E");
    else
      Util.Try (Ctx.Flow, "ANY", Found);
      if Found then
        Info.List := Asu_Tus ("A");
      end if;
    end if;
    if not Found then
      -- A list of sub-elements, possibly containing #PCDATA,
      --  possibly followed by '*'
      -- Must be '('
      Util.Get (Ctx.Flow, Char);
      if Char /= '(' then
        Util.Error (Ctx.Flow, "Unexpected character " & Char
                 & " at start of ELEMENT list");
      end if;
      -- Get children definition (until ')' matching the '(' got)
      Util.Parse_Until_Close (Ctx.Flow);
      Util.Skip_Separators (Ctx.Flow);
      Info.List := Util.Remove_Separators (Util.Get_Curr_Str (Ctx.Flow));
      Util.Fix_Text (Ctx, Adtd, Info.List, True, False);
      Util.Reset_Curr_Str (Ctx.Flow);
      -- Now see if it is mixed or children
      if Asu.Index (Info.List, "#PCDATA") /= 0 then
        -- Mixed
        if Asu_Ts (Info.List) = "#PCDATA" then
          Info.List := Asu_Null;
        elsif Asu.Slice (Info.List, 1, 8) = "#PCDATA|" then
          -- Remove heading #PCDATA
          Info.List := Asu_Tus (
              String_Mng.Cut (Asu_Ts (Info.List), 8));
          -- Expand variables if any
          Util.Fix_Text (Ctx, Adtd, Info.List, True, False);
          Info.List := Util.Remove_Separators (Info.List);
          -- Check that everything between "|" are names
          if Asu.Element (Info.List, Asu.Length (Info.List)) = '|'
          or else Asu.Element (Info.List, 1) = '|' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          if not Util.Names_Ok (Info.List, "|") then
            Util.Error (Ctx.Flow, "Invalid name in Mixed definition");
          end if;
          -- Last ')' must be followed by "*", remove it
          Util.Get (Ctx.Flow, Char);
          if Char /= '*' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          -- Replace '|' by '#' and prepend and append a '#'
          Info.List := Asu_Tus (
            String_Mng.Replace ("#" & Asu_Ts (Info.List) & "#", "|", "#"));
        else
          Util.Error (Ctx.Flow, "Invalid Mixed definition");
        end if;
        Info.List := "M" & Info.List;
      else
        -- A regexp of children:
        -- Put into "(" ")" and append "?", "*" or, "+" if needed
        Util.Get (Ctx.Flow, Char);
        if Char = '?' or else Char = '*' or else Char = '+' then
          Info.List := "(" & Info.List & ")" & Char;
        else
          Util.Unget (Ctx.Flow);
        end if;
        -- Expand variables if any
        Util.Fix_Text (Ctx, Adtd, Info.List, True, False);
        Info.List := Util.Remove_Separators (Info.List);
        -- Check valid names
        if not Util.Names_Ok (Info.List, "|,?*+()") then
          Util.Error (Ctx.Flow, "Invalid name in Children definition");
        end if;
        -- Fix regex: each name becomes "(#name#)"
        Build_Regexp (Ctx, Info.List);
        Info.List := "C" & Info.List;
      end if;
    end if;
    -- Directive must end now
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character " & Char
                          & " at end of ELEMENT");
    end if;
    -- Store element
    Info_Mng.Insert (Adtd.Info_List, Info);
    Trace ("Dtd parsed directive ELEMENT -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Element;


  -- Parse <!ATTLIST
  procedure Parse_Attlist (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Atl, Att and Id info blocs
    Info, Attinfo, Idinfo : Info_Rec;
    Found : Boolean;
    -- Element, attribute name and type
    Elt_Name, Att_Name, Att_Type : Asu_Us;
    -- Type and default chars
    Typ_Char, Def_Char : Character;
    -- Enum List
    Enum : Asu_Us;
    -- Default value
    Def_Val : Asu_Us;
    -- Has an ID already been parsed for this element
    Elt_Has_Id : Boolean;
    -- Is this attribute already defined
    Attr_Already_Set : Boolean;
    use type Asu_Us;

    function Try (Str : String) return Boolean is
      B : Boolean;
    begin
      Util.Try (Ctx.Flow, Str, B);
      return B;
    end Try;
    function Get return Character is
      C : Character;
    begin
      Util.Get (Ctx.Flow, C);
      return C;
    end Get;

  begin
    -- Parse element name
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & Util.Stop);
    Util.Unget (Ctx.Flow);
    Elt_Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Elt_Name, In_Dtd => True);
    if not Util.Name_Ok (Elt_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Asu_Ts (Elt_Name));
    end if;
    Info.Name := "Atl" & Info_Sep & Elt_Name;
    -- Attribute list of this element may already exist => merge
    Info_Mng.Search (Adtd.Info_List, Info, Found);
    if Found then
      Info_Mng.Read (Adtd.Info_List, Info, Info);
      Trace ("Dtd retrieved previous ATTLIST -> " & Asu_Ts (Info.Name)
           & " " & Asu_Ts (Info.List));
    end if;

    -- Loop on all attributes
    Elt_Has_Id := False;
    loop
      Util.Skip_Separators (Ctx.Flow);
      -- Name of attribute or end of list
      Util.Try (Ctx.Flow, Util.Stop & "", Found);
      exit when Found;
      -- Get attribute name
      Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
      Att_Name := Util.Get_Curr_Str (Ctx.Flow);
      Util.Reset_Curr_Str (Ctx.Flow);
      Util.Expand_Name (Ctx, Adtd, Att_Name, In_Dtd => True);
      if not Util.Name_Ok (Att_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute " & Asu_Ts (Att_Name));
      end if;
      -- Check that this attribute is not already defined, otherwise discard any new
      -- Look for #attribute##
      Attr_Already_Set := String_Mng.Locate (Asu_Ts (Info.List),
             Info_Sep & Asu_Ts (Att_Name) & Info_Sep & Info_Sep) /= 0;
      -- Check supported att types
      Util.Skip_Separators (Ctx.Flow);
      Util.Check_Cdata (Ctx.Flow, 9);
      if Try ("CDATA ") then
        -- String type
        Typ_Char := 'S';
      elsif Try ("ID ") then
        -- ID type
        Typ_Char := 'I';
      elsif Try ("IDREF ") then
        -- IDREF type
        Typ_Char := 'R';
      elsif Try ("IDREFS ") then
        -- IDREFS type
        Typ_Char := 'r';
      elsif Try ("NMTOKEN ") then
        -- NMTOKEN type
        Typ_Char := 'T';
      elsif Try ("NMTOKENS ") then
        -- NMTOKENS type
        Typ_Char := 't';
      elsif Get = '(' then
        -- Enum type
        Typ_Char := 'E';
        Util.Parse_Until_Char (Ctx.Flow, ")");
        Enum := Util.Get_Curr_Str (Ctx.Flow);
        Util.Fix_Text (Ctx, Adtd, Enum, True, False);
        Enum := Util.Remove_Separators (Enum);
        Util.Reset_Curr_Str (Ctx.Flow);
        -- Check that everything between "|" are names
        if Asu.Element (Enum, Asu.Length (Enum)) = '|'
        or else Asu.Element (Enum, 1) = '|' then
          Util.Error (Ctx.Flow, "Invalid Enum definition");
        end if;
        if not Util.Names_Ok (Enum, "|") then
          Util.Error (Ctx.Flow, "Invalid name in Enum definition");
        end if;
        -- Replace '|' by '#' and prepend and append a '#'
        Enum := Asu_Tus (
          String_Mng.Replace ("#" & Asu_Ts (Enum) & "#", "|", "#"));
      else
        -- Get type
        Util.Unget (Ctx.Flow);
        Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
        Att_Type := Util.Get_Curr_Str (Ctx.Flow);
        Util.Reset_Curr_Str (Ctx.Flow);
        if Asu_Ts (Att_Type) = "ENTITY"
        or else Asu_Ts (Att_Type) = "ENTITIES"
        or else Asu_Ts (Att_Type) = "NOTATION" then
          Util.Error (Ctx.Flow, "Unsupported attribute type "
                              & Asu_Ts (Att_Type));
        else
          Util.Error (Ctx.Flow, "Invalid attribute type " & Asu_Ts (Att_Type));
        end if;
      end if;

      -- Check supported att defaults
      Util.Skip_Separators (Ctx.Flow);
      Util.Check_Cdata (Ctx.Flow, 9);
      if Try ("#REQUIRED") then
        Def_Char := 'R';
      elsif Try ("#IMPLIED") then
        Def_Char := 'I';
      elsif Try ("#FIXED ") then
        Def_Char := 'F';
      else
        Def_Char := 'D';
      end if;

      -- Check no default value or get default value
      Util.Skip_Separators (Ctx.Flow);
      if Def_Char = 'R' or else Def_Char = 'I' then
        -- There shall be no default value for required or implied attribute
        if Try ("""") or else Try ("'") then
          Util.Error (Ctx.Flow, "Unexpected default value for attribute "
                    &  Asu_Ts (Att_Name));
        end if;
      else
        -- Get default value for fixed or default attribute
        Parse_Value (Ctx, Adtd, True, Def_Val);
      end if;

      -- Check ID
      if Typ_Char = 'I' and then not Attr_Already_Set then
        -- Only one ID per element
        if Elt_Has_Id then
          Util.Error (Ctx.Flow, "Element " & Asu_Ts (Elt_Name)
                    & " has already an ID attribute");
        end if;
        -- Id must be implied or required
        if Def_Char /= 'R' and then Def_Char /= 'I' then
          Util.Error (Ctx.Flow,
               "Id attribute must be of type required or implied");
        end if;
        -- Initialise an Empty Ide info
        Elt_Has_Id := True;
        Idinfo.Name := "Ide" & Elt_Name;
        Idinfo.List := Asu_Null;
        Info_Mng.Insert (Adtd.Info_List, Idinfo);
      end if;

      -- Check Enum
      if Typ_Char = 'E'
      and then (Def_Char = 'D' or else Def_Char = 'F') then
        -- Enum and (default or fixed), check default is in enum
        --  and set the default in first pos
        if (String_Mng.Locate (Asu_Ts (Enum),
              Info_Sep & Asu_Ts (Def_Val) & Info_Sep) = 0) then
          Util.Error (Ctx.Flow, "Default or fixed value "
                    & Asu_Ts (Def_Val) & " not in Enum");
        end if;
        Enum := Asu_Tus (String_Mng.Replace (
                 Asu_Ts (Enum),
                 Info_Sep & Asu_Ts (Def_Val) & Info_Sep,
                 ""));
        Enum := Info_Sep & Asu_Ts (Def_Val) & Info_Sep & Enum;
      end if;

      -- If enum store Att of enum
      --  or if fixed or default store Att of default
      if not Attr_Already_Set then
        Attinfo.Name := "Att" & Info_Sep & Elt_Name & Info_Sep & Att_Name;
        if Typ_Char = 'E' then
          Attinfo.List := Enum;
          Info_Mng.Insert (Adtd.Info_List, Attinfo);
          Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
           & " " & Asu_Ts(Attinfo.List));
        elsif Def_Char = 'F' or else Def_Char = 'D' then
          Attinfo.List := Def_Val;
          Info_Mng.Insert (Adtd.Info_List, Attinfo);
          Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
           & " " & Asu_Ts(Attinfo.List));
        end if;
        -- Append this attribute in list: #attribute##td#attribute##td#...
        if Info.List = Asu_Null then
          Asu.Append (Info.List, Info_Sep);
        end if;
        Asu.Append (Info.List, Att_Name
                  & Info_Sep & Info_Sep & Typ_Char & Def_Char & Info_Sep);
      else
        Trace ("Dtd discarding duplicate ATTLIST -> " & Asu_Ts (Info.Name)
             & " " & Asu_Ts (Att_Name) & Info_Sep & Info_Sep
             & Typ_Char & Def_Char);
      end if;
    end loop;
    -- Attlist is ended: store
    Info_Mng.Insert (Adtd.Info_List, Info);
    Trace ("Dtd parsed directive ATTLIST -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Attlist;

  -- Parse <!ENTITY
  procedure Parse_Entity (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Entity name and value
    Name, Value : Asu_Us;
    -- Is it a parameter entity
    Parameter : Boolean;
    -- Is entity found
    Found : Boolean;
    Parstr : Asu_Us;
    Char : Character;
    use type Asu_Us;
  begin
    -- See if this is a parameter entity
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char = '%' then
      Parameter := True;
      Parstr := Asu_Tus ("%");
      -- Must have a separator after "%"
      Util.Get (Ctx.Flow, Char);
      if not Util.Is_Separator (Char) then
        Util.Error (Ctx.Flow, "Expect a separator after '%'");
      end if;
      Util.Skip_Separators (Ctx.Flow);
    else
      Parameter := False;
      Util.Unget (Ctx.Flow);
    end if;
    -- Parse entity name
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Name, In_Dtd => True);
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid entity name " & Asu_Ts (Name));
    end if;
    Util.Skip_Separators (Ctx.Flow);
    -- Check that it does not exist
    Entity_Mng.Exists (Adtd.Entity_List, Name, Parameter, Found);
    if Found then
      Util.Error (Ctx.Flow, "Entity " & Asu_Ts (Parstr & Name)
                          & " already defined");
    end if;
    -- Only accept local entities
    Util.Check_Cdata (Ctx.Flow, 7);
    Util.Try (Ctx.Flow, "SYSTEM ", Found);
    if Found then
      Util.Error (Ctx.Flow, "Unsupported SYSTEM external entity");
     else
      Util.Try (Ctx.Flow, "PUBLIC ", Found);
      if Found then
        Util.Error (Ctx.Flow, "Unsupported PUBLIC external entity");
      end if;
    end if;
    -- Parse and expand value
    Parse_Value (Ctx, Adtd, True, Value);
    -- Must stop now
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character at end of entity " & Char);
    end if;
    -- Store entity
    Entity_Mng.Add (Adtd.Entity_List, Name, Value, Parameter);
    Trace ("Dtd parsed directive ENTITY -> " &  Asu_Ts (Parstr & Name)
         & " " & Asu_Ts(Value));
  end Parse_Entity;

  -- Parse a conditional directive
  procedure Parse_Condition (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Word : Asu_Us;
  begin
    -- After '[', possible separators, then IGNORE or INCLUDE directive
    -- then possible separators then '['
    Util.Skip_Separators (Ctx.Flow);
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & "[");
    Word := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);

    -- Expand dtd entities
    Util.Expand_Name (Ctx, Adtd, Word, In_Dtd => True);
    if Asu_Ts (Word) = "IGNORE" then
      -- IGNORE directive, skip up to "]]>"
      Util.Parse_Until_Char (Ctx.Flow, "[");
      Util.Parse_Until_Str (Ctx.Flow, "]]" & Util.Stop);
      Trace ("Dtd ignored " & Asu_Ts (
         Util.Normalize_Separators (Util.Get_Curr_Str (Ctx.Flow))));
      Util.Reset_Curr_Str (Ctx.Flow);
      return;
    elsif Asu_Ts (Word) = "INCLUDE" then
      -- INCLUDE directive
      Util.Parse_Until_Char (Ctx.Flow, "[");
      -- Go on parsing, knowing that we are in an Include directive
      Trace ("Dtd starting inclusion");
      Adtd.In_Include := True;
    else
      Util.Error (Ctx.Flow, "Unknown conditional directive " & Asu_Ts (Word));
    end if;
  end Parse_Condition;

  -- Parse a directive
  procedure Parse_Directive (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Char : Character;
    Word : Asu_Us;
    Ok : Boolean;
  begin
    -- Check for CDATA
    Util.Skip_Cdata (Ctx.Flow, False, Ok);
    if Ok then
      return;
    end if;
    Util.Check_Cdata (Ctx.Flow, 7);
    -- Check for Comment
    Util.Try (Ctx.Flow, Util.Comment, Ok, Consume => False);
    if not Ok then
      -- Check for DOCTYPE
      Util.Try (Ctx.Flow, Util.Doctype, Ok, Consume => False);
    end if;
    if Ok then
      Parse_Directive (Ctx, Adtd, Allow_Dtd => False, In_Dtd => True);
      return;
    end if;
    -- Check for conditional directive
    Util.Get (Ctx.Flow, Char);
    if Char = '[' then
      Parse_Condition (Ctx, Adtd);
      return;
    else
      Util.Unget (Ctx.Flow);
    end if;
    -- Now, expect KEYWORD and a space
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Word := Util.Get_Curr_Str (Ctx.Flow);
    Util.Reset_Curr_Str (Ctx.Flow);
    declare
      Str : constant String := Asu_Ts (Word);
    begin
      if Str = "ELEMENT" then
        Parse_Element (Ctx, Adtd);
      elsif Str = "ATTLIST" then
        Parse_Attlist (Ctx, Adtd);
      elsif Str = "ENTITY" then
        Parse_Entity (Ctx, Adtd);
      elsif Str = "NOTATION" then
        Util.Error (Ctx.Flow, "Unsupported NOTATION directive");
      else
        Util.Error (Ctx.Flow, "Invalid directive " & Str);
      end if;
    end;
  end Parse_Directive;

  -- Parse current dtd
  -- If external, will stop end end of file
  -- otherwise, will stop on ']'
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   External : in Boolean) is
    Found, Cdata_Found : Boolean;
    Char : Character;
  begin
    loop
      begin
        Util.Skip_Separators (Ctx.Flow);
      exception
        when Util.End_Error =>
          if External then
            return;
          else
            Util.Error (Ctx.Flow,
                        "Unexpected end of file while parsing internal dtd");
          end if;
      end;
      -- Skip any Cdata section
      Util.Skip_Cdata (Ctx.Flow, True, Cdata_Found);
      if not Cdata_Found then
        -- Parse instruction
        Util.Check_Cdata (Ctx.Flow, 2);
        Util.Try (Ctx.Flow, Util.Start & Util.Instruction, Found);
        if Found then
          Parse_Instruction (Ctx, Adtd, External);
        else
          -- Parse directive
          Util.Try (Ctx.Flow, Util.Start & Util.Directive, Found);
          if Found then
            Parse_Directive (Ctx, Adtd);
          end if;
        end if;
        if not Found and then Adtd.In_Include then
          -- Detect end of include
          Util.Try (Ctx.Flow, "]]" & Util.Stop, Found);
          if Found then
            Adtd.In_Include := False;
            Trace ("Dtd ending inclusion");
          end if;
        end if;
        if not Found then
          Util.Get (Ctx.Flow, Char);
          if Char = (']') and then not External then
            return;
          else
            Util.Error (Ctx.Flow,
                        "Unexpected character while parsing dtd " & Char);
          end if;
        end if;
      end if;
    end loop;
  end Parse;

  -- Parse a dtd (either a external file or internal if name is empty)
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   File_Name : in String;
                   Name_Raise_Parse : in Boolean := True) is
  begin
    if File_Name = String_Flow then
      -- String of Ctx
      Trace ("Dtd parsing string");
      Parse (Ctx, Adtd, True);
    elsif File_Name = Internal_Flow then
      -- Internal declarations (string or file) of Ctx
      Trace ("Dtd parsing internal definition");
      Parse (Ctx, Adtd, False);
    else
      -- File name
      Trace ("Dtd parsing file " & File_Name);
      File_Mng.Open (File_Name, Ctx.Flow.Dtd_File);
      Ctx.Flow.Kind := Dtd_File;
      Ctx.Flow.Dtd_Line := 1;
      Parse (Ctx, Adtd, True);
      Ctx.Flow.Kind := Xml_File;
      File_Mng.Close (Ctx.Flow.Dtd_File);
    end if;
    -- Dtd is now valid
    Trace ("Dtd parsed dtd");
    Adtd.Set := True;
  exception
    when File_Error =>
      -- Can only be raised if not internal nor string flow
      if Name_Raise_Parse then
        Util.Error (Ctx.Flow, "Cannot open dtd file " & File_Name);
      else
        raise;
      end if;
  end Parse;

  -- Replace "##" by "," then suppress "#"
  function Strip_Sep (Us : in Asu_Us) return String is
    use String_Mng;
  begin
    return Replace (Replace (Asu_Ts (Us), "##", ","), "#", "");
  end Strip_Sep;

  -- Check children of element
  procedure Check_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Name : in Asu_Us;
                            Line_No : in Natural;
                            Children : in Asu_Us;
                            Is_Mixed : in Boolean) is
    -- Element info
    Info : Info_Rec;
    -- Char of info List
    Char : Character;
    -- Parser iterator
    Iter_Xml : Parser.Iterator;
    -- General purpose Boolean
    Ok : Boolean;
    use type Asu_Us;
  begin
    Trace ("Dtd check Xml children list " & Asu_Ts (Children)
         & " Mixed: " & Is_Mixed'Img);
    -- Read its element def
    Info.Name := "Elt" & Info_Sep & Name;
    Info_Mng.Search (Adtd.Info_List, Info, Ok);
    if not Ok then
      -- No ELEMENT directive, no constraint
      return;
    end if;
    Info_Mng.Read (Adtd.Info_List, Info, Info);
    -- Check children
    Trace ("Dtd check Dtd element info " & Asu_Ts (Info.List));
    -- Separate element type
    Char := Asu.Element (Info.List, 1);
    Info.List := Asu.Delete (Info.List, 1, 1);
    case Char is
      when 'E' =>
        -- Must be empty
        if Asu.Length (Children) /= 0 then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " must be empty",
                      Line_No);
        end if;
      when 'A' =>
        -- Any
        null;
      when 'M' =>
        -- Check mixed: all children of xml must appear in dtd list
        Parser.Set (Iter_Xml, Asu_Ts (Children), Is_Sep'Access);
        loop
          declare
            -- Next Child from xml
            Child : constant String := Parser.Next_Word (Iter_Xml);
          begin
            exit when Child = "";
            -- Child must appear in dtd
            if String_Mng.Locate (Asu_Ts (Info.List),
                                  Info_Sep & Child & Info_Sep) = 0 then
              Util.Error (Ctx.Flow, "According to dtd, element "
                        & Asu_Ts (Name)
                        & " does not allow child " & Child,
                          Line_No);
            end if;
            Trace ("Dtd checked mixed child " & Child
                 & " versus " & Strip_Sep (Info.List));
          end;
        end loop;
        Parser.Del (Iter_Xml);
      when 'C' =>
        if Is_Mixed then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " must not have text",
                      Line_No);
        end if;
        -- Strictly check that list matches criteria
        if not Regular_Expressions.Match (Asu_Ts (Info.List),
                Asu_Ts (Children), Strict => True) then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " allows children " & Strip_Sep (Info.List)
                    & " but has children " & Strip_Sep (Children));
        end if;
        Trace ("Dtd checked children " & Strip_Sep (Children)
             & " versus " & Strip_Sep (Info.List));
      when others =>
        Trace ("Dtd check: Unexpected element type " & Char);
        raise Internal_Error;
    end case;
  exception
    when Regular_Expressions.No_Criteria =>
      -- Normally it was checks at parsing
      Trace ("Dtd regex does not compile for check " & Asu_Ts (Info.List));
      raise Internal_Error;
  end Check_Children;

  -- Check attributes of element
  procedure Check_Attributes (Ctx  : in out Ctx_Type;
                              Adtd : in out Dtd_Type;
                              Name : in Asu_Us;
                              Line_No : in Natural;
                              Attributes : in Asu_Us) is
    -- Atl, Att and Id info blocs
    Info, Attinfo, Idinfo : Info_Rec;
    -- Name looked in Info list (for error tracing)
    Error_Name : Asu_Us;
    -- Is info found in info list
    Info_Found : Boolean;
    -- Parser iterators
    Iter_Dtd, Iter_Xml : Parser.Iterator;
    -- Is an attribute set in xml
    Att_Set : Boolean;
    -- Attribute value
    Att_Value : Asu_Us;
    -- Attribute value in Xml
    Xml_Val : Asu_Us;
    -- List of dtd attribute names
    Att_Names : Asu_Us;
    use type Asu_Us;
  begin
     Trace ("Dtd check Xml attributes list " & Asu_Ts (Attributes) );
    -- Read its ATTLIST def
    Info.Name := "Atl" & Info_Sep & Name;
    Info_Mng.Search (Adtd.Info_List, Info, Info_Found);
    if not Info_Found then
      -- No ATTLIST directive, no constraint
      return;
    else
      Info_Mng.Read (Adtd.Info_List, Info, Info);
    end if;
    if Info.List = Asu_Null then
      -- Empty ATTLIST for this element
      if Attributes = Asu_Null then
        Trace ("Dtd checked element " & Asu_Ts (Name)
             & " with no attributes, versus no or empty attlist");
        return;
      else
        Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                  & " is not allowed to have attributes",
                  Line_No);
      end if;
    end if;
    -- Check attributes
    Trace ("Dtd check Dtd attlist info " & Asu_Ts (Info.List));
    -- Check attributes xml vs dtd
    -- First extract list of dtd attribute names
    Parser.Set (Iter_Dtd, Asu_Ts (Info.List), Is_Sep'Access);
    loop
      declare
        -- Next attribute from dtd
        Attr : constant String := Parser.Next_Word (Iter_Dtd);
      begin
        exit when Attr = "";
        Asu.Append (Att_Names, Info_Sep & Attr & Info_Sep);
     end;
     -- Skip type and default spec
     Parser.Next_Word (Iter_Dtd);
    end loop;
    Parser.Del (Iter_Dtd);
    -- Now check that any attribute of xml is in the list of dtd
    Parser.Set (Iter_Xml, Asu_Ts (Attributes), Is_Sep'Access);
    loop
      declare
        -- Next attribute from xml
        Attr : constant String := Parser.Next_Word (Iter_Xml);
      begin
        exit when Attr = "";
        -- Attribute must appear in list of attributes from dtd
        if String_Mng.Locate (Asu_Ts (Att_Names),
                              Info_Sep & Attr & Info_Sep) = 0 then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " cannot have attribute " & Attr,
                      Line_No);
        end if;
      end;
    end loop;
    Parser.Del (Iter_Xml);

    -- Check attributes dtd vs xml
    --  Any Fixed in dtd must appear in xml and have correct value
    --  Any default, if it does not appear in Attributes, must be added
    --   to tree with default value
    Parser.Set (Iter_Dtd, Asu_Ts (Info.List), Is_Sep'Access);
    loop
      declare
        -- Next dtd attribute and type+default from dtd
        Attr : constant String := Parser.Next_Word (Iter_Dtd);
        Td : String(1 .. 2);
      begin
        -- Read all recessary info
        exit when Attr = "";
        Td := Parser.Next_Word (Iter_Dtd);
        -- Corresponding attribute info from dtd if any
        if Td(1) = 'E' or else Td(2) = 'F' or else Td(2) = 'D' then
          Attinfo.Name := "Att" & Info_Sep & Name & Info_Sep & Attr;
          Error_Name := Attinfo.Name;
          Info_Mng.Read (Adtd.Info_List, Attinfo, Attinfo);
        end if;
        -- Does this attribute appear in xml
        Att_Set := String_Mng.Locate (Asu_Ts (Attributes),
                   Info_Sep & Attr & Info_Sep) /= 0;
        if Att_Set then
          -- Get the Xml Attribute
          Tree_Mng.Get_Attribute (Ctx.Elements.all, Asu_Tus(Attr), Xml_Val);
        end if;

        --  Any Required or Fixed in dtd must appear in xml
        if (Td(2) = 'R' or else Td(2) = 'F')
        and then not Att_Set then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " must have attribute " & Attr, Line_No);
        end if;
        -- Enum and Fixed must have correct value
        if Td(2) = 'F' then
          -- Fixed (Enum or string): first #<val># is the one required
          declare
            -- Get the first value from dtd list, from 2 to second sep
            Sep : constant Positive
                := String_Mng.Locate (Asu_Ts (Attinfo.List),
                                      Info_Sep & "", 2);
            Dtd_Val : constant String := Asu.Slice (Attinfo.List, 2, Sep - 1 );
          begin
            if Asu_Ts (Xml_Val) /= Dtd_Val then
              Util.Error (Ctx.Flow, "According to dtd, attribute " & Attr
                        & " must have fixed value " & Dtd_Val, Line_No);
            end if;
          end;
        elsif Td(1) = 'E' and then Att_Set then
          -- Not fixed Enum in dtd with xml value: #<val># must be in dtd list
          if String_Mng.Locate (Asu_Ts (Attinfo.List),
                 Info_Sep  & Asu_Ts (Xml_Val) & Info_Sep) = 0 then
            Util.Error (Ctx.Flow, "According to dtd, Enum attribute " & Attr
                      & " has incorrect value "
                      & Asu_Ts (Xml_Val), Line_No);
          end if;
        elsif Td(2) = 'D' and then not Att_Set then
          -- Default in dtd with no xml value: insert default in tree
          if Td(1) = 'E' then
            -- Default of enum is first string after Info_Sep
            declare
              -- Get the first value from dtd list, from 2 to second sep
              Sep : constant Positive
                  := String_Mng.Locate (Asu_Ts (Attinfo.List),
                                        Info_Sep & "", 2);
              Dtd_Val : constant String
                      := Asu.Slice (Attinfo.List, 2, Sep - 1 );
            begin
              Tree_Mng.Add_Attribute (Ctx.Elements.all,
                  Asu_Tus (Attr), Asu_Tus (Dtd_Val), Line_No);
            end;
          else
            -- Default of not enum is the value
            Tree_Mng.Add_Attribute (Ctx.Elements.all,
                  Asu_Tus (Attr), Attinfo.List, Line_No);
          end if;
        elsif Att_Set then
          -- Comformance checks on ID, IDREF(s) and NMTOKEN(s)
          -- Store ID and IDREFs and Sanity checks on I
          if (Td(1) = 'I' or else Td(1) = 'R')
          and then not Util.Name_Ok (Xml_Val) then
            Util.Error (Ctx.Flow, "Invalid name for ID or IDREF "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 'r'
          and then  not Util.Names_Ok (Xml_Val, Util.Space & "") then
            Util.Error (Ctx.Flow, "Invalid name in IDREFS "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 'T'
          and then not Util.Name_Ok (Xml_Val, Allow_Token => True) then
            Util.Error (Ctx.Flow, "Invalid token for NMTOKEN "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 't'
          and then not Util.Names_Ok (Xml_Val,
                                      Util.Space & "",
                                      Allow_Token => True) then
            Util.Error (Ctx.Flow, "Invalid name in NMTOKENS "
                      & Asu_Ts (Xml_Val), Line_No);
          end if;

          -- Store IDs and IDREFs
          if Td(1) = 'I' then
            -- Check this ID is not already set for this element
            Idinfo.Name := "Ide" & Info_Sep & Name;
            Info_Mng.Search (Adtd.Info_List, Idinfo, Info_Found);
            if Info_Found then
              Info_Mng.Read (Adtd.Info_List, Idinfo, Idinfo);
              if String_Mng.Locate (Asu_Ts (Idinfo.List),
                           Info_Sep & Asu_Ts (Xml_Val) & Info_Sep) /= 0 then
                Util.Error (Ctx.Flow, "This ID " & Asu_Ts (Xml_Val)
                          & " is already set to this element", Line_No);
              end if;
            end if;
            -- Append ID to the list of this element
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep);
            Info_Mng.Insert (Adtd.Info_List, Idinfo);
            -- Append ID to global list
            Idinfo.Name := Asu_Tus ("Idl");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Adtd.Info_List, Idinfo, Idinfo);
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep);
            Info_Mng.Insert (Adtd.Info_List, Idinfo);
            Trace (" Check, added ID " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'R' then
            -- Append this ID ref and line_no to list of IDREFs
            Idinfo.Name := Asu_Tus ("Idr");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Adtd.Info_List, Idinfo, Idinfo);
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep
                      & Line_Image(Line_No) & Info_Sep);
            Info_Mng.Insert (Adtd.Info_List, Idinfo);
            Trace (" Check, added IDREF " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'r' then
            -- Append each of the IDREFs to the global list
            Idinfo.Name := Asu_Tus ("Idr");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Adtd.Info_List, Idinfo, Idinfo);
            -- Parse IDREFs separated by spaces
            Parser.Set (Iter_Xml, Asu_Ts (Xml_Val), Util.Is_Separator'Access);
            loop
              declare
                -- Next IDREF from xml
                Idref : constant String := Parser.Next_Word (Iter_Xml);
              begin
                exit when Idref = "";
                -- Append this ID ref and line_no to list of IDREFs
                Asu.Append (Idinfo.List, Info_Sep & Asu_Tus (Idref) & Info_Sep
                      & Line_Image(Line_No) & Info_Sep);
                Trace (" Check, added IDREF " & Idref);
              end;
            end loop;
            Parser.Del (Iter_Xml);
            Info_Mng.Insert (Adtd.Info_List, Idinfo);
          end if;
        end if;
        Trace ("Dtd checked versus dtd attribute " & Attr & " type " & Td);
      end;
    end loop;
    Parser.Del (Iter_Dtd);
  exception
    when Info_Mng.Not_In_List =>
      Trace ("Dtd check: Cannot find info " & Asu_Ts (Error_Name));
      Parser.Del (Iter_Dtd);
      raise Internal_Error;
  end Check_Attributes;

  -- Check Current element of the tree
  procedure Check_Element (Ctx : in out Ctx_Type;
                           Adtd : in out Dtd_Type;
                           Check_The_Attributes : in Boolean) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    -- Lists of attributes and of children from xml tree
    Attributes, Children : Asu_Us;
    -- Has current element mixed children (text)
    Is_Mixed : Boolean;
    use type Asu_Us;
  begin
    if not Adtd.Set then
      -- No dtd => no check
      return;
    end if;
    if Debug_Level /= 0 then
      My_Tree.Read (Ctx.Elements.all, Cell);
      Trace ("Dtd checking element " & Asu_Ts(Cell.Name));
    end if;
    -- Read current element from tree and make its attribute and children lists
    Is_Mixed := False;
    if My_Tree.Children_Number (Ctx.Elements.all) /= 0 then
      for I in 1 .. My_Tree.Children_Number (Ctx.Elements.all) loop
        if I = 1 then
          My_Tree.Move_Child (Ctx.Elements.all);
        else
          My_Tree.Move_Brother (Ctx.Elements.all, False);
        end if;
        My_Tree.Read (Ctx.Elements.all, Cell);
        case Cell.Kind is
          when Xml_Parser.Attribute =>
            Asu.Append (Attributes, Info_Sep & Cell.Name & Info_Sep);
          when Xml_Parser.Element =>
            Asu.Append (Children, Info_Sep & Cell.Name & Info_Sep);
          when Xml_Parser.Text =>
            Is_Mixed := True;
          when Xml_Parser.Comment =>
            null;
        end case;
      end loop;
      My_Tree.Move_Father (Ctx.Elements.all);
    end if;
    My_Tree.Read (Ctx.Elements.all, Cell);
    if Check_The_Attributes then
      -- Check Attributes
      Check_Attributes (Ctx, Adtd, Cell.Name, Cell.Line_No, Attributes);
    else
      -- Check children
      Check_Children (Ctx, Adtd, Cell.Name, Cell.Line_No, Children, Is_Mixed);
    end if;

  end Check_Element;

  -- Check a whole element tree recursively
  procedure Check_Subtree (Ctx  : in out Ctx_Type;
                           Adtd : in out Dtd_Type) is
    Cell : My_Tree_Cell;
  begin
    -- Check current element
    Check_Element (Ctx, Adtd, Check_The_Attributes => True);
    -- Check children recursively
    if My_Tree.Children_Number (Ctx.Elements.all) = 0 then
      -- No child
      return;
    end if;
    My_Tree.Read (Ctx.Elements.all, Cell);
    if Cell.Nb_Attributes = My_Tree.Children_Number (Ctx.Elements.all) then
      -- All children are attributes
      return;
    end if;

    -- Move to first real child (not attribute)
    My_Tree.Move_Child (Ctx.Elements.all);
    for I in 1 .. Cell.Nb_Attributes loop
      My_Tree.Move_Brother (Ctx.Elements.all, False);
    end loop;

    loop
      My_Tree.Read (Ctx.Elements.all, Cell);
      -- Skip Text and Comments
      if Cell.Kind = Element then
        -- Recursive check this sub element
        Check_Subtree (Ctx, Adtd);
      end if;
      exit when not My_Tree.Has_Brother (Ctx.Elements.all, False);
      My_Tree.Move_Brother (Ctx.Elements.all, False);
    end loop;

    -- Done, move back to father
    My_Tree.Move_Father (Ctx.Elements.all);
  end Check_Subtree;

  -- Final checks
  -- Check that all attribute values of Xml tagged IDREF(s) in Dtd
  --  and thus collected in "Idr" info,
  --  exist in the list of attribute values of Xml tagged ID
  --  and thus collected in "Idl" info,
  procedure Final_Check (Ctx : in out Ctx_Type;
                         Adtd : in out Dtd_Type) is
    -- List of IDs and list of IDrefs
    Ids, Idrefs : Info_Rec;
    -- Parser iterator on IDrefs
    Iter_Idref : Parser.Iterator;
  begin
    -- Read Ids
    Ids.Name := Asu_Tus ("Idl");
    -- For error
    Idrefs.Name := Ids.Name;
    Info_Mng.Read (Adtd.Info_List, Ids, Ids);
    Trace ("Check final, got IDs " & Asu_Ts (Ids.List));
    -- Read Idrefs
    Idrefs.Name := Asu_Tus ("Idr");
    Info_Mng.Read (Adtd.Info_List, Idrefs, Idrefs);
    Trace ("Check final, got IDREFs " & Asu_Ts (Idrefs.List));
    -- Idl is a list of #Id#, Idr is a list of #Idref#Line_No#
    -- Parse IDREFs separated by Sep
    Parser.Set (Iter_Idref, Asu_Ts (Idrefs.List), Is_Sep'Access);
    loop
      declare
        -- Next IDREF from xml
        Idref : constant String := Parser.Next_Word (Iter_Idref);
        Line : constant String := Parser.Next_Word (Iter_Idref);
      begin
        exit when Idref = "";
          -- Check that this Idref is in Ids
          if String_Mng.Locate (Asu_Ts (Ids.List),
                            Info_Sep & Idref & Info_Sep) = 0 then
            Util.Error (Ctx.Flow, "The ID of the ref " & Idref
                        & " does not exist", Natural'Value(Line));
          end if;
      end;
    end loop;
    Parser.Del (Iter_Idref);
    Trace ("Checked final");
  exception
    when Info_Mng.Not_In_List =>
      Trace ("Dtd check: Cannot find info " & Asu_Ts (Idrefs.Name));
  end Final_Check;

end Dtd;

