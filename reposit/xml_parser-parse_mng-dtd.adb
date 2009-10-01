with Regular_Expressions, Parser, Int_Image, Mixed_Str;
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
  begin
    -- No Dtd set
    Adtd.Set := False;
    -- No xml instruction found (yet)
    Adtd.Xml_Found := False;
    -- No encoding
    Adtd.Encoding := Asu_Null;
    -- Reset entities
    Entity_Mng.Initialise (Adtd.Entity_List);
    -- Reset info list
    Adtd.Info_List.Delete_List;
    -- Reset lists of attributes of type notation
    Adtd.Notation_Attrs := Asu_Null;
  end Init;

  -- Parse an instruction:
  -- Check xml version, append any other instruction to prologue
  procedure Parse_Instruction (Ctx : in out Ctx_Type;
                               Adtd : in out Dtd_Type;
                               External : in Boolean) is
    Ok : Boolean;
    Char : Character;
    Dummy : My_Tree_Cell;
  begin
    -- See if this is the xml directive
    Util.Try (Ctx.Flow, "xml", Ok);
    if Ok then
      Util.Get (Ctx.Flow, Char);
      Ok := Util.Is_Separator (Char);
    end  if;
    if Ok then
      if not External then
        Util.Error (Ctx.Flow, "Invalid xml instruction in internal dtd");
      end if;
      if Adtd.Xml_Found then
        Util.Error (Ctx.Flow, "Late or second declaration of xml in dtd");
      end if;
      -- Add a dummy prologue root or a dummy child to prologue root
      Trace ("Dtd parsing xml");
      Dummy.Line_No := 0;
      if Ctx.Prologue.Is_Empty then
        Ctx.Prologue.Insert_Father (Dummy);
      else
        Ctx.Prologue.Insert_Child (Dummy, False);
      end if;
      Parse_Attributes (Ctx, Adtd, True);
      Check_Xml_Attributes (Ctx, False);
      -- Delete this dummy child
      Ctx.Prologue.Delete_Tree;
      -- Done
      Trace ("Dtd parsed xml instruction");
    else
      -- Skip processing instruction
      Util.Parse_Until_Str (Ctx.Flow, Util.Instruction & Util.Stop);
      Util.Reset_Curr_Str (Ctx.Flow);
    end if;
    -- Xml instruction not allowed any more
    Adtd.Xml_Found := True;
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
          Asu.Append (Res, Info_Sep & ")");
          In_Word := False;
        end if;
      else
        if not In_Word then
          -- Start of word
          Asu.Append (Res, "(" & Info_Sep);
          In_Word := True;
        end if;
      end if;
      Asu.Append (Res, C);
    end loop;
    if In_Word then
      Asu.Append (Res, Info_Sep & ")");
    end if;
    -- Check that words are always separated: a '(' shall always be
    -- either the first of precceded by ( | or ,
    declare
      Index : Natural;
      Str : constant String := Asu_Ts (Res);
    begin
      Index := 1;
      loop
        Index := String_Mng.Locate (Asu_Ts (Res), "(", Index + 1);
        exit when Index = 0;
        C := Str(Index - 1);
        if C /= '(' and then C /= '|' and then C /= ',' then
          Trace ("Dtd missing seperator in >" & Asu_Ts (Res)
               & "< at index " & Line_Image (Index));
          Util.Error (Ctx.Flow, "Invalid children definition");
        end if;
      end loop;
    end;
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
    Util.Get_Curr_Str (Ctx.Flow, Info_Name);
    Util.Expand_Name (Ctx, Adtd, Info_Name, Ref_Dtd_Mark);
    Util.Fix_Text (Ctx, Info_Name, Ref_Dtd_Mark, False);
    if not Util.Name_Ok (Info_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Asu_Ts (Info_Name));
    end if;
    Info.Name := "Elt" & Info_Sep & Info_Name;
    Info.Line := Util.Get_Line_No (Ctx.Flow);
    -- Element must not exist
    Adtd.Info_List.Search (Info, Found);
    if Found then
      Util.Error (Ctx.Flow, "ELEMENT " & Asu_Ts (Info_Name)
                          & " already exists");
    end if;
    -- Parse content
    Util.Skip_Separators (Ctx.Flow);

    Util.Parse_Until_Stop (Ctx.Flow);
    Util.Unget (Ctx.Flow);
    Util.Get_Curr_Str (Ctx.Flow, Info.List);
    -- Expand potential parameter entities and re-insert
    Util.Expand_Vars (Ctx, Adtd, Info.List, Ref_Dtd_Mark);
    Util.Fix_Text (Ctx, Info.List, Ref_Dtd_Mark, False);
    Info.List := Util.Normalize_Separators (Info.List);
    Util.Insert (Ctx.Flow, Asu_Ts (Info.List));

    -- Check possible content: EMPTY, ANY or (<list>)
    Util.Try (Ctx.Flow, "EMPTY", Found);
    if Found then
      Info.List := Asu_Tus ("E");
    end if;
    if not Found then
      Util.Try (Ctx.Flow, "ANY", Found);
      if Found then
        Info.List := Asu_Tus ("A");
      end if;
    end if;
    if not Found then
      Util.Try (Ctx.Flow, "(", Found);
      if not Found then
        Util.Error (Ctx.Flow, "Unexpected character "
                 & Asu.Element (Info.List, 1)
                 & " at start of ELEMENT list");
      else
        -- A (mixed) list
        Found := False;
      end if;
    end if;

    if not Found then
      -- A (mixed) list: parse until ')' and remove any seperator
      Util.Parse_Until_Close (Ctx.Flow);
      Util.Get_Curr_Str (Ctx.Flow, Info.List);
      Info.List := Util.Remove_Separators (Info.List);
      -- Now see if it is mixed or children
      if Asu.Index (Info.List, "#PCDATA") /= 0 then
        -- Mixed
        if Asu_Ts (Info.List) = "#PCDATA" then
          -- Possible '*' after ')', skip it
          Util.Try (Ctx.Flow, "*", Found);
          Info.List := Asu_Null;
        elsif Asu.Slice (Info.List, 1, 8) = "#PCDATA|" then
          -- Remove heading #PCDATA
          Info.List := Asu_Tus (
              String_Mng.Cut (Asu_Ts (Info.List), 8));
          Info.List := Util.Remove_Separators (Info.List);
          -- Check that everything between "|" are names
          if Asu.Element (Info.List, Asu.Length (Info.List)) = '|'
          or else Asu.Element (Info.List, 1) = '|' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          if not Util.Names_Ok (Info.List, "|") then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          -- Last ')' must be followed by '*', remove it
          Util.Get (Ctx.Flow, Char);
          if Char /= '*' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          -- Replace '|' by '#' and prepend and append a '#'
          Info.List := Asu_Tus (
            String_Mng.Replace (Info_Sep & Asu_Ts (Info.List) & Info_Sep,
                                "|", "" & Info_Sep));
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
        Util.Expand_Vars (Ctx, Adtd, Info.List, Ref_Dtd_Mark);
        Util.Fix_Text (Ctx, Info.List, Ref_Dtd_Mark, False);
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
    Adtd.Info_List.Insert (Info);
    Trace ("Dtd parsed directive ELEMENT -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Element;

  -- Parse <!ATTLIST
  procedure Parse_Attlist (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Atl, Att and Id info blocs
    Info, Attinfo : Info_Rec;
    Found : Boolean;
    -- Element, attribute name and type
    Elt_Name, Att_Name, Att_Type : Asu_Us;
    -- Complete Attlist to expand
    Attlist : Asu_Us;
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
    -- Parser iterator
    Iter : Parser.Iterator;
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
    Util.Get_Curr_Str (Ctx.Flow, Elt_Name);
    Util.Expand_Name (Ctx, Adtd, Elt_Name, Ref_Dtd_Mark);
    Util.Fix_Text (Ctx, Elt_Name, Ref_Dtd_Mark, False);
    if not Util.Name_Ok (Elt_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Asu_Ts (Elt_Name));
    end if;
    Info.Name := "Atl" & Info_Sep & Elt_Name;
    Info.Line := Util.Get_Line_No (Ctx.Flow);
    -- Attribute list of this element may already exist => merge
    Adtd.Info_List.Search (Info, Found);
    if Found then
      Adtd.Info_List.Read (Info, Info);
      Trace ("Dtd retrieved previous ATTLIST -> " & Asu_Ts (Info.Name)
           & " " & Asu_Ts (Info.List));
      if Ctx.Warnings then
        Util.Warning (Ctx.Flow, "Attlist already defined at line "
           & Line_Image (Info.Line)
           & " for element " & Asu_Ts (Elt_Name));
      end if;
    end if;

    -- Parse Attlist
    Util.Skip_Separators (Ctx.Flow);

    Util.Parse_Until_Stop (Ctx.Flow);
    Util.Unget (Ctx.Flow);
    Util.Get_Curr_Str (Ctx.Flow, Attlist);
    -- Expand potential parameter entities and re-insert
    Util.Expand_Vars (Ctx, Adtd, Attlist, Ref_Dtd_Mark);
    Util.Fix_Text (Ctx, Attlist, Ref_Dtd_Mark, False);
    Attlist := Util.Normalize_Separators (Attlist);
    Util.Insert (Ctx.Flow, Asu_Ts (Attlist));

    -- Loop on all attributes
    Elt_Has_Id := False;
    loop
      Util.Skip_Separators (Ctx.Flow);
      -- Name of attribute or end of list
      Util.Try (Ctx.Flow, Util.Stop & "", Found);
      exit when Found;
      -- Get attribute name
      Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
      Util.Get_Curr_Str (Ctx.Flow, Att_Name);
      Util.Expand_Name (Ctx, Adtd, Att_Name, Ref_Dtd_Mark);
      Util.Fix_Text (Ctx, Att_Name, Ref_Dtd_Mark, False);
      if not Util.Name_Ok (Att_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute " & Asu_Ts (Att_Name));
      end if;
      -- Check that this attribute is not already defined, otherwise discard
      --  any new definition
      -- Look for #attribute##
      Attr_Already_Set := String_Mng.Locate (Asu_Ts (Info.List),
             Info_Sep & Asu_Ts (Att_Name) & Info_Sep & Info_Sep) /= 0;
      -- Check supported att types
      Util.Skip_Separators (Ctx.Flow);
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
      elsif Try ("ENTITY ") then
        -- ENTITY type
        Typ_Char := 'Y';
      elsif Try ("ENTITIES ") then
        -- ENTITIES type
        Typ_Char := 'y';
      elsif Try ("NOTATION ") then
        -- NOTATION type
        Typ_Char := 'N';
        Util.Skip_Separators (Ctx.Flow);
        if Get /= '(' then
          Util.Error (Ctx.Flow, "Invalid Notation definition");
        end if;
      elsif Get = '(' then
        -- Enum type
        Typ_Char := 'E';
      else
        -- Unknown ATTLIST
        Util.Unget (Ctx.Flow);
        Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
        Util.Get_Curr_Str (Ctx.Flow, Att_Type);
        Util.Error (Ctx.Flow, "Invalid attribute type " & Asu_Ts (Att_Type));
      end if;

      -- Parse enumeration for Enum or Notation
      if Typ_Char = 'E' or else Typ_Char = 'N' then
        Util.Parse_Until_Char (Ctx.Flow, ")");
        Util.Get_Curr_Str (Ctx.Flow, Enum);
        Enum := Util.Remove_Separators (Enum);
        if Enum = Asu_Null then
          Util.Error (Ctx.Flow, "Empty enumeration");
        end if;
        -- Check that everything between "|" are
        -- nmtokens if Enum, names if Notation
        if Asu.Element (Enum, Asu.Length (Enum)) = '|'
        or else Asu.Element (Enum, 1) = '|' then
          Util.Error (Ctx.Flow, "Invalid Enum definition");
        end if;
        if not Util.Names_Ok (Enum, "|", Allow_Token => Typ_Char = 'E') then
          if Typ_Char = 'E' then
            Util.Error (Ctx.Flow, "Invalid nmtoken in Enum definition");
          else
            Util.Error (Ctx.Flow, "Invalid name in Notation definition");
          end if;
        end if;
        -- Replace '|' by '#' and prepend and append a '#'
        Enum := Asu_Tus (
          String_Mng.Replace (Info_Sep & Asu_Ts (Enum) & Info_Sep,
                              "|", "" & Info_Sep));
        -- Check unicity of entries
        Iter.Set (Asu_Ts (Enum), Is_Sep'Access);
        loop
          declare
            -- Next enum value
            Val : constant String := Parser.Next_Word (Iter);
          begin
            exit when Val = "";
            -- The val from Istart to Istop shall appear only once
            if String_Mng.Locate (Asu_Ts (Enum),
                                  Info_Sep & Val & Info_Sep,
                                  Occurence => 2) /= 0 then
              Util.Error (Ctx.Flow, "Enumerated value "
                   & Val & " already used");
            end if;
          end;
        end loop;
        Iter.Del;
      end if;

      -- Check supported att defaults
      Util.Skip_Separators (Ctx.Flow);
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
        Parse_Value (Ctx, Adtd, Ref_Dtd_Mark, Def_Val);
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
               "Id attribute must be required or implied");
        end if;
        -- Initialise an Empty Ide info
        Elt_Has_Id := True;
      end if;

      -- Check Enum
      if (Typ_Char = 'E' or else Typ_Char = 'N')
      and then (Def_Char = 'D' or else Def_Char = 'F') then
        -- Enum and (default or fixed), check default is in enum
        --  and set the default in first pos
        if (String_Mng.Locate (Asu_Ts (Enum),
              Info_Sep & Asu_Ts (Def_Val) & Info_Sep) = 0) then
          Util.Error (Ctx.Flow, "Default or fixed value "
                    & Asu_Ts (Def_Val) & " not in Enum");
        end if;
        -- Remove #default and insert #default in head
        Enum := Asu_Tus (String_Mng.Replace (
                 Asu_Ts (Enum),
                 Info_Sep & Asu_Ts (Def_Val),
                 ""));
        Enum := Info_Sep & Asu_Ts (Def_Val) & Enum;
      end if;

      -- Discard re-definition of the same attribute
      if not Attr_Already_Set then
        -- If enum store Att of enum
        --  or if fixed or default store Att of default
        Attinfo.Name := "Att" & Info_Sep & Elt_Name & Info_Sep & Att_Name;
        Attinfo.Line := Util.Get_Line_No (Ctx.Flow);
        if Typ_Char = 'E' or else Typ_Char = 'N' then
          Attinfo.List := Enum;
          Adtd.Info_List.Insert (Attinfo);
          Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
           & " " & Asu_Ts(Attinfo.List));
        elsif Def_Char = 'F' or else Def_Char = 'D' then
          Attinfo.List := Def_Val;
          Adtd.Info_List.Insert (Attinfo);
          Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
           & " " & Asu_Ts(Attinfo.List));
        end if;
        -- Verify Notation is not used twice (##N) for this element
        if String_Mng.Locate (Asu_Ts (Info.List), Info_Sep & Info_Sep & "N")
           /= 0 then
          Util.Error (Ctx.Flow, "Notation already defined for element "
                               & Asu_Ts (Elt_Name));
        end if;
        if Typ_Char = 'N' then
          -- Append Elt##Attr# to the list of notation attributes
          if Adtd.Notation_Attrs = Asu_Null then
            Adtd.Notation_Attrs := Asu_Tus ("" & Info_Sep);
          end if;
          Asu.Append (Adtd.Notation_Attrs,
              Elt_Name & Info_Sep & Info_Sep & Att_Name & Info_Sep);
        end if;
        -- Append this attribute in list: #attribute##td#attribute##td#...
        if Info.List = Asu_Null then
          Asu.Append (Info.List, Info_Sep);
        end if;
        Asu.Append (Info.List, Att_Name
                  & Info_Sep & Info_Sep & Typ_Char & Def_Char & Info_Sep);
      else
        -- This attribute is already defined (for this element)
        Trace ("Dtd discarding duplicate ATTLIST -> " & Asu_Ts (Info.Name)
             & " " & Asu_Ts (Att_Name) & Info_Sep & Info_Sep
             & Typ_Char & Def_Char);
        if Ctx.Warnings then
          Util.Warning (Ctx.Flow, "Attribute " & Asu_Ts (Att_Name)
             & " already defined for element " & Asu_Ts (Elt_Name));
        end if;
      end if;
    end loop;
    -- Attlist is ended: store
    Adtd.Info_List.Insert (Info);
    Trace ("Dtd parsed directive ATTLIST -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Attlist;

  -- Parse <!ENTITY
  procedure Parse_Entity (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Entity name, value (or URI or notation)
    Name, Value : Asu_Us;
    -- Public and System Ids
    Public_Id, System_Id : Asu_Us;
    -- Is it a parameter entity
    Parameter : Boolean;
    -- Is it Internal, and parsed
    Internal : Boolean;
    Parsed : Boolean;
    -- Is entity found
    Found : Boolean;
    -- Is it public
    Public : Boolean;
    -- Parameter entity indicator
    Parstr : Asu_Us;
    -- Unparsed entity rec
    Unparsed_Rec : Unparsed_Type;
    Char : Character;
    use type Asu_Us;
  begin
    -- See if this is a parameter entity
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char = '%' then
      -- Check if this is "% name" of parameter entity definition
      --  or "%name;" of entity definition of a name that is a reference
      --  to a parameter entity
      Util.Get (Ctx.Flow, Char);
      if Util.Is_Separator (Char) then
        -- This is the definition of a parameter entity
        Parameter := True;
        Parstr := Asu_Tus ("%");
      else
        -- This is a reference to a parameter entity
        Parameter := False;
        Parstr := Asu_Null;
        Util.Unget (Ctx.Flow, 2);
      end if;
    else
      -- This is a entity definition
      Util.Unget (Ctx.Flow);
      Parameter := False;
      Parstr := Asu_Null;
    end if;

    -- Parse entity name
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Util.Get_Curr_Str (Ctx.Flow, Name);
    Util.Expand_Name (Ctx, Adtd, Name, Ref_Dtd_Mark);
    -- Strip separators
    Util.Fix_Text (Ctx, Name, Ref_Dtd_Mark, False);
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid entity name " & Asu_Ts (Name));
    end if;
    Util.Skip_Separators (Ctx.Flow);

    -- Is it a public a system or an internal entity
    Internal := False;
    Public := False;
    Util.Try (Ctx.Flow, "SYSTEM ", Found);
    if not Found then
      Public := True;
      Util.Try (Ctx.Flow, "PUBLIC ", Found);
      if not Found then
        Internal := True;
      end if;
    end if;

    -- Parse value or URI
    if Internal then
      -- ENTITY [ % ] name "value"
      -- Parse and expand value
      Parse_Value (Ctx, Adtd, Ref_Entity, Value);
      Util.Skip_Separators (Ctx.Flow);
    else
      if Public then
        -- ENTITY [ % ] name PUBLIC "PubId" "URI"
        -- Skip PubId
        -- Parse URI or Id
        Util.Get (Ctx.Flow, Char);
        if Char = ''' then
          Util.Parse_Until_Char (Ctx.Flow, "'");
        elsif Char = '"' then
          Util.Parse_Until_Char (Ctx.Flow, """");
        else
          Util.Error (Ctx.Flow, "Unexpected delimiter of PUBLIC Id");
        end if;
        Util.Get_Curr_Str (Ctx.Flow, Public_Id);
        Util.Skip_Separators (Ctx.Flow);
      end if;
      -- PUBLIC or SYSTEM: get URI
      Util.Get (Ctx.Flow, Char);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Unexpected delimiter of PUBLIC Id");
      end if;
      Util.Get_Curr_Str (Ctx.Flow, System_Id);
      Util.Skip_Separators (Ctx.Flow);
    end if;

    -- See if Parsed. If not, will store notation
    Parsed := True;
    if not Parameter and then not Internal then
      Util.Try (Ctx.Flow, "NDATA ", Found);
      if Found then
        -- Unparsed entity: the value is the notation
        Parsed := False;
        Util.Parse_Name (Ctx.Flow, Value);
        Util.Skip_Separators (Ctx.Flow);
      else
        -- Parsed external entity, the value is the URI
        Value := System_Id;
      end if;
    elsif Parameter and then not Internal then
      -- Parsed external parameter entity, the value is the URI
      Value := System_Id;
    end if;

    -- Must stop now
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character at end of entity " & Char);
    end if;

    -- Check that it does not exist. Discard re-definition
    Entity_Mng.Exists (Adtd.Entity_List, Name, Parameter, Found);
    if Found then
      Trace ("Dtd discarding re-definition of entity "
           & Asu_Ts (Parstr & Name));
      return;
    end if;
    -- Store Entity, line and associated notation if unparsed entity
    if not Parsed then
      Unparsed_Rec.Is_Entity := True;
      Unparsed_Rec.Name := Name;
      Unparsed_Rec.Line_No := Util.Get_Line_No (Ctx.Flow);
      Unparsed_Rec.System_Id := System_Id;
      Unparsed_Rec.Public_Id := Public_Id;
      Unparsed_Rec.Notation := Value;
      Ctx.Unparsed_List.Insert (Unparsed_Rec);
      -- Associated value will be empty
      Value := Asu_Null;
    end if;

    -- Store entity
    Entity_Mng.Add (Adtd.Entity_List, Name, Value, Parameter, Internal, Parsed);
    Trace ("Dtd parsed directive ENTITY -> " &  Asu_Ts (Parstr & Name)
         & " " & Asu_Ts (Value) & " " & Mixed_Str (Internal'Img)
         & " " & Mixed_Str (Parsed'Img) );
  end Parse_Entity;

  -- Parse a <!NOTATION
  procedure Parse_Notation (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Notation name
    Name : Asu_Us;
    -- Is Notation found
    Found : Boolean;
    -- Is it public
    Public : Boolean;
    -- Public and System Ids
    Public_Id, System_Id : Asu_Us;
    -- Unparsed entity rec
    Unparsed_Rec : Unparsed_Type;
    Char : Character;
    use type Asu_Us;
  begin
    -- Parse notation name
    Util.Skip_Separators (Ctx.Flow);
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Util.Get_Curr_Str (Ctx.Flow, Name);
    Util.Expand_Name (Ctx, Adtd, Name, Ref_Dtd_Mark);
    -- Strip separators
    Util.Fix_Text (Ctx, Name, Ref_Dtd_Mark, False);
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid notation name " & Asu_Ts (Name));
    end if;
    Util.Skip_Separators (Ctx.Flow);
    -- See if SYSTEM or PUBLIC
    Public := False;
    Util.Try (Ctx.Flow, "SYSTEM ", Found);
    if not Found then
      Public := True;
      Util.Try (Ctx.Flow, "PUBLIC ", Found);
    end if;
    if not Found then
      Util.Error (Ctx.Flow, "Invalid notation definition");
    end if;
    -- Parse URI or Id
    Util.Get (Ctx.Flow, Char);
    if Char = ''' then
      Util.Parse_Until_Char (Ctx.Flow, "'");
    elsif Char = '"' then
      Util.Parse_Until_Char (Ctx.Flow, """");
    else
      Util.Error (Ctx.Flow, "Unexpected delimiter of notation");
    end if;
    Util.Get_Curr_Str (Ctx.Flow, Public_Id);
    -- Parse URI if PUBLIC not end
    Util.Skip_Separators (Ctx.Flow);
    Util.Try (Ctx.Flow, Util.Stop & "", Found, False);
    if Public and then not Found then
      Util.Get (Ctx.Flow, Char);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Invalid notation definition");
      end if;
    end if;
    Util.Get_Curr_Str (Ctx.Flow, System_Id);
    -- Must stop now
    Util.Skip_Separators (Ctx.Flow);
    Util.Get (Ctx.Flow, Char);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character at end of notation " & Char);
    end if;

    -- When if is SYSTEM, the first field is the system Id
    if not Public then
      System_Id := Public_Id;
      Public_Id := Asu_Null;
    end if;

    -- Store notation
    Unparsed_Rec.Is_Entity := False;
    Unparsed_Rec.Name := Name;
    Unparsed_Rec.Line_No := Util.Get_Line_No (Ctx.Flow);
    Unparsed_Rec.System_Id := System_Id;
    Unparsed_Rec.Public_Id := Public_Id;
    Unparsed_Rec.Notation := Asu_Null;
    Ctx.Unparsed_List.Insert (Unparsed_Rec);
    Trace ("Dtd parsed directive NOTATION -> " &  Asu_Ts (Name));
  end Parse_Notation;

  -- Parse a conditional directive
  procedure Parse_Condition (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Word : Asu_Us;
  begin
    -- After '[', possible separators, then IGNORE or INCLUDE directive
    -- then possible separators then '['
    Util.Skip_Separators (Ctx.Flow);
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & "[");
    Util.Get_Curr_Str (Ctx.Flow, Word);

    -- Expand dtd entities
    Util.Expand_Name (Ctx, Adtd, Word, Ref_Dtd_Mark);
    -- Strip separators
    Util.Fix_Text (Ctx, Word, Ref_Dtd_Mark, False);
    if Asu_Ts (Word) = "IGNORE" then
      -- IGNORE directive, skip up to "]]>"
      Util.Parse_Until_Char (Ctx.Flow, "[");
      Util.Parse_Until_Str (Ctx.Flow, "]]" & Util.Stop);
      Util.Get_Curr_Str (Ctx.Flow, Word);
      Word := Util.Normalize_Separators (Word);
      Trace ("Dtd ignored " & Asu_Ts (Word));
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
    -- Xml instruction not allowed any more
    Adtd.Xml_Found := True;
    -- Check for Comment
    Util.Try (Ctx.Flow, Util.Comment, Ok, Consume => False);
    if not Ok then
      -- Check for DOCTYPE
      Util.Try (Ctx.Flow, Util.Doctype, Ok, Consume => False);
    end if;
    if Ok then
      Parse_Directive (Ctx, Adtd, False, Ref_Dtd_Mark, null);
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
    Util.Get_Curr_Str (Ctx.Flow, Word);
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
        Parse_Notation (Ctx, Adtd);
      else
        Util.Error (Ctx.Flow, "Invalid directive " & Str);
      end if;
    end;
  end Parse_Directive;

  -- Switch input to Text, Parse it up to the end
  procedure Switch_Input (Ctx : in out Ctx_Type;
                          Adtd : in out Dtd_Type;
                          Text : in out Asu_Us);

  -- Parse current dtd
  -- If external, will stop at end of file
  -- otherwise, will stop on ']'
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   External : in Boolean) is
    Found : Boolean;
    Entity_Value : Asu_Us;
    Char : Character;
    Is_Recorded : Boolean;
    use type Asu_Us;
  begin
    if External and then Ctx.Flow.Curr_Flow.Is_File then
      -- Autodetect encoding and check
      Util.Guess_Encoding (Ctx.Flow);
      Trace ("Detected dtd encoding format "
           & Ctx.Flow.Curr_Flow.Encod'Img);
    end if;

    loop
      Util.Skip_Separators (Ctx.Flow);
      -- Try instruction
      Util.Try (Ctx.Flow, Util.Start & Util.Instruction, Found);
      if Found then
        Parse_Instruction (Ctx, Adtd, External);
      end if;
      if not Found then
        -- Try directive
        Util.Try (Ctx.Flow, Util.Start & Util.Directive, Found);
        if Found then
          Parse_Directive (Ctx, Adtd);
        end if;
      end if;
      if not Found then
        -- Try parameter entity '%'
        Util.Try (Ctx.Flow, Util.Ent_Param & "", Found);
        if Found then
          -- Get entity reference
          Util.Unget (Ctx.Flow);
          Util.Parse_Until_Char (Ctx.Flow, Util.Ent_End & "");
          Util.Get_Curr_Str (Ctx.Flow, Entity_Value);
          Entity_Value := Entity_Value & Util.Ent_End;
          -- Expand
          Trace ("Dtd expanding parameter entity " & Asu_Ts (Entity_Value));
          Util.Expand_Name (Ctx, Adtd, Entity_Value, Ref_Dtd);
          -- Suspend recording for the entity replacement
          Is_Recorded := Ctx.Flow.Recording;
          Ctx.Flow.Recording := False;
          -- Parse
          Switch_Input (Ctx, Adtd, Entity_Value);
          Ctx.Flow.Recording := Is_Recorded;
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
        -- Should be the end: End_Error if external, ']' if internal
        begin
          Util.Get (Ctx.Flow, Char);
        exception
          when Util.End_Error =>
            if External then
              return;
            else
              Util.Error (Ctx.Flow,
                      "Unexpected end of file while parsing internal dtd");
            end if;
        end;
        if Char = (']') and then not External then
          return;
        else
          Util.Error (Ctx.Flow,
                      "Unexpected character while parsing dtd " & Char);
        end if;
      end if;
    end loop;
  end Parse;

  -- Switch input to Text, Parse it up to the end
  procedure Switch_Input (Ctx : in out Ctx_Type;
                          Adtd : in out Dtd_Type;
                          Text : in out Asu_Us) is
  begin
    -- Save current flow
    Util.Push_Flow (Ctx.Flow);
    -- Prepare new string flow, keep file name
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.File := null;
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    Ctx.Flow.Curr_Flow.In_Str := Text;
    Ctx.Flow.Curr_Flow.In_Stri := 0;
    Ctx.Flow.Curr_Flow.Same_Line := True;
    -- Parse new flow as dtd
    Trace ("Switching input to " & Asu_Ts (Text));
    Parse (Ctx, Adtd, External => True);
    -- Restore flow
    Util.Pop_Flow (Ctx.Flow);
  end Switch_Input;

  procedure Check_Warnings (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type);

  -- Parse a dtd (either a external file or internal if name is empty)
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   File_Name : in Asu_Us;
                   Name_Raise_Parse : in Boolean := True) is
    use type Asu_Us;
  begin
    Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
    if File_Name = String_Flow then
      -- String of Ctx
      Trace ("Dtd parsing string");
      Ctx.Flow.Curr_Flow.Is_File := False;
      Ctx.Flow.Curr_Flow.File := null;
      Parse (Ctx, Adtd, True);
    elsif File_Name = Internal_Flow then
      -- Internal declarations (string or file) of Ctx
      Trace ("Dtd parsing internal definition");
      Ctx.Flow.Curr_Flow.Kind := Int_Dtd_Flow;
      Parse (Ctx, Adtd, False);
      Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    else
      -- File name
      Trace ("Dtd parsing file " & Asu_Ts (File_Name));
      Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
      Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
      File_Mng.Open (Asu_Ts (File_Name), Ctx.Flow.Curr_Flow.File.all);
      Ctx.Flow.Curr_Flow.Is_File := True;
      Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
      Ctx.Flow.Curr_Flow.Name := File_Name;
      Ctx.Flow.Curr_Flow.Line := 1;
      Ctx.Flow.Curr_Flow.Same_Line := False;
      Parse (Ctx, Adtd, True);
    end if;
    -- Dtd is now valid
    Trace ("Dtd parsed dtd");
    Adtd.Set := True;
    if Ctx.Warnings then
      Trace ("Dtd checking warnings");
      Check_Warnings (Ctx, Adtd);
      Trace ("Dtd checked warnings");
    end if;
  exception
    when File_Error =>
      -- Can only be raised if not internal nor string flow
      if Name_Raise_Parse then
        Util.Error (Ctx.Flow, "Cannot open dtd file " & Asu_Ts (File_Name));
      else
        raise;
      end if;
    when Entity_Mng.Entity_Forbidden =>
      Util.Error (Ctx.Flow, "Forbidden entity reference in dtd");
  end Parse;

  -- Perform final checks after DTD parsing: unparsed entities v.s. notations
  procedure Final_Dtd_Check (Ctx  : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Iterators
    Iter, Iter1, Iter2 : Parser.Iterator;
    -- Element/att info
    Info : Info_Rec;
    -- List of unparsed entities, line of def and notation
    Entities, Notations, Lines : Asu_Us;
    -- Unparsed entity or Notation
    Unparsed_Rec : Unparsed_Type;
    Ok : Boolean;
    use type Asu_Us;
  begin
    -- All unparsed entities have a notation associated
    Trace ("Dtd final: All unparsed entities have a notation");
    if not Ctx.Unparsed_List.Is_Empty then
      -- Make a string list of notations used by unparsed entities
      Ctx.Unparsed_List.Rewind;
      loop
        Ctx.Unparsed_List.Read (Unparsed_Rec, Moved => Ok);
        Asu.Append (Entities, Unparsed_Rec.Name & Info_Sep);
        Asu.Append (Lines, Line_Image(Unparsed_Rec.Line_No) & Info_Sep);
        Asu.Append (Notations, Unparsed_Rec.Notation & Info_Sep);
        exit when not Ok;
      end loop;
      -- Locate notation
      Iter.Set (Asu_Ts (Entities), Is_Sep'Access);
      Iter1.Set (Asu_Ts (Lines), Is_Sep'Access);
      Iter2.Set (Asu_Ts (Notations), Is_Sep'Access);
      loop
        declare
          -- Next entity and its notation
          Entity : constant String := Iter.Next_Word;
          Line : constant String := Iter1.Next_Word;
          Notation : constant String := Iter2.Next_Word;
        begin
          exit when Notation = "";
          Trace ("Checking notation " & Notation
               & " for unparsed entity " & Entity);
          -- Name must appear in notations
          Unparsed_Rec.Is_Entity := False;
          Unparsed_Rec.Name := Asu_Tus (Notation);
          Ctx.Unparsed_List.Search (Unparsed_Rec, Ok);
          if not Ok then
            Util.Error (Ctx.Flow,
               "No notation " & Notation & " for unparsed entity " & Entity
             & " defined at line " & Line);
          end if;
        end;
      end loop;
      Iter.Del;
      Iter1.Del;
      Iter2.Del;
    end if;

    -- All ATTLIST NOTATION values shall refer to a NOTATION
    -- Any element having a notation is not EMPTY
    Trace ("Dtd final: All notation attlist values have a notation");
    Trace ("       and elements with notation attlist are not empty");
    -- For all Elt#Att of Notation_Attrs
    Iter.Set (Asu_Ts (Adtd.Notation_Attrs), Is_Sep'Access);
    loop
      declare
        -- Next element and notation attribute
        Elt : constant String := Iter.Next_Word;
        Att : constant String := Iter.Next_Word;
      begin
        exit when Elt = "";

        -- Read info with the list of enum values
       Trace ("Checking notation attribute " & Att & " of element " & Elt);
        Info.Name := Asu_Tus ("Att" & Info_Sep & Elt & Info_Sep & Att);
        Adtd.Info_List.Read (Info, Info);
        Iter1.Set (Asu_Ts (Info.List), Is_Sep'Access);
        declare
          Val : constant String := Iter1.Next_Word;
        begin
          exit when Val = "";
          Trace ("  Checking definition of notation " & Val
               & " of attribute " & Att
               & " of element " & Elt);
          -- Each value must be defined by a notation
          Unparsed_Rec.Is_Entity := False;
          Unparsed_Rec.Name := Asu_Tus (Val);
          Ctx.Unparsed_List.Search (Unparsed_Rec, Ok);
          if not Ok then
            Util.Error (Ctx.Flow,
                 "No notation for value " & Val
               & " of attribute " & Att & " defined at line "
               & Line_Image (Info.Line));
          end if;
        end;
        Iter1.Del;

        -- Read element, must not be EMPTY
        Info.Name := Asu_Tus ("Elt" & Info_Sep & Elt);
        Adtd.Info_List.Read (Info, Info);
        if Asu.Element (Info.List, 1) = 'E' then
          Util.Error (Ctx.Flow,
            "Element " & Elt & " defined at line " & Line_Image (Info.Line)
            & " is EMPTY and has an attribute type notation");
        end if;
      end;
    end loop;
    Iter.Del;

  end Final_Dtd_Check;

  -- Check for warnings
  type Elt_Ref_Type is record
    Father, Child : Asu_Us;
    Line : Natural;
  end record;
  package Us_Pool_Mng is new Unlimited_Pool (Elt_Ref_Type);
  procedure Check_Warnings (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type) is
    Info : Info_Rec;
    Moved : Boolean;
    Info_Kind : String (1 .. 3);
    Child_Kind, C : Character;
    In_Name : Boolean;
    Elt_Ref : Elt_Ref_Type;
    Pool : Us_Pool_Mng.Pool_Type;
    Found : Boolean;
    use type Asu_Us;
  begin
    -- Check that all elements referenced as children or in attlist are defined
    if Adtd.Info_List.Is_Empty then
      return;
    end if;

    -- Push in pool a list of all referenced elements
    -- Read all Infos
    Adtd.Info_List.Rewind;
    loop
      Adtd.Info_List.Read (Info, Moved => Moved);
      Info_Kind := Asu.Slice (Info.Name, 1, 3);
      if Info_Kind = "Elt" then
        -- Elt directive, need to parse children if Mixed of Children
        Child_Kind := Asu.Element (Info.List, 1);
        if Child_Kind = 'M' or else Child_Kind = 'C' then
          Child_Kind := 'C';
        else
          Child_Kind := 'X';
        end if;
      elsif Info_Kind = "Atl" then
        Child_Kind := 'A';
      else
        -- Att => discard
        Child_Kind := 'X';
      end if;

      if Child_Kind = 'C' then
        -- This is an element with a (mixed or not) list of children
        -- Push an entry for each child
        -- Save name (skip "Elt#")
        Elt_Ref.Father := Asu_Tus (Asu.Slice (
                   Info.Name, 5, Asu.Length (Info.Name)) );
        Elt_Ref.Line := Info.Line;
        Elt_Ref.Child := Asu_Null;
        -- Parse children names from list (skip first char)
        In_Name := False;
        for I in 2 .. Asu.Length (Info.List) loop
          C := Asu.Element (Info.List, I);
          if Util.Is_Valid_In_Name (C) then
            Asu.Append (Elt_Ref.Child, C);
            In_Name := True;
          else
            if In_Name then
              -- End of name, completed
              -- Push this child
              Pool.Push (Elt_Ref);
              Elt_Ref.Child := Asu_Null;
            end if;
            In_Name := False;
          end if;
        end loop;

      elsif Child_Kind = 'A' then
        -- This is a attlist, push an etry with empty Father
        Elt_Ref.Father := Asu_Null;
        Elt_Ref.Line := Info.Line;
        Elt_Ref.Child := Asu_Tus (Asu.Slice (
                   Info.Name, 5, Asu.Length (Info.Name)) );
        Pool.Push (Elt_Ref);
      end if;
      exit when not Moved;
    end loop;

    -- Check that each entry of the pool exists in list as Elt
    while not Pool.Is_Empty loop
      Pool.Pop (Elt_Ref);
      Info.Name := "Elt" & Info_Sep & Elt_Ref.Child;
      Adtd.Info_List.Search (Info, Found);
      if not Found then
        if Elt_Ref.Father /= Asu_Null then
          Util.Warning (Ctx.Flow,
            "Element " & Asu_Ts (Elt_Ref.Father) & " references unknown child "
                       &  Asu_Ts (Elt_Ref.Child),
            Elt_Ref.Line);
        else
          Util.Warning (Ctx.Flow,
            "Undefined element " & Asu_Ts (Elt_Ref.Child) & " used in ATTLIST",
            Elt_Ref.Line);
        end if;
      end if;
    end loop;
  end Check_Warnings;

  -- Replace "##" by "," then suppress "#"
  function Strip_Sep (Us : in Asu_Us) return String is
    use String_Mng;
  begin
    return Replace (Replace (Asu_Ts (Us), Info_Sep & Info_Sep, ","),
                    "" & Info_Sep, "");
  end Strip_Sep;

  -- Check children of element
  procedure Check_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Name : in Asu_Us;
                            Line_No : in Natural;
                            Children : in Children_Desc) is
    -- Element info
    Info : Info_Rec;
    -- Char of info List
    Char : Character;
    -- Parser iterator
    Iter_Xml : Parser.Iterator;
    -- Childs of current element
    Childstr : Asu_Us := Children.Children;
    -- General purpose Boolean
    use type Asu_Us;
  begin
    Trace ("Dtd check Xml children list " & Asu_Ts (Children.Children)
         & " Empty: " & Children.Is_Empty'Img
         & " Text : " & Children.Has_Text'Img);
    -- Remove tail from root (Info_Sep & Info_Sep) if any
    if not Ctx.Elements.Has_Father
    and then Asu.Length (Childstr) >= 2
    and then Asu.Slice (Childstr,
                        Asu.Length (Childstr) - 1,
                        Asu.Length (Childstr)) = Info_Sep & Info_Sep
    then
      Asu.Delete (Childstr,
                  Asu.Length (Childstr) - 1,
                  Asu.Length (Childstr));
    end if;
    -- Read its element def
    Info.Name := "Elt" & Info_Sep & Name;
    begin
      Adtd.Info_List.Read (Info, Info);
    exception
      when Info_Mng.Not_In_List =>
        -- Should have been detected by Check_Attributes
        Trace ("Dtd check children. Element name " & Asu_Ts (Name)
              & " does not exist");
        raise Internal_Error;
    end;
    -- Check children
    Trace ("Dtd check Dtd element info " & Asu_Ts (Info.List));
    -- Separate element type
    Char := Asu.Element (Info.List, 1);
    Info.List := Asu.Delete (Info.List, 1, 1);
    case Char is
      when 'E' =>
        -- Must be empty
        if not Children.Is_Empty then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " must be empty",
                      Line_No);
        end if;
      when 'A' =>
        -- Any
        null;
      when 'M' =>
        -- Check mixed: all children of xml must appear in dtd list
        Iter_Xml.Set (Asu_Ts (Childstr), Is_Sep'Access);
        loop
          declare
            -- Next Child from xml
            Child : constant String := Iter_Xml.Next_Word;
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
        Iter_Xml.Del;
      when 'C' =>
        if Children.Has_Text then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " must not have text",
                      Line_No);
        end if;
        -- Strictly check that list matches criteria
        if not Regular_Expressions.Match (Asu_Ts (Info.List),
                Asu_Ts (Childstr), Strict => True) then
          Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                    & " allows children " & Strip_Sep (Info.List)
                    & " but has children " & Strip_Sep (Childstr));
        end if;
        Trace ("Dtd checked children " & Strip_Sep (Childstr)
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

  -- INTERNAL: set Unparsed tag on attribute Attr
  procedure Set_Unparsed (Ctx : in out Ctx_Type;
                          Attr : in Asu_Us) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    use type Asu_Us;
  begin
    Trace ("Dtd setting unparsed on attribute " & Asu_Ts(Attr));
    -- Read current element from tree and make its attribute list
    for I in 1 .. Ctx.Elements.Children_Number loop
      if I = 1 then
        Ctx.Elements.Move_Child;
      else
        Ctx.Elements.Move_Brother (False);
      end if;
      Ctx.Elements.Read (Cell);
      if Cell.Kind = Xml_Parser.Attribute
      and then Cell.Name = Attr then
        -- Found the correct attribute
        Cell.Unparsed := True;
        Ctx.Elements.Replace (Cell);
        exit;
      end if;
    end loop;
    Ctx.Elements.Move_Father;
  end Set_Unparsed;

  -- INTERNAL
  -- Check attributes of element
  procedure Check_Attributes (Ctx        : in out Ctx_Type;
                              Adtd       : in out Dtd_Type;
                              Name       : in Asu_Us;
                              Line_No    : in Natural;
                              Attributes : in Asu_Us) is
    -- Atl, Att and Id info blocs
    Info, Attinfo : Info_Rec;
    -- Name looked in Info list (for error tracing)
    Error_Name : Asu_Us;
    -- Is info found in info list
    Info_Found : Boolean;
    -- Parser iterators
    Iter_Dtd, Iter_Xml : Parser.Iterator;
    -- Is an attribute set in xml
    Att_Set : Boolean;
    -- Attribute value in Xml
    Xml_Val : Asu_Us;
    -- List of dtd attribute names
    Att_Names : Asu_Us;
    -- Cell of ID (or IDREF), and if Found
    Idcell : Id_Cell;
    Found :  Boolean;
    -- Iterator of entities
    Iter : Parser.Iterator;
    -- Descriptor of unparsed entity
    Unparsed_Rec : Unparsed_Type;
    use type Asu_Us;
  begin
    Trace ("Dtd check Xml attributes list " & Asu_Ts (Attributes) );
    -- Read element def
    Info.Name := "Elt" & Info_Sep & Name;
    Adtd.Info_List.Search (Info, Info_Found);
    if not Info_Found then
      Util.Error (Ctx.Flow, "Element " &  Asu_Ts (Name)
                 & " is not defined in dtd");
    end if;
    -- Read its ATTLIST def
    Info.Name := "Atl" & Info_Sep & Name;
    Adtd.Info_List.Search (Info, Info_Found);
    if Info_Found then
      Adtd.Info_List.Read (Info, Info);
    end if;
    if not Info_Found or else Info.List = Asu_Null then
      -- No or empty ATTLIST for this element
      if Attributes = Asu_Null then
        Trace ("Dtd checked element " & Asu_Ts (Name)
             & " with no attributes, versus no or empty attlist");
        return;
      else
        -- Attributes must have be declared
        Util.Error (Ctx.Flow, "According to dtd, element " & Asu_Ts (Name)
                  & " is not allowed to have attributes",
                  Line_No);
      end if;
    end if;
    -- Check attributes
    Trace ("Dtd check Dtd attlist info " & Asu_Ts (Info.List));
    -- Check attributes xml vs dtd
    -- First extract list of dtd attribute names
    Iter_Dtd.Set (Asu_Ts (Info.List), Is_Sep'Access);
    loop
      declare
        -- Next attribute from dtd
        Attr : constant String := Iter_Dtd.Next_Word;
      begin
        exit when Attr = "";
        Asu.Append (Att_Names, Info_Sep & Attr & Info_Sep);
      end;
      -- Skip type and default spec
      Iter_Dtd.Next_Word;
    end loop;
    Iter_Dtd.Del;
    -- Now check that any attribute of xml is in the list of dtd
    Iter_Xml.Set (Asu_Ts (Attributes), Is_Sep'Access);
    loop
      declare
        -- Next attribute from xml
        Attr : constant String := Iter_Xml.Next_Word;
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
    Iter_Xml.Del;

    -- Check attributes dtd vs xml
    --  Any Fixed in dtd must appear in xml and have correct value
    --  If Expand, then any default, if it does not appear in Attributes,
    --   must be added to tree with default value
    Iter_Dtd.Set (Asu_Ts (Info.List), Is_Sep'Access);
    loop
      declare
        -- Next dtd attribute and type+default from dtd
        Attr : constant String := Iter_Dtd.Next_Word;
        Td : String(1 .. 2);
      begin
        -- Read all necessary info
        exit when Attr = "";
        Td := Iter_Dtd.Next_Word;
        -- Corresponding attribute info from dtd if any
        if Td(1) = 'E' or else Td(1) = 'N'
        or else Td(2) = 'F' or else Td(2) = 'D' then
          Attinfo.Name := "Att" & Info_Sep & Name & Info_Sep & Attr;
          Error_Name := Attinfo.Name;
          Adtd.Info_List.Read (Attinfo, Attinfo);
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
        elsif (Td(1) = 'E' or else Td (1) = 'N') and then Att_Set then
          -- Not fixed Enum in dtd with xml value: #<val># must be in dtd list
          if String_Mng.Locate (Asu_Ts (Attinfo.List),
                 Info_Sep  & Asu_Ts (Xml_Val) & Info_Sep) = 0 then
            Util.Error (Ctx.Flow, "According to dtd, Enum attribute " & Attr
                      & " has incorrect value "
                      & Asu_Ts (Xml_Val), Line_No);
          end if;
        elsif Td(2) = 'D' and then not Att_Set then
          -- Default in dtd with no xml value: insert default in tree
          --  and set Xml_Val
          if Td(1) = 'E' or else Td(1) = 'N' then
            -- Default of enum is first string after Info_Sep
            declare
              -- Get the first value from dtd list, from 2 to second sep
              Sep : constant Positive
                  := String_Mng.Locate (Asu_Ts (Attinfo.List),
                                        Info_Sep & "", 2);
              Dtd_Val : constant String
                      := Asu.Slice (Attinfo.List, 2, Sep - 1 );
            begin
              if Ctx.Expand then
                Tree_Mng.Add_Attribute (Ctx.Elements.all,
                    Asu_Tus (Attr), Asu_Tus (Dtd_Val), Line_No);
              end if;
              Xml_Val := Asu_Tus (Dtd_Val);
              if Attr = "xml:space" and then Dtd_Val = "preserve" then
                Tree_Mng.Add_Tuning (Ctx.Elements.all,
                                     Tree_Mng.Xml_Space_Preserve);
                Trace (" Check, added tuning " & Tree_Mng.Xml_Space_Preserve);
              end if;
            end;
          else
            if Ctx.Expand then
              -- Default of not enum is the value
              Tree_Mng.Add_Attribute (Ctx.Elements.all,
                    Asu_Tus (Attr), Attinfo.List, Line_No);
            end if;
            Xml_Val := Attinfo.List;
          end if;
        elsif Att_Set then
          -- Comformance checks on ID, IDREF(s) and NMTOKEN(s)
          -- Store ID and IDREFs and Sanity checks on I
          if (Td(1) = 'I' or else Td(1) = 'R' or else Td(1) = 'Y')
          and then not Util.Name_Ok (Xml_Val) then
            Util.Error (Ctx.Flow, "Invalid name for ID, IDREF or ENTITY"
                      & Asu_Ts (Xml_Val), Line_No);
          elsif (Td(1) = 'r' or else Td(1) = 'y')
          and then  not Util.Names_Ok (Xml_Val, Util.Space & "") then
            Util.Error (Ctx.Flow, "Invalid name in IDREFS or ENTITIES"
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
            -- Check this ID is not already set and add it (with its line_no)
            Idcell.Name := Xml_Val;
            Idcell.Line_No := Line_No;
            Ctx.Ids.Search (Idcell, Found);
            if Found then
              Ctx.Ids.Read (Idcell, Idcell);
              Util.Error (Ctx.Flow,
                 "ID " & Asu_Ts (Xml_Val)
                       & " already defined at line "
                       & Line_Image (Idcell.Line_No),
                 Line_No);
            end if;
            Ctx.Ids.Insert (Idcell);
            Trace (" Check, added ID " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'R' then
            -- Store this IDREF and line_no to list of IDREFs
            Idcell.Name := Xml_Val;
            Idcell.Line_No := Line_No;
            Ctx.Idrefs.Insert (Idcell);
            Trace (" Check, added IDREF " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'r' then
            Idcell.Line_No := Line_No;
            -- Store these IDREFs and line_no to list of IDREFs
            Xml_Val := Util.Normalize_Separators (Xml_Val);
            -- Split IDREFS and insert each IDREF
            Iter_Xml.Set (Asu_Ts (Xml_Val),
                        Parser.Is_Space_Or_Htab_Function'Access);
            Trace (" Check, adding IDREFs " & Asu_Ts (Xml_Val));
            loop
              Idcell.Name := Asu_Tus (Iter_Xml.Next_Word);
              exit when Idcell.Name = Asu_Null;
              Ctx.Idrefs.Insert (Idcell);
              Trace (" Check, added IDREF " & Asu_Ts (Idcell.Name));
            end loop;
            Iter_Xml.Del;
          end if;
        end if;

        -- Check that ENTITY or ENTITIES are unparsed entities
        if Td(1) = 'Y' or else Td(1) = 'y' then
          -- This attribute is ENTITY or ENTITIES => notify
          Set_Unparsed (Ctx, Asu_Tus (Attr));
          Iter.Set (Asu_Ts (Xml_Val));
          loop
            declare
              Entity : constant String := Iter.Next_Word;
            begin
              exit when Entity = "";
              Trace (" Check, unparsed entity " & Entity);
              Unparsed_Rec.Is_Entity := True;
              Unparsed_Rec.Name := Asu_Tus (Entity);
              Ctx.Unparsed_List.Search (Unparsed_Rec, Found);
              if not Found then
                Util.Error (Ctx.Flow, "Unknown unparsed entity " & Entity);
              end if;
            end;
          end loop;
          Iter.Del;
        end if;
        Trace ("Dtd checked versus dtd attribute " & Attr & " type " & Td);
      end;
    end loop;
    Iter_Dtd.Del;
  exception
    when Info_Mng.Not_In_List =>
      Trace ("Dtd check: Cannot find info " & Asu_Ts (Error_Name));
      Iter_Dtd.Del;
      raise Internal_Error;
  end Check_Attributes;

  -- Check attributes of current element of the tree
  procedure Check_Attributes (Ctx : in out Ctx_Type;
                              Adtd : in out Dtd_Type) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    -- Lists of attributes
    Attributes : Asu_Us;
    use type Asu_Us;
  begin
    if not Adtd.Set then
      -- No dtd => no check
      return;
    end if;
    if Debug_Level /= 0 then
      Ctx.Elements.Read (Cell);
      Trace ("Dtd checking attributes of element " & Asu_Ts(Cell.Name));
    end if;
    -- Read current element from tree and make its attribute list
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        if Cell.Kind = Xml_Parser.Attribute then
          Asu.Append (Attributes, Info_Sep & Cell.Name & Info_Sep);
        else
          -- Children
          exit;
        end if;
      end loop;
      Ctx.Elements.Move_Father;
    end if;
    Ctx.Elements.Read (Cell);
    -- Check Attributes
    Check_Attributes (Ctx, Adtd, Cell.Name, Cell.Line_No, Attributes);
  end Check_Attributes;

  -- Add current element to list of children
  procedure Add_Current_Element (List : in out Asu_Us; Name : in Asu_Us) is
    use type Asu_Us;
  begin
    Asu.Append (List, Info_Sep & Name & Info_Sep);
  end Add_Current_Element;

  -- Check that list matches Dtd definition of current element
  procedure Check_Element (Ctx      : in out Ctx_Type;
                           Adtd     : in out Dtd_Type;
                           Children : in Children_Desc) is
    Cell : My_Tree_Cell;
  begin
    if not Adtd.Set then
      -- No dtd => no check
      return;
    end if;
    Ctx.Elements.Read (Cell);
    Check_Children (Ctx, Adtd, Cell.Name, Cell.Line_No, Children);
  end Check_Element;

  -- INTERNAL
  -- Build from tree the descriptor of children
  procedure Build_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Children : out Children_Desc) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
  begin
    if not Adtd.Set then
      -- No dtd => no check
      return;
    end if;
    -- Read current element from tree and make its children lists
    Children.Is_Empty := True;
    Children.Has_Text := False;
    Children.Children := Asu_Null;
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        case Cell.Kind is
          when Xml_Parser.Attribute =>
            -- Attribute: skip
            null;
          when Xml_Parser.Element =>
            Children.Is_Empty := False;
            Add_Current_Element (Children.Children, Cell.Name);
          when Xml_Parser.Text =>
            Children.Is_Empty := False;
            Children.Has_Text := True;
          when Xml_Parser.Pi | Xml_Parser.Comment =>
            Children.Is_Empty := False;
        end case;
      end loop;
      Ctx.Elements.Move_Father;
    end if;
  end Build_Children;

  -- INTERNAL Check tail
  procedure Check_Tail (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    if Ctx.Elements.Children_Number = 0 then
      -- No child
      return;
    end if;
    Ctx.Elements.Read (Cell);
    if Cell.Nb_Attributes /= 0 then
      Util.Error (Ctx.Flow, "Tail has attributes");
    end if;

    -- Only Pis and Comments in tail
    Ctx.Elements.Move_Child;
    loop
      Ctx.Elements.Read (Cell);
      if Cell.Kind /= Pi and then Cell.Kind /= Comment then
         Util.Error (Ctx.Flow, "Invalid node type " & Mixed_Str (Cell.Kind'Img)
                              & " in tail");
      end if;
      exit when not Ctx.Elements.Has_Brother (False);
      Ctx.Elements.Move_Brother (False);
    end loop;
  end Check_Tail;


  -- Check a whole element tree recursively
  procedure Check_Subtree (Ctx  : in out Ctx_Type;
                           Adtd : in out Dtd_Type) is
    Cell : My_Tree_Cell;
    Children : Children_Desc;
    Is_Root : constant Boolean := not Ctx.Elements.Has_Father;
    use type Asu_Us;
  begin
    -- Check current element, attributes then children
    Check_Attributes (Ctx, Adtd);
    Build_Children (Ctx, Adtd, Children);
    Check_Element (Ctx, Adtd, Children);
    -- Check children recursively
    if Ctx.Elements.Children_Number = 0 then
      -- No child
      return;
    end if;
    Ctx.Elements.Read (Cell);
    if Cell.Nb_Attributes = Ctx.Elements.Children_Number then
      -- All children are attributes
      return;
    end if;

    -- Move to first real child (not attribute)
    Ctx.Elements.Move_Child;
    for I in 1 .. Cell.Nb_Attributes loop
      Ctx.Elements.Move_Brother (False);
    end loop;

    loop
      Ctx.Elements.Read (Cell);
      -- Skip Text, Comments and tail
      if Cell.Kind = Element then
        if Is_Root and then Cell.Name = Asu_Null
        and then not Ctx.Elements.Has_Brother (False) then
          Check_Tail (Ctx);
        else
          -- Recursive check this sub element
          Check_Subtree (Ctx, Adtd);
        end if;
      end if;
      exit when not Ctx.Elements.Has_Brother (False);
      Ctx.Elements.Move_Brother (False);
    end loop;

    -- Done, move back to father
    Ctx.Elements.Move_Father;
  end Check_Subtree;


  -- For sorting IDREFs
  function Less_Than (I1, I2 : Id_Cell) return Boolean is
    use type Asu_Us;
  begin
    -- Sort by Name then Line_No
    return I1.Name < I2.Name
    or else (I1.Name = I2.Name and then I1.Line_No < I2.Line_No);
  end Less_Than;
  procedure Id_Sort is new Idref_List_Mng.Sort (Less_Than);
  -- Final checks
  -- Check that all attribute values of Xml tagged IDREF(s) in Dtd
  --  and thus collected in Idrefs
  --  exist in the list of attribute values of Xml tagged ID
  --  and thus collected in Ids
  procedure Final_Check (Ctx : in out Ctx_Type) is
    Done : Boolean;
    Idref, Prev_Ref : Id_Cell;
    Found : Boolean;
  begin
    Trace ("Checking final");
    -- Each IDREF must exist in IDs
    if not Ctx.Idrefs.Is_Empty then
      Id_Sort (Ctx.Idrefs.all);
      Ctx.Idrefs.Rewind;
      loop
        Ctx.Idrefs.Read (Idref, Done => Done);
        -- Check for Id if this reference is new
        if Idref /= Prev_Ref then
          Ctx.Ids.Search (Idref, Found);
          if not Found then
            Util.Error (Ctx.Flow,"No ID for this IDREF " & Asu_Ts (Idref.Name),
                        Idref.Line_No);
          end if;
          Prev_Ref := Idref;
        end if;
        exit when not Done;
      end loop;
    end if;
    Trace ("Checked final");
  end Final_Check;

end Dtd;

