with Parser, Images, Upper_Char;
separate (Xml_Parser.Parse_Mng)
package body Dtd is

 -- Separator within Info name and list
  Info_Sep : constant Character := '#';
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Info_Sep;
  end Is_Sep;

  -- Image of line_no without leading space
  function Line_Image (I : Integer) return String renames Images.Integer_Image;

  -- Init (clear) Dtd data
  procedure Init (Adtd : in out Dtd_Type) is
  begin
    -- No Dtd set
    Adtd.Set := False;
    -- No xml instruction found (yet)
    Adtd.Xml_Found := False;
    -- No encoding
    Adtd.Encoding.Set_Null;
    -- Reset entities
    Entity_Mng.Initialise (Adtd.Entity_List);
    -- Reset info list
    Adtd.Info_List.Delete_List;
    -- Reset lists of attributes of type notation
    Adtd.Notation_Attrs.Set_Null;
    -- Reset list of internal attlist and entity
    Adtd.Internals.Set_Null;
    -- Clean include level
    Adtd.Include_Level := 0;
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
    Ok := Util.Try (Ctx.Flow, "xml");
    if Ok then
      Char := Util.Get (Ctx.Flow);
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
      Debug ("Dtd parsing xml instruction");
      Dummy.Line_No := 0;
      if Ctx.Prologue.Is_Empty then
        Ctx.Prologue.Insert_Father (Dummy);
      else
        Ctx.Prologue.Insert_Child (Dummy, False);
      end if;
      Parse_Attributes (Ctx, Adtd, Of_Xml => True);
      Check_Xml_Attributes (Ctx, False);
      -- Delete this dummy child
      Ctx.Prologue.Delete_Tree;
      -- Done
      Debug ("Dtd parsed xml instruction");
    else
      -- Skip processing instruction
      Util.Parse_Until_Str (Ctx.Flow, Util.Instruction & Util.Stop);
      Util.Reset_Curr_Str (Ctx.Flow);
    end if;
    -- Xml instruction not allowed any more
    Adtd.Xml_Found := True;
  end Parse_Instruction;

  -- Build the regexp: <name> -> (#<name>#) ,  . -> \.
  procedure Build_Regexp (Ctx : in out Ctx_Type; Str : in out As.U.Asu_Us) is
    -- Separators
    Seps : constant String := "|,?*+()";
    -- Result with the info sep
    Res : As.U.Asu_Us;
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
    for I in 1 .. Str.Length loop
      C := Str.Element (I);
      if Is_Sep (C) then
        if In_Word then
          -- End of word
          Res.Append (Info_Sep & ")");
          In_Word := False;
        end if;
      else
        if not In_Word then
          -- Start of word
          Res.Append ("(" & Info_Sep);
          In_Word := True;
        end if;
      end if;
      Res.Append (C);
    end loop;
    if In_Word then
      Res.Append (Info_Sep & ")");
    end if;
    -- Check that words are always separated: a '(' shall always be
    -- either the first of precceded by ( | or ,
    declare
      Index : Natural;
      Str : constant String := Res.Image;
    begin
      Index := 1;
      loop
        Index := Str_Util.Locate (Res.Image, "(", Index + 1);
        exit when Index = 0;
        C := Str(Index - 1);
        if C /= '(' and then C /= '|' and then C /= ',' then
          Debug ("Dtd missing seperator in >" & Res.Image
               & "< at index " & Line_Image (Index));
          Util.Error (Ctx.Flow, "Invalid children definition");
        end if;
      end loop;
    end;
    -- Remove any ','
    Res := As.U.Tus (Str_Util.Substit (Res.Image, ",", ""));
    -- Now compile to check it
    if not Regular_Expressions.Check (Res.Image) then
      Debug ("Dtd regex does node compile >" & Res.Image & "<");
      Util.Error (Ctx.Flow, "Invalid children definition");
    end if;
    -- Done
    Str := Res;
  end Build_Regexp;

  -- Parse <!ELEMENT
  procedure Parse_Element (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Info : Info_Rec;
    Info_Name : As.U.Asu_Us;
    Found : Boolean;
    Char : Character;
    -- Parser iterator
    Iter : Parser.Iterator;
    use type As.U.Asu_Us;
  begin
    -- Parse element name
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Info_Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Info_Name, Ref_Dtd_Mark);
    Util.Normalize (Info_Name);
    Util.Normalize_Spaces (Info_Name);
    if not Util.Name_Ok (Info_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Info_Name.Image);
    end if;
    Info.Name := "Elt" & Info_Sep & Info_Name;
    Info.Line := Util.Get_Line_No (Ctx.Flow);
    -- Element must not exist
    Adtd.Info_List.Search (Info, Found);
    if Found then
      Util.Error (Ctx.Flow, "ELEMENT " & Info_Name.Image
                          & " already exists");
    end if;
    -- Parse content
    Util.Skip_Separators (Ctx.Flow);

    Util.Parse_Until_Stop (Ctx.Flow);
    Util.Unget (Ctx.Flow);
    Info.List := Util.Get_Curr_Str (Ctx.Flow);
    -- Expand potential parameter entities and re-insert
    Util.Expand_Vars (Ctx, Adtd, Info.List, Ref_Dtd_Content);
    Util.Normalize (Info.List);
    Util.Normalize_Spaces (Info.List);
    Util.Insert (Ctx.Flow, Info.List.Image);

    -- Check possible content: EMPTY, ANY or (<list>)
    Found := Util.Try (Ctx.Flow, "EMPTY");
    if Found then
      Info.List := As.U.Tus ("E");
    end if;
    if not Found then
      Found := Util.Try (Ctx.Flow, "ANY");
      if Found then
        Info.List := As.U.Tus ("A");
      end if;
    end if;
    if not Found then
      Found := Util.Try (Ctx.Flow, "(");
      if not Found then
        Util.Error (Ctx.Flow, "Unexpected character "
                 & Info.List.Element (1)
                 & " at start of ELEMENT list");
      else
        -- A (mixed) list
        Found := False;
      end if;
    end if;

    if not Found then
      -- A (mixed) list: parse until ')' and remove any seperator
      Util.Parse_Until_Close (Ctx.Flow);
      Info.List := Util.Get_Curr_Str (Ctx.Flow);
      Util.Remove_Separators (Info.List, "?*+()|,");
      -- Now see if it is mixed or children
      if Info.List.Locate ("#PCDATA") /= 0 then
        -- Mixed
        if Info.List.Image = "#PCDATA" then
          -- Possible '*' after ')', skip it
          Found := Util.Try (Ctx.Flow, "*");
          Info.List.Set_Null;
        elsif Info.List.Slice (1, 8) = "#PCDATA|" then
          -- Remove heading #PCDATA
          Info.List := As.U.Tus (
              Str_Util.Cut (Info.List.Image, 8));
          Util.Remove_Separators (Info.List, "?*+()|,");
          -- Check that everything between "|" are names
          if Info.List.Element (Info.List.Length) = '|'
          or else Info.List.Element (1) = '|' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          if not Util.Names_Ok (Info.List, "|") then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          -- Last ')' must be followed by '*', remove it
          if Util.Get (Ctx.Flow) /= '*' then
            Util.Error (Ctx.Flow, "Invalid Mixed definition");
          end if;
          -- Replace '|' by '#' and prepend and append a '#'
          Info.List := As.U.Tus (
            Str_Util.Substit (Info_Sep & Info.List.Image & Info_Sep,
                                "|", "" & Info_Sep));
          -- Check unicity of entries
          Iter.Set (Info.List.Image, Is_Sep'Access);
          loop
            declare
              -- Next enum value
              Val : constant String := Parser.Next_Word (Iter);
            begin
              exit when Val = "";
              -- The val shall appear only once
              if Str_Util.Locate (Info.List.Image,
                                    Info_Sep & Val & Info_Sep,
                                    Occurence => 2) /= 0 then
                Util.Error (Ctx.Flow, "Mixed value "
                     & Val & " already used");
              end if;
            end;
          end loop;
          Iter.Del;

        else
          Util.Error (Ctx.Flow, "Invalid Mixed definition");
        end if;
        Info.List := "M" & Info.List;
      else
        -- A regexp of children:
        -- Put into "(" ")" and append "?", "*" or, "+" if needed
        Char := Util.Get (Ctx.Flow);
        if Char = '?' or else Char = '*' or else Char = '+' then
          Info.List := "(" & Info.List & ")" & Char;
        else
          Util.Unget (Ctx.Flow);
        end if;
        Util.Remove_Separators (Info.List, "?*+()|,");
        -- Check valid names
        if not Util.Names_Ok (Info.List, "?*+()|,") then
          Util.Error (Ctx.Flow, "Invalid name in Children definition");
        end if;
        -- Fix regex: each name becomes "(#name#)"
        Build_Regexp (Ctx, Info.List);
        Info.List := "C" & Info.List;
      end if;
    end if;
    -- Directive must end now
    Util.Skip_Separators (Ctx.Flow);
    Char := Util.Get (Ctx.Flow);
    if Char /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character " & Char
                          & " at end of ELEMENT");
    end if;

    -- If in internal dtd and not in include, add @element to Internals
    if Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow then
      if Adtd.Internals.Is_Null then
        Adtd.Internals.Append (Info_Sep);
      end if;
      Adtd.Internals.Append ("@" & Info_Name.Image & Info_Sep);
    end if;
    -- Store element
    Adtd.Info_List.Insert (Info);
    Debug ("Dtd parsed directive ELEMENT -> " & Info.Name.Image
         & " " & Info.List.Image);
  end Parse_Element;

  -- Parse <!ATTLIST
  procedure Parse_Attlist (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Atl, Att and Id info blocs
    Info, Attinfo : Info_Rec;
    Found : Boolean;
    -- Element, attribute name and type
    Elt_Name, Att_Name, Att_Type : As.U.Asu_Us;
    -- Complete Attlist to expand
    Attlist : As.U.Asu_Us;
    -- Type and default chars
    Typ_Char, Def_Char : Character;
    -- Enum List
    Enum : As.U.Asu_Us;
    -- Default value
    Def_Val : As.U.Asu_Us;
    -- Has an ID already been parsed for this element
    Elt_Has_Id : Boolean;
    -- Is this attribute already defined
    Attr_Already_Set : Boolean;
    -- Parser iterator
    Iter : Parser.Iterator;

    function Try (Str : String) return Boolean is
    begin
      return Util.Try (Ctx.Flow, Str);
    end Try;
    function Get return Character is
    begin
      return Util.Get (Ctx.Flow);
    end Get;

    use type As.U.Asu_Us;
  begin
    -- Parse element name
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & Util.Stop);
    Util.Unget (Ctx.Flow);
    Elt_Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Elt_Name, Ref_Dtd_Mark);
    Util.Normalize (Elt_Name);
    Util.Normalize_Spaces (Elt_Name);
    if not Util.Name_Ok (Elt_Name) then
      Util.Error (Ctx.Flow, "Invalid name " & Elt_Name.Image);
    end if;
    Info.Name := "Atl" & Info_Sep & Elt_Name;
    Info.Line := Util.Get_Line_No (Ctx.Flow);
    -- Attribute list of this element may already exist => merge
    Adtd.Info_List.Search (Info, Found);
    if Found then
      Adtd.Info_List.Read (Info);
      Debug ("Dtd retrieved previous ATTLIST -> " & Info.Name.Image
           & " " & Info.List.Image);
      Util.Warning (Ctx, "Attlist already defined at line "
           & Line_Image (Info.Line)
           & " for element " & Elt_Name.Image);
    end if;

    -- Parse Attlist
    Util.Skip_Separators (Ctx.Flow);

    Util.Parse_Until_Stop (Ctx.Flow);
    Util.Unget (Ctx.Flow);
    Attlist := Util.Get_Curr_Str (Ctx.Flow);
    -- Expand potential parameter entities and re-insert
    Util.Expand_Vars (Ctx, Adtd, Attlist, Ref_Dtd_Mark);
    Util.Normalize (Attlist);
    Util.Normalize_Spaces (Attlist);
    Util.Insert (Ctx.Flow, Attlist.Image);

    -- Loop on all attributes
    Elt_Has_Id := False;
    loop
      Util.Skip_Separators (Ctx.Flow);
      -- Name of attribute or end of list
      exit when Util.Try (Ctx.Flow, Util.Stop & "");
      -- Get attribute name
      Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
      Att_Name := Util.Get_Curr_Str (Ctx.Flow);
      Util.Expand_Name (Ctx, Adtd, Att_Name, Ref_Dtd_Mark);
      Util.Normalize (Att_Name);
      Util.Normalize_Spaces (Attlist);
      if not Util.Name_Ok (Att_Name) then
        Util.Error (Ctx.Flow, "Invalid attribute " & Att_Name.Image);
      end if;
      -- Check that this attribute is not already defined, otherwise discard
      --  any new definition
      -- Look for #attribute##
      Attr_Already_Set := Str_Util.Locate (Info.List.Image,
             Info_Sep & Att_Name.Image & Info_Sep & Info_Sep) /= 0;
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
        Att_Type := Util.Get_Curr_Str (Ctx.Flow);
        Util.Error (Ctx.Flow, "Invalid attribute type " & Att_Type.Image);
      end if;

      -- Parse enumeration for Enum or Notation
      if Typ_Char = 'E' or else Typ_Char = 'N' then
        Util.Parse_Until_Char (Ctx.Flow, ")");
        Enum := Util.Get_Curr_Str (Ctx.Flow);
        Util.Remove_Separators (Enum, "()|");
        if Enum.Is_Null then
          Util.Error (Ctx.Flow, "Empty enumeration");
        end if;
        -- Check that everything between "|" are
        -- nmtokens if Enum, names if Notation
        if Enum.Element (Enum.Length) = '|'
        or else Enum.Element (1) = '|' then
          Util.Error (Ctx.Flow, "Invalid Enum definition");
        end if;
        if not Util.Names_Ok (Enum, "|", Allow_Token => Typ_Char = 'E') then
           Util.Error (Ctx.Flow,
             (if Typ_Char = 'E' then "Invalid nmtoken in Enum definition"
              else "Invalid name in Notation definition"));
        end if;
        -- Replace '|' by '#' and prepend and append a '#'
        Enum := As.U.Tus (
          Str_Util.Substit (Info_Sep & Enum.Image & Info_Sep,
                              "|", "" & Info_Sep));
        -- Check unicity of entries
        Iter.Set (Enum.Image, Is_Sep'Access);
        loop
          declare
            -- Next enum value
            Val : constant String := Parser.Next_Word (Iter);
          begin
            exit when Val = "";
            -- The val from Istart to Istop shall appear only once
            if Str_Util.Locate (Enum.Image,
                                  Info_Sep & Val & Info_Sep,
                                  Occurence => 2) /= 0 then
              Util.Error (Ctx.Flow, "Enumerated value "
                   & Val & " already used");
            end if;
            -- Check xml:space has either "default" or "preserve or both
            if Att_Name.Image = Tree_Mng.Xml_Space then
              if Val /= Tree_Mng.Preserve and then Val /= "default" then
                Util.Error (Ctx.Flow, "Enumerated value "
                     & Val & " not allowed for " & Tree_Mng.Xml_Space);
              end if;
              if Val = Tree_Mng.Preserve then
                -- Store this info appart in Ctx for when the Dtd is cleared
                --  (no expand)
                Ctx.Preserved.Append (Info_Sep & Elt_Name);
                Debug ("Dtd saving preserve of " & Elt_Name.Image);
              end if;
            end if;
          end;
        end loop;
        Iter.Del;
      end if;

      -- Check xml:space is enum
      if Att_Name.Image = Tree_Mng.Xml_Space and then Typ_Char /= 'E' then
        Util.Error (Ctx.Flow, "Attribute " & Tree_Mng.Xml_Space
                            & " must be enum");
      end if;

      -- Check supported att defaults
      Util.Skip_Separators (Ctx.Flow);
      Def_Char := (if Try ("#REQUIRED") then 'R'
                   elsif Try ("#IMPLIED") then 'I'
                   elsif Try ("#FIXED ") then 'F'
                   else 'D');

      -- Check no default value or get default value
      Util.Skip_Separators (Ctx.Flow);
      if Def_Char = 'R' or else Def_Char = 'I' then
        -- There shall be no default value for required or implied attribute
        if Try ("""") or else Try ("'") then
          Util.Error (Ctx.Flow, "Unexpected default value for attribute "
                    &  Att_Name.Image);
        end if;
      else
        -- Get default value for fixed or default attribute
        Parse_Value (Ctx, Adtd, Ref_Dtd_Mark, Def_Val);
      end if;

      -- Check ID
      if Typ_Char = 'I' and then not Attr_Already_Set then
        -- Only one ID per element
        if Elt_Has_Id then
          Util.Error (Ctx.Flow, "Element " & Elt_Name.Image
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
        if (Str_Util.Locate (Enum.Image,
              Info_Sep & Def_Val.Image & Info_Sep) = 0) then
          Util.Error (Ctx.Flow, "Default or fixed value "
                    & Def_Val.Image & " not in Enum");
        end if;
        -- Remove #default and insert #default in head
        Enum := As.U.Tus (Str_Util.Substit (
                 Enum.Image,
                 Info_Sep & Def_Val.Image,
                 ""));
        Enum := Info_Sep & Def_Val.Image & Enum;
      end if;

      -- Check validity of default value
      if Def_Char = 'D' or else Def_Char = 'F' then
        case Typ_Char is
          when 'R' | 'Y' =>
            if not Util.Name_Ok (Def_Val) then
              Util.Error (Ctx.Flow, "Invalid name for default value");
            end if;
          when 'r' | 'y' =>
            if not Util.Names_Ok (Def_Val, Util.Space & "") then
              Util.Error (Ctx.Flow, "Invalid name in default value");
            end if;
          when 'T' =>
            if not Util.Name_Ok (Def_Val, Allow_Token => True) then
              Util.Error (Ctx.Flow, "Invalid token for default value");
            end if;
          when 't' =>
            if not Util.Names_Ok (Def_Val, Util.Space & "",
                                  Allow_Token => True) then
              Util.Error (Ctx.Flow, "Invalid token in default value");
            end if;
          when others => null;
        end case;
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
          Debug ("Dtd stored attribute type -> " & Attinfo.Name.Image
           & " " & Attinfo.List.Image);
        elsif Def_Char = 'F' or else Def_Char = 'D' then
          Attinfo.List := Info_Sep & Def_Val & Info_Sep;
          Adtd.Info_List.Insert (Attinfo);
          Debug ("Dtd stored attribute type -> " & Attinfo.Name.Image
           & " " & Attinfo.List.Image);
        end if;
        -- Verify Notation is not used twice (##N) for this element
        if Typ_Char = 'N'
         and then Str_Util.Locate (Info.List.Image,
                                     Info_Sep & Info_Sep & "N") /= 0 then
          declare
            Stop : constant Positive
                 := Str_Util.Locate (Info.List.Image,
                                       Info_Sep & Info_Sep & "N");
            Start : constant Positive
                  := Str_Util.Locate (Info.List.Image, Info_Sep & "",
                                        From_Index => Stop - 1,
                                        Forward => False);
          begin
            Util.Error (Ctx.Flow, "Notation "
              & Info.List.Slice (Start + 1, Stop - 1)
              & " already defined for element " & Elt_Name.Image);
          end;
        end if;
        if Typ_Char = 'N' then
          -- Append Elt##Attr# to the list of notation attributes
          if Adtd.Notation_Attrs.Is_Null then
            Adtd.Notation_Attrs := As.U.Tus ("" & Info_Sep);
          end if;
          Adtd.Notation_Attrs.Append (
              Elt_Name & Info_Sep & Info_Sep & Att_Name & Info_Sep);
        end if;
        -- Append this attribute in list: #attribute##td#attribute##td#...
        if Info.List.Is_Null then
          Info.List.Append (Info_Sep);
        end if;
        Info.List.Append (Att_Name & Info_Sep & Info_Sep
                        & Typ_Char & Def_Char & Info_Sep);
        -- If in internal dtd and not in include, add it to internals
        if Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow then
          if Adtd.Internals.Is_Null then
            Adtd.Internals.Append (Info_Sep);
          end if;
          Adtd.Internals.Append (Elt_Name.Image & Info_Sep & Info_Sep
                               & Att_Name.Image & Info_Sep);
        end if;
      else
        -- This attribute is already defined (for this element)
        Debug ("Dtd discarding duplicate ATTLIST -> " & Info.Name.Image
             & " " & Att_Name.Image & Info_Sep & Info_Sep
             & Typ_Char & Def_Char);
        Util.Warning (Ctx, "Attribute " & Att_Name.Image
             & " already defined for element " & Elt_Name.Image);
      end if;
    end loop;
    -- Attlist is ended: store
    Adtd.Info_List.Insert (Info);
    Debug ("Dtd parsed directive ATTLIST -> " & Info.Name.Image
         & " " & Info.List.Image);
  end Parse_Attlist;

  -- Parse <!ENTITY
  procedure Parse_Entity (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Entity name, value (or URI or notation)
    Name, Value : As.U.Asu_Us;
    -- Public and System Ids
    Public_Id, System_Id : As.U.Asu_Us;
    -- Is it a parameter entity
    Parameter : Boolean;
    -- Is it Internal, and parsed
    Internal : Boolean;
    Parsed : Boolean;
    -- Is it public
    Public : Boolean;
    -- Parameter entity indicator
    Parstr : As.U.Asu_Us;
    -- Unparsed entity rec
    Unparsed_Rec : Unparsed_Type;
    Char : Character;
  begin
    -- See if this is a parameter entity
    Util.Skip_Separators (Ctx.Flow);
    Char := Util.Get (Ctx.Flow);
    if Char = '%' then
      -- Check if this is "% name" of parameter entity definition
      --  or "%name;" of entity definition of a name that is a reference
      --  to a parameter entity
      Char := Util.Get (Ctx.Flow);
      if Util.Is_Separator (Char) then
        -- This is the definition of a parameter entity
        Parameter := True;
        Parstr := As.U.Tus ("%");
      else
        -- This is a reference to a parameter entity
        Parameter := False;
        Parstr.Set_Null;
        Util.Unget (Ctx.Flow, 2);
      end if;
    else
      -- This is a entity definition
      Util.Unget (Ctx.Flow);
      Parameter := False;
      Parstr.Set_Null;
    end if;

    -- Parse entity name
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Name, Ref_Dtd_Mark);
    -- Strip separators
    Util.Normalize (Name);
    Util.Normalize_Spaces (Name);
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid entity name " & Name.Image);
    end if;
    Util.Skip_Separators (Ctx.Flow);

    -- Is it a public a system or an internal entity
    Internal := False;
    Public := False;
    if not Util.Try (Ctx.Flow, "SYSTEM ") then
      Public := True;
      if not Util.Try (Ctx.Flow, "PUBLIC ") then
        Internal := True;
      end if;
    end if;
    Util.Skip_Separators (Ctx.Flow);

    -- Parse value or URI
    if Internal then
      -- ENTITY [ % ] name "value"
      -- Parse and expand value
      Parse_Value (Ctx, Adtd, Ref_Entity, Value);
    else
      if Public then
        -- ENTITY [ % ] name PUBLIC "PubId" "URI"
        -- Skip PubId
        -- Parse URI or Id
        Char := Util.Get (Ctx.Flow);
        if Char = ''' then
          Util.Parse_Until_Char (Ctx.Flow, "'");
        elsif Char = '"' then
          Util.Parse_Until_Char (Ctx.Flow, """");
        else
          Util.Error (Ctx.Flow, "Unexpected delimiter of PUBLIC Id");
        end if;
        Public_Id := Util.Get_Curr_Str (Ctx.Flow);
        Util.Skip_Separators (Ctx.Flow);
      end if;
      -- PUBLIC or SYSTEM: get URI
      Char := Util.Get (Ctx.Flow);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Unexpected delimiter of PUBLIC Id");
      end if;
      System_Id := Util.Get_Curr_Str (Ctx.Flow);
    end if;
    Util.Skip_Separators (Ctx.Flow);

    -- See if Parsed. If not, will store notation
    Parsed := True;
    if not Parameter and then not Internal then
      if Util.Try (Ctx.Flow, "NDATA ") then
        -- Unparsed entity: the value is the notation
        Parsed := False;
        Util.Skip_Separators (Ctx.Flow);
        Util.Parse_Until_Char (Ctx.Flow, Util.Space & Util.Stop);
        Util.Unget (Ctx.Flow);
        Value := Util.Get_Curr_Str (Ctx.Flow);
        if not Util.Name_Ok (Value) then
          Util.Error (Ctx.Flow, "Invalid name of NDATA");
        end if;
        Util.Skip_Separators (Ctx.Flow);
      else
        -- Parsed external entity, the value is the URI
        Value := System_Id;
      end if;
    elsif Parameter and then not Internal then
      -- Parsed external parameter entity, the value is the URI
      Value := System_Id;
    end if;
    Util.Skip_Separators (Ctx.Flow);

    -- Must stop now
    if Util.Get (Ctx.Flow) /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character at end of entity " & Char);
    end if;

    -- Check that it does not exist. Discard re-definition
    if Entity_Mng.Exists (Adtd.Entity_List, Name, Parameter) then
      Debug ("Dtd discarding re-definition of entity "
           & Parstr.Image  & Name.Image);
      Util.Warning (Ctx, "Entity " & Parstr.Image & Name.Image
           & " already defined");
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
      Value.Set_Null;
    end if;

    -- Store entity
    Entity_Mng.Add (Adtd.Entity_List, Name, Value, Parameter, Internal,
                    Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow, Parsed);
    Debug ("Dtd parsed directive ENTITY -> " &  Parstr.Image & Name.Image
         & " " & Value.Image & " " & Mixed_Str (Internal'Img)
         & " " & Mixed_Str (Parsed'Img) );
  end Parse_Entity;

  -- Parse a <!NOTATION
  procedure Parse_Notation (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Notation name
    Name : As.U.Asu_Us;
    -- Is Notation found
    Found : Boolean;
    -- Is it public
    Public : Boolean;
    -- Public and System Ids
    Public_Id, System_Id : As.U.Asu_Us;
    -- Unparsed entity rec
    Unparsed_Rec : Unparsed_Type;
    Char : Character;
  begin
    -- Parse notation name
    Util.Skip_Separators (Ctx.Flow);
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Name := Util.Get_Curr_Str (Ctx.Flow);
    Util.Expand_Name (Ctx, Adtd, Name, Ref_Dtd_Mark);
    -- Strip separators
    Util.Normalize (Name);
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error (Ctx.Flow, "Invalid notation name " & Name.Image);
    end if;
    Util.Skip_Separators (Ctx.Flow);
    -- See if SYSTEM or PUBLIC
    Public := False;
    Found := Util.Try (Ctx.Flow, "SYSTEM ");
    if not Found then
      Public := True;
      Found := Util.Try (Ctx.Flow, "PUBLIC ");
    end if;
    if not Found then
      Util.Error (Ctx.Flow, "Invalid notation definition");
    end if;
    -- Parse URI or Id
    Char := Util.Get (Ctx.Flow);
    if Char = ''' then
      Util.Parse_Until_Char (Ctx.Flow, "'");
    elsif Char = '"' then
      Util.Parse_Until_Char (Ctx.Flow, """");
    else
      Util.Error (Ctx.Flow, "Unexpected delimiter of notation");
    end if;
    Public_Id := Util.Get_Curr_Str (Ctx.Flow);
    -- Parse URI if PUBLIC not end
    Util.Skip_Separators (Ctx.Flow);
    Found := Util.Try (Ctx.Flow, Util.Stop & "", False);
    if Public and then not Found then
      Char := Util.Get (Ctx.Flow);
      if Char = ''' then
        Util.Parse_Until_Char (Ctx.Flow, "'");
      elsif Char = '"' then
        Util.Parse_Until_Char (Ctx.Flow, """");
      else
        Util.Error (Ctx.Flow, "Invalid notation definition");
      end if;
    end if;
    System_Id := Util.Get_Curr_Str (Ctx.Flow);
    -- Must stop now
    Util.Skip_Separators (Ctx.Flow);
    if Util.Get (Ctx.Flow) /= Util.Stop then
      Util.Error (Ctx.Flow, "Unexpected character at end of notation " & Char);
    end if;

    -- When if is SYSTEM, the first field is the system Id
    if not Public then
      System_Id := Public_Id;
      Public_Id.Set_Null;
    end if;

    -- Store notation
    Unparsed_Rec.Is_Entity := False;
    Unparsed_Rec.Name := Name;
    Unparsed_Rec.Line_No := Util.Get_Line_No (Ctx.Flow);
    Unparsed_Rec.System_Id := System_Id;
    Unparsed_Rec.Public_Id := Public_Id;
    Unparsed_Rec.Notation.Set_Null;
    Ctx.Unparsed_List.Insert (Unparsed_Rec);
    Debug ("Dtd parsed directive NOTATION -> " &  Name.Image);
  end Parse_Notation;

  -- Parse a conditional directive
  procedure Parse_Condition (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Char : Character;
    Word : As.U.Asu_Us;
    Nb_Open : Natural;
    Index : Natural;
  begin
    -- Not in internal Dtd
    if Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow then
      Util.Error (Ctx.Flow, "Conditional directive forbidden in internal dtd");
    end if;
    -- After '[', possible separators, then IGNORE or INCLUDE directive
    -- then possible separators then '['
    Util.Skip_Separators (Ctx.Flow);
    Util.Parse_Until_Char (Ctx.Flow, Util.Space & "[");
    Util.Unget (Ctx.Flow);
    Word := Util.Get_Curr_Str (Ctx.Flow);

    -- Expand dtd entities and check keywork and format
    Util.Expand_Name (Ctx, Adtd, Word, Ref_Dtd_Mark);
    Util.Normalize (Word);
    if Word.Image = "IGNORE" or else Word.Image = "INCLUDE" then
      Util.Skip_Separators (Ctx.Flow);
      Char := Util.Get (Ctx.Flow);
      if Char /= '[' then
        Util.Error (Ctx.Flow, "Unexpected character " & Char & " in condition");
      end if;
    else
      Util.Error (Ctx.Flow, "Unknown conditional directive " & Word.Image);
    end if;

    -- Include or ignore
    if Word.Image = "IGNORE" then
      -- IGNORE directive, skip included "<![...]]>" up to a "]]>"
      Nb_Open := 1;
      loop
        Util.Parse_Until_Str (Ctx.Flow, "]]" & Util.Stop);
        Word := Util.Get_Curr_Str (Ctx.Flow);
        Util.Normalize_Spaces (Word);
        -- Count the number of instances of "<!["
        -- Add to the number of expected "]]>"
        Index := 0;
        loop
          Index := Str_Util.Locate (Word.Image,
            Util.Start & Util.Directive & '[', Index + 1);
          exit when Index = 0;
          Nb_Open := Nb_Open + 1;
        end loop;
        Debug ("Dtd ignored " & Word.Image);
        exit when Nb_Open = 1;
        Nb_Open := Nb_Open - 1;
      end loop;
      return;
    elsif Word.Image = "INCLUDE" then
      -- INCLUDE directive
      -- Go on parsing, knowing that we are in an Include directive
      Adtd.Include_Level := Adtd.Include_Level + 1;
      Debug ("Dtd starting inclusion" & Adtd.Include_Level'Img);
    end if;
  end Parse_Condition;

  -- Parse a directive
  procedure Parse_Directive (Ctx : in out Ctx_Type; Adtd : in out Dtd_Type) is
    Char : Character;
    Word : As.U.Asu_Us;
    Ok : Boolean;
  begin
    -- Xml instruction not allowed any more
    Adtd.Xml_Found := True;
    -- Check for Comment
    Ok := Util.Try (Ctx.Flow, Util.Comment, Consume => False);
    if not Ok then
      -- Check for DOCTYPE
      Ok := Util.Try (Ctx.Flow, Util.Doctype, Consume => False);
    end if;
    if Ok then
      Parse_Directive (Ctx, Adtd, False, Ref_Dtd_Mark, null);
      return;
    end if;
    -- Check for conditional directive
    Char := Util.Get (Ctx.Flow);
    if Char = '[' then
      Parse_Condition (Ctx, Adtd);
      return;
    else
      Util.Unget (Ctx.Flow);
    end if;
    -- Now, expect KEYWORD and a space
    Util.Parse_Until_Char (Ctx.Flow, "" & Util.Space);
    Word := Util.Get_Curr_Str (Ctx.Flow);
    declare
      Str : constant String := Word.Image;
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
                          Text : in out As.U.Asu_Us);

  -- Parse current dtd
  -- If external, will stop at end of file
  -- otherwise, will stop on ']'
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   External : in Boolean) is
    Found : Boolean;
    Entity_Value : As.U.Asu_Us;
    Char : Character;
    Is_Recorded : Boolean;
    use type As.U.Asu_Us;
  begin
    if External then
      -- Autodetect encoding and check
      Util.Guess_Encoding (Ctx.Flow);
      Debug ("Detected dtd encoding format "
           & Ctx.Flow.Curr_Flow.Encod'Img);
    end if;

    loop
      Util.Skip_Separators (Ctx.Flow);
      -- Try instruction
      Found := Util.Try (Ctx.Flow, Util.Start & Util.Instruction);
      if Found then
        Parse_Instruction (Ctx, Adtd, External);
      end if;
      if not Found then
        -- Try directive
        Found := Util.Try (Ctx.Flow, Util.Start & Util.Directive);
        if Found then
          Parse_Directive (Ctx, Adtd);
        end if;
      end if;
      if not Found then
        -- Try parameter entity '%'
        Found := Util.Try (Ctx.Flow, Util.Ent_Param & "");
        if Found then
          -- Get entity reference
          Util.Unget (Ctx.Flow);
          Util.Parse_Until_Char (Ctx.Flow, Util.Ent_End & "");
          Entity_Value := Util.Get_Curr_Str (Ctx.Flow);
          Entity_Value := Entity_Value & Util.Ent_End;
          -- Expand
          Debug ("Dtd expanding parameter entity " & Entity_Value.Image);
          Util.Expand_Name (Ctx, Adtd, Entity_Value, Ref_Dtd);
          -- Suspend recording for the entity replacement
          Is_Recorded := Ctx.Flow.Recording;
          Ctx.Flow.Recording := False;
          -- Parse
          Switch_Input (Ctx, Adtd, Entity_Value);
          Ctx.Flow.Recording := Is_Recorded;
        end if;
      end if;
      if not Found and then Adtd.Include_Level /= 0 then
        -- Detect end of include
        Found := Util.Try (Ctx.Flow, "]]" & Util.Stop);
        if Found then
          Debug ("Dtd ending inclusion" & Adtd.Include_Level'Img);
          Adtd.Include_Level := Adtd.Include_Level - 1;
        end if;
      end if;
      if not Found then
        -- Should be the end: End_Error if external, ']' if internal
        begin
          Char := Util.Get (Ctx.Flow);
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
                          Text : in out As.U.Asu_Us) is
  begin
    -- Save current flow
    Util.Push_Flow (Ctx.Flow);
    -- Prepare new string flow, keep file name
    Ctx.Flow.Curr_Flow.Is_File := False;
    Ctx.Flow.Curr_Flow.File := null;
    Ctx.Flow.Curr_Flow.In_Str := Text;
    Ctx.Flow.Curr_Flow.In_Stri := 0;
    Ctx.Flow.Curr_Flow.Same_Line := True;
    -- Parse new flow as dtd
    Debug ("Switching input to " & Text.Image);
    Parse (Ctx, Adtd, External => True);
    -- Restore flow
    Util.Pop_Flow (Ctx.Flow);
  end Switch_Input;

  procedure Check_Warnings (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type);

  -- Parse a dtd (either a external file or internal if name is empty)
  procedure Parse (Ctx : in out Ctx_Type;
                   Adtd : in out Dtd_Type;
                   File_Name : in As.U.Asu_Us;
                   Name_Raise_Parse : in Boolean := True) is
    Close_File : Boolean := False;
    use type As.U.Asu_Us;
  begin
    if File_Name = String_Flow then
      -- String of Ctx
      Debug ("Dtd parsing string");
      Ctx.Flow.Curr_Flow.Is_File := False;
      Ctx.Flow.Curr_Flow.File := null;
      Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
      Parse (Ctx, Adtd, True);
    elsif File_Name = Internal_Flow then
      -- Internal declarations (string or file) of Ctx
      Debug ("Dtd parsing internal definition");
      Ctx.Flow.Curr_Flow.Kind := Int_Dtd_Flow;
      Parse (Ctx, Adtd, False);
      Ctx.Flow.Curr_Flow.Kind := Xml_Flow;
    else
      -- File name
      Debug ("Dtd parsing file " & File_Name.Image);
      Ctx.Flow.Curr_Flow.File := new Text_Char.File_Type;
      Ctx.Flow.Files.Push (Ctx.Flow.Curr_Flow.File);
      File_Mng.Open (File_Name.Image, Ctx.Flow.Curr_Flow.File.all);
      Close_File := True;
      Ctx.Flow.Curr_Flow.Is_File := True;
      Ctx.Flow.Curr_Flow.Kind := Dtd_Flow;
      Ctx.Flow.Curr_Flow.Name := File_Name;
      Ctx.Flow.Curr_Flow.Line := 1;
      Ctx.Flow.Curr_Flow.Same_Line := False;
      Parse (Ctx, Adtd, True);
    end if;
    -- Preserved is empty or "#Name#Name...#Name#"
    if not Ctx.Preserved.Is_Null then
      Ctx.Preserved.Append (Info_Sep);
    end if;
    -- Dtd is now valid
    Debug ("Dtd parsed dtd");
    Adtd.Set := True;
    if Ctx.Warnings /= null then
      Debug ("Dtd checking warnings");
      Check_Warnings (Ctx, Adtd);
      Debug ("Dtd checked warnings");
    end if;
    if Close_File then
      File_Mng.Close (Ctx.Flow.Curr_Flow.File.all);
    end if;
  exception
    when File_Error =>
      if Close_File then
        File_Mng.Close (Ctx.Flow.Curr_Flow.File.all);
      end if;
      -- Can only be raised if not internal nor string flow
      if Name_Raise_Parse then
        Util.Error (Ctx.Flow, "Cannot open dtd file " & File_Name.Image);
      else
        raise;
      end if;
    when Entity_Mng.Entity_Forbidden =>
      if Close_File then
        File_Mng.Close (Ctx.Flow.Curr_Flow.File.all);
      end if;
      Util.Error (Ctx.Flow, "Forbidden entity reference in dtd");
  end Parse;

  -- Perform final checks after DTD parsing: unparsed entities v.s. notations
  procedure Final_Dtd_Check (Ctx  : in out Ctx_Type; Adtd : in out Dtd_Type) is
    -- Iterators
    Iter, Iter1, Iter2 : Parser.Iterator;
    -- Element/att info
    Info : Info_Rec;
    -- List of unparsed entities, line of def and notation
    Entities, Notations, Lines : As.U.Asu_Us;
    -- Unparsed entity or Notation
    Unparsed_Rec : Unparsed_Type;
    Ok : Boolean;
    use type As.U.Asu_Us;
  begin
    -- All unparsed entities have a notation associated
    Debug ("Dtd final: All unparsed entities have a notation");
    if not Ctx.Unparsed_List.Is_Empty then
      -- Make a string list of notations used by unparsed entities
      Ctx.Unparsed_List.Rewind;
      loop
        Ctx.Unparsed_List.Read_Next (Unparsed_Rec, Moved => Ok);
        Entities.Append (Unparsed_Rec.Name & Info_Sep);
        Lines.Append (Line_Image(Unparsed_Rec.Line_No) & Info_Sep);
        Notations.Append (Unparsed_Rec.Notation & Info_Sep);
        exit when not Ok;
      end loop;
      -- Locate notation
      Iter.Set (Entities.Image, Is_Sep'Access);
      Iter1.Set (Lines.Image, Is_Sep'Access);
      Iter2.Set (Notations.Image, Is_Sep'Access);
      loop
        declare
          -- Next entity and its notation
          Entity : constant String := Iter.Next_Word;
          Line : constant String := Iter1.Next_Word;
          Notation : constant String := Iter2.Next_Word;
        begin
          exit when Notation = "";
          Debug ("Checking notation " & Notation
               & " for unparsed entity " & Entity);
          -- Name must appear in notations
          Unparsed_Rec.Is_Entity := False;
          Unparsed_Rec.Name := As.U.Tus (Notation);
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
    Debug ("Dtd final: All notation attlist values have a notation");
    Debug ("       and elements with notation attlist are not empty");
    -- For all Elt#Att of Notation_Attrs
    Iter.Set (Adtd.Notation_Attrs.Image, Is_Sep'Access);
    loop
      declare
        -- Next element and notation attribute
        Elt : constant String := Iter.Next_Word;
        Att : constant String := Iter.Next_Word;
      begin
        exit when Elt = "";

        -- Read info with the list of enum values
       Debug ("Checking notation attribute " & Att & " of element " & Elt);
        Info.Name := As.U.Tus ("Att" & Info_Sep & Elt & Info_Sep & Att);
        Adtd.Info_List.Read (Info);
        Iter1.Set (Info.List.Image, Is_Sep'Access);
        declare
          Val : constant String := Iter1.Next_Word;
        begin
          exit when Val = "";
          Debug ("  Checking definition of notation " & Val
               & " of attribute " & Att
               & " of element " & Elt);
          -- Each value must be defined by a notation
          Unparsed_Rec.Is_Entity := False;
          Unparsed_Rec.Name := As.U.Tus (Val);
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
        Info.Name := As.U.Tus ("Elt" & Info_Sep & Elt);
        Adtd.Info_List.Read (Info);
        if Info.List.Element (1) = 'E' then
          Util.Error (Ctx.Flow,
            "Element " & Elt & " defined at line " & Line_Image (Info.Line)
            & " in dtd is EMPTY and has an attribute type notation");
        end if;
      end;
    end loop;
    Iter.Del;

  end Final_Dtd_Check;

  -- Check for warnings
  type Elt_Ref_Type is record
    Father, Child : As.U.Asu_Us;
    Line : Natural;
  end record;
  package Us_Pool_Manager is new Unlimited_Pool (Elt_Ref_Type);
  package Us_Pool_Mng renames Us_Pool_Manager.Upool;
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
    use type As.U.Asu_Us;
  begin
    -- Check that all elements referenced as children or in attlist are defined
    if Adtd.Info_List.Is_Empty then
      return;
    end if;

    -- Push in pool a list of all referenced elements
    -- Read all Infos
    Adtd.Info_List.Rewind;
    loop
      Adtd.Info_List.Read_Next (Info, Moved => Moved);
      Info_Kind := Info.Name.Slice (1, 3);
      if Info_Kind = "Elt" then
        -- Elt directive, need to parse children if Mixed of Children
        Child_Kind := Info.List.Element (1);
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
        Elt_Ref.Father := As.U.Tus (Info.Name.Slice (5, Info.Name.Length));
        Elt_Ref.Line := Info.Line;
        Elt_Ref.Child.Set_Null;
        -- Parse children names from list (skip first Char)
        In_Name := False;
        for I in 2 .. Info.List.Length loop
          C := Info.List.Element (I);
          -- Names are delimited by Info_Sep or any valid character in the
          -- definition of children
          if C = Info_Sep
          or else Str_Util.Locate (Info_Sep & "?*+()|,", "" & C) /= 0 then
            if In_Name then
              -- End of name, completed
              -- Push this child
              Pool.Push (Elt_Ref);
              Elt_Ref.Child.Set_Null;
            end if;
            In_Name := False;
          else
            -- In name
            Elt_Ref.Child.Append (C);
            In_Name := True;
          end if;
        end loop;

      elsif Child_Kind = 'A' then
        -- This is a attlist, push an etry with empty Father
        Elt_Ref.Father.Set_Null;
        Elt_Ref.Line := Info.Line;
        Elt_Ref.Child := As.U.Tus (Info.Name.Slice (5, Info.Name.Length));
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
        if not Elt_Ref.Father.Is_Null then
          Util.Warning (Ctx,
            "Element " & Elt_Ref.Father.Image & " references unknown child "
                       &  Elt_Ref.Child.Image,
            Elt_Ref.Line);
        else
          Util.Warning (Ctx,
            "Undefined element " & Elt_Ref.Child.Image & " used in ATTLIST",
            Elt_Ref.Line);
        end if;
      end if;
    end loop;
  end Check_Warnings;

  -- Replace "##" by "," then suppress "#"
  function Strip_Sep (Us : in As.U.Asu_Us) return String is
    use Str_Util;
  begin
    return Substit (Substit (Us.Image, Info_Sep & Info_Sep, ","),
                    "" & Info_Sep, "");
  end Strip_Sep;

  -- Check children of element
  procedure Check_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Name : in As.U.Asu_Us;
                            Line_No : in Natural;
                            Put_Empty : in Boolean;
                            Children : in Children_Desc) is
    -- Element info
    Info : Info_Rec;
    -- Char of info List
    Char : Character;
    -- Parser iterator
    Iter_Xml : Parser.Iterator;
    -- Childs of current element
    Childstr : As.U.Asu_Us := Children.Children;
    -- Is Dtd defintion EMPTY
    Dtd_Empty : Boolean;
    -- "only " or "" depending to Dtd_Empty
    Only : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Debug ("Dtd check Xml children list " & Children.Children.Image
         & " Empty: " & Children.Is_Empty'Img
         & " Text : " & Children.Has_Text'Img);
    -- Remove tail from root (Info_Sep & Info_Sep) if any
    if not Ctx.Elements.Has_Father
    and then Childstr.Length >= 2
    and then Childstr.Slice (Childstr.Length - 1, Childstr.Length)
                            = Info_Sep & Info_Sep
    then
      Childstr.Delete (Childstr.Length - 1, Childstr.Length);
    end if;
    -- Read its element def
    Info.Name := "Elt" & Info_Sep & Name;
    begin
      Adtd.Info_List.Read (Info);
    exception
      when Info_Mng.Not_In_List =>
        -- Should have been detected by Check_Attributes
        Debug ("Dtd check children. Element name " & Name.Image
              & " does not exist");
        raise Internal_Error;
    end;
    -- Check children
    Debug ("Dtd check Dtd element " & Name.Image
         & " with infolist " & Info.List.Image);
    -- Separate element type
    Char := Info.List.Element (1);
    Info.List.Delete (1, 1);
    Dtd_Empty := False;
    -- When Xml_Empty and not Dtd_Empty
    Only := As.U.Tus ("only ");
    case Char is
      when 'E' =>
        Dtd_Empty := True;
        -- When Dtd_Empty and not Xml_Empty
        Only.Set_Null;
        -- Must be empty
        if not Children.Is_Empty then
          Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
                    & " must be empty",
                      Line_No);
        end if;
      when 'A' =>
        -- Any
        null;
      when 'M' =>
        -- Check mixed: all children of xml must appear in dtd list
        Iter_Xml.Set (Childstr.Image, Is_Sep'Access);
        loop
          declare
            -- Next Child from xml
            Child : constant String := Iter_Xml.Next_Word;
          begin
            exit when Child = "";
            -- Child must appear in dtd
            if Str_Util.Locate (Info.List.Image,
                                  Info_Sep & Child & Info_Sep) = 0 then
              Util.Error (Ctx.Flow, "According to dtd, element "
                        & Name.Image
                        & " does not allow child " & Child,
                          Line_No);
            end if;
            Debug ("Dtd checked mixed child " & Child
                 & " versus " & Info.List.Image);
          end;
        end loop;
        Iter_Xml.Del;
      when 'C' =>
        if Children.Has_Text then
          Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
                    & " must not have text",
                      Line_No);
        end if;
        -- Strictly check that list matches criteria
        -- Regexp is lazy, the ^ and $ make it know that we want the full match
        if not Regular_Expressions.Match ("^(" & Info.List.Image & ")$",
                Childstr.Image, Strict => True) then
          Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
                    & " allows children " & Strip_Sep (Info.List)
                    & " but has children " & Strip_Sep (Childstr));
        end if;
        Debug ("Dtd checked children " & Strip_Sep (Childstr)
             & " versus " & Strip_Sep (Info.List));
      when others =>
        Debug ("Dtd check: Unexpected element type " & Char);
        raise Internal_Error;
    end case;

    if Put_Empty /= Dtd_Empty then
      Util.Warning (Ctx,
        "Empty-Element tag shall " & Only.Image
      & "be used for EMPTY elements");
    end if;
  exception
    when Regular_Expressions.No_Criteria =>
      -- Normally it was checks at parsing
      Debug ("Dtd regex does not compile for check " & Info.List.Image);
      raise Internal_Error;
  end Check_Children;

  -- INTERNAL: set Unparsed tag on attribute Attr
  procedure Set_Unparsed (Ctx : in out Ctx_Type;
                          Attr : in As.U.Asu_Us) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    use type As.U.Asu_Us;
  begin
    Debug ("Dtd setting unparsed on attribute " & Attr.Image);
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
                              Name       : in As.U.Asu_Us;
                              Line_No    : in Natural;
                              Attributes : in As.U.Asu_Us) is
    -- Atl, Att and Id info blocs
    Info, Attinfo : Info_Rec;
    -- Name looked in Info list (for error tracing)
    Error_Name : As.U.Asu_Us;
    -- Is info found in info list
    Info_Found : Boolean;
    -- Parser iterators
    Iter_Dtd, Iter_Xml : Parser.Iterator;
    -- Is an attribute set in xml
    Att_Set : Boolean;
    -- Attribute value in Xml
    Xml_Val : As.U.Asu_Us;
    -- List of dtd attribute names
    Att_Names : As.U.Asu_Us;
    -- Cell of ID (or IDREF), and if Found
    Idcell : Id_Cell;
    Found :  Boolean;
    -- Iterator of entities
    Iter : Parser.Iterator;
    -- Descriptor of unparsed entity
    Unparsed_Rec : Unparsed_Type;
    use type As.U.Asu_Us;
  begin
    Debug ("Dtd check Xml attributes list " & Attributes.Image);
    -- Read element def
    Info.Name := "Elt" & Info_Sep & Name;
    Adtd.Info_List.Search (Info, Info_Found);
    if not Info_Found then
      Util.Error (Ctx.Flow, "Element " &  Name.Image
                 & " is not defined in dtd");
    end if;
    -- Read its ATTLIST def
    Info.Name := "Atl" & Info_Sep & Name;
    Adtd.Info_List.Search (Info, Info_Found);
    if Info_Found then
      Adtd.Info_List.Read (Info);
    end if;
    if not Info_Found or else Info.List.Is_Null then
      -- No or empty ATTLIST for this element
      if Attributes.Is_Null then
        Debug ("Dtd checked element " & Name.Image
             & " with no attributes, versus no or empty attlist");
        return;
      else
        -- Attributes must have be declared
        Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
                  & " is not allowed to have attributes",
                  Line_No);
      end if;
    end if;
    -- Check attributes
    Debug ("Dtd check Dtd attlist with infolist " & Info.List.Image);
    -- Check attributes xml vs dtd
    -- First extract list of dtd attribute names
    Iter_Dtd.Set (Info.List.Image, Is_Sep'Access);
    loop
      declare
        -- Next attribute from dtd
        Attr : constant String := Iter_Dtd.Next_Word;
      begin
        exit when Attr = "";
        Att_Names.Append (Info_Sep & Attr & Info_Sep);
      end;
      -- Skip type and default spec
      Iter_Dtd.Next_Word;
    end loop;
    Iter_Dtd.Del;
    -- Now check that any attribute of xml is in the list of dtd
    Iter_Xml.Set (Attributes.Image, Is_Sep'Access);
    loop
      declare
        -- Next attribute from xml
        Attr : constant String := Iter_Xml.Next_Word;
      begin
        exit when Attr = "";
        -- Attribute must appear in list of attributes from dtd
        if Str_Util.Locate (Att_Names.Image,
                              Info_Sep & Attr & Info_Sep) = 0 then
          Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
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
    Iter_Dtd.Set (Info.List.Image, Is_Sep'Access);
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
          Adtd.Info_List.Read (Attinfo);
        end if;
        -- Does this attribute appear in xml
        Att_Set := Str_Util.Locate (Attributes.Image,
                   Info_Sep & Attr & Info_Sep) /= 0;
        if Att_Set then
          -- Get the Xml Attribute
          Xml_Val := Tree_Mng.Get_Attribute (Ctx.Elements.all, As.U.Tus(Attr));
        end if;

        --  Any Required in dtd must appear in xml
        if Td(2) = 'R' and then not Att_Set then
          Util.Error (Ctx.Flow, "According to dtd, element " & Name.Image
                    & " must have attribute " & Attr, Line_No);
        end if;

        -- Enum and Fixed must have correct value
        if Td(2) = 'F' and then Att_Set then
          -- Fixed (Enum or string): first #<val># is the one required
          declare
            -- Get the first value from dtd list, from 2 to second sep
            Sep : constant Positive
                := Str_Util.Locate (Attinfo.List.Image,
                                      Info_Sep & "", 2);
            Dtd_Val : constant String := Attinfo.List.Slice (2, Sep - 1 );
          begin
            if Xml_Val.Image /= Dtd_Val then
              Util.Error (Ctx.Flow, "According to dtd, attribute " & Attr
                        & " must have fixed value " & Dtd_Val, Line_No);
            end if;
          end;
        elsif (Td(1) = 'E' or else Td (1) = 'N') and then Att_Set then
          -- Not fixed Enum or notation in dtd with value set in xml:
          -- #<val># must be in dtd list
          if Str_Util.Locate (Attinfo.List.Image,
                 Info_Sep  & Xml_Val.Image & Info_Sep) = 0 then
            Util.Error (Ctx.Flow, "According to dtd, Enum attribute " & Attr
                      & " has incorrect value "
                      & Xml_Val.Image, Line_No);
          end if;
        elsif (Td(2) = 'D' or else Td(2) = 'F') and then not Att_Set then
          -- Default in dtd with no xml value: insert default in tree
          --  and set Xml_Val
          if Ctx.Standalone then
            -- This default must be defined in internal Dtd
            if Str_Util.Locate (Adtd.Internals.Image, Info_Sep
                     & Name.Image & Info_Sep & Info_Sep & Attr) = 0 then
              Util.Error (Ctx.Flow, "Attribute " & Attr
                     & " needs default value in standalone document");
            end if;
          end if;
          -- Default / Fixed is first string after Info_Sep
          declare
            -- Get the first value from dtd list, from 2 to second sep
            Sep : constant Positive
                := Str_Util.Locate (Attinfo.List.Image,
                                      Info_Sep & "", 2);
            Dtd_Val : constant String
                    := Attinfo.List.Slice (2, Sep - 1 );
            Attr_Us : constant As.U.Asu_Us := As.U.Tus (Attr);
            Namespace : As.U.Asu_Us;
            pragma Unreferenced (Namespace);
          begin
            if Ctx.Expand then
              if Ctx.Namespace then
                -- Just to check existance of the Namespace
                Namespace := Namespaces.Get (Ctx, Attr_Us, False);
              end if;
              Debug ("Dtd adding attribute " & Attr_Us.Image & " type " & Td
                    & " val " & Dtd_Val);
              Tree_Mng.Add_Attribute (Ctx.Elements.all,
                  Attr_Us, As.U.Tus (Dtd_Val), Line_No);
            end if;
            Xml_Val := As.U.Tus (Dtd_Val);
            if Attr = Tree_Mng.Xml_Space
            and then Dtd_Val = Tree_Mng.Preserve then
              Tree_Mng.Add_Tuning (Ctx.Elements.all,
                                   Tree_Mng.Xml_Space_Preserve);
              Debug (" Check, added tuning " & Tree_Mng.Xml_Space_Preserve);
            end if;
          end;
        elsif Att_Set then
          -- Comformance checks on ID, IDREF(s) and NMTOKEN(s)
          -- Store ID and IDREFs and Sanity checks on I
          if (Td(1) = 'I' or else Td(1) = 'R' or else Td(1) = 'Y')
          and then not Util.Name_Ok (Xml_Val) then
            Util.Error (Ctx.Flow, "Invalid name for ID, IDREF or ENTITY "
                      & Xml_Val.Image, Line_No);
          elsif (Td(1) = 'r' or else Td(1) = 'y')
          and then  not Util.Names_Ok (Xml_Val, Util.Space & "") then
            Util.Error (Ctx.Flow, "Invalid name in IDREFS or ENTITIES "
                      & Xml_Val.Image, Line_No);
          elsif Td(1) = 'T'
          and then not Util.Name_Ok (Xml_Val, Allow_Token => True) then
            Util.Error (Ctx.Flow, "Invalid token for NMTOKEN "
                      & Xml_Val.Image, Line_No);
          elsif Td(1) = 't'
          and then not Util.Names_Ok (Xml_Val,
                                      Util.Space & "",
                                      Allow_Token => True) then
            Util.Error (Ctx.Flow, "Invalid name in NMTOKENS "
                      & Xml_Val.Image, Line_No);
          end if;

          -- Store IDs and IDREFs
          if Td(1) = 'I' then
            -- Check this ID is not already set and add it (with its line_no)
            Idcell.Name := Xml_Val;
            Idcell.Line_No := Line_No;
            Ctx.Ids.Search (Idcell, Found);
            if Found then
              Ctx.Ids.Read (Idcell);
              Util.Error (Ctx.Flow,
                 "ID " & Xml_Val.Image
                       & " already defined at line "
                       & Line_Image (Idcell.Line_No),
                 Line_No);
            end if;
            Ctx.Ids.Insert (Idcell);
            Debug (" Check, added ID " & Xml_Val.Image);
          elsif Td(1) = 'R' then
            -- Store this IDREF and line_no to list of IDREFs
            Idcell.Name := Xml_Val;
            Idcell.Line_No := Line_No;
            Ctx.Idrefs.Insert (Idcell);
            Debug (" Check, added IDREF " & Xml_Val.Image);
          elsif Td(1) = 'r' then
            Idcell.Line_No := Line_No;
            -- Store these IDREFs and line_no to list of IDREFs
            Util.Normalize_Spaces (Xml_Val);
            -- Split IDREFS and insert each IDREF
            Iter_Xml.Set (Xml_Val.Image,
                        Parser.Is_Space_Or_Htab_Function'Access);
            Debug (" Check, adding IDREFs " & Xml_Val.Image);
            loop
              Idcell.Name := As.U.Tus (Iter_Xml.Next_Word);
              exit when Idcell.Name.Is_Null;
              Ctx.Idrefs.Insert (Idcell);
              Debug (" Check, added IDREF " & Idcell.Name.Image);
            end loop;
            Iter_Xml.Del;
          end if;
        end if;

        -- Check that ENTITY or ENTITIES are unparsed entities
        if Td(1) = 'Y' or else Td(1) = 'y' then
          -- This attribute is ENTITY or ENTITIES => notify
          Set_Unparsed (Ctx, As.U.Tus (Attr));
          Iter.Set (Xml_Val.Image);
          loop
            declare
              Entity : constant String := Iter.Next_Word;
            begin
              exit when Entity = "";
              Debug (" Check, unparsed entity " & Entity);
              Unparsed_Rec.Is_Entity := True;
              Unparsed_Rec.Name := As.U.Tus (Entity);
              Ctx.Unparsed_List.Search (Unparsed_Rec, Found);
              if not Found then
                Util.Error (Ctx.Flow, "Unknown unparsed entity " & Entity);
              end if;
            end;
          end loop;
          Iter.Del;
        end if;

        if Ctx.Namespace then
          -- Attr can be CDATA, Enumeration or NMTOKEN(s) with a ':'
          --  otherwise no ':'
          Namespaces.Validate (Ctx, As.U.Tus (Attr),
              (if Td(1) = 'S' or else Td(1) = 'E'
               or else Upper_Char (Td(1)) = 'T' then Namespaces.Attr
               else Namespaces.Other));
        end if;
        Debug ("Dtd checked versus dtd attribute " & Attr & " type " & Td);
      end;
    end loop;
    Iter_Dtd.Del;
  exception
    when Info_Mng.Not_In_List =>
      Debug ("Dtd check: Cannot find info " & Error_Name.Image);
      Iter_Dtd.Del;
      raise Internal_Error;
  end Check_Attributes;

  -- Check attributes of current element of the tree
  procedure Check_Attributes (Ctx : in out Ctx_Type;
                              Adtd : in out Dtd_Type) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    -- Lists of attributes
    Attributes : As.U.Asu_Us;
    -- Current namespace and expanded attribute name
    Namespace, Expanded : As.U.Asu_Us;
    -- List of expanded attributes
    Expanded_List : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Ctx.Elements.Read (Cell);
    Debug ("Dtd checking attributes of element " & Cell.Name.Image);
    if Cell.Name.Is_Null then
      Util.Error (Ctx.Flow, "Empty element name");
    end if;

    -- Make its attribute list
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        if Cell.Kind /= Xml_Parser.Attribute then
          -- Children
          exit;
        end if;
        if Cell.Name.Is_Null then
          Util.Error (Ctx.Flow, "Empty attribute name");
        end if;
        Attributes.Append (Info_Sep & Cell.Name & Info_Sep);
        if Ctx.Namespace then
          if not Adtd.Set then
            -- No Dtd so name will not be checked (in Check_Attributes)
            --  considering its type. Check name considering it is CDATA
            Namespaces.Validate (Ctx, Cell.Name, Namespaces.Attr);
          end if;
          -- Define namespace
          Namespaces.Add (Ctx, Cell.Name, Cell.Value);
        end if;
      end loop;
      Ctx.Elements.Move_Father;
    end if;

    if Adtd.Set then
      -- Check Attributes
      Ctx.Elements.Read (Cell);
      Check_Attributes (Ctx, Adtd, Cell.Name, Cell.Line_No, Attributes);
    end if;

    if not Ctx.Namespace then
      return;
    end if;

    -- Define namespaces
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        if Cell.Kind /= Xml_Parser.Attribute then
          -- Children
          exit;
        end if;
        if Ctx.Namespace then
          if not Adtd.Set then
            -- No Dtd so name will not be checked (in Check_Attributes)
            --  considering its type. Check name considering it is CDATA
            Namespaces.Validate (Ctx, Cell.Name, Namespaces.Attr);
          end if;
          -- Define namespace
          Namespaces.Add (Ctx, Cell.Name, Cell.Value);
        end if;
      end loop;
      Ctx.Elements.Move_Father;
    end if;

    -- Check uniqueness of expanded attributes
    --  and set namespace of attributes
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        if Cell.Kind /= Xml_Parser.Attribute then
          -- Children
          exit;
        end if;
        -- Get namespace for this attribute
        Namespace := Namespaces.Get (Ctx, Cell.Name, False);
        -- Check uniqueness of expanded name
        Expanded := Expand_Name (Cell.Name, Namespace);
        if Str_Util.Locate (
                Expanded_List.Image,
                Info_Sep & Expanded.Image & Info_Sep) /= 0 then
          Util.Error (Ctx.Flow, "Duplicated expanded attribute name "
                               & Expanded.Image);
        end if;
        if Expanded.Is_Null then
          Expanded.Set (Info_Sep);
        end if;
        Expanded.Append (Expanded & Info_Sep);
        -- Set this attribute namespace
        Cell.Namespace := Namespace;
        Ctx.Elements.Replace (Cell);
      end loop;
      Ctx.Elements.Move_Father;
    end if;

  end Check_Attributes;

  -- Is this element defined as Mixed
  function Is_Mixed (Adtd : in out Dtd_Type;
                     Elt  : in As.U.Asu_Us) return Boolean is
    Info : Info_Rec;
    Info_Found : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Default: No (not mixed)
    if not Adtd.Set then
      -- No dtd => not mixed
      return False;
    end if;
    -- Read ELEMENT def of Elt
    Info.Name := As.U.Tus ("Elt" & Info_Sep) & Elt;
    Adtd.Info_List.Search (Info, Info_Found);
    if Info_Found then
      Adtd.Info_List.Read (Info);
    else
      -- Not found => Elt not defined
      return False;
    end if;
    -- Element is mixed?
    return Info.List.Element (1) = 'M';
  end Is_Mixed;

  -- Is this element defined in internal dtd or else has not Content def
  function Can_Have_Spaces (Adtd : in out Dtd_Type;
                             Elt  : in As.U.Asu_Us) return Boolean is
    Info : Info_Rec;
    Info_Found : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Default: Yes (not Content or else internal)
    if not Adtd.Set then
      -- No dtd => not Content
      return True;
    end if;
    -- Read ELEMENT def of Elt
    Info.Name := As.U.Tus ("Elt" & Info_Sep) & Elt;
    Adtd.Info_List.Search (Info, Info_Found);
    if Info_Found then
      Adtd.Info_List.Read (Info);
    else
      -- Not found => Elt not defined
      return True;
    end if;
    -- Element can have spaces if not Content
    if Info.List.Element (1) /= 'C' then
      return True;
    end if;
    -- The element is defined in internal DTD if there is #@Elt# in Internals
    return Str_Util.Locate (Adtd.Internals.Image,
                            Info_Sep & "@" & Elt.Image & Info_Sep) /= 0;
  end Can_Have_Spaces;

  -- Is this attribute of this element CDATA
  function Is_Cdata (Adtd      : in out Dtd_Type;
                     Elt, Attr : in As.U.Asu_Us) return Boolean is
    Info : Info_Rec;
    Info_Found : Boolean;
    use type As.U.Asu_Us;
  begin
    -- Default: Yes, it is CDATA
    if not Adtd.Set then
      -- No dtd => CDATA
      return True;
    end if;
    -- Read ATTLIST def of Elt
    Info.Name := As.U.Tus ("Atl" & Info_Sep) & Elt;
    Adtd.Info_List.Search (Info, Info_Found);
    if Info_Found then
      Adtd.Info_List.Read (Info);
    end if;
    if not Info_Found or else Info.List.Is_Null then
      return True;
    end if;
    -- Locate attribute name in List
    declare
      Str : constant String := Info.List.Image;
      Index : Natural;
    begin
      Index := Str_Util.Locate (Str, Info_Sep & Attr.Image
                                     & Info_Sep & Info_Sep);
      if Index = 0 then
        -- Not found
        return True;
      end if;
      -- Skip # and attribute name and ##
      Index := Index + Attr.Length + 3;
      -- 'S' for CDATA
      return Info.List.Element (Index) = 'S';
    end;
  end Is_Cdata;

  -- Has this element the xml:spaces=preserve
  function Has_Preserve (Ctx : Ctx_Type; Elt  : As.U.Asu_Us) return Boolean is
  begin
    return Str_Util.Locate (Ctx.Preserved.Image,
                              Info_Sep & Elt.Image & Info_Sep) /= 0;
  end Has_Preserve;

  -- Add current element to list of children
  procedure Add_Current_Element (List : in out As.U.Asu_Us;
                                 Name : in As.U.Asu_Us) is
    use type As.U.Asu_Us;
  begin
    List.Append (Info_Sep & Name & Info_Sep);
  end Add_Current_Element;

  -- Check that list matches Dtd definition of current element
  procedure Check_Element (Ctx      : in out Ctx_Type;
                           Adtd     : in out Dtd_Type;
                           Children : in Children_Desc) is
    Cell : My_Tree_Cell;
  begin
    if Adtd.Set then
      Ctx.Elements.Read (Cell);
      Check_Children (Ctx, Adtd, Cell.Name, Cell.Line_No, Cell.Put_Empty,
                      Children);
    end if;
    -- Clean namespaces of attributes
    if not Ctx.Namespace then
      return;
    end if;
    if Ctx.Elements.Children_Number /= 0 then
      for I in 1 .. Ctx.Elements.Children_Number loop
        if I = 1 then
          Ctx.Elements.Move_Child;
        else
          Ctx.Elements.Move_Brother (False);
        end if;
        Ctx.Elements.Read (Cell);
        if Cell.Kind /= Xml_Parser.Attribute then
          -- Children
          exit;
        end if;
        -- Delete namespace if this attribute is defining one
        Namespaces.Del (Ctx, Cell.Name);
      end loop;
      Ctx.Elements.Move_Father;
    end if;
  end Check_Element;

  -- INTERNAL
  -- Build from tree the descriptor of children
  procedure Build_Children (Ctx  : in out Ctx_Type;
                            Adtd : in out Dtd_Type;
                            Children : out Children_Desc) is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    Can_Have_Text : Boolean;
  begin
    if not Adtd.Set then
      -- No dtd => no check
      return;
    end if;
    -- Set Is_Mixed from Dtd in Children
    Ctx.Elements.Read (Cell);
    Children.Is_Mixed := Is_Mixed (Adtd, Cell.Name);
    Can_Have_Text := Children.Is_Mixed;
    -- Read current element from tree and make its children lists
    Children.Is_Empty := True;
    Children.Has_Text := False;
    Children.Children.Set_Null;
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
            if Add_Text (Ctx, Cell.Name) then
              Children.Is_Empty := False;
              Children.Has_Text := True;
            else
              -- Force the Is_Mixed in tree
              Can_Have_Text := True;
            end if;
          when Xml_Parser.Pi | Xml_Parser.Comment =>
            Children.Is_Empty := False;
        end case;
      end loop;
      Ctx.Elements.Move_Father;
      Tree_Mng.Set_Is_Mixed (Ctx.Elements.all, Can_Have_Text);
    end if;
  end Build_Children;

  -- Check tail: only comments and PIs
  procedure Check_Tail (Ctx : in out Ctx_Type) is
    Cell : My_Tree_Cell;
  begin
    Ctx.Tail.Move_Root;
    Ctx.Tail.Read (Cell);
    -- Check Tail dummy node
    if Cell.Kind /= Element then
      Util.Error (Ctx.Flow, "Invalid node type for tail "
                & Mixed_Str (Cell.Kind'Img));
    end if;
    if Cell.Nb_Attributes /= 0 then
      Util.Error (Ctx.Flow, "Tail has attributes");
    end if;
    if not Cell.Name.Is_Null then
      Util.Error (Ctx.Flow, "Tail has a name");
    end if;
    if not Cell.Value.Is_Null then
      Util.Error (Ctx.Flow, "Tail has a value");
    end if;
    if Cell.Put_Empty then
      Util.Error (Ctx.Flow, "Tail has Put_Empty set");
    end if;
    if Cell.Is_Mixed then
      Util.Error (Ctx.Flow, "Tail has Is_Mixed set");
    end if;

    if Ctx.Tail.Children_Number = 0 then
      -- No child
      return;
    end if;

    -- Only Pis and Comments in tail
    Ctx.Tail.Move_Child;
    loop
      Ctx.Tail.Read (Cell);
      if Cell.Kind /= Pi and then Cell.Kind /= Comment then
         Util.Error (Ctx.Flow, "Invalid node type " & Mixed_Str (Cell.Kind'Img)
                              & " in tail");
      end if;
      exit when not Ctx.Tail.Has_Brother (False);
      Ctx.Tail.Move_Brother (False);
    end loop;
  end Check_Tail;

  -- Check a whole element tree recursively
  procedure Check_Subtree (Ctx  : in out Ctx_Type;
                           Adtd : in out Dtd_Type) is
    Cell : My_Tree_Cell;
    Children : Children_Desc;
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
        -- Recursive check this sub element
        Check_Subtree (Ctx, Adtd);
      end if;
      exit when not Ctx.Elements.Has_Brother (False);
      Ctx.Elements.Move_Brother (False);
    end loop;

    -- Done, move back to father
    Ctx.Elements.Move_Father;
  end Check_Subtree;


  -- For sorting IDREFs
  function Less_Than (I1, I2 : Id_Cell) return Boolean is
    use type As.U.Asu_Us;
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
    Moved : Boolean;
    Idref, Prev_Ref : Id_Cell;
    Found : Boolean;
  begin
    Debug ("Checking final");
    -- Each IDREF must exist in IDs
    if not Ctx.Idrefs.Is_Empty then
      Id_Sort (Ctx.Idrefs.all);
      Ctx.Idrefs.Rewind;
      loop
        Ctx.Idrefs.Read (Idref, Moved => Moved);
        -- Check for Id if this reference is new
        if Idref /= Prev_Ref then
          Ctx.Ids.Search (Idref, Found);
          if not Found then
            Util.Error (Ctx.Flow,"No ID for this IDREF " & Idref.Name.Image,
                        Idref.Line_No);
          end if;
          Prev_Ref := Idref;
        end if;
        exit when not Moved;
      end loop;
    end if;
    Debug ("Checked final");
  end Final_Check;

end Dtd;

