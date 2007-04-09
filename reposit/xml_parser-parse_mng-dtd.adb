with Unique_List, String_Mng, Regular_Expressions, Parser, Int_Image;
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

  -- Dtd info per element (Elt, Atl, Att)
  type Info_Rec is record
    -- Kind'Img#Element_name[#Attribute_Name]
    Name : Asu_Us;
    -- Elt: Possible children, first chars is <type> ::= E|A|M|C
    --  (empty, any, mixed or children), then
    --  for Mixed the list of "#<name>#" without #PCDATA (empty if only #PCDATA)
    --  for Children the regexp of "#<name>#"
    -- Atl: Possible attributes, list of "<name>#<type><default>#"
    --  <type> ::= S|I|R|r|T|t|E (String, ID, IDREF, IDREFS, NMTOKEN, NMTOKENS
    --   or enum)
    --  <default> ::= R|I|F|D (required, implied, fixed or default)
    -- Att: for a fixed of any type or the a default of not enum, the value
    --   for an Enum, the list of possible "<name>#" and, if there is a default
    --   this value is the first
    List : Asu_Us;
  end record;

  -- Unique list of Info_Rec
  type Info_Access is access all Info_Rec;
  procedure Set (To : out Info_Rec; Val : in Info_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Info_Rec) return String is
  begin
    return Asu_Ts (Element.Name);
  end Image;
  function "=" (Current : Info_Rec; Criteria : Info_Rec) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  package Info_Mng is new Unique_List (Info_Rec, Info_Access, Set, Image, "=");
  Info_List : Info_Mng.List_Type;

  -- Is there a dtd set, otherwise check is always ok
  Dtd_Set : Boolean;

  -- Is there a xml instruction found in the dtd
  Xml_Found : Boolean;

  -- Init (clear) Dtd data
  procedure Init is
    Info : Info_Rec;
  begin
    -- No Dtd set
    Dtd_Set := False;
    -- No xml nstruction found (yet)
    Xml_Found := False;
    -- Reset list
    Info_Mng.Delete_List (Info_List);
    -- Init with empty IDs and empty IDREFs
    Info.Name := Asu_Tus ("Idl");
    Info_Mng.Insert (Info_List, Info);
    Info.Name := Asu_Tus ("Idr");
    Info_Mng.Insert (Info_List, Info);
  end Init;

  -- Parse an instruction:
  -- Check xml version, append any other instruction to prologue
  procedure Parse_Instruction (External : in Boolean) is
  begin
    -- See if this is the xml directive
    if Util.Try ("xml") then
      if not External then
        Util.Error ("Invalid xml instruction in internal dtd");
      end if;
      if Xml_Found then
        Util.Error ("Second declaration of xml in dtd");
      end if;
      Util.Parse_Until_Str ("?>");
      Trace ("Dtd parsed instruction " & Asu_Ts(Util.Get_Curr_Str));
      Util.Reset_Curr_Str;
    else
      -- Parse instruction as if in Xml
      Parse_Instruction;
    end if;
  end Parse_Instruction;

  -- Build the regexp: <name> -> (#<name>#) ,  . -> \.
  procedure Build_Regexp (Str : in out Asu_Us) is
    -- Separators
    Seps : constant String := "|,?*+()";
    -- Result with the info sep
    Res : Asu_Us;
    -- Intermediate logic
    In_Word : Boolean := False;
    C : Character;
    -- Regexp compilation result (for check)
    Pat : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
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
    Regular_Expressions.Compile (Pat, Ok, Asu_Ts (Res));
    Regular_Expressions.Free (Pat);
    if not Ok then
      Trace ("Dtd regex does node compile >" & Asu_Ts (Res) & "<");
      Util.Error ("Invalid children definition");
    end if;
    -- Done
    Str := Res;
  end Build_Regexp;

  -- Parse <!ELEMENT
  procedure Parse_Element is
    Info : Info_Rec;
    Found : Boolean;
    use type Asu_Us;
  begin
    -- Parse element name
    Util.Parse_Until_Char ("" & Util.Space);
    Info.Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    if not Util.Name_Ok (Info.Name) then
      Util.Error ("Invalid name " & Asu_Ts (Info.Name));
    end if;
    Info.Name := "Elt" & Info_Sep & Info.Name;
    -- Element must not exist
    Info_Mng.Search (Info_List, Info, Found);
    if Found then
      Util.Error ("ELEMENT " & Asu_Ts (Info.Name) & " already exists");
    end if;
    -- Parse type
    Util.Skip_Separators;
    if Util.Try ("EMPTY") then
      Info.List := Asu_Tus ("E");
    elsif Util.Try ("Any") then
      Info.List := Asu_Tus ("A");
    else
      -- A list of sub-elements, possibly containing #PCDATA
      -- Must be '('
      if Util.Get /= '(' then
        Util.Error ("Unexpected character " & Util.Read
                 & " at start of ELEMENT list");
      end if;
      -- Get children definition (until ')' matching the '(' got)
      Util.Parse_Until_Close;
      Info.List := Util.Remove_Separators (Util.Get_Curr_Str);
      Info.List := Util.Fix_Text (Info.List, True, False);
      Util.Reset_Curr_Str;
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
          Info.List := Util.Fix_Text (Info.List, True, False);
          Info.List := Util.Remove_Separators (Info.List);
          -- Check that everything between "|" are names
          if Asu.Element (Info.List, Asu.Length (Info.List)) = '|'
          or else Asu.Element (Info.List, 1) = '|' then
            Util.Error ("Invalid Mixed definition");
          end if;
          if not Util.Names_Ok (Info.List, "|") then
            Util.Error ("Invalid name in Mixed definition");
          end if;
          -- Replace '|' by '#' and prepend and append a '#'
          Info.List := Asu_Tus (
            String_Mng.Replace ("#" & Asu_Ts (Info.List) & "#", "|", "#"));
        else
          Util.Error ("Invalid Mixed definition");
        end if;
        Info.List := "M" & Info.List;
      else
        -- A regexp of children: Append string
        -- Expand variables if any
        Info.List := Util.Fix_Text (Info.List, True, False);
        Info.List := Util.Remove_Separators (Info.List);
        -- Check valid names
        if not Util.Names_Ok (Info.List, "|,?*+()") then
          Util.Error ("Invalid name in Children definition");
        end if;
        -- Fix regex: each name becomes "(#name#)"
        Build_Regexp (Info.List);
        Info.List := "C" & Info.List;
      end if;
    end if;
    -- Directive must end now
    Util.Skip_Separators;
    if Util.Get /= Util.Stop then
      Util.Error ("Unexpected character " & Util.Read & " at end of ELEMENT");
    end if;
    -- Store element
    Info_Mng.Insert (Info_List, Info);
    Trace ("Dtd parsed directive ELEMENT -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Element;


  -- Parse <!ATTLIST
  procedure Parse_Attlist is
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
    Has_Id : Boolean;
    use type Asu_Us;
  begin
    -- Parse element name
    Util.Parse_Until_Char ("" & Util.Space & Util.Stop);
    Elt_Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    if not Util.Name_Ok (Elt_Name) then
      Util.Error ("Invalid name " & Asu_Ts (Elt_Name));
    end if;
    Info.Name := "Atl" & Info_Sep & Elt_Name;
    -- Attribute list of this element must not exist
    Info_Mng.Search (Info_List, Info, Found);
    if Found then
      Util.Error ("ATTLIST " & Asu_Ts (Info.Name) & " already exists");
    end if;
    if Util.Read = Util.Stop then
      -- Empty Attlist: store and done
      Info_Mng.Insert (Info_List, Info);
      Trace ("Dtd parsed directive ATTLIST -> " & Asu_Ts (Info.Name)
           & " " & Asu_Ts(Info.List));
      return;
    end if;

    -- Loop on all attributes
    Has_Id := False;
    loop
      Util.Skip_Separators;
      -- Name of attribute or end of list
      exit when Util.Get = Util.Stop;
      Util.Unget;
      -- Get attribute name
      Util.Parse_Until_Char ("" & Util.Space);
      Att_Name := Util.Get_Curr_Str;
      Util.Reset_Curr_Str;
      -- Check supported att types
      Util.Skip_Separators;
      if Util.Try ("CDATA ") then
        -- String type
        Typ_Char := 'S';
      elsif Util.Try ("ID ") then
        -- ID type
        Typ_Char := 'I';
      elsif Util.Try ("IDREF ") then
        -- IDREF type
        Typ_Char := 'R';
      elsif Util.Try ("IDREFS ") then
        -- IDREFS type
        Typ_Char := 'r';
      elsif Util.Try ("NMTOKEN ") then
        -- NMTOKEN type
        Typ_Char := 'T';
      elsif Util.Try ("NMTOKENS ") then
        -- NMTOKENS type
        Typ_Char := 't';
      elsif Util.Get = '(' then
        -- Enum type
        Typ_Char := 'E';
        Util.Parse_Until_Char (")");
        Enum := Util.Fix_Text (Util.Get_Curr_Str, True, False);
        Enum := Util.Remove_Separators (Enum);
        Util.Reset_Curr_Str;
        -- Check that everything between "|" are names
        if Asu.Element (Enum, Asu.Length (Enum)) = '|'
        or else Asu.Element (Enum, 1) = '|' then
          Util.Error ("Invalid Enum definition");
        end if;
        if not Util.Names_Ok (Enum, "|") then
          Util.Error ("Invalid name in Enum definition");
        end if;
        -- Replace '|' by '#' and prepend and append a '#'
        Enum := Asu_Tus (
          String_Mng.Replace ("#" & Asu_Ts (Enum) & "#", "|", "#"));
      else
        -- Get type
        Util.Unget;
        Util.Parse_Until_Char ("" & Util.Space);
        Att_Type := Util.Get_Curr_Str;
        Util.Reset_Curr_Str;
        if Asu_Ts (Att_Type) = "ENTITY"
        or else Asu_Ts (Att_Type) = "ENTITIES"
        or else Asu_Ts (Att_Type) = "NOTATION" then
          Util.Error ("Unsupported attribute type " & Asu_Ts (Att_Type));
        else
          Util.Error ("Invalid attribute type " & Asu_Ts (Att_Type));
        end if;
      end if;

      -- Check supported att defaults
      Util.Skip_Separators;
      if Util.Try ("#REQUIRED") then
        Def_Char := 'R';
      elsif Util.Try ("#IMPLIED") then
        Def_Char := 'I';
      elsif Util.Try ("#FIXED ") then
        Def_Char := 'F';
      else
        Def_Char := 'D';
      end if;

      -- Check no default value or get default value
      if Def_Char = 'R' or else Def_Char = 'I' then
        -- There shall be no default value for required or implied attribute
        Util.Skip_Separators;
        if Util.Try ("""") or else Util.Try ("'") then
          Util.Error ("Unexpected default value for attribute "
                    &  Asu_Ts (Att_Name));
        end if;
      else
        -- Get default value for fixed or default attribute
        Util.Skip_Separators;
        Def_Val := Parse_Value (True);
      end if;

      -- Check ID
      if Typ_Char = 'I' then
        -- Only one ID per element
        if Has_Id then
          Util.Error ("Element " & Asu_Ts (Elt_Name)
                    & " has already an ID attribute");
        end if;
        -- Id must be implied or required
        if Def_Char /= 'R' and then Def_Char /= 'I' then
          Util.Error ("Id attribute must be of type required or implied");
        end if;
        -- Initialise an Empty Ide info
        Has_Id := True;
        Idinfo.Name := "Ide" & Elt_Name;
        Idinfo.List := Asu_Null;
        Info_Mng.Insert (Info_List, Idinfo);
      end if;

      -- Check Enum
      if Typ_Char = 'E'
      and then (Def_Char = 'D' or else Def_Char = 'F') then
        -- Enum and (default or fixed), check default is in enum
        --  and set the default in first pos
        if (String_Mng.Locate (Asu_Ts (Enum), 1,
              Info_Sep & Asu_Ts (Def_Val) & Info_Sep) = 0) then
          Util.Error ("Default or fixed value "
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
      Attinfo.Name := "Att" & Info_Sep & Elt_Name & Info_Sep & Att_Name;
      if Typ_Char = 'E' then
        Attinfo.List := Enum;
        Info_Mng.Insert (Info_List, Attinfo);
        Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
         & " " & Asu_Ts(Attinfo.List));
      elsif Def_Char = 'F' or else Def_Char = 'D' then
        Attinfo.List := Def_Val;
        Info_Mng.Insert (Info_List, Attinfo);
        Trace ("Dtd stored attribute type -> " & Asu_Ts (Attinfo.Name)
         & " " & Asu_Ts(Attinfo.List));
      end if;
      -- Append this attribute in list: #attribute#td#attribute#td#...
      if Info.List = Asu_Null then
        Info.List := Asu_Tus (Info_Sep & "");
      end if;
      Asu.Append (Info.List,
                  Att_Name & Info_Sep & Typ_Char & Def_Char & Info_Sep);
    end loop;
    -- Attlist is ended: store
    Info_Mng.Insert (Info_List, Info);
    Trace ("Dtd parsed directive ATTLIST -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Attlist;

  -- Parse <!ENTITY
  procedure Parse_Entity is
    -- Entity name and value
    Name, Value : Asu_Us;
    -- Is it a parameter entity
    Parameter : Boolean;
    Parstr : Asu_Us;
    use type Asu_Us;
  begin
    -- See if this is a parameter entity
    Util.Skip_Separators;
    if Util.Get = '%' then
      Parameter := True;
      Parstr := Asu_Tus ("%");
      Util.Skip_Separators;
    else
      Parameter := False;
      Util.Unget;
    end if;
    -- Parse entity name
    Util.Parse_Until_Char ("" & Util.Space);
    Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    -- Check name is valid and not already defined
    if not Util.Name_Ok (Name) then
      Util.Error ("Invalid entity name " & Asu_Ts (Name));
    end if;
    Util.Skip_Separators;
    -- Check that it does not exist
    if Entity_Mng.Exists (Name, Parameter) then
      Util.Error ("Entity " & Asu_Ts (Parstr & Name) & " already defined");
    end if;
    -- Only accept local entities
    if Util.Try ("SYSTEM ") then
      Util.Error ("Unsupported SYSTEM external entity");
    elsif Util.Try ("PUBLIC ") then
      Util.Error ("Unsupported PUBLIC external entity");
    end if;
    -- Parse and expand value
    Value := Parse_Value (True);
    -- Must stop now
    Util.Skip_Separators;
    if Util.Get /= Util.Stop then
      Util.Error ("Unexpected character at end of entity " & Util.Read);
    end if;
    -- Store entity
    Entity_Mng.Add (Name, Value, Parameter);
    Trace ("Dtd parsed directive ENTITY -> " &  Asu_Ts (Parstr & Name)
         & " " & Asu_Ts(Value));
  end Parse_Entity;

  -- Parse a directive
  procedure Parse_Directive is
    Info : Info_Rec;
    use type Asu_Us;
  begin
    if Util.Try ("ELEMENT ") then
      Parse_Element;
    elsif Util.Try ("ATTLIST ") then
      Parse_Attlist;
    elsif Util.Try ("ENTITY ") then
      Parse_Entity;
    elsif Util.Try ("NOTATION ") then
      Util.Error ("Unsupported NOTATION directive");
    else
      -- Skip CDATA and comments
      Parse_Directive (Only_Skip => True);
    end if;
  end Parse_Directive;

  -- Parse current dtd
  -- If external, will stop end end of file
  -- otherwise, will stop on ']'
  procedure Parse (External : in Boolean) is
  begin
    loop
      begin
        Util.Skip_Separators;
      exception
        when Util.End_Error =>
          if External then
            return;
          else
            Util.Error ("Unexpected end of file while parsing internal dtd");
          end if;
      end;
      if Util.Try (Util.Start & Util.Instruction) then
        Parse_Instruction (External);
      elsif Util.Try (Util.Start & Util.Directive) then
        Parse_Directive;
      elsif Util.Get = (']') and then not External then
        return;
      else
        Util.Error ("Unexpected character while parsing dtd" & Util.Get);
      end if;
    end loop;
  end Parse;

  -- Parse a dtd (either a external file or internal if name is empty)
  procedure Parse (File_Name : in String) is
    Dtd_File, Dummy_File : Text_Char.File_Type;
  begin
    if File_Name = "" then
      -- Internal declarations
      Trace ("Dtd parsing internal definition");
      Parse (False);
    else
      -- External declarations
      Trace ("Dtd parsing file " & File_Name);
      File_Mng.Open (File_Name, Dtd_File);
      Util.Init (False, Dtd_File, True);
      Parse (True);
      Util.Init (True, Dummy_File, False);
      File_Mng.Close (Dtd_File);
    end if;
    -- Dtd is now valid
    Trace ("Dtd parsed dtd");
    Dtd_Set := True;
  exception
    when File_Error =>
      Util.Error ("Cannot open dtd file " & File_Name);
  end Parse;

  -- Replace "##" by "," then suppress "#"
  function Strip_Sep (Us : in Asu_Us) return String is
    use String_Mng;
  begin
    return Replace (Replace (Asu_Ts (Us), "##", ","), "#", "");
  end Strip_Sep;
  -- Check children of element
  procedure Check_Children (Name : in Asu_Us;
                            Line_No : in Positive;
                            Children : in Asu_Us;
                            Is_Mixed : in Boolean) is
    -- Element info
    Info : Info_Rec;
    -- Char of info List
    Char : Character;
    -- Parser iterator
    Iter_Xml : Parser.Iterator;
    -- Pattern of regexp
    Pat : Regular_Expressions.Compiled_Pattern;
    -- Regexp pattern match result
    N_Match : Natural;
    Match_Info : Regular_Expressions.One_Match_Array;
    -- General purpose Boolean
    Ok : Boolean;
    use type Asu_Us;
  begin
    Trace ("Dtd check Xml children list " & Asu_Ts (Children)
         & " Mixed: " & Is_Mixed'Img);
    -- Read its element def
    Info.Name := "Elt" & Info_Sep & Name;
    Info_Mng.Search (Info_List, Info, Ok);
    if not Ok then
      Util.Error ("According to dtd, element " & Asu_Ts (Name)
                & " is not allowed",
                  Line_No);
    end if;
    Info_Mng.Read (Info_List, Info, Info);
    -- Check children
    Trace ("Dtd check Dtd element info " & Asu_Ts (Info.List));
    -- Separate element type
    Char := Asu.Element (Info.List, 1);
    Info.List := Asu.Delete (Info.List, 1, 1);
    case Char is
      when 'E' =>
        -- Must be empty
        if Asu.Length (Children) /= 0 then
          Util.Error ("According to dtd, element " & Asu_Ts (Name)
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
            if String_Mng.Locate (Asu_Ts (Info.List), 1,
                                  Info_Sep & Child & Info_Sep) = 0 then
              Util.Error ("According to dtd, element " & Asu_Ts (Name)
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
          Util.Error ("According to dtd, element " & Asu_Ts (Name)
                    & " must not have text",
                      Line_No);
        end if;
        -- Build children regexp
        Regular_Expressions.Compile (Pat, Ok, Asu_Ts (Info.List));
        if not Ok then
          Trace ("Dtd regex does not compile for check " & Asu_Ts (Info.List));
          raise Internal_Error;
        end if;
        -- Check children regexp, the complete Children string must match
        Regular_Expressions.Exec (Pat, Asu_Ts (Children), N_Match, Match_Info);
        if N_Match = 0
        or else Match_Info(1).Start_Offset /= 1
        or else Match_Info(1).End_Offset /= Asu.Length (Children) then
          Regular_Expressions.Free (Pat);
          Util.Error ("According to dtd, element " & Asu_Ts (Name)
                    & " allows children " & Strip_Sep (Info.List)
                    & " but has children " & Strip_Sep (Children));
        end if;
        Regular_Expressions.Free (Pat);
        Trace ("Dtd checked children " & Strip_Sep (Children)
             & " versus " & Strip_Sep (Info.List));
      when others =>
        Trace ("Dtd check: Unexpected element type " & Char);
        raise Internal_Error;
    end case;
  end Check_Children;

  -- Check attributes of element
  procedure Check_Attributes (Name : in Asu_Us;
                              Line_No : in Positive;
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
    Info_Mng.Search (Info_List, Info, Info_Found);
    if Info_Found then
      Info_Mng.Read (Info_List, Info, Info);
    end if;
    if not Info_Found or else Info.List = Asu_Null then
      -- No or empty ATTLIST for this element
      if  Attributes = Asu_Null then
        Trace ("Dtd checked element " & Asu_Ts (Name)
             & " with no attributes, versus no or empty attlist");
        return;
      else
        Util.Error ("According to dtd, element " & Asu_Ts (Name)
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
        if String_Mng.Locate (Asu_Ts (Att_Names), 1,
                              Info_Sep & Attr & Info_Sep) = 0 then
          Util.Error ("According to dtd, element " & Asu_Ts (Name)
                    & " cannot have attribute " & Attr,
                      Line_No);
        end if;
      end;
    end loop;
    Parser.Del (Iter_Xml);

    -- Check attributes dtd vw xml
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
          Info_Mng.Read (Info_List, Attinfo, Attinfo);
        end if;
        -- Does this attribute appear in xml
        Att_Set := String_Mng.Locate (Asu_Ts (Attributes), 1,
                   Info_Sep & Attr & Info_Sep) /= 0;
        if Att_Set then
          -- Get the Xml Attribute
          Xml_Val := Tree_Mng.Get_Attribute (Asu_Tus(Attr));
        end if;

        --  Any Required or Fixed in dtd must appear in xml
        if (Td(2) = 'R' or else Td(2) = 'F')
        and then not Att_Set then
          Util.Error ("According to dtd, element " & Asu_Ts (Name)
                    & " must have attribute " & Attr, Line_No);
        end if;
        -- Enum and Fixed must have correct value
        if Td(2) = 'F' then
          -- Fixed (Enum or string): first #<val># is the one required
          declare
            -- Get the first value from dtd list, from 2 to second sep
            Sep : constant Positive
                := String_Mng.Locate (Asu_Ts (Attinfo.List), 1,
                                      Info_Sep & "", 2);
            Dtd_Val : constant String := Asu.Slice (Attinfo.List, 2, Sep - 1 );
          begin
            if Asu_Ts (Xml_Val) /= Dtd_Val then
              Util.Error ("According to dtd, attribute " & Attr
                        & " must have fixed value " & Dtd_Val, Line_No);
            end if;
          end;
        elsif Td(1) = 'E' and then Att_Set then
          -- Not fixed Enum in dtd with xml value: #<val># must be in dtd list
          if String_Mng.Locate (Asu_Ts (Attinfo.List), 1,
                 Info_Sep  & Asu_Ts (Xml_Val) & Info_Sep) = 0 then
            Util.Error ("According to dtd, Enum attribute " & Attr
                      & " has incorrect value "
                      & Asu_Ts (Xml_Val), Line_No);
          end if;
        elsif Td(2) = 'D' and then not Att_Set then
          -- Default in dtd with no xml value: insert default in tree
          declare
            -- Get the first value from dtd list, from 2 to second sep
            Sep : constant Positive
                := String_Mng.Locate (Asu_Ts (Attinfo.List), 1,
                                      Info_Sep & "", 2);
            Dtd_Val : constant String := Asu.Slice (Attinfo.List, 2, Sep - 1 );
          begin
            Tree_Mng.Add_Attribute (Asu_Tus (Attr), Asu_Tus (Dtd_Val), Line_No);
          end;
        elsif Att_Set then
          -- Comformance checks on ID, IDREF(s) and NMTOKEN(s)
          -- Store ID and IDREFs and Sanity checks on I
          if (Td(1) = 'I' or else Td(1) = 'R')
          and then not Util.Name_Ok (Xml_Val) then
            Util.Error ("Invalid name for ID or IDREF "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 'r'
          and then  not Util.Names_Ok (Xml_Val, Util.Space & "") then
            Util.Error ("Invalid name in IDREFS "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 'T'
          and then not Util.Name_Ok (Xml_Val, Allow_Token => True) then
            Util.Error ("Invalid token for NMTOKEN "
                      & Asu_Ts (Xml_Val), Line_No);
          elsif Td(1) = 't'
          and then not Util.Names_Ok (Xml_Val,
                                      Util.Space & "",
                                      Allow_Token => True) then
            Util.Error ("Invalid name in NMTOKENS "
                      & Asu_Ts (Xml_Val), Line_No);
          end if;

          -- Store IDs and IDREFs
          if Td(1) = 'I' then
            -- Check this ID is not already set for this element
            Idinfo.Name := "Ide" & Info_Sep & Name;
            Info_Mng.Search (Info_List, Idinfo, Info_Found);
            if Info_Found then
              Info_Mng.Read (Info_List, Idinfo, Idinfo);
              if String_Mng.Locate (Asu_Ts (Idinfo.List), 1,
                              Info_Sep & Asu_Ts (Xml_Val) & Info_Sep) /= 0 then
                Util.Error ("This ID " & Asu_Ts (Xml_Val)
                          & " is already set to this element", Line_No);
              end if;
            end if;
            -- Append ID to the list of this element
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep);
            Info_Mng.Insert (Info_List, Idinfo);
            -- Append ID to global list
            Idinfo.Name := Asu_Tus ("Idl");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Info_List, Idinfo, Idinfo);
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep);
            Info_Mng.Insert (Info_List, Idinfo);
            Trace (" Check, added ID " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'R' then
            -- Append this ID ref and line_no to list of IDREFs
            Idinfo.Name := Asu_Tus ("Idr");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Info_List, Idinfo, Idinfo);
            Asu.Append (Idinfo.List, Info_Sep & Xml_Val & Info_Sep
                      & Line_Image(Line_No) & Info_Sep);
            Info_Mng.Insert (Info_List, Idinfo);
            Trace (" Check, added IDREF " & Asu_Ts (Xml_Val));
          elsif Td(1) = 'r' then
            -- Append each of the IDREFs to the global list
            Idinfo.Name := Asu_Tus ("Idr");
            Error_Name := Idinfo.Name;
            Info_Mng.Read (Info_List, Idinfo, Idinfo);
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
            Info_Mng.Insert (Info_List, Idinfo);
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
  procedure Check_Element is
    -- Current cell in tree
    Cell : My_Tree_Cell;
    -- Lists of attributes and of children from xml tree
    Attributes, Children : Asu_Us;
    -- Has current element mixed children (text)
    Is_Mixed : Boolean;
    use type Asu_Us;
  begin
    if not Dtd_Set then
      -- No dtd => no check
      return;
    end if;
    -- Read current element from tree and make its attribute and children lists
    Is_Mixed := False;
    if My_Tree.Children_Number (Tree_Mng.Tree) /= 0 then
      for I in 1 .. My_Tree.Children_Number (Tree_Mng.Tree) loop
        if I = 1 then
          My_Tree.Move_Child (Tree_Mng.Tree);
        else
          My_Tree.Move_Brother (Tree_Mng.Tree, False);
        end if;
        My_Tree.Read (Tree_Mng.Tree, Cell);
        case Cell.Kind is
          when Xml_Parser.Attribute =>
            Asu.Append (Attributes, Info_Sep & Cell.Name & Info_Sep);
          when Xml_Parser.Element =>
            Asu.Append (Children, Info_Sep & Cell.Name & Info_Sep);
          when Xml_Parser.Text =>
            Is_Mixed := True;
        end case;
      end loop;
      My_Tree.Move_Father (Tree_Mng.Tree);
    end if;
    My_Tree.Read (Tree_Mng.Tree, Cell);
    -- Check Attributes
    Check_Attributes (Cell.Name, Cell.Line_No, Attributes);
    -- Check children
    Check_Children (Cell.Name, Cell.Line_No, Children, Is_Mixed);

  end Check_Element;

  -- Final checks
  -- Check that all attribute values of Xml tagged IDREF(s) in Dtd
  --  and thus collected in "Idr" info,
  --  exist in the list of attribute values of Xml tagged ID
  --  and thus collected in "Idl" info,
  procedure Final_Check is
    -- List of IDs and list of IDrefs
    Ids, Idrefs : Info_Rec;
    -- Parser iterator on IDrefs
    Iter_Idref : Parser.Iterator;
  begin
    -- Read Ids
    Ids.Name := Asu_Tus ("Idl");
    -- For error
    Idrefs.Name := Ids.Name;
    Info_Mng.Read (Info_List, Ids, Ids);
    Trace ("Check final, got IDs " & Asu_Ts (Ids.List));
    -- Read Idrefs
    Idrefs.Name := Asu_Tus ("Idr");
    Info_Mng.Read (Info_List, Idrefs, Idrefs);
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
          if String_Mng.Locate (Asu_Ts (Ids.List), 1,
                            Info_Sep & Idref & Info_Sep) = 0 then
            Util.Error ("The ID of the ref " & Idref
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

