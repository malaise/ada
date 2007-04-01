with Unique_List, String_Mng, Regular_Expressions;
separate (Xml_Parser.Parse_Mng)
package body Dtd is

  -- Dtd info per element (Elt, Atl, Att)
  Info_Sep : constant Character := '#';
  type Info_Rec is record
    -- Kind'Img#Element_name[#Attribute_Name]
    Name : Asu_Us;
    -- Elt: Possible children, first chars is <type> ::= E|A|M|C
    --  (empty, any, mixed or children), then
    --  for Mixed the list of "#<name>#" without #PCDATA (empty if only #PCDATA)
    --  for Children the regexp of "#<name>#"
    -- Atl: Possible attributes, list of "<name>#<type><default>#"
    --  <type> ::= S|E (string or enum) and <default> ::= R|I|F|D (required,
    --  implied, fixed or default)
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
  begin
    -- No Dtd set
    Dtd_Set := False;
    -- No xml nstruction found (yet)
    Xml_Found := False;
    -- Reset list
    Info_Mng.Delete_List (Info_List);
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
      Util.reset_Curr_Str;
    else
      -- Parse instruction as if in Xml
      Parse_Instruction;
    end if;
  end Parse_Instruction;

  -- Build the regexp: <name> -> (#<name>#) ,  . -> \.
  procedure Build_Regexp (Str : in out Asu_Us) is
    Seps : constant String := "|,?*+";
    Res : Asu_Us;
    In_Word : Boolean := False;
    C : Character;
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
    Res := Asu_Tus (String_Mng.Replace (",", "", Asu_Ts (Res)));
    -- Now compile to check it 
    Regular_Expressions.Compile (Pat, Ok, Asu_Ts (Res));
    Regular_Expressions.Free (Pat);
    if not Ok then
      Trace ("Regex does node compile >" & Asu_Ts (Res) & "<");
      Util.Error ("Invalid children definition");
    end if;
    -- Done
    Str := Res;
  end Build_Regexp;

  -- Parse <!ELEMENT
  procedure Parse_Element is
    Info : Info_Rec;
    use type Asu_Us;
    Found : Boolean;
  begin
    -- Parse element name
    Util.Parse_Until_Char ("" & Util.Space);
    Info.Name := Util.Get_Curr_Str;
    Util.Reset_Curr_Str;
    if not Util.Name_Ok (Info.Name) then
      Util.Error ("Invalid name " & Asu_Ts (Info.Name));
    end if;
    Info.Name := "Elt" & Info_Sep & Info.Name;
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
      Util.Parse_Until_Char (")");
      -- Now see if it is mixed or children
      if Asu.Index (Util.Get_Curr_Str, "#PCDATA") /= 0 then
        -- Mixed: Append string, without separators
        Info.List := Util.Remove_Separators (Util.Get_Curr_Str);
        if Asu_Ts (Info.List) = "#PCDATA" then
          Info.List := Asu_Null;
        elsif Asu.Slice (Info.List, 1, 8) = "#PCDATA|" then
          -- Remove heading #PCDATA
          Info.List := Asu_Tus (
              String_Mng.Cut (Asu_Ts (Info.List), 8));
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
            String_Mng.Replace ("|", "#", "#" & Asu_Ts (Info.List) & "#"));
        else
          Util.Error ("Invalid Mixed definition");
        end if;
        Info.List := "M" & Info.List;
      else
        -- A regexp of children: Append string, without separators
        Info.List := Util.Remove_Separators (Util.Get_Curr_Str);
        -- Check valid names
        if not Util.Names_Ok (Info.List, "|,?*+") then
          Util.Error ("Invalid name in Children definition");
        end if;
        -- Fix regex: each name becomes "(#name#)"
        Build_Regexp (Info.List);
        Info.List := "C" & Info.List;
      end if;
      Util.Reset_Curr_Str;
    end if;
    -- Directive must end now 
    Util.Skip_Separators;
    if Util.Get /= Util.Stop then
      Util.Error ("Unexpected character " & Util.Read & " at end of ELEMENT");
    end if;
    -- Element mut not exist
    Info_Mng.Search (Info_List, Info, Found);
    if Found then
      Util.Error ("ELEMENT " & Asu_Ts (Info.Name) & " already exists");
    end if;
    Info_Mng.Insert (Info_List, Info);
    Trace ("Dtd parsed directive ELEMENT -> " & Asu_Ts (Info.Name)
         & " " & Asu_Ts(Info.List));
  end Parse_Element;

  -- Parse a directive
  procedure Parse_Directive is
    Info : Info_Rec;
    use type Asu_Us;
  begin
    if Util.Try ("ELEMENT ") then
      Parse_element;
    elsif Util.Try ("ATTLIST ") then
      -- @@@
      Util.Parse_Until_Str (">");
      Trace ("Dtd parsed directive ATTLIST " & Asu_Ts (Util.Get_Curr_Str));
      Util.reset_Curr_Str;
    elsif Util.Try ("ENTITY ") then
      Util.Error ("Unsupported ENTITY directive");
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
        Util.Error ("Unexpected characters while parsing dtd");
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
      Util.Init (False, Dtd_File);
      Parse (True);
      Util.Init (True, Dummy_File);
      File_Mng.Close (Dtd_File);
    end if;
    -- Dtd is now valid
    -- @@@ Dtd_Set := True;
  exception
    when File_Error =>
      Util.Error ("Cannot open dtd file " & File_Name);
  end Parse;

  -- Check Current element of the tree
  procedure Check_Element is
  begin
    if not Dtd_Set then
      -- No dtd => no check
      return;
    end if;
    -- @@@ Check current element, attributes and children, vs dtd setting
  end Check_Element;

end Dtd;

