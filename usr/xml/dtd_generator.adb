-- Generate a valid DTD from a XML file or flow
-- Ignores internal and external DTD
with Argument, Argument_Parser, Basic_Proc, As.U, Str_Util, Mixed_Str,
     Unbounded_Arrays, Trace.Loggers,
     Xml_Parser, Parser, Hashed_List.Unique, Limited_List;
procedure Dtd_Generator is

  -- Current version
  Version : constant String := "V2.5";

  -- Algorithm criteria

  -- When merging a new children sequence with the current fusionned sequence,
  --  the maximum number of changes (insertion of new child as optional,
  --  or change of fusionned chiled into optional) before giving up.
  --  Giving up means:
  --  - While merging two sequences, stop exploring the current combination
  --  - When changing fusionned child => change the sequence into a choice.
  --  0 disables.
  Max_Deviation : Natural := 11;

  -- When merging sequences, choice or any, maximum number of children in
  --  the list before giving up and changing the list into a any. 0 disables.
  Max_Elements : Natural := 210;

  -- When merging definitions of an enum attribute, maximum number of values
  --  before giving up and changing the attribute into nmtoken. 0 disables.
  Max_Enums : Natural := 21;

  -- Put usage
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ <options> ] [ { <xml_file> } ]");
    Basic_Proc.Put_Line_Error ("or " & Argument.Get_Program_Name
        & " -h | --help | -v | --version");
    Basic_Proc.Put_Line_Error (
        "  <options> ::= [ [ <deviation> ] [ <elements> ] [ <enums> ]");
    Basic_Proc.Put_Line_Error (
        "  <deviation> ::= --deviation=<val>    // default"
        & Max_Deviation'Img);
    Basic_Proc.Put_Line_Error (
        "  <elements>  ::= --elements=<val>     // default"
        & Max_Elements'Img);
    Basic_Proc.Put_Line_Error (
        "  <enums>     ::= --enums=<val>        // default"
        & Max_Enums'Img);
    Basic_Proc.Put_Line_Error (
        "Outputs on stdout the DTD of the XML file or stdin.");
  end Usage;

  -- Log an error
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- The Logger
  Logger : Trace.Loggers.Logger;
  procedure Dbg (Msg : in String) is
  begin
    Logger.Log_Debug (Msg);
  end Dbg;

  -- For output
  procedure Po (Str : in String) renames Basic_Proc.Put_Output;
  procedure Plo (Str : in String) renames Basic_Proc.Put_Line_Output;
  procedure Nlo renames Basic_Proc.New_Line_Output;

  -- Argument keys
   -- The argument keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"), False),
   02 => (False, 'v', As.U.Tus ("version"), False),
   03 => (True, Argument_Parser.No_Key_Char,
                As.U.Tus ("deviation"), False, True, As.U.Asu_Null),
   04 => (True, Argument_Parser.No_Key_Char,
                As.U.Tus ("elements"), False, True, As.U.Asu_Null),
   05 => (True, Argument_Parser.No_Key_Char,
                As.U.Tus ("enums"), False, True, As.U.Asu_Null));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -- The XML file name
  File_Name : As.U.Asu_Us;

  -- The contex of the XML tree
  Ctx : Xml_Parser.Ctx_Type;
  -- Result of XML parsing
  Parse_Ok : Boolean;

  -- Separator of different values of enum in data and iterator
  Sep : constant Character := '#';
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sep;
  end Is_Sep;
  Iter : Parser.Iterator;

  -- Kind of children of the element
  -- Empty     : EMPTY, really Empty
  -- Not_Empty : Has comment or Pi, cannot be Empty, but can become Sequence
  -- Sequence  : Elt,Elt.. with Opt and/or Mult (+, * or ?)
  -- Choice    : (Elt|Elt..)*
  -- Mixed     : (#PCDATA|Elt|Elt...)*
  -- Any       : ANY, any combination of text and elements
  -- Pcdata    : (#PCDATA), text
  type Elt_Kind_List is (Empty, Not_Empty, Sequence, Choice, Mixed,
                         Any, Pcdata);
  type Child_Type is record
    Name : As.U.Asu_Us;
    Opt, Mult : Boolean := False;
  end record;
  type Child_Array is array (Positive range <>) of Child_Type;
  package Child_Unbs is new Unbounded_Arrays (Child_Type, Child_Array);

  -- Attributes of the element: from the most to the less stringent
  type Attr_Kind_List is (Enum, Nmtoken, Nmtokens, Cdata);
  type Attr_Type is record
    -- Key, the attribute name
    Name : As.U.Asu_Us;
    -- The attribute kind
    Kind : Attr_Kind_List := Cdata;
    -- Required or implied
    Required : Boolean := True;
    -- Values of enum, separated by Sep
    Values : As.U.Asu_Us;
  end record;
  type Attr_Array is array (Positive range <>) of Attr_Type;
  package Attr_Unbs is new Unbounded_Arrays (Attr_Type, Attr_Array);

  -- List of elements scanned
  type Element_Type is record
    -- Key: the element name
    Name : As.U.Asu_Us;
    -- Order of creation of the element
    Order : Positive := 1;
    -- Kind of children series
    Kind : Elt_Kind_List := Empty;
    -- Children
    Children : Child_Unbs.Unb_Array;
    -- Attributes
    Attributes : Attr_Unbs.Unb_Array;
  end record;

  -- Hashed list of elements
  type Element_Access is access all Element_Type;
  procedure Set (To : out Element_Type; Val : in Element_Type) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Element_Type; Criteria : Element_Type)
                return Boolean is
    use type As.U.Asu_Us;
  begin
    -- Sale Name
    return Current.Name = Criteria.Name;
  end "=";
  function Image (Element : Element_Type) return String is
  begin
    return Element.Name.Image;
  end Image;
  package Elem_Hash is new Hashed_List (Element_Type, Element_Access, Set,
                                        "=", Image);
  package Elem_Unique is new Elem_Hash.Unique;
  Elements : Elem_Unique.Unique_List_Type;

  -- Sorted List of elements (by order)
  package Elem_List is new Limited_List (Element_Type, Set);
  function Less_Than (El1, El2 : Element_Type) return Boolean is
  begin
    return El1.Order < El2.Order;
  end Less_Than;
  procedure Sort_Elements is new Elem_List.Sort (Less_Than);
  Sorted_Elements : Elem_List.List_Type;

  -- Reduce Into Choice or Mixed so that each child appears only once
  procedure Reduce (Elt : in out Element_Type) is
    Child : Child_Type;
    use type As.U.Asu_Us;
  begin
    Dbg ("  Reducing list of " & Elt.Name.Image);
    for I in 1 .. Elt.Children.Length - 1 loop
      -- We are deleting children within this loop
      exit when I >= Elt.Children.Length;
      Child := Elt.Children.Element (I);
      -- Reverse so that deleting J does not affect algo
      for J in reverse I + 1 .. Elt.Children.Length loop
        if Elt.Children.Element (J).Name = Child.Name then
          Elt.Children.Delete (J, J);
        end if;
      end loop;
    end loop;
  end Reduce;

  -- False if max elements would be exceeded by adding offset
  function Check_Elements (Number : in Natural;
                           Offset : Integer := 1) return Boolean is
  begin
    return Max_Elements = 0 or else Number + Offset <= Max_Elements;
  end Check_Elements;

  -- Merge current element (children and attributes) with definition
  --  elaborated so far
  procedure Merge (Into : in out Element_Type; Val : in out Element_Type)
            is separate;

  -- Is an attribute value compatible bo be a Name (or Nmtoken)
  function Is_Name (Val : in String; Strict : in Boolean) return Boolean is
    C : Character;
  begin
    if Val = "" then
      return False;
    end if;
    for I in Val'Range loop
      C := Val(I);
      if I = Val'First and then Strict then
        if (C < 'A' or else C > 'Z')
        and then (C < 'a' or else C > 'z')
        and then C /= ':' and then C /= '_' then
          return False;
        end if;
      else
        if (C < 'A' or else C > 'Z')
        and then (C < 'a' or else C > 'z')
        and then (C < '0' or else C > '9')
        and then C /= ':' and then C /= '_'
        and then C /= '-' and then C /= '.' then
          return False;
        end if;
      end if;
    end loop;
    return True;
  end Is_Name;

  -- Is an attribute value compatible bo be a Names (Nmtokens)
  function Is_Names (Val : in String) return Boolean is
    Start, Stop : Natural;
  begin
    if Val = "" then
      return False;
    end if;
    -- Get slices separated by space
    Start := Val'First;
    loop
      Stop := Str_Util.Locate (Val, " ", Start);
      -- Names cannot start nor end with space
      if Stop = Val'First or else Stop = Val'Last then
        return False;
      end if;
      -- Last slice
      if Stop = 0 then
        Stop := Val'Last + 1;
      end if;
      -- Check slice
      if not Is_Name (Val(Start .. Stop - 1), False) then
        return False;
      end if;
      -- Done or prepare next iteration
      exit when Stop > Val'Last;
      Start :=  Stop + 1;
    end loop;
    return True;
  end Is_Names;

  -- Add or update an element
  Current_Order : Natural := 0;
  procedure Add_Element (Node : Xml_Parser.Element_Type) is
    Cur_Elt, Sto_Elt : Element_Type;
    Child_Node : Xml_Parser.Node_Type;
    Attr_Node : Xml_Parser.Attribute_Rec;
    Child : Child_Type;
    Attr : Attr_Type;
    Cur_Name : As.U.Asu_Us;
    Found : Boolean;
    Len : Natural;
    use type As.U.Asu_Us, Xml_Parser.Node_Kind_List;
  begin
    -- Build the list of children of this element
    -- Kind is Empty, Not_Empty, Mixed, Data or
    --  a Sequence where children are not Opt but can be Mult
    Cur_Elt.Name := Ctx.Get_Name (Node);
    Dbg ("Add element " & Cur_Elt.Name.Image);
    Cur_Elt.Kind := Empty;
    for I in 1 .. Ctx.Get_Nb_Children (Node) loop
      Child_Node := Ctx.Get_Child (Node, I);
      Len := Cur_Elt.Children.Length;
      case Child_Node.Kind is
        when Xml_Parser.Element =>
           Cur_Name := Ctx.Get_Name (Child_Node);
           Dbg ("  Child " & Cur_Name.Image & " of Elt kind "
              & Mixed_Str (Cur_Elt.Kind'Img));
           case Cur_Elt.Kind is
             when Empty | Not_Empty | Sequence =>
               -- Start or complete a sequence
               Cur_Elt.Kind := Sequence;
               -- Append curr child by default
               Child.Name.Set_Null;
               if Len /= 0 then
                 Child := Cur_Elt.Children.Element (Len);
                 if Child.Name =  Cur_Name then
                   -- Last child is repeated => Mult
                   if not Child.Mult then
                     Child.Mult := True;
                     Cur_Elt.Children.Replace_Element (Len, Child);
                   end if;
                   Dbg ("  Change last Seq " & Cur_Name.Image & " Mult");
                 else
                   -- Append
                   Child.Name.Set_Null;
                 end if;
               end if;
               if Child.Name.Is_Null then
                 -- Check Max element will not be exceeded
                 if Check_Elements (Cur_Elt.Children.Length) then
                   -- First or new child
                   Child := (Name => Cur_Name, Mult | Opt => False);
                   Cur_Elt.Children.Append (Child);
                   Dbg ("  Add to Seq " & Cur_Name.Image);
                 else
                   Dbg ("  Seq exceedes max elements => Any");
                   Cur_Elt.Kind := Any;
                   Cur_Elt.Children.Set_Null;
                 end if;
               end if;
             when Mixed =>
               -- Element after Mixed: Insert element only if it is new
               Found := False;
               for I in 1 .. Cur_Elt.Children.Length loop
                 if Cur_Elt.Children.Element (I).Name = Cur_Name then
                   Found := True;
                   exit;
                 end if;
               end loop;
               if Found then
                 Dbg ("  Element " & Cur_Name.Image & " already in mixed");
               else
                 -- Check Max element will not be exceeded
                 if Check_Elements (Cur_Elt.Children.Length) then
                   Dbg ("  Add to Mixed " & Cur_Name.Image & " kind _");
                   Child := (Name => Cur_Name, Mult | Opt => False);
                   Cur_Elt.Children.Append (Child);
                 else
                   Dbg ("  Mixed exceedes max elements => Any");
                   Cur_Elt.Kind := Any;
                   Cur_Elt.Children.Set_Null;
                 end if;
               end if;
             when Pcdata =>
               -- Child after text => Mixed
               Dbg ("  Change to Mixed " & Cur_Name.Image);
               Child := (Name => Cur_Name, Mult | Opt => False);
               Cur_Elt.Kind := Mixed;
               Cur_Elt.Children.Append (Child);
             when Any | Choice =>
               -- Kind not used for current element
               null;
           end case;
        when Xml_Parser.Text =>
          if Xml_Parser.Is_Separators (String'(Ctx.Get_Text (Child_Node))) then
             -- This text is only separators (due to lack of Dtd)
            Dbg ("  Text is only separators");
            case Cur_Elt.Kind is
               when Empty =>
                 Dbg ("  Change to Not_Empty");
                 Cur_Elt.Kind :=  Not_Empty;
               when Not_Empty | Sequence | Mixed | Any | Choice | Pcdata =>
                 -- no change
                 null;
             end case;
          else
            case Cur_Elt.Kind is
               when Empty | Not_Empty =>
                 Dbg ("  Change to Pcdata");
                 Cur_Elt.Kind := Pcdata;
               when Sequence =>
                 -- Text after children => Mixed
                 Dbg ("  Change to Mixed");
                 Cur_Elt.Kind := Mixed;
                 Reduce (Cur_Elt);
               when Mixed | Any | Choice =>
                 -- Mixed => no change, others => not used
                 null;
               when Pcdata =>
                 -- Several data
                 Dbg ("  Change to Mixed");
                 Cur_Elt.Kind := Mixed;
             end case;
           end if;
        when Xml_Parser.Pi | Xml_Parser.Comment =>
          case Cur_Elt.Kind is
             when Empty =>
               Dbg ("  Change to Not_Empty");
               Cur_Elt.Kind := Not_Empty;
               Reduce (Cur_Elt);
             when Not_Empty | Sequence | Mixed | Any | Choice | Pcdata =>
               -- => no change
               null;
           end case;
      end case;
    end loop;

    -- Process the attributes
    for I in 1 .. Ctx.Get_Nb_Attributes (Node) loop
      Attr_Node := Ctx.Get_Attribute (Node, I);
      Attr.Name := Attr_Node.Name;
      Attr.Values := Attr_Node.Value;
      -- See if it can be an enum (or Nmtoken) or Nmtokens, otherwise Cdata
      if Is_Name (Attr.Values.Image, True) then
        Attr.Kind := Enum;
      elsif Is_Name (Attr.Values.Image, False) then
        Attr.Kind := Nmtoken;
      elsif Is_Names (Attr.Values.Image) then
        Attr.Kind := Nmtokens;
      else
        Attr.Kind := Cdata;
      end if;
      Dbg ("  Attr " & Attr.Name.Image & " " & Mixed_Str (Attr.Kind'Img)
           & " " & Attr.Values.Image);
      Cur_Elt.Attributes.Append (Attr);
    end loop;

    -- Find Element by Name, merge and update
    Sto_Elt.Name := Cur_Elt.Name;
    if Elements.Search (Sto_Elt) then
      Elements.Read (Sto_Elt);
      -- Merge children and attribtues definition of current element
      --  into the stored one
      Merge (Sto_Elt, Cur_Elt);
    else
      -- New element, to be stored
      Current_Order := Current_Order + 1;
      Cur_Elt.Order := Current_Order;
      Sto_Elt := Cur_Elt;
      Dbg ("Inserting element " & Sto_Elt.Name.Image);
    end if;
    Elements.Insert (Sto_Elt);

    -- Recurse on children elements
    for I in 1 .. Ctx.Get_Nb_Children (Node) loop
      Child_Node := Ctx.Get_Child (Node, I);
      if Child_Node.Kind = Xml_Parser.Element then
        Dbg ("Recursing on child " & Ctx.Get_Name (Child_Node));
        Add_Element (Child_Node);
      end if;
    end loop;
  end Add_Element;

  -- Argument number and index
  Arg_Nb : Natural;
  Arg_Index : Positive;
  -- Elements read to generate DTD
  Element : Element_Type;
  Child : Child_Type;
  Attr : Attr_Type;
  Moved : Boolean;
  -- First child or attribute
  First : Boolean;
  -- Nb of enum values and last value
  Nb_Enum : Natural;
  Val, Last_Val : As.U.Asu_Us;
  -- Pad for ATTLIST lines
  Padding : As.U.Asu_Us;

  use type As.U.Asu_Us;
begin

  -- Init logger
  Logger.Init ("Dtd_Generator");

  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
    return;
  end if;
   -- Any path/file spec must be after options
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("File name must appear after option");
    return;
  end if;

  begin
    -- Help and version
    if Arg_Dscr.Is_Set (1) then
      Usage;
      return;
    elsif Arg_Dscr.Is_Set (2) then
      Basic_Proc.Put_Line_Output (
          "Parser version:        " & Xml_Parser.Version);
      Basic_Proc.Put_Line_Output (
          "Dtd_Generator version: " & Version);
      Basic_Proc.Set_Error_Exit_Code;
      return;
    end if;

    -- Options
    if Arg_Dscr.Is_Set (3) then
      Max_Deviation := Natural'Value (Arg_Dscr.Get_Option (3));
    end if;
    if Arg_Dscr.Is_Set (4) then
      Max_Elements := Natural'Value (Arg_Dscr.Get_Option (4));
    end if;
    if Arg_Dscr.Is_Set (5) then
      Max_Enums := Natural'Value (Arg_Dscr.Get_Option (5));
    end if;
  exception
    when others =>
      Error ("Invalid argument");
      Usage;
      return;
  end;

  -- Init the iterator of Enum values
  Iter.Set ("", Is_Sep'Unrestricted_Access);

  -- Process files or stdin
  Arg_Nb := Arg_Dscr.Get_Nb_Occurences (No_Key_Index);
  Arg_Index := 1;
  loop
    if Arg_Nb = 0 then
      -- No arg => Stdin
      File_Name := As.U.Tus (Xml_Parser.Stdin);
    else
      File_Name := As.U.Tus (Arg_Dscr.Get_Option (No_Key_Index, Arg_Index));
    end if;

    -- Parse the file or stdin
    Logger.Log_Info ("Processing "
        & (if File_Name.Is_Null then "stdin"
           else "file " & File_Name.Image));
    begin
      -- Allow comments  otherwise we detect wrongly Empty
      Ctx.Parse (File_Name.Image, Parse_Ok, Comments => True, Use_Dtd => False);
    exception
      when Xml_Parser.File_Error =>
        Error ("File " & File_Name.Image & " not found");
        Usage;
        return;
    end;
    if not Parse_Ok then
      Error (Ctx.Get_Parse_Error_Message);
      return;
    end if;

    -- Recursively process from root element
    Add_Element (Ctx.Get_Root_Element);

    -- Check and prepare next loop
    exit when Arg_Nb = 0 or else Arg_Index = Arg_Nb;
    Arg_Index := Arg_Index + 1;
    Ctx.Clean;
  end loop;

  -- Output DTD
  Plo ("<?xml version=""1.0"" encoding=""UTF-8""?>");
  Plo ("<!-- Generated by dtd_generator -->");
  Nlo;

  -- Loop on all elements
  if not Elements.Is_Empty then

    -- Sort elements
    Elements.Rewind;
    loop
      Elements.Read_Next (Element, Moved);
      Sorted_Elements.Insert (Element);
      exit when not Moved;
    end loop;
    Sort_Elements (Sorted_Elements);

    Sorted_Elements.Rewind;
    loop
      Sorted_Elements.Read (Element, Moved => Moved);
      -- Put definition of Element
      Po ("<!ELEMENT " & Element.Name.Image & " ");
      case Element.Kind is
        when Empty =>
          Plo ("EMPTY>");
        when Sequence =>
          Po ("(");
          -- Separate each child by ','
          for I in 1 .. Element.Children.Length loop
            if I /= 1 then
              Po (",");
            end if;
            Child := Element.Children.Element (I);
            Po (Child.Name.Image);
            if Child.Mult then
              if Child.Opt then
                Po ("*");
              else
                Po ("+");
              end if;
            elsif Child.Opt then
              Po ("?");
            end if;
          end loop;
          Plo (")>");
        when Choice | Mixed =>
          Po ("(");
          First := True;
          if Element.Kind = Mixed then
            -- Start list by PCDATA
            Po ("#PCDATA");
            First := False;
          end if;
          -- Separate each child by '|'
          for I in 1 .. Element.Children.Length loop
            if not First then
              Po ("|");
            end if;
            Child := Element.Children.Element (I);
            Po (Child.Name.Image);
            First := False;
          end loop;
          Plo (")*>");
        when Any =>
          Plo ("ANY>");
        when Pcdata | Not_Empty =>
          Plo ("(#PCDATA)>");
      end case;

      -- Put definition of attributes
      if not Element.Attributes.Is_Null then
        Padding := As.U.Tus ("<!ATTLIST " & Element.Name.Image & " ");
        for I in 1 .. Element.Attributes.Length loop
          Po (Padding.Image);
          -- Name
          Attr := Element.Attributes.Element(I);
          Po (Attr.Name.Image & " ");
          -- Type
          case Attr.Kind is
            when Enum =>
              Po ("(");
              Iter.Set (Attr.Values.Image, Is_Sep'Unrestricted_Access);
              First := True;
              -- Will be FIXED if only one Enum
              Nb_Enum := 0;
              loop
                Val := As.U.Tus (Iter.Next_Word);
                exit when Val.Is_Null;
                Last_Val := Val;
                Po ((if not First then "|" else "") & Val.Image);
                First := False;
                Nb_Enum := Nb_Enum + 1;
              end loop;
              Po (") ");
            when Nmtoken =>
              Po ("NMTOKEN ");
            when Nmtokens =>
              Po ("NMTOKENS ");
            when Cdata =>
              Po ("CDATA ");
          end case;
          -- Default
          if Attr.Kind /= Enum or else Nb_Enum /= 1 then
            Po (if Attr.Required then "#REQUIRED" else "#IMPLIED");
          else
            Po ("#FIXED """ & Last_Val.Image & """");
          end if;

          -- End of line
          if I = Element.Attributes.Length then
            Plo (">");
          else
            Nlo;
          end if;
          -- Pad with spaces the following lines
          Padding := Padding.Length * ' ';
        end loop;
      end if;

      -- Skip a line before next element, or after last element
      Nlo;
      exit when not Moved;
    end loop;
  end if;

end Dtd_Generator;

