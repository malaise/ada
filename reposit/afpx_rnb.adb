-- Insert or remove fields in a Afpx.xml file
-- Update other field number and references to them
with Ada.Exceptions;
with Basic_Proc, Argument, Argument_Parser, Environ, Lower_Str,
     As.U, Images, Unbounded_Arrays, Str_Util, Text_Line,
     Afpx_Typ, Xml_Parser.Generator;
procedure Afpx_Rnb is
    -- Options
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"),       False),
    2 => (True,  'D', As.U.Tus ("descriptor"), False, True, As.U.Tus ("descriptor_num")),
    3 => (True,  'i', As.U.Tus ("insert"),     False, True, As.U.Tus ("field_num")),
    4 => (True,  'd', As.U.Tus ("delete"),     False, True, As.U.Tus ("field_num")),
    5 => (True,  'm', As.U.Tus ("move"),       False, True, As.U.Tus ("from_field:to_field")),
    6 => (True,  'c', As.U.Tus ("copy"),       False, True, As.U.Tus ("from_field:to_field")),
    7 => (True,  'n', As.U.Tus ("number"),     False, True, As.U.Tus ("number")),
    8 => (True,  'f', As.U.Tus ("file"),       False, True, As.U.Tus ("file_name")),
    9 => (True,  'o', As.U.Tus ("output"),     False, True, As.U.Tus ("file_name")),
   10 => (False, 'F', As.U.Tus ("force"),      False));
  Args : Argument_Parser.Parsed_Dscr;
  Default_File_Name : constant String := "Afpx.xml";

  -- Usage and Error message
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " [ <file> ] [ <output> ] [ <force> ] <descriptor> <action> [ <number> ]");
    Basic_Proc.Put_Line_Error (
       " <file>       ::= " & Argument_Parser.Image (Keys(8)));
    Basic_Proc.Put_Line_Error (
       " <output>     ::= " & Argument_Parser.Image (Keys(9)));
    Basic_Proc.Put_Line_Error (
       " <force>      ::= " & Argument_Parser.Image (Keys(10)));
    Basic_Proc.Put_Line_Error (
       " <descriptor> ::= " & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error (
       " <action>     ::= <insert> | <delete> | <move> | <copy>");
    Basic_Proc.Put_Line_Error (
       " <insert>     ::= " & Argument_Parser.Image (Keys(3)));
    Basic_Proc.Put_Line_Error (
       " <delete>     ::= " & Argument_Parser.Image (Keys(4)));
    Basic_Proc.Put_Line_Error (
       " <move>       ::= " & Argument_Parser.Image (Keys(5)));
    Basic_Proc.Put_Line_Error (
       " <copy>       ::= " & Argument_Parser.Image (Keys(6)));
    Basic_Proc.Put_Line_Error (
       " <number>     ::= " & Argument_Parser.Image (Keys(7)));
    Basic_Proc.Put_Line_Error (
       "Insert <number> fields after <field_num> (<field_num>=0 for from 1)");
    Basic_Proc.Put_Line_Error (
       "  or delete <number> fields from <field_num> included");
    Basic_Proc.Put_Line_Error (
       "  or move <number> fields from <from_field> after <to_field>.");
    Basic_Proc.Put_Line_Error (
       "  or copy <number> fields from <from_field> after <to_field>.");
    Basic_Proc.Put_Line_Error (
       "Default input is " & Default_File_Name
       &  ", default output is to overwrite input.");
    Basic_Proc.Put_Line_Error (
       "use <force> to delete fields even if they are referenced.");
  end Usage;

  Abort_Error : exception;
  procedure Error (Msg : in String; Dot : in Boolean := True) is
  begin
    Basic_Proc.Put_Error ("ERROR: " & Msg);
    if Dot then
      Basic_Proc.Put_Line_Error (".");
    else
      Basic_Proc.New_Line_Error;
    end if;
    raise Abort_Error;
  end Error;

  -- Debug option
  Debug : Boolean;

  -- Afpx descriptor
  Dscr : Afpx_Typ.Descriptor_Range;

  -- Number if fields initialy in the dscr
  Nb_Fields : Afpx_Typ.Absolute_Field_Range;

  -- XML descriptor and nodes
  Xml : Xml_Parser.Generator.Ctx_Type;
  -- The descriptor
  Dscr_Elt : Xml_Parser.Element_Type;

  Field_Name : constant String := "Field";
  Field_Num_Name : constant String := "Num";

  -- Find field Fld_Num
  -- If 0 (Insert(0)) then set Start_Node to last node not text before first
  --  field, No_Node means that first field has to be inserted as first child
  --  of Dscr
  function Find_Field (Field_Num : Afpx_Typ.Absolute_Field_Range)
           return Xml_Parser.Node_Type is
    -- Tempo node
    Tmp_Node : Xml_Parser.Node_Type;
    -- Last node not text before first field
    Prev_Node : Xml_Parser.Node_Type;
    -- Result
    Node : Xml_Parser.Node_Type;
    Found : Boolean;
  use type Afpx_Typ.Absolute_Field_Range, Xml_Parser.Node_Kind_List;
  begin
    Found := False;
    Nb_Fields := 0;
    Tmp_Node := Xml_Parser.No_Node;
    for I in 1 ..  Xml.Get_Nb_Children (Dscr_Elt) loop
      if Nb_Fields = 0 and then Tmp_Node.Kind /= Xml_Parser.Text then
        -- Keep track of last node not text before first field
        Prev_Node := Tmp_Node;
      end if;
      Tmp_Node := Xml.Get_Child (Dscr_Elt, I);
      if Tmp_Node.Kind = Xml_Parser.Element
      and then Xml.Get_Name (Tmp_Node) = Field_Name then
        Nb_Fields := Nb_Fields + 1;
        -- Store last node not text before first field
        if Field_Num = 0 and then Nb_Fields = 1 then
          Node := Prev_Node;
        end if;
        if Afpx_Typ.Field_Range'Value (
            Xml.Get_Attribute (Tmp_Node, Field_Num_Name)) = Field_Num then
          Found := True;
          Node := Tmp_Node;
        end if;
      end if;
    end loop;
    if not Xml_Parser.Is_Valid (Node)
    and then Xml_Parser.Is_Valid (Prev_Node) then
      -- There is a node, not text but not field
      --  (e.g. only a list)
      Node := Prev_Node;
    end if;
    if Debug and then Xml_Parser.Is_Valid (Node) then
      Basic_Proc.Put_Output ("Start_Node is " & Node.Kind'Img);
      if Node.Kind = Xml_Parser.Element then
        Basic_Proc.Put_Output (" Name " & Xml.Get_Name (Node));
        if Xml.Get_Name (Node) = Field_Name then
          Basic_Proc.Put_Line_Output (" Num "
                                    & Xml.Get_Attribute (Node, Field_Num_Name));
        else
          Basic_Proc.New_Line_Output;
        end if;
      else
        Basic_Proc.New_Line_Output;
      end if;
    end if;

    -- Check that field is found
    if Field_Num /= 0 and then not Found then
      Error ("Field " & Images.Integer_Image (Positive (Field_Num))
          & " does not exist in descriptor "
          & Images.Integer_Image (Positive (Dscr)));
    end if;
    if Debug then
      Basic_Proc.Put_Line_Output ("Got "
          & Images.Integer_Image (Natural (Nb_Fields)) & " fields");
    end if;
    return Node;
  end Find_Field;

  -- Is an node a field
  function Is_Field (Field : Xml_Parser.Node_Type) return Boolean is
    use type Xml_Parser.Node_Kind_List;
  begin
    return Field.Kind = Xml_Parser.Element
           and then Xml.Get_Name (Field) = Field_Name;
  end Is_Field;


  -- Find next node named Field_Name after Fld
  -- Return No_Node if none or if Fld is No_Node
  function Next_Field (Fld : Xml_Parser.Node_Type)
           return Xml_Parser.Node_Type is
    Result : Xml_Parser.Node_Type;
  begin
    if not Xml_Parser.Is_Valid (Fld) then
      return Xml_Parser.No_Node;
    end if;
    Result := Fld;
    loop
      if not Xml.Has_Brother (Result) then
        return Xml_Parser.No_Node;
      end if;
      Result := Xml.Get_Brother (Result);
      if Is_Field (Result) then
        return Result;
      end if;
    end loop;
  end Next_Field;

  -- Content if Text field that makes the indentation of Field:
  -- Line_Feed and 4 spaces
  Indent : constant String := Text_Line.Line_Feed_Str & "    ";

  -- Default attributes when creating a field
  Ini_Attrs : Boolean := False;
  Fld_Attrs : Xml_Parser.Attributes_Array (1 ..2);
  Geo_Attrs : Xml_Parser.Attributes_Array (1 ..4);
  Col_Attrs : Xml_Parser.Attributes_Array (1 ..2);

  -- Add a field (brother or child) of reference node
  -- If Copy_From is set (not No_Node) then copy from it, other insert a default
  -- Set its Fld_Num.
  -- Insert before element a Text "Lf   "
  -- Add to element the attributes Num and Kind,
  -- Add to element the children Geometry and Colors with attribtues
  procedure Add_Field (Ref_Node  : in Xml_Parser.Node_Type;
                       Child     : in Boolean;
                       Fld_Num   : in Afpx_Typ.Field_Range;
                       Copy_From : in Xml_Parser.Node_Type;
                       Field     : out Xml_Parser.Element_Type) is
    -- Text for Indentation
    Indent_Node : Xml_Parser.Text_Type;
    -- geometry and color
    Child_Node : Xml_Parser.Element_Type;
  begin
    -- Init global attributes once
    if not Ini_Attrs then
      Fld_Attrs(1).Name  := As.U.Tus (Field_Num_Name);
      Fld_Attrs(2).Name  := As.U.Tus ("Kind");
      Fld_Attrs(2).Value := As.U.Tus ("Put");
      Geo_Attrs(1).Name  := As.U.Tus ("Up");
      Geo_Attrs(1).Value := As.U.Tus ("0");
      Geo_Attrs(2).Name  := As.U.Tus ("Left");
      Geo_Attrs(2).Value := As.U.Tus ("0");
      Geo_Attrs(3).Name  := As.U.Tus ("Height");
      Geo_Attrs(3).Value := As.U.Tus ("1");
      Geo_Attrs(4).Name  := As.U.Tus ("Width");
      Geo_Attrs(4).Value := As.U.Tus ("1");
      Col_Attrs(1).Name  := As.U.Tus ("Foreground");
      Col_Attrs(1).Value := As.U.Tus ("Black");
      Col_Attrs(2).Name  := As.U.Tus ("Background");
      Col_Attrs(2).Value := As.U.Tus ("White");
      Ini_Attrs := True;
    end if;

    if Xml_Parser.Is_Valid (Copy_From) then
      -- Copy as first child or next brother
      Xml.Copy (Copy_From, Ref_Node, Field, Child, not Child);
    else
      -- Insert a new child or brother
      if Child then
        Xml.Add_Child (Ref_Node, Field_Name, Xml_Parser.Element, Field, False);
      else
        Xml.Add_Brother (Ref_Node, Field_Name, Xml_Parser.Element, Field);
      end if;
    end if;

    -- Complete
    -- Prepend a Text with Lf + 4 spaces
    Xml.Add_Brother (Field, Indent, Xml_Parser.Text, Indent_Node,
                     Next => False);
    -- Set Field num
    Fld_Attrs(1).Value := As.U.Tus (Images.Integer_Image (Positive (Fld_Num)));
    if Debug then
      Basic_Proc.Put_Line_Output ("Insert Field " & Fld_Attrs(1).Value.Image);
    end if;
    if Xml_Parser.Is_Valid (Copy_From) then
      -- Set field Num attribute with provided Num
      Xml.Set_Attribute (Field, Fld_Attrs(1).Name.Image,
                                Fld_Attrs(1).Value.Image);
    else
      -- Set attributes to (Field_Num_Name, Num)
      Xml.Set_Attributes (Field, Fld_Attrs);
      -- Insert Geometry
      Xml.Add_Child (Field, "Geometry", Xml_Parser.Element, Child_Node);
      Xml.Set_Put_Empty (Child_Node, True);
      Xml.Set_Attributes (Child_Node, Geo_Attrs);
      -- Insert Colors
      Xml.Add_Child (Field, "Colors", Xml_Parser.Element, Child_Node);
      Xml.Set_Put_Empty (Child_Node, True);
      Xml.Set_Attributes (Child_Node, Col_Attrs);
    end if;
  end Add_Field;


  -- Arguments and ENV
  Input_File_Name, Output_File_Name : As.U.Asu_Us;
  Force : Boolean := False;
  type Action_List is (Insert, Delete, Move, Copy);
  Action : Action_List;
  Field_Numi : Afpx_Typ.Absolute_Field_Range := 0;
  Field_Numd : Afpx_Typ.Field_Range := 1;
  Number : Afpx_Typ.Field_Range := 1;

  -- Node to append to when Field_Num = 0, No_Node => 1st child of Dscr
  Start_Nodei : Xml_Parser.Node_Type;
  -- First node to delete or move
  Start_Noded : Xml_Parser.Node_Type;
  -- Tempo node
  Tmp_Node : Xml_Parser.Node_Type;

  -- Map of new field nums
  type Field_Array is array (Positive range <>)
                            of Afpx_Typ.Absolute_Field_Range;
  package Field_Map_Mng is new Unbounded_Arrays (
                    Afpx_Typ.Absolute_Field_Range,
                    Field_Array);
  Field_Map : Field_Map_Mng.Unb_Array;
  Old_Num, New_Num : Afpx_Typ.Absolute_Field_Range;

  -- Ok so far (mainly about unresolved references)
  Ok : Boolean;

  -- Update a Text
  -- Skip "\${...\})"
  -- Find ${Field_xx.yy} and replace xx by XX
  -- If XX = 0 => Raise a warning and set Ok to false
  procedure Update (Text : in out As.U.Asu_Us;
                    Line : in Natural;
                    Updated : out Boolean) is
    Input : constant As.U.Asu_Us := Text;
    I, J : Natural;
    New_Num : Afpx_Typ.Absolute_Field_Range;
    Old_Num : Afpx_Typ.Absolute_Field_Range;
    use type Afpx_Typ.Absolute_Field_Range;
  begin
    I := 0;
    Updated := False;
    loop
      I := Str_Util.Locate (Text.Image, "${Field_", I);
      exit when I = 0;
      if not Str_Util.Is_Backslashed (Text.Image, I) then
        -- I and J are indexes of Field Num in Text
        I := I + 8;
        begin
          J := Str_Util.Locate (Text.Image, ".", I) - 1;
          Old_Num := Afpx_Typ.Absolute_Field_Range'Value (
                 Text.Slice (I, J));
          if Old_Num = Afpx_Typ.List_Field_No then
            New_Num := Old_Num;
          else
            New_Num := Field_Map.Element (Positive (Old_Num));
          end if;
        exception
          when others =>
            Error ("INTERNAL ERROR: Cannot update field num");
        end;
        if New_Num = 0 and then Action = Delete then
          -- Reference to a deleted field
          Basic_Proc.Put_Line_Output ("Warning: Text """ & Input.Image
            & """ at line " & Images.Integer_Image (Line)
            & " has a reference to field " & Text.Slice (I, J)
            & ".");
          Ok := False;
        end if;
        if Old_Num /= New_Num then
          Updated := True;
          Text.Replace (I, J, Images.Integer_Image (Natural (New_Num)));
        end if;
      end if;
      exit when I >= Text.Length;
      I := I + 1;
    end loop;

  end Update;

   procedure Insert_Nodes (Start_Node : in Xml_Parser.Node_Type;
                           Field_Num  : Afpx_Typ.Absolute_Field_Range;
                           From_Node  : in Xml_Parser.Node_Type;
                           Number     : in Afpx_Typ.Field_Range) is
    Next_Node_To, Next_Node_From : Xml_Parser.Node_Type;
    Start_Num : Afpx_Typ.Field_Range;
    use type Afpx_Typ.Field_Range;
  begin
    -- For Action Insert, Move or Copy
    if Field_Num = 0 then
      if Xml_Parser.Is_Valid (Start_Node) then
        -- Append first field as brother of Start_Node
        Add_Field (Start_Node, False, 1, From_Node, Next_Node_To);
      else
        -- Insert first field as First child of Dscr
        Add_Field (Dscr_Elt, True, 1, From_Node, Next_Node_To);
      end if;
      Next_Node_From := Next_Field (From_Node);
      Start_Num := 2;
    else
      Next_Node_To := Start_Node;
      Next_Node_From := From_Node;
      Start_Num := 1;
    end if;
    -- Append remaining brothers
    for I in Start_Num .. Number loop
      Add_Field (Next_Node_To, False, Field_Num + I, Next_Node_From,
                 Next_Node_To);
      Next_Node_From := Next_Field (Next_Node_From);
    end loop;
  end Insert_Nodes;

  procedure Delete_Nodes (Start_Node : in Xml_Parser.Node_Type;
                          Number     : in Afpx_Typ.Field_Range) is
    Curr_Node, Prev_Node, Next_Node : Xml_Parser.Node_Type;
    use type Afpx_Typ.Field_Range, Xml_Parser.Node_Kind_List;
  begin
    -- For Action Delete or Move
    Curr_Node := Start_Node;
    for I in 1 .. Number loop

      -- If previous node is indentation then remove it
      if Xml.Has_Brother (Curr_Node, False) then
        Prev_Node := Xml.Get_Brother (Curr_Node, False);
        if Prev_Node.Kind = Xml_Parser.Text
        and then Xml.Get_Text (Prev_Node) = Indent then
          Xml.Delete_Node (Prev_Node, Prev_Node);
        end if;
      end if;
      -- Get next field and delete current field
      Next_Node := Next_Field (Curr_Node);
      Xml.Delete_Node (Curr_Node, Curr_Node);
      if Debug then
        Basic_Proc.Put_Line_Output ("Delete Field");
      end if;

      -- Move to next field if any
      exit when I = Number;
      if not Xml_Parser.Is_Valid (Next_Node) then
          Error ("INTERNAL ERROR: No more Field");
      end if;
      Curr_Node := Next_Node;
    end loop;
  end Delete_Nodes;

  use type Afpx_Typ.Field_Range, Xml_Parser.Node_Kind_List;

begin

  ------------------
  -- Parse arguments
  ------------------
  Debug := Environ.Is_Yes ("AFPX_RNB_DEBUG");
  Args := Argument_Parser.Parse (Keys);
  if not Args.Is_Ok then
    Error ("Invalid arguments: " & Args.Get_Error);
  end if;
  if Args.Is_Set (1) then
    -- Help
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  -- General validity
  if Args.Get_Nb_Embedded_Arguments /= 0
  or else Args.Get_Nb_Occurences (Argument_Parser.No_Key_Index) > 0 then
    Error ("Invalid arguments");
  end if;

  -- Input file
  if Args.Is_Set (8) then
    Input_File_Name := As.U.Tus (Args.Get_Option (8));
    if Input_File_Name.Is_Null then
      Error ("Invalid argument: Empty input file name");
    end if;
  else
    Input_File_Name := As.U.Tus (Default_File_Name);
  end if;
  -- Output file
  if Args.Is_Set (9) then
    Output_File_Name := As.U.Tus (Args.Get_Option (9));
    if Output_File_Name.Is_Null then
      Error ("Invalid argument: Empty output file name");
    end if;
  else
    Output_File_Name := Input_File_Name;
  end if;
  -- Force
  Force := Args.Is_Set (10);

  -- Descriptor
  if not Args.Is_Set (2) then
    Error ("Invalid arguments: Missing descriptor");
  end if;
  begin
    Dscr := Afpx_Typ.Descriptor_Range'Value (Args.Get_Option (2));
  exception
    when others =>
      Error ("Invalid argument: Invalid descriptor " & Args.Get_Option (2));
  end;

  -- Action
  declare
    Nb_Action : Natural := 0;
    No_Key : Argument_Parser.The_Keys_Range;
  begin
    if Args.Is_Set (3) then
      Action := Insert;
      Nb_Action := Nb_Action + 1;
      No_Key := 3;
    end if;
    if Args.Is_Set (4) then
      Action := Delete;
      Nb_Action := Nb_Action + 1;
      No_Key := 4;
    end if;
    if Args.Is_Set (5) then
      Action := Move;
      Nb_Action := Nb_Action + 1;
      No_Key := 5;
    end if;
    if Args.Is_Set (6) then
      Action := Copy;
      Nb_Action := Nb_Action + 1;
      No_Key := 6;
    end if;
    if Nb_Action = 0 then
      Error ("Invalid arguments:. Missing action");
    end if;
    if Nb_Action /= 1 then
      Error ("Invalid arguments: Too many actions");
    end if;
    -- Parse field no or src:dst fields
    case Action is
      when Insert | Delete =>
        begin
          if Action = Insert then
            Field_Numi := Afpx_Typ.Absolute_Field_Range'Value (
                Args.Get_Option (No_Key));
          else
            Field_Numd := Afpx_Typ.Field_Range'Value (
                Args.Get_Option (No_Key));
          end if;
        exception
          when others =>
            Error ("Invalid argument: Invalid field num "
                 & Args.Get_Option (No_Key));
        end;
      when Move | Copy =>
        declare
          Nums : constant String := Args.Get_Option (No_Key);
          Sep : constant Natural := Str_Util.Locate (Nums, ":");
        begin
          Field_Numd := Afpx_Typ.Field_Range'Value (
                          Nums(Nums'First .. Sep - 1));
          Field_Numi := Afpx_Typ.Absolute_Field_Range'Value (
                          Nums(Sep + 1 .. Nums'Last));
        exception
          when others =>
            Error ("Invalid argument: Invalid field nums " & Nums);
        end;
    end case;
  end;

  -- Number
  if Args.Is_Set (7) then
    begin
      Number := Afpx_Typ.Field_Range'Value (Args.Get_Option (7));
    exception
      when others =>
      Error ("Invalid argument: Invalid number of fields "
        & Args.Get_Option (Argument_Parser.No_Key_Index));
    end;
  end if;

  -- Nothing when Moving from i to i or from i to i-1
  if Action = Move and then
  (Field_Numd = Field_Numi or else Field_Numi = Field_Numd - 1) then
    return;
  end if;

  -- Check no overlap
  if Action = Move or else Action = Copy then
    if Field_Numi >= Field_Numd
    and then Field_Numi < Field_Numd + Number then
      Error ("Invalid argument: source and destination overlap");
    end if;
  end if;


  -----------------
  -- Parse XML file
  -----------------
  declare
    Parse_Ok : Boolean;
    use Xml_Parser;
  begin
    Xml.Parse (Input_File_Name.Image, Parse_Ok,
               Comments => True,
               Cdata => Keep_Cdata_Section,
               Expand => False,
               Normalize => False,
               Use_Dtd => True, Dtd_File  => "",
               Namespace => False,
               Warn_Cb => null, Parse_Cb => null);
    if not Parse_Ok then
      Error ("Cannot parse file " & Input_File_Name.Image
        & ": " & Xml.Get_Parse_Error_Message, False);
    end if;
  exception
    when Xml_Parser.File_Error =>
      Error ("Cannot read XML file " & Input_File_Name.Image);
  end;


  ----------------
  -- Move to field
  ----------------
  -- Find descriptor
  declare
    Found : Boolean;
    use type Afpx_Typ.Descriptor_Range;
  begin
    Dscr_Elt := Xml.Get_Root_Element;
    Found := False;
    for I in 1 ..  Xml.Get_Nb_Children (Dscr_Elt) loop
      Tmp_Node := Xml.Get_Child (Dscr_Elt, I);
      if Tmp_Node.Kind = Xml_Parser.Element
      and then Xml.Get_Name (Tmp_Node) = "Descriptor"
      and then Afpx_Typ.Descriptor_Range'Value (
          Xml.Get_Attribute (Tmp_Node, Field_Num_Name)) = Dscr then
        Found := True;
        exit;
      end if;
    end loop;
    if not Found then
      Error ("Dscr " & Images.Integer_Image (Positive (Dscr))
           & " does not exist");
    end if;
    Dscr_Elt := Tmp_Node;
  end;

  -- Find fields
  if Action /= Delete then
    -- Insert or Move: Find start node, also counts fields
    Start_Nodei := Find_Field (Field_Numi);
  end if;

  if Action /= Insert then
    -- Delete, Move or Copy: Find node to delete, also counts fields
    --  In Copy, node to delete is simply not deleted
    Start_Noded:= Find_Field (Field_Numd);
    if Field_Numd + Number > Nb_Fields + 1 then
      -- Not enough fields to delete or copy
      Error ("Cannot " & Lower_Str (Action'Img)
            & " " & Images.Integer_Image (Positive (Number))
            & " fields from field "
            & Images.Integer_Image (Positive (Field_Numd))
            & " in descriptor " & Images.Integer_Image (Positive (Dscr)) );
    end if;
  end if;


  -----------
  -- Make Map
  -----------
  -- Map indexed by initial field nums xx and containing new nums XX
  -- (0 if deleted)
  for I in 1 .. Nb_Fields loop
    Field_Map.Append ( (I) );
  end loop;
  case Action is
    when Insert | Copy =>
      -- Add offest to fields after Field_Num
      for I in Field_Numi + 1 .. Nb_Fields loop
        Field_Map.Replace_Element (Positive(I), (I + Number) );
      end loop;
    when Delete =>
      -- Set 0 to Number fields from Field_Num included
      for I in Field_Numd .. Field_Numd + Number - 1 loop
        Field_Map.Replace_Element (Positive(I), (0) );
      end loop;
      -- Sub offset to following fields
      for I in Field_Numd + Number .. Nb_Fields loop
        Field_Map.Replace_Element (Positive(I), (I - Number) );
      end loop;
    when Move =>
      for I in 1 .. Nb_Fields loop
        Old_Num := Field_Map.Element (Positive(I));
        New_Num := Old_Num;
        if Old_Num >= Field_Numd
        and then Old_Num <= Field_Numd + Number - 1 then
          -- If field is moved then set new number
          New_Num := Old_Num - Field_Numd + Field_Numi + 1;
          if Field_Numi > Field_Numd then
            -- Move forward
            New_Num := New_Num - Number;
          end if;
        else
          -- Adjust new number for other fields, after deleted, or after
          --  inserted, or both
          if I > Field_Numd then
            New_Num := New_Num - Number;
          end if;
          if I > Field_Numi then
            New_Num := New_Num + Number;
          end if;
        end if;
        if New_Num /= Old_Num then
          Field_Map.Replace_Element (Positive(I), (New_Num));
        end if;
      end loop;
  end case;
  if Debug then
    Basic_Proc.Put_Output ("Translation map is: " );
    for I in 1 .. Field_Map.Length loop
      Basic_Proc.Put_Output (Images.Integer_Image (Positive (I)) & "->"
          & Images.Integer_Image (Natural (Field_Map.Element(I))) & ", ");
    end loop;
    Basic_Proc.New_Line_Output;
  end if;


  ------------------------------
  -- Insert and/or delete fields
  ------------------------------
  case Action is
    when Insert =>
      Insert_Nodes (Start_Nodei, Field_Numi, Start_Noded, Number);
    when Delete =>
      Delete_Nodes (Start_Noded, Number);
    when Move =>
      Insert_Nodes (Start_Nodei, Field_Numi, Start_Noded, Number);
      Delete_Nodes (Start_Noded, Number);
    when Copy =>
      Insert_Nodes (Start_Nodei, Field_Numi, Start_Noded, Number);
  end case;


  --------------
  -- Update Nums
  --------------
  declare
    Field : Xml_Parser.Node_Type;
    Child : Xml_Parser.Element_Type;
    Num_Read, Num_Comp : Afpx_Typ.Absolute_Field_Range;
    Geometry : Xml_Parser.Attributes_Array (1 .. 4);
    Txt : As.U.Asu_Us;
    Updated, Changed : Boolean;
  begin
    -- Find first field (variables before first field cannot reference fields)
    if Action /= Delete
    or else Number /= Nb_Fields then
      Field := Xml.Get_Child (Dscr_Elt, 1);
      if not Is_Field (Field) then
        Field := Next_Field (Field);
      end if;
    end if;
    Num_Comp := 1;

    Ok := True;
    -- From Field included up to end of Dscr, update field num and variable
    -- references
    -- Same for Vars
    loop
      exit when not Xml_Parser.Is_Valid (Field);

      if Xml.Get_Name (Field) = Field_Name then
        -- Field num
        Num_Read := Afpx_Typ.Absolute_Field_Range'Value (
            Xml.Get_Attribute (Field, Field_Num_Name));

        if Debug then
           Basic_Proc.Put_Line_Output (
               Images.Integer_Image (Positive (Num_Read)) & "->"
             & Images.Integer_Image (Positive (Num_Comp)));
        end if;
       if Num_Read /= Num_Comp then
          Xml.Set_Attribute (Field, Field_Num_Name,
            Images.Integer_Image(Positive(Num_Comp)));
        end if;
        Num_Comp := Num_Comp + 1;

        -- Field Geometry
        Child := Xml_Parser.No_Node;
        for I in 1 .. Xml.Get_Nb_Children (Field) loop
          Tmp_Node := Xml.Get_Child (Field, I);
          if Tmp_Node.Kind = Xml_Parser.Element
          and then Xml.Get_Name (Tmp_Node) = "Geometry" then
            Child := Tmp_Node;
            exit;
          end if;
        end loop;
        if not Xml_Parser.Is_Valid (Child) then
          Error ("INTERNAL ERROR: No child Geometry found");
        end if;
        begin
          Geometry := Xml.Get_Attributes (Child);
        exception
          when Constraint_Error =>
            Error ("INTERNAL ERROR: Invalid Geometry");
        end;
        Changed := False;
        for I in Geometry'Range loop
          Update (Geometry(I).Value, Xml.Get_Line_No (Tmp_Node), Updated);
          Changed := Changed or else Updated;
          if Debug and then Updated then
            Basic_Proc.Put_Line_Output ("Update Geometry " &
              Geometry(I).Name.Image & " to " & Geometry(I).Value.Image);
          end if;
        end loop;
        if Changed then
          Xml.Set_Attributes (Child, Geometry);
        end if;

        -- Field Init if any, brother of Geometry
        Tmp_Node := Child;
        Child := Xml_Parser.No_Node;
        while Xml.Has_Brother (Tmp_Node) loop
          Tmp_Node := Xml.Get_Brother (Tmp_Node);
          if Tmp_Node.Kind = Xml_Parser.Element
          and then Xml.Get_Name (Tmp_Node) = "Init" then
            Child := Tmp_Node;
            exit;
          end if;
        end loop;
        if Xml_Parser.Is_Valid (Child) then
          -- Get Text nodes
          for I in 1 .. Xml.Get_Nb_Children (Child) loop
            Tmp_Node := Xml.Get_Child (Child, I);
            if Tmp_Node.Kind = Xml_Parser.Text then
              Txt := Xml.Get_Text (Tmp_Node);
              Update (Txt, Xml.Get_Line_No (Tmp_Node), Updated);
              if Updated then
                Xml.Set_Text (Tmp_Node, Txt.Image);
                if Debug then
                  Basic_Proc.Put_Line_Output ("Update Init text to " &
                     Txt.Image);
                end if;
              end if;
            end if;
          end loop;
        end if;

      elsif Xml.Get_Name (Field) = "Var" then
        -- Var value
        begin
          Txt := Xml.Get_Attribute (Field, "Value");
        exception
          when Xml_Parser.Attribute_Not_Found =>
            Error ("INTERNAL ERROR: Invalid Var");
        end;
        Update (Txt, Xml.Get_Line_No (Field), Updated);
        if Updated then
          Xml.Set_Attribute (Field, "Value", Txt.Image);
          if Debug then
            Basic_Proc.Put_Line_Output ("Update Var "
                & Xml.Get_Attribute (Field, "Name")
                & " to " & Txt.Image);
          end if;
        end if;

      end if;

      -- Move to next Field or Var
      Tmp_Node := Field;
      Field := Xml_Parser.No_Node;
      while Xml.Has_Brother (Tmp_Node) loop
        Tmp_Node := Xml.Get_Brother (Tmp_Node);
        if Tmp_Node.Kind = Xml_Parser.Element
        and then  (Xml.Get_Name (Tmp_Node) /= Field_Name
           or else Xml.Get_Name (Tmp_Node) /= "Var") then
           Field := Tmp_Node;
           exit;
        end if;
      end loop;

    end loop;
  end;
  if not Ok and then Action = Delete and then not Force then
    Error ("Some deleted fields are referenced, aborting"
         & " (use ""force"" flag to overwrite).");
  end if;


  -----------------
  -- Commit changes
  -----------------
  -- Check XML
  Xml.Check (Ok);
  if not Ok then
    Error ("INTERNAL ERROR: Xml check failed: "
         & Xml.Get_Parse_Error_Message, False);
  end if;

  -- Write output
  Xml.Put (Output_File_Name.Image);

exception
  when Abort_Error =>
    Basic_Proc.Set_Error_Exit_Code;
  when Error: others =>
    Basic_Proc.Put_Line_Error ("Unexpected exception: "
      & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Afpx_Rnb;

