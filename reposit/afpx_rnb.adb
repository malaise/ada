-- Insert or remove fields in a Afpx.xml file
-- Update other field number and references to them
with Ada.Exceptions;
with Basic_Proc, Argument, Argument_Parser, Environ,
     As.U, Integer_Image, Unbounded_Arrays, String_Mng, Text_Line,
     Afpx_Typ, Xml_Parser.Generator;
procedure Afpx_Rnb is
    -- Options
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"),       False),
    2 => (True,  'D', As.U.Tus ("descriptor"), False, True, As.U.Tus ("descriptor_num")),
    3 => (True,  'i', As.U.Tus ("insert"),     False, True, As.U.Tus ("field_num")),
    4 => (True,  'd', As.U.Tus ("delete"),     False, True, As.U.Tus ("field_num")),
    5 => (True,  'f', As.U.Tus ("file"),       False, True, As.U.Tus ("file_name")),
    6 => (True,  'o', As.U.Tus ("output"),     False, True, As.U.Tus ("file_name")),
    7 => (False, 'F', As.U.Tus ("force"),      False));
  Args : Argument_Parser.Parsed_Dscr;
  Default_File_Name : constant String := "Afpx.xml";

  -- Usage and Error message
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " [ <file> ] [ <output> ] [ <force> ] <descriptor> <action> [ <number> ]");
    Basic_Proc.Put_Line_Error (
       " <file>       ::= " & Argument_Parser.Image (Keys(5)));
    Basic_Proc.Put_Line_Error (
       " <output>     ::= " & Argument_Parser.Image (Keys(6)));
    Basic_Proc.Put_Line_Error (
       " <force>      ::= " & Argument_Parser.Image (Keys(7)));
    Basic_Proc.Put_Line_Error (
       " <descriptor> ::= " & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error (
       " <action>     ::= <insert> | <delete>");
    Basic_Proc.Put_Line_Error (
       " <insert>     ::= " & Argument_Parser.Image (Keys(3)));
    Basic_Proc.Put_Line_Error (
       " <delete>     ::= " & Argument_Parser.Image (Keys(4)));
    Basic_Proc.Put_Line_Error (
       "Insert <number> fields after <field_num> (<field_num>=0 for from 1)");
    Basic_Proc.Put_Line_Error (
       "  or delete <number> fields from <field_num> included.");
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

  -- Complete an element:
  -- Insert before element a Text "Lf   "
  -- Add to element the attributes Num and Kind,
  -- Add to element the children Geometry and Colors with attribtues
  Ini_Attrs : Boolean := False;
  Fld_Attrs : Xml_Parser.Attributes_Array (1 ..2);
  Geo_Attrs : Xml_Parser.Attributes_Array (1 ..4);
  Col_Attrs : Xml_Parser.Attributes_Array (1 ..2);
  procedure Complete (Ctx : in out Xml_Parser.Generator.Ctx_Type;
                      Field : in out Xml_Parser.Element_Type;
                      Fld_Num : in Positive) is
    Indent : Xml_Parser.Text_Type;
    Child : Xml_Parser.Element_Type;
  begin
    if not Ini_Attrs then
      -- Init global attributes once
      Fld_Attrs(1).Name  := As.U.Tus ("Num");
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
    -- Prepend a Text with Lf + 4 spaces
    Ctx.Add_Brother (Field, Text_Line.Line_Feed_Str & "    ",
                     Xml_Parser.Text, Indent, Next => False);
    -- Set field attributes with provided Num
    Fld_Attrs(1).Value := As.U.Tus (Integer_Image (Fld_Num));
    Ctx.Set_Attributes (Field, Fld_Attrs);
    -- Insert Geometry
    Ctx.Add_Child (Field, "Geometry", Xml_Parser.Element, Child);
    Ctx.Set_Put_Empty (Child, True);
    Ctx.Set_Attributes (Child, Geo_Attrs);
    -- Insert Colors
    Ctx.Add_Child (Field, "Colors", Xml_Parser.Element, Child);
    Ctx.Set_Put_Empty (Child, True);
    Ctx.Set_Attributes (Child, Col_Attrs);
  end Complete;

  -- Arguments and ENV
  Input_File_Name, Output_File_Name : As.U.Asu_Us;
  Force : Boolean := False;
  Dscr : Afpx_Typ.Descriptor_Range;
  type Action_List is (Insert, Delete);
  Action : Action_List;
  Field_Num : Afpx_Typ.Absolute_Field_Range;
  Number : Afpx_Typ.Field_Range := 1;
  Debug : Boolean;

  -- XML descriptor and nodes
  Xml : Xml_Parser.Generator.Ctx_Type;
  -- Node to append to when Field_Num = 0, No_Node => 1st child of Dscr
  Start_Node : Xml_Parser.Node_Type;
  -- The descriptor and reference field
  Dscr_Elt, Field_Elt : Xml_Parser.Element_Type;
  -- Tempo node
  Tmp_Node : Xml_Parser.Node_Type;

  -- Number if fields initialy in the dscr
  Nb_Fields : Afpx_Typ.Absolute_Field_Range;
  -- Map of new field nums
  type Field_Array is array (Positive range <>)
                            of  Afpx_Typ.Absolute_Field_Range;
  package Field_Map_Mng is new Unbounded_Arrays (
                    Afpx_Typ.Absolute_Field_Range,
                    Field_Array);
  Field_Map : Field_Map_Mng.Unb_Array;

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
    Old_Num : Afpx_Typ.Field_Range;
    use type Afpx_Typ.Field_Range;
  begin
    I := 0;
    Updated := False;
    loop
      I := String_Mng.Locate (Text.Image, "${Field_", I);
      exit when I = 0;
      if not String_Mng.Is_Backslashed (Text.Image, I) then
        -- I and J are indexes of Field Num in Text
        I := I + 8;
        begin
          J := String_Mng.Locate (Text.Image, ".", I) - 1;
          Old_Num := Afpx_Typ.Absolute_Field_Range'Value (
                 Text.Slice (I, J));
          New_Num := Field_Map.Element (Positive (Old_Num));
        exception
          when others =>
            Error ("INTERNAL ERROR: Cannot update field num");
        end;
        if New_Num = 0 then
          -- Reference to a deleted field
          Basic_Proc.Put_Line_Output ("Warning: Text """ & Input.Image
            & """ at line " & Integer_Image (Line)
            & " has a reference to field " & Text.Slice (I, J)
            & ".");
          Ok := False;
        end if;
        if Old_Num /= New_Num then
          Updated := True;
          Text.Replace (I, J, Integer_Image (Natural (New_Num)));
        end if;
      end if;
      exit when I >= Text.Length;
      I := I + 1;
    end loop;

  end Update;

  use type Afpx_Typ.Field_Range, Xml_Parser.Node_Kind_List;

begin


  ------------------
  -- Parse arguments
  ------------------
  Debug := Environ.Is_Yes ("AFPX_RENUMBER_DEBUG");
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
  or else Args.Get_Nb_Occurences (Argument_Parser.No_Key_Index) > 1 then
    Error ("Invalid arguments.");
  end if;

  -- Input file
  if Args.Is_Set (5) then
    Input_File_Name := As.U.Tus (Args.Get_Option (5));
    if Input_File_Name.Is_Null then
      Error ("Invalid argument: Empty input file name");
    end if;
  else
    Input_File_Name := As.U.Tus (Default_File_Name);
  end if;
  -- Output file
  if Args.Is_Set (6) then
    Output_File_Name := As.U.Tus (Args.Get_Option (6));
    if Output_File_Name.Is_Null then
      Error ("Invalid argument: Empty output file name");
    end if;
  else
    Output_File_Name := Input_File_Name;
  end if;
  -- Force
  Force := Args.Is_Set (7);

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
    if Nb_Action = 0 then
      Error ("Invalid arguments:. Missing action");
    end if;
    if Nb_Action /= 1 then
      Error ("Invalid arguments: Too many actions");
    end if;
    begin
      Field_Num := Afpx_Typ.Absolute_Field_Range'Value (
          Args.Get_Option (No_Key));
    exception
      when others =>
        Error ("Invalid argument: Invalid field num "
             & Args.Get_Option (No_Key));
    end;
  end;
  if Field_Num = 0 and then Action /= Insert then
    Error ("Invalid argument: Field 0 is only allowed when inserting");
  end if;

  -- Number
  if Args.Is_Set (Argument_Parser.No_Key_Index) then
    begin
      Number := Afpx_Typ.Field_Range'Value (
          Args.Get_Option (Argument_Parser.No_Key_Index));
    exception
      when others =>
      Error ("Invalid argument: Invalid number of fields "
        & Args.Get_Option (Argument_Parser.No_Key_Index));
    end;
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
    -- Last node not text before first field
    Prev_Node : Xml_Parser.Node_Type;
    use type Afpx_Typ.Descriptor_Range;
  begin
    Dscr_Elt := Xml.Get_Root_Element;
    Found := False;
    for I in 1 ..  Xml.Get_Nb_Children (Dscr_Elt) loop
      Tmp_Node := Xml.Get_Child (Dscr_Elt, I);
      if Tmp_Node.Kind = Xml_Parser.Element
      and then Xml.Get_Name (Tmp_Node) = "Descriptor"
      and then Afpx_Typ.Descriptor_Range'Value (
          Xml.Get_Attribute (Tmp_Node, "Num")) = Dscr then
        Found := True;
        exit;
      end if;
    end loop;
    if not Found then
      Error ("Dscr " & Integer_Image (Positive (Dscr)) & " does not exist");
    end if;
    Dscr_Elt := Tmp_Node;

    -- Find field
    -- If Insert(0) then set Start_Node to last node not text before first field
    --  No_Node means that first field has to be inserted as first child of Dscr
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
      and then Xml.Get_Name (Tmp_Node) = "Field" then
        Nb_Fields := Nb_Fields + 1;
        -- Store last node not text before first field
        if Field_Num = 0 and then Nb_Fields = 1 then
          Start_Node := Prev_Node;
        end if;
        if Afpx_Typ.Field_Range'Value (
          Xml.Get_Attribute (Tmp_Node, "Num")) = Field_Num then
          Found := True;
          Field_Elt := Tmp_Node;
        end if;
      end if;
    end loop;

    -- Check that field is found
    if Field_Num /= 0 and then not Found then
      Error ("Field " & Integer_Image (Positive (Field_Num))
          & " does not exist in descriptor "
          & Integer_Image (Positive (Dscr)));
    end if;
    if Debug then
      Basic_Proc.Put_Line_Output ("Got " & Integer_Image (Positive (Nb_Fields))
          & " fields");
    end if;

    -- Check that there are enough fields to delete
    if Action = Delete and then Field_Num + Number > Nb_Fields + 1 then
      Error ("Cannot delete " & Integer_Image (Positive (Number))
            & " fields from field " & Integer_Image (Positive (Field_Num))
            & " in descriptor " & Integer_Image (Positive (Dscr)) );
    end if;
  end;


  -----------
  -- Make Map
  -----------
  -- Map indexed by initial field nums xx and contaning new nums XX
  -- (0 if deleted)
  begin
    for I in 1 .. Nb_Fields loop
      Field_Map.Append ( (I) );
    end loop;
    case Action is
      when Insert =>
        -- Add offest to fields after Field_Num
        for I in Field_Num + 1 .. Nb_Fields loop
          Field_Map.Replace_Element (Positive(I), (I + Number) );
        end loop;
      when Delete =>
        -- Set 0 to Number fields from Field_Num included
        for I in Field_Num .. Field_Num + Number - 1 loop
          Field_Map.Replace_Element (Positive(I), (0) );
        end loop;
        -- Sub offset to following fields
        for I in Field_Num + Number .. Nb_Fields loop
          Field_Map.Replace_Element (Positive(I), (I - Number) );
        end loop;
    end case;
    if Debug then
      Basic_Proc.Put_Output ("Translation map is: " );
      for I in 1 .. Field_Map.Length loop
        Basic_Proc.Put_Output (Integer_Image (Positive (I)) & "->"
            & Integer_Image (Natural (Field_Map.Element(I))) & ", ");
      end loop;
      Basic_Proc.New_Line_Output;
    end if;
  end;


  ----------------
  -- Update Fields
  ----------------
  -- Insert / delete
  -- Finally update Field_Elt to the one to start with
  --  and Update Field_Num to its new num
  --  if Insert => First after inserted, Num + Number
  --  if Delete => the new field now at the place of Field_Elt, Num
  declare
    Prev_Node, Next_Node : Xml_Parser.Node_Type;
  begin
    case Action is
      when Insert =>

        if Field_Num = 0 then
          if Xml_Parser.Is_Valid (Start_Node) then
            -- Append first field as brother of Start_Node
            Xml.Add_Brother (Start_Node, "Field", Xml_Parser.Element, Tmp_Node);
          else
            -- Append first field as first child of Dscr
            Xml.Add_Child (Dscr_Elt, "Field", Xml_Parser.Element, Tmp_Node,
                           Append => False);
          end if;
          Complete (Xml, Tmp_Node, 1);
          if Debug then
            Basic_Proc.Put_Line_Output ("Insert Field 1");
          end if;
          -- Append remaining brothers
          for I in 2 .. Positive(Number) loop
            Xml.Add_Brother (Tmp_Node, "Field", Xml_Parser.Element, Tmp_Node);
            Complete (Xml, Tmp_Node, I);
            if Debug then
              Basic_Proc.Put_Line_Output ("Append Field" & I'Img);
            end if;
          end loop;
        else
          -- Append brothers
          Tmp_Node := Field_Elt;
          for I in 1 .. Positive(Number) loop
            Xml.Add_Brother (Tmp_Node, "Field", Xml_Parser.Element, Tmp_Node);
            Complete (Xml, Tmp_Node, Positive(Field_Num) + I);
            if Debug then
              Basic_Proc.Put_Line_Output ("Append Field" & I'Img);
            end if;
          end loop;
        end if;

        -- Now set Field_Elt to the next Field after last inserted, Tmp_Node
        --  (if any)
        loop
          if not Xml.Has_Brother (Tmp_Node) then
            Field_Elt := Xml_Parser.No_Node;
            exit;
          end if;
          Tmp_Node := Xml.Get_Brother (Tmp_Node);
          if Tmp_Node.Kind = Xml_Parser.Element
          and then Xml.Get_Name (Tmp_Node) = "Field" then
            Field_Elt := Tmp_Node;
            exit;
          end if;
        end loop;
        Field_Num := Field_Num + Number;

      when Delete =>
        Tmp_Node := Field_Elt;
        Fields:
        for I in 1 .. Number loop
          -- Has current field a successor (even a Var)
          if Xml.Has_Brother (Tmp_Node) then
            Next_Node :=  Xml.Get_Brother (Tmp_Node);
          else
            Next_Node := Xml_Parser.No_Node;
          end if;
          -- If previous node is Text then remove indentation
          -- It is Lf and maybe spaces => set it to "Lf"
          if Xml.Has_Brother (Tmp_Node, False) then
            Prev_Node := Xml.Get_Brother (Tmp_Node, False);
            if Prev_Node.Kind = Xml_Parser.Text then
              Xml.Set_Text (Prev_Node, Text_Line.Line_Feed_Str);
            end if;
          end if;
          -- Delete current field
          Xml.Delete_Node (Tmp_Node, Tmp_Node);
          if Debug then
            Basic_Proc.Put_Line_Output ("Delete Field");
          end if;

          -- Move to next field if any
          Field_Elt := Xml_Parser.No_Node;
          if not Xml_Parser.Is_Valid (Next_Node) then
            if I = Number then
              -- No Field_Elt to start updates from
              exit;
            else
              -- There is no mor child to delete
              Error ("INTERNAL ERROR: No more Child");
            end if;
          end if;
          -- Restart from this next node
          Tmp_Node := Next_Node;
          -- Skip until it is a field, if any
          Skip:
          while Tmp_Node.Kind /= Xml_Parser.Element
          or else Xml.Get_Name (Tmp_Node) /= "Field" loop
            if Xml.Has_Brother (Tmp_Node) then
              Tmp_Node := Xml.Get_Brother (Tmp_Node);
            elsif I = Number then
              -- No Field_Elt to start updates from
              exit Fields;
            else
              Error ("INTERNAL ERROR: No more Field");
            end if;
          end loop Skip;
          -- We have found a valid next field to start from
          Field_Elt := Tmp_Node;
        end loop Fields;
    end case;
  end;



  --------------
  -- Update Nums
  --------------
  declare
    Child : Xml_Parser.Element_Type;
    Geometry : Xml_Parser.Attributes_Array (1 .. 4);
    Txt : As.U.Asu_Us;
    Updated, Changed : Boolean;
  begin
    Ok := True;
    -- From Field_Elt included up to end of Dscr, update
    -- Field_Elt = No_Node means that there is no more field after the
    --  fields inserted or deleted
    if Xml_Parser.Is_Valid (Field_Elt) then
      loop

        if Xml.Get_Name (Field_Elt) = "Field" then
          -- Field num
          if Debug then
            Basic_Proc.Put_Line_Output ("Update Field num " &
              Xml.Get_Attribute (Field_Elt, "Num")
              & "->" & Field_Num'Img);
          end if;
          Xml.Set_Attribute (Field_Elt, "Num",
                             Integer_Image(Positive(Field_Num)));
          Field_Num := Field_Num + 1;

          -- Field Geometry
          Child := Xml_Parser.No_Node;
          for I in 1 .. Xml.Get_Nb_Children (Field_Elt) loop
            Tmp_Node := Xml.Get_Child (Field_Elt, I);
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

        elsif Xml.Get_Name (Field_Elt) = "Var" then
          -- Var value
          begin
            Txt := Xml.Get_Attribute (Field_Elt, "Value");
          exception
            when Xml_Parser.Attribute_Not_Found =>
              Error ("INTERNAL ERROR: Invalid Var");
          end;
          Update (Txt, Xml.Get_Line_No (Field_Elt), Updated);
          if Updated then
            Xml.Set_Attribute (Field_Elt, "Value", Txt.Image);
            if Debug then
              Basic_Proc.Put_Line_Output ("Update Var "
                  & Xml.Get_Attribute (Field_Elt, "Name")
                  & " to " & Txt.Image);
            end if;
          end if;

        end if;

        -- Move to next Field or Var
        Tmp_Node := Field_Elt;
        Field_Elt := Xml_Parser.No_Node;
        while Xml.Has_Brother (Tmp_Node) loop
          Tmp_Node := Xml.Get_Brother (Tmp_Node);
          if Tmp_Node.Kind = Xml_Parser.Element
          and then  (Xml.Get_Name (Tmp_Node) /= "Field"
             or else Xml.Get_Name (Tmp_Node) /= "Var") then
             Field_Elt := Tmp_Node;
             exit;
          end if;
        end loop;

        exit when not Xml_Parser.Is_Valid (Field_Elt);
      end loop;
    end if;
  end;
  if not Ok and then not Force then
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

