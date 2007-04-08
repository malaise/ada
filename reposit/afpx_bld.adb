with Ada.Text_Io, Ada.Direct_Io, Ada.Strings.Unbounded;
with Con_Io, Text_Handler, Normal, Argument, Directory,
     Upper_Str, Mixed_Str, Basic_Proc, Xml_Parser;
with Afpx_Typ;
use  Afpx_Typ;
-- Read AFPX.LIS, check it
-- Build AFPX.DSC list of Descr_Rec
--       AFPX.FLD list of Fields_Array
--       AFPX.INI list of Char_Str
procedure Afpx_Bld is

  -- Xml parser
  package Xp renames Xml_Parser;
  use type Xp.Node_Kind_List;

  -- Unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  use type Asu_Us;
  function Strof (Us : Asu_Us) return String renames Asu.To_String;
  function "&" (Str : String; Us : Asu_Us) return String is
  begin
    return Str & Strof (Us);
  end "&";

  -- Maximum length of each init string
  Max_Init_Length : constant := 132;

  -- Inputs name
  Default_List_File_Name : constant String := "Afpx.xml";
  List_File_Name : Text_Handler.Text (Directory.Max_Dir_Name_Len * 2);

  -- Direct_Io of descriptors, fields, init strings
  package Dscr_Io is new Ada.Direct_Io (Afpx_Typ.Descriptors_Array);
  Dscr_File : Dscr_Io.File_Type;
  package Fld_Io  is new Ada.Direct_Io (Afpx_Typ.Fields_Array);
  Fld_File : Fld_Io.File_Type;
  package Init_Io is new Ada.Direct_Io (Afpx_Typ.Char_Str);
  Init_File : Init_Io.File_Type;

  -- List of descriptors
  Descriptors : Afpx_Typ.Descriptors_Array;

  -- List of fields
  Fields : Afpx_Typ.Fields_Array;

  -- Index in Init_Str
  Init_Index : Positive;

  -- Initial characters of the fields
  Init_Str : Afpx_Typ.Char_Str;

  -- Errors
  Argument_Error : exception;
  File_Syntax_Error : exception;
  File_Not_Found : exception;

  -- Expected number of arguments
  Expected_Args : Natural;

  -- Roots of prologue and of parsed elements
  Prologue, Root : Xp.Element_Type;

  -- Close or deletes (on error) output files
  procedure Close (On_Error : in Boolean) is
  begin
    if On_Error and then Dscr_Io.Is_Open (Dscr_File) then
      begin
        Dscr_Io.Delete (Dscr_File);
      exception
        when others =>
          null;
      end;
      begin
        Fld_Io.Delete (Fld_File);
      exception
        when others =>
          null;
      end;
      begin
        Init_Io.Delete (Init_File);
      exception
        when others =>
          null;
      end;
    else
      begin
        Dscr_Io.Close (Dscr_File);
      exception
        when others =>
          null;
      end;
      begin
        Fld_Io.Close (Fld_File);
      exception
        when others =>
          null;
      end;
      begin
        Init_Io.Close (Init_File);
      exception
        when others =>
          null;
      end;
    end if;
  end Close;

  function Match (Str : Asu_Us; Keyword : String) return Boolean is
  begin
    -- Maybe one day we allow uppercase and lowercase keywords?
    return Strof (Str) = Keyword;
  end Match;

  procedure File_Error (Node : Xp.Node_Type; Msg : in String) is
  begin
    Basic_Proc.Put_Error ("Error: " & Msg);
    Basic_Proc.Put_Line_Error (
          " at line" & Positive'Image (Xp.Get_Line_No (Node))
        & " of file " & Text_Handler.Value(List_File_Name));
    raise File_Syntax_Error;
  end File_Error;

  -- Check and return size of root
  Root_Name : constant String := "Afpx_Descriptors";
  function Load_Size (Root : in Xp.Node_Type) return Con_Io.Full_Square is
    Size : Con_Io.Full_Square;
    Width, Height : Boolean := False;
  begin
    if Root.Kind /= Xp.Element
    or else not Match (Xp.Get_Name (Root), Root_Name) then
      File_Error (Root, "Invalid root, expected " & Root_Name);
    end if;
    if Xp.Get_Nb_Attributes (Root) = 0 then
      -- No attribute : default size
      return (Con_Io.Full_Def_Row_Last, Con_Io.Full_Def_Col_Last);
    elsif Xp.Get_Nb_Attributes (Root) /= 2 then
      File_Error (Root,
             "Afpx_Descriptors expects no attribute or Height and Width");
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Xp.Get_Attributes (Root);
      P : Positive;
    begin
      for I in Attrs'Range loop
        -- Load upper left then lower right
        if Match (Attrs(I).Name, "Height") then
          if Height then
            File_Error (Root, "Duplicated height " & Attrs(I).Name);
          end if;
          Height := True;
          P := Natural'Value(Strof (Attrs(I).Value));
          Size.Row := Con_Io.Full_Row_Range(P - 1);
        elsif Match (Attrs(I).Name, "Width") then
          if Width then
            File_Error (Root, "Duplicated width " & Attrs(I).Name);
          end if;
          Width := True;
          P := Natural'Value(Strof (Attrs(I).Value));
          Size.Col := Con_Io.Full_Col_Range(P - 1);
        else
          File_Error (Root, "Invalid Size " & Attrs(I).Name);
        end if;
      end loop;
    exception
      when File_Syntax_Error =>
        raise;
      when others =>
        File_Error (Root, "Invalid size");
        raise File_Syntax_Error;
    end;
    if not (Height and then Width) then
      File_Error (Root, "Invalid size. Missing some coordinate");
      raise File_Syntax_Error;
    end if;
    return Size;
  end Load_Size;

  -- Check and store upper_left and lower right
  procedure Load_Geometry (Node : in Xp.Node_Type;
                           Fn : in Afpx_Typ.Absolute_Field_Range;
                           Screen_Size : in Con_Io.Full_Square) is
    Up, Left, Low, Right : Boolean := False;
  begin
    if Node.Kind /= Xp.Element
    or else not Match (Xp.Get_Name (Node), "Geometry")
    or else Xp.Get_Nb_Attributes (Node) /= 4
    or else Xp.Get_Nb_Children (Node) /= 0 then
      File_Error (Node, "Invalid geometry " & Xp.Get_Name (Node)
                 & ", expected Up, Left, Down and Right");
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Xp.Get_Attributes (Node);
    begin
      for I in Attrs'Range loop
        -- Load upper left then lower right
        if Match (Attrs(I).Name, "Up") then
          if Up then
            File_Error (Node, "Duplicated coordinate " & Attrs(I).Name);
          end if;
          Up := True;
          Fields(Fn).Upper_Left.Row :=
           Con_Io.Row_Range'Value(Strof (Attrs(I).Value));
        elsif Match (Attrs(I).Name, "Left") then
          if Left then
            File_Error (Node, "Duplicated coordinate " & Attrs(I).Name);
          end if;
          Left := True;
          Fields(Fn).Upper_Left.Col :=
           Con_Io.Col_Range'Value(Strof (Attrs(I).Value));
        elsif Match (Attrs(I).Name, "Low") then
          if Low then
            File_Error (Node, "Duplicated coordinate " & Attrs(I).Name);
          end if;
          Low := True;
          Fields(Fn).Lower_Right.Row :=
           Con_Io.Row_Range'Value(Strof (Attrs(I).Value));
        elsif Match (Attrs(I).Name, "Right") then
          if Right then
            File_Error (Node, "Duplicated coordinate " & Attrs(I).Name);
          end if;
          Right := True;
          Fields(Fn).Lower_Right.Col :=
           Con_Io.Col_Range'Value(Strof (Attrs(I).Value));
        else
          File_Error (Node, "Invalid geometry " & Attrs(I).Name);
        end if;
      end loop;
    exception
      when File_Syntax_Error =>
        raise;
      when others =>
        File_Error (Node, "Invalid geometry");
    end;
    if not (Up and then Left and then Low and then Right) then
      File_Error (Node, "Invalid geometry. Missing some coordinate");
    end if;
    if      Fields(Fn).Upper_Left.Row > Fields(Fn).Lower_Right.Row
    or else Fields(Fn).Upper_Left.Col > Fields(Fn).Lower_Right.Col
    then
      File_Error (Node, "Invalid geometry. Upper_left < lower_right");
    end if;

    -- Compute size and check vs screen max
    Fields(Fn).Height :=
     Fields(Fn).Lower_Right.Row - Fields(Fn).Upper_Left.Row + 1;
    if Fields(Fn).Height - 1 > Screen_Size.Row then
      File_Error (Node, "Invalid geometry. Field is higher than screen");
    end if;
    Fields(Fn).Width :=
     Fields(Fn).Lower_Right.Col - Fields(Fn).Upper_Left.Col + 1;
    if Fields(Fn).Width - 1 > Screen_Size.Col then
      File_Error (Node, "Invalid geometry. Field is wider than screen");
    end if;

    -- One Row for Get fields
    if Fields(Fn).Kind = Afpx_Typ.Get and then Fields(Fn).Height /= 1 then
      File_Error (Node, "Invalid geometry. Get fields must have ONE row");
    end if;
  end Load_Geometry;

  procedure Load_Colors (Node : in Xp.Node_Type;
                         Fn : in Afpx_Typ.Absolute_Field_Range) is
    Foreground, Background, Blink, Selected : Boolean := False;
  begin
    if Node.Kind /= Xp.Element
    or else not Match (Xp.Get_Name (Node), "Colors")
    or else Xp.Get_Nb_Children (Node) /= 0 then
      File_Error (Node, "Invalid colors " & Xp.Get_Name (Node));
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Xp.Get_Attributes (Node);
    begin
      for I in Attrs'Range loop
        if Match (Attrs(I).Name, "Foreground") then
          if Foreground then
            File_Error (Node, "Duplicated Color " & Attrs(I).Name);
          end if;
          Foreground := True;
          Fields(Fn).Colors.Foreground :=
           Con_Io.Effective_Colors'Value (Strof (Attrs(I).Value));
        elsif Match (Attrs(I).Name, "Background") then
          if Background then
            File_Error (Node, "Duplicated Color " & Attrs(I).Name);
          end if;
          Background := True;
          Fields(Fn).Colors.Background :=
           Con_Io.Effective_Basic_Colors'Value (Strof (Attrs(I).Value));
        elsif Match (Attrs(I).Name, "Blink") then
          if Blink then
            File_Error (Node, "Duplicated Color " & Attrs(I).Name);
          end if;
          Blink := True;
          if Boolean'Value (Strof (Attrs(I).Value)) then
            Fields(Fn).Colors.Blink_Stat := Con_Io.Blink;
          else
            Fields(Fn).Colors.Blink_Stat := Con_Io.Not_Blink;
          end if;
        elsif Match (Attrs(I).Name, "Selected") then
          if Selected then
            File_Error (Node, "Duplicated Color " & Attrs(I).Name);
          end if;
          Selected := True;
          Fields(Fn).Colors.Selected :=
           Con_Io.Effective_Basic_Colors'Value (Strof (Attrs(I).Value));
        else
          File_Error (Node, "Invalid Color " & Attrs(I).Name);
        end if;
      end loop;

      -- Parse colors
      if Fn = 0 or else Fields(Fn).Kind = Afpx_Typ.Get then
        if Xp.Get_Nb_Attributes (Node) /= 3
        or else not (Foreground and then Background and then Selected)
        or else Blink then
          File_Error (Node,
                      "Expected colors for foreground, background and selected");
        end if;
        Fields(Fn).Colors.Blink_Stat := Con_Io.Not_Blink;
      elsif Fields(Fn).Kind = Put then
        if Xp.Get_Nb_Attributes (Node) /= 3
        or else not (Foreground and then Background and then Blink)
        or else Selected then
          File_Error (Node,
                      "Expected colors for foreground, blink and background");
        end if;
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;
      elsif Fields(Fn).Kind = Button then
        if Xp.Get_Nb_Attributes (Node) /= 2
        or else not (Foreground and then Background)
        or else Blink or else Selected then
          File_Error (Node,
                      "Expected colors for foreground and background");
        end if;
        Fields(Fn).Colors.Blink_Stat := Con_Io.Not_Blink;
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;
      end if;
    exception
      when File_Syntax_Error =>
        raise;
      when others =>
        File_Error (Node, "Invalid colors specification");
    end;

    -- Foreground has to be basic for all but Put fields
    if (Fn = 0 or else Fields(Fn).Kind /= Put)
    and then Fields(Fn).Colors.Foreground
             not in Con_Io.Effective_Basic_Colors then
      -- For list, Get and Button, Foreground has to be basic
      File_Error (Node,
                  "For all but Put fields, Foreground has to be basic color");
    end if;
  end Load_Colors;

  procedure Load_List (Node : in Xp.Node_Type;
                       Screen_Size : in Con_Io.Full_Square) is
  begin
    if Node.Kind /= Xp.Element
    or else Xp.Get_Nb_Attributes (Node) /= 0
    or else Xp.Get_Nb_Children (Node) /= 2 then
      File_Error (Node, "Invalid list definition");
    end if;
    -- In List
    Fields(0).Kind := Afpx_Typ.Button;
    Fields(0).Activated := True;
    Fields(0).Isprotected := False;
    Load_Geometry (Xp.Get_Child (Node, 1), 0, Screen_Size);
    Load_Colors (Xp.Get_Child (Node, 2), 0);
    Fields(0).Char_Index := 1;
  end Load_List;

  procedure Loc_Load_Field (Node : in Xp.Node_Type;
                            No : in Afpx_Typ.Field_Range;
                            Screen_Size : in Con_Io.Full_Square)  is
    Attrs : constant Xp.Attributes_Array := Xp.Get_Attributes (Node);
    Child : Xp.Node_Type;
    First_Init : Boolean;
    Prev_Init_Square : Con_Io.Full_Square;
    -- Location in field of init string
    Finit_Square : Con_Io.Full_Square;
    -- Init string
    Finit_String : Asu_Us;
    Finit_Length : Positive;
    -- Index in Char_Str of beginning of Init string
    Finit_Index : Afpx_Typ.Char_Str_Range;
    -- Are num and kind of field, and row and col of init set
    Kind, Num, Row, Col : Boolean := False;
  begin
    if Attrs'Length /= 2 then
      File_Error (Node, "Invalid field definition, expected Num and Kind");
    end if;
    for I in Attrs'Range loop
      if Match (Attrs(I).Name, "Num") then
        if Num then
          File_Error (Node, "Duplicated num of field");
        end if;
        Num := True;
        begin
          if Afpx_Typ.Field_Range'Value(Strof (Attrs(I).Value)) /= No then
            raise Constraint_Error;
          end if;
        exception
          when others =>
            File_Error (Node,
                        "Invalid field number. They must crescent positives");
        end;
      elsif Match (Attrs(I).Name, "Kind") then
        if Kind then
          File_Error (Node, "Duplicated kind of field");
        end if;
        Kind := True;
        if not Match (Attrs(I).Value, "Put")
        and then not Match (Attrs(I).Value, "Get")
        and then not Match (Attrs(I).Value, "Button") then
          File_Error (Node, "Invalid field kind. Put, Get or Button expected");
        end if;
        Fields(No).Kind := Afpx_Typ.Field_Kind_List'Value(
                                  Strof (Attrs(I).Value));
      else
        File_Error (Node, "Invalid field attribute " & Attrs(I).Name);
      end if;
    end loop;
    if not (Num and then Kind) then
      File_Error (Node, "Invalid field definition, expected Num and Kind");
    end if;
    Fields(No).Activated := True;
    Fields(No).Isprotected := False;
    Load_Geometry (Xp.Get_Child (Node, 1), No, Screen_Size);
    Load_Colors (Xp.Get_Child (Node, 2), No);

    -- Check global number of characters for the field
    Fields(No).Char_Index := Init_Index;
    Finit_Length := Fields(No).Height * Fields(No).Width;
    begin
      Init_Index := Init_Index + Finit_Length + 1;
    exception
      when others =>
        File_Error (Node, "Too many init characters for this field");
    end;

    First_Init := True;
    Prev_Init_Square := (0, 0);
    for I in 3 .. Xp.Get_Nb_Children (Node) loop
      Child := Xp.Get_Child (Node, I);
      if not Match (Xp.Get_Name (Child), "Init")
      or else Xp.Get_Nb_Attributes (Child) < 2
      or else Xp.Get_Nb_Children (Child) > 1 then
        File_Error (Child, "Invalid Init, expected row, col and text");
      end if;

      declare
        Child_Attrs : constant Xp.Attributes_Array := Xp.Get_Attributes (Child);
      begin
        Row := False;
        Col := False;
        for I in Child_Attrs'Range loop
          if Match (Child_Attrs(I).Name, "Row") then
            if Row then
              File_Error (Child, "Duplicated coordinate");
            end if;
            Row := True;
            Finit_Square.Row :=
             Con_Io.Row_Range'Value(Strof (Child_Attrs(I).Value));
          elsif Match (Child_Attrs(I).Name, "Col") then
            if Col then
              File_Error (Child, "Duplicated coordinate");
            end if;
            Col := True;
            Finit_Square.Col :=
             Con_Io.Row_Range'Value(Strof (Child_Attrs(I).Value));
          elsif Match (Child_Attrs(I).Name, "xml:space") then
            -- Discard
            null;
          else
            File_Error (Child, "Invalid Init attribute " & Child_Attrs(I).Name);
          end if;
        end loop;
        if not (Row and then Col) then
          File_Error (Child, "Invalid Init, expected row and col");
        end if;
      exception
        when File_Syntax_Error =>
          raise;
        when others =>
          File_Error (Child, "Invalid init row or col");
      end;

      -- Check init squares crescent
      if not First_Init then
        if Finit_Square.Row < Prev_Init_Square.Row then
          File_Error (Child, "Invalid init row. Must be crescent");
        elsif    Finit_Square.Row  = Prev_Init_Square.Row
        and then Finit_Square.Col <= Prev_Init_Square.Col then
          File_Error (Child,
                      "Invalid init col. Must be crescent and not overlap");
        end if;
      end if;
      First_Init := False;
      Prev_Init_Square := Finit_Square;

      -- Check init square is in field
      if not Afpx_Typ.In_Field (Fields(No), Finit_Square) then
        File_Error (Child, "Init row or col not in field");
      end if;

      -- Get the whole line to extract init string
      if Xp.Get_Nb_Children (Child) = 1 then
        declare
          Child_Child : constant Xp.Node_Type := Xp.Get_Child (Child, 1);
        begin
          if Child_Child.Kind /= Xp.Text then
            File_Error (Child_Child, "Invalid init string");
          end if;
          Finit_String := Xp.Get_Text (Child_Child);
          Finit_Length := Asu.Length (Finit_String);
          -- Check Finit col + string length compatible with field width
          if not Afpx_Typ.In_Field (Fields(No),
             (Finit_Square.Row, Finit_Square.Col + Finit_Length - 1)) then
            File_Error (Child_Child,
                        "Init string too long for this col in this field");
          end if;
        end;
        -- Update prev_init_square col to last char of init string
        Prev_Init_Square.Col := Finit_Square.Col + Finit_Length - 1;
        -- Copy in init string
        Finit_Index := Fields(No).Char_Index
         + Finit_Square.Row * Fields(No).Width
         + Finit_Square.Col;
        Init_Str (Finit_Index .. Finit_Index + Finit_Length - 1) :=
          Strof (Finit_String);
      end if;
    end loop;
  end Loc_Load_Field;

  procedure Check_Overlap (Dscr_No  : in Descriptor_Range;
                           Fi1, Fi2 : in Absolute_Field_Range) is
    F1 : Field_Rec renames Fields(Fi1);
    F2 : Field_Rec renames Fields(Fi2);

  begin
            -- F1 above F2
    if      F1.Upper_Left.Row  > F2.Lower_Right.Row
            -- F1 below F2
    or else F1.Lower_Right.Row < F2.Upper_Left.Row
            -- F1 left to F2
    or else F1.Upper_Left.Col  > F2.Lower_Right.Col
            --  F1 right to F2
    or else F1.Lower_Right.Col < F2.Upper_Left.Col
    then
      -- No overlap
      return;
    end if;
    if Fi1 = 0 then
      Basic_Proc.Put_Error ("Error : List");
    else
      Basic_Proc.Put_Error ("Error : Field " & Afpx_Typ.Field_Range'Image(Fi1));
    end if;
    Basic_Proc.Put_Line_Error (" and Field " & Afpx_Typ.Field_Range'Image(Fi2)
                        & " of descriptor "
                        & Afpx_Typ.Descriptor_Range'Image(Dscr_No)
                        & " overlap.");
    raise File_Syntax_Error;
  end Check_Overlap;

  procedure Load_Dscrs (Root : Xp.Element_Type;
                        Check_Only : in Boolean) is
    Size : Con_Io.Full_Square;
    Dscr_Index : Afpx_Typ.Descriptor_Range;
    Dscr_No    : Afpx_Typ.Descriptor_Range;
    Child, Child_Child : Xp.Node_Type;

  begin
    -- Parse size
    Size := Load_Size (Root);

    -- If not check_only, delete then create binary files
    if not Check_Only then
      begin
        Dscr_Io.Open (Dscr_File, Dscr_Io.In_File,
                      Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Dscr_File_Name);
        Dscr_Io.Delete (Dscr_File);
      exception
        when Dscr_Io.Name_Error => null;
      end;
      Dscr_Io.Create (Dscr_File, Dscr_Io.Out_File,
                      Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Dscr_File_Name);

      begin
        Fld_Io.Open (Fld_File, Fld_Io.In_File,
                     Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Fld_File_Name);
        Fld_Io.Delete (Fld_File);
      exception
        when Fld_Io.Name_Error => null;
      end;
      Fld_Io.Create (Fld_File, Fld_Io.Out_File,
                     Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Fld_File_Name);

      begin
        Init_Io.Open (Init_File, Init_Io.In_File,
                      Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Init_File_Name);
        Init_Io.Delete (Init_File);
      exception
        when Init_Io.Name_Error => null;
      end;
      Init_Io.Create (Init_File, Init_Io.Out_File,
                      Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Init_File_Name);
    end if;


    -- Initialize the descriptors array as not used
    for I in Afpx_Typ.Descriptor_Range loop
      Descriptors(I).Version  := Afpx_Typ.Afpx_Version;
      Descriptors(I).Size  := Size;
      Descriptors(I).Modified := False;
      Descriptors(I).Dscr_Index := Afpx_Typ.Descriptor_Range'First;
      Descriptors(I).Nb_Fields := 0;
    end loop;

    -- Loop on descriptors
    -- Descriprors are stored in the descriptor file at Dscr_No
    -- Fields and init tables are stored in their files at Dscr_Index
    Dscr_Index := 1;
    Dscrs:
    for I in 1 .. Xp.Get_Nb_Children (Root) loop
      -- Descriptor
      Child := Xp.Get_Child (Root, I);
      if Child.Kind /= Xp.Element then
        File_Error (Child, "Unexpected text as descriptor");
      end if;
      if not Match (Xp.Get_Name (Child), "Descriptor") then
        File_Error (Child, "Expected Descriptor");
      end if;
      if Xp.Get_Nb_Attributes (Child) /= 1
      or else not Match (Xp.Get_Attribute (Child, 1).Name, "Num") then
        File_Error (Child, "Expected descriptor Num");
      end if;
      begin
        Dscr_No := Afpx_Typ.Descriptor_Range'Value (
                    Strof (Xp.Get_Attribute (Child, 1).Value));
      exception
        when others =>
        File_Error (Child, "Invalid descriptor num");
      end;
      Ada.Text_Io.Put_Line ("   descriptor " &
                        Normal(Integer(Dscr_No), 2, Gap => '0'));
      -- Dscr no has to be unique
      if Descriptors(Dscr_No).Modified then
        File_Error (Child,
                    "Descriptor " & Afpx_Typ.Descriptor_Range'Image(Dscr_No)
                  & " already defined");
      end if;
      -- Init dscr and fields array. No list at init
      Descriptors(Dscr_No).Modified := True;
      Descriptors(Dscr_No).Dscr_Index := Dscr_Index;
      Descriptors(Dscr_No).Nb_Fields := 0;
      Init_Index := 1;
      Init_Str := (others => ' ');
      Fields(0).Kind := Put;

      for I in 1 .. Xp.Get_Nb_Children (Child) loop
        Child_Child := Xp.Get_Child (Child, I);
        if Child_Child.Kind /= Xp.Element then
          File_Error (Child_Child, "Unexpected text as field");
        end if;
        if I = 1 and then Match(Xp.Get_Name (Child_Child), "List") then
          Load_List (Child_Child, Size);
        elsif not Match(Xp.Get_Name (Child_Child), "Field") then
          File_Error (Child_Child, "Unexpected field");
        elsif Descriptors(Dscr_No).Nb_Fields > Afpx_Typ.Field_Range'Last then
          File_Error (Child_Child, "Too many fields. Maximum is"
           & Field_Range'Image(Afpx_Typ.Field_Range'Last) & " per descriptor");
        else
          Descriptors(Dscr_No).Nb_Fields := Descriptors(Dscr_No).Nb_Fields + 1;
          Loc_Load_Field (Child_Child, Descriptors(Dscr_No).Nb_Fields, Size);
        end if;
      end loop;

      -- Check no overlapping of fields
      if Fields(0).Kind /= Put then
        -- Check list and each field
        for J in 1 .. Descriptors(Dscr_No).Nb_Fields loop
          Check_Overlap (Dscr_No, 0, J);
        end loop;
      end if;
      -- Check each field with others
      for I in 1 .. Descriptors(Dscr_No).Nb_Fields - 1 loop
        for J in I + 1 .. Descriptors(Dscr_No).Nb_Fields loop
          Check_Overlap (Dscr_No, I, J);
        end loop;
      end loop;

      -- If not check_only, write fields and init
      if not Check_Only then
        Fld_Io.Write  (Fld_File , Fields,   Fld_Io.Positive_Count(Dscr_Index));
        Init_Io.Write (Init_File, Init_Str, Init_Io.Positive_Count(Dscr_Index));
      end if;

      Dscr_Index := Dscr_Index + 1;
    end loop Dscrs;

    if not Check_Only then
      Dscr_Io.Write (Dscr_File, Descriptors);
    end if;

  exception
    when others =>
      Close (True);
      raise;
  end Load_Dscrs;

begin
  -- Help
  begin
    Argument.Get_Parameter (List_File_Name, Param_Key => "h");
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                        & " [ -l<afpx_list_file> ] [ -d<destination_dir> ]");
    return;
  exception
    when others =>
      null;
  end;

  -- Source file and dest path arguments
  Expected_Args := 0;
  begin
    Argument.Get_Parameter (List_File_Name, Param_Key => "l");
    if Text_Handler.Empty (List_File_Name) then
      raise Argument_Error;
    end if;
    -- Argument found
    Expected_Args := Expected_Args + 1;
  exception
    when Argument.Argument_Not_Found =>
      Text_Handler.Set (List_File_Name, Default_List_File_Name);
    when others =>
      raise Argument_Error;
  end;

  begin
    Argument.Get_Parameter (Afpx_Typ.Dest_Path, Param_Key => "d");
    if Text_Handler.Empty (Afpx_Typ.Dest_Path) then
      raise Argument_Error;
    end if;
    -- Argument found
    Expected_Args := Expected_Args + 1;
  exception
    when Argument.Argument_Not_Found =>
      Text_Handler.Set (Afpx_Typ.Dest_Path, ".");
    when others =>
      raise Argument_Error;
  end;

  if Argument.Get_Nbre_Arg /= Expected_Args then
    raise Argument_Error;
  end if;

  Ada.Text_Io.Put_Line ("Reading " & Text_Handler.Value(List_File_Name));
  Ada.Text_Io.Put_Line ("Writing in " & Text_Handler.Value(Afpx_Typ.Dest_Path));
  Text_Handler.Append (Afpx_Typ.Dest_Path, "/");

  -- First check
  Ada.Text_Io.Put_Line ("Parsing:");
  begin
    Xp.Parse (Text_Handler.Value(List_File_Name), Prologue, Root);
  exception
    when Xp.File_Error =>
      Basic_Proc.Put_Line_Error ("Error accessing file "
                            & Text_Handler.Value(List_File_Name));
      raise File_Not_Found;
    when Xp.Parse_Error =>
      Basic_Proc.Put_Line_Error (Xp.Get_Parse_Error_Message);
      raise File_Syntax_Error;
  end;

  Ada.Text_Io.Put_Line ("Checking:");
  Load_Dscrs(Root, True);
  -- Then write
  Ada.Text_Io.Put_Line ("Building:");
  Load_Dscrs(Root, False);
  Ada.Text_Io.Put_Line ("Done.");
exception
  when Argument_Error =>
    Close (True);
    Basic_Proc.Put_Line_Error ("Argument error. Try -h option.");
    Basic_Proc.Set_Error_Exit_Code;
  when File_Not_Found =>
    Close (True);
    Basic_Proc.Put_Line_Error ("Directory or file not found. Try -h option.");
    Basic_Proc.Set_Error_Exit_Code;
  when File_Syntax_Error =>
    Close (True);
    Basic_Proc.Put_Line_Error ("Syntax error.");
    Basic_Proc.Set_Error_Exit_Code;
  when others =>
    Close (True);
    Basic_Proc.Put_Line_Error ("Unexpected exception.");
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Afpx_Bld;

