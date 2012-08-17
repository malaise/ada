with Ada.Text_Io, Ada.Direct_Io;
with As.U;
with Con_Io, Normal, Argument,
     Mixed_Str, Basic_Proc, Xml_Parser,
     Ada_Words, Parser, String_Mng, Computer, Int_Image,
     Language;
with Afpx_Typ;
-- Read Afpx.xml, check it
-- Build AFPX.DSC list of Descr_Rec
--       AFPX.FLD list of Fields_Array
--       AFPX.INI list of Char_Str
-- Build cross reference Ada package
procedure Afpx_Bld is

  -- Xml parser
  package Xp renames Xml_Parser;
  use type Xp.Node_Kind_List;
  Ctx : Xp.Ctx_Type;

  -- Computer memory
  Memory : Computer.Memory_Type;

  -- Unbounded strings
  subtype Asu_Us is As.U.Asu_Us;
  function "&" (Str : String; Us : Asu_Us) return String is
  begin
    return Str & Us.Image;
  end "&";

  -- Inputs name
  Default_List_File_Name : constant String := "Afpx.xml";
  List_File_Name : Asu_Us;

  -- Direct_Io of descriptors, fields, init strings
  package Dscr_Io is new Ada.Direct_Io (Afpx_Typ.Descriptors_Array);
  Dscr_File : Dscr_Io.File_Type;
  package Fld_Io  is new Ada.Direct_Io (Afpx_Typ.Fields_Array);
  Fld_File : Fld_Io.File_Type;
  package Init_Io is new Ada.Direct_Io (Afpx_Typ.Char_Str);
  Init_File : Init_Io.File_Type;

  -- Ada package to generate
  package Xref is
    Invalid_Identifier : exception;
    Identifier_Redefined : exception;
    -- Setting file/dscr/field name
    procedure Set_Package_Name (Name : in Asu_Us);
    procedure Set_Dscr_Name (Dscr : in Afpx_Typ.Descriptor_Range;
                             Name : in Asu_Us);
    procedure Set_Field_Name (Dscr : in Afpx_Typ.Descriptor_Range;
                              Field : in Afpx_Typ.Field_Range;
                              Name : in Asu_Us);

    -- Generate the file if the package name has been set
    procedure Generate;

    -- Clean all dscr and field names
    procedure Clean;
  end Xref;
  package body Xref is separate;


  -- The color names and default background
  Color_Names : Afpx_Typ.Color_Names := (others => Afpx_Typ.No_Color);
  Color_Defs : Con_Io.Colors_Definition;
  Default_Background : Con_Io.Effective_Colors;
  Colors_Loaded : Boolean;

  -- List of descriptors
  Descriptors : Afpx_Typ.Descriptors_Array;

  -- List of fields
  Fields : Afpx_Typ.Fields_Array;
  use type Afpx_Typ.Absolute_Field_Range;
  use type Afpx_Typ.Field_Kind_List;

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
    return Str.Image = Keyword;
  end Match;

  procedure File_Error (Node : in Xp.Node_Type; Msg : in String) is
  begin
    Basic_Proc.Put_Error ("Error: " & Msg);
    Basic_Proc.Put_Line_Error (
          " at line" & Positive'Image (Ctx.Get_Line_No (Node))
        & " of file " & List_File_Name.Image);
    raise File_Syntax_Error;
  end File_Error;

  -- For parsing variable names
  function Is_Dot (C : Character) return Boolean is
  begin
    return C = '.';
  end Is_Dot;

  -- Check a variable and add it to computer
  procedure Add_Variable (Node : in Xp.Node_Type;
                          Name : in String;
                          Value : in String;
                          Modifiable : in Boolean;
                          Persistent : in Boolean) is

    Iter : Parser.Iterator;
  begin
    -- Check name
    -- Name must not be empty, not start nor end by '.' nor have ".."
    if Name = ""
    or else Name (Name'First) = '.'
    or else Name (Name'Last) = '.'
    or else String_Mng.Locate (Name, "..") /= 0 then
      File_Error (Node, "Invalid variable name " & Name);
    end if;
    -- Strings between '.' must be valid identifiers
    Parser.Set (Iter, Name, Is_Dot'Unrestricted_Access);
    loop
      declare
        Word : constant String := Parser.Next_Word (Iter);
      begin
        exit when Word = "";
        if not Ada_Words.Is_Identifier (Word) then
          File_Error (Node, "Invalid variable name " & Name);
        end if;
      end;
    end loop;
    Parser.Del (Iter);
    -- Checked OK, store
    Memory.Set (Name, Value, Modifiable, Persistent);
  exception
    when Computer.Constant_Exists =>
      if Modifiable then
        File_Error (Node, "A constant with this name "
                        & Name & " already exists");
      else
        File_Error (Node, "A variable or constant with this name "
                        & Name & " already exists");
      end if;
    when File_Syntax_Error =>
      raise;
    when others =>
      File_Error (Node, "Cannot add variable " & Name);
      raise File_Syntax_Error;
  end Add_Variable;

  -- Geometry variable value
  function Geo_Image is new Int_Image (Natural);

  -- Check and return size of screen
  Root_Name : constant String := "Afpx_Descriptors";
  function Load_Size (Root : in Xp.Node_Type) return Con_Io.Square is
    Size : Con_Io.Square;
    Height, Width : Boolean;
  begin
    if Root.Kind /= Xp.Element
    or else not Match (Ctx.Get_Name (Root), Root_Name) then
      File_Error (Root, "Invalid root, expected " & Root_Name);
    end if;
    -- Add constant persistent of up left
    Add_Variable (Root, "Screen.Up", Geo_Image (0), False, True);
    Add_Variable (Root, "Screen.Left", Geo_Image (0), False, True);
    if Ctx.Get_Nb_Attributes (Root) = 0 then
      -- No attribute : default size
      Size := (Con_Io.Def_Row_Last, Con_Io.Def_Col_Last);
      -- Add constant persistent
      Add_Variable (Root, "Screen.Low", Geo_Image (Size.Row), False, True);
      Add_Variable (Root, "Screen.Right", Geo_Image (Size.Col), False, True);
      Add_Variable (Root, "Screen.Height", Geo_Image (Size.Row + 1),
                    False, True);
      Add_Variable (Root, "Screen.Width", Geo_Image (Size.Col + 1),
                    False, True);
      -- Add
      return Size;
    elsif Ctx.Get_Nb_Attributes (Root) /= 2 then
      File_Error (Root,
     "Afpx_Descriptors expects either no attribute or both Height and Width");
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Root);
      Err_Val : Asu_Us;
      P : Positive;
    begin
      for I in Attrs'Range loop
        Err_Val := Attrs(I).Value;
        -- Load upper left then lower right
        if Match (Attrs(I).Name, "Height") then
          Height := True;
          P := Memory.Compute (Attrs(I).Value.Image);
          Size.Row := Con_Io.Row_Range(P - 1);
          -- Add constant persistent
          Add_Variable (Root, "Screen.Height", Geo_Image (Size.Row + 1), False, True);
        elsif Match (Attrs(I).Name, "Width") then
          Width := True;
          P := Memory.Compute (Attrs(I).Value.Image);
          Size.Col := Con_Io.Col_Range(P - 1);
          -- Add constant persistent
          Add_Variable (Root, "Screen.Width", Geo_Image (Size.Col + 1), False, True);
        else
          File_Error (Root, "Invalid Size " & Attrs(I).Name);
        end if;
      end loop;
    exception
      when File_Syntax_Error =>
        raise;
      when Computer.Unknown_Variable =>
        File_Error (Root, "Unknown variable when evaluating "
                        & Err_Val.Image);
      when others =>
        File_Error (Root, "Invalid size");
        raise File_Syntax_Error;
    end;
    if not (Height and then Width) then
      File_Error (Root, "Invalid size. Missing some coordinate");
      raise File_Syntax_Error;
    end if;
    Add_Variable (Root, "Screen.Low", Geo_Image (Size.Row), False, True);
    Add_Variable (Root, "Screen.Right", Geo_Image (Size.Col), False, True);
    return Size;
  exception
    when File_Syntax_Error =>
      raise;
    when others =>
      File_Error (Root, "Invalid size definition");
      raise File_Syntax_Error;
  end Load_Size;

  -- Load a user defined variable
  procedure Load_Variable (Node : in Xp.Node_Type;
                           Persistent : in Boolean) is
  begin
    if Ctx.Get_Nb_Attributes (Node) /= 2 then
      File_Error (Node,
          "Invalid variable definition, expects Name and Value");
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Node);
    begin
      if Match (Attrs(1).Name, "Name")
      and then Match (Attrs(2).Name, "Value") then
        Add_Variable (Node, Attrs(1).Value.Image,
                            Attrs(2).Value.Image,
                            True, Persistent);
      elsif Match (Attrs(1).Name, "Value")
      and then Match (Attrs(2).Name, "Name") then
        Add_Variable (Node, Attrs(2).Value.Image,
                            Attrs(1).Value.Image,
                            True, Persistent);
      else
        File_Error (Node,
            "Invalid variable definition, expects Name and Value");
      end if;
    end;
  exception
    when File_Syntax_Error =>
      raise;
    when others =>
      File_Error (Node, "Invalid variable definition");
      raise File_Syntax_Error;
  end Load_Variable;

  -- Name of variable of a field
  function Fn_Image is new Int_Image (Afpx_Typ.Absolute_Field_Range);
  function Name_Of (Fn : Afpx_Typ.Absolute_Field_Range) return String is
  begin
    if Fn = 0 then
      return "List";
    else
      return "Field_" & Fn_Image(Fn);
    end if;
  end Name_Of;

  function Color_Image (Color : Con_Io.Effective_Colors) return String is
  begin
    return Mixed_Str (Con_Io.Color_Name_Of (Color));
  end Color_Image;

  -- Load the variables with names of colors
  procedure Load_Color_Names (Node : in Xp.Node_Type) is
  begin
    -- Load colors only once despite called several times
    if Colors_Loaded then
      return;
    end if;
    Colors_Loaded := True;
    -- Define variables Colorxy
    for I in Con_Io.Effective_Colors'Range loop
      Add_Variable (Node,
                    Mixed_Str (Con_Io.Effective_Colors'Image (I)),
                    Color_Defs(I).Image, False, True);
    end loop;
    Add_Variable (Node, "Default_Background", Color_Image (Default_Background),
                  False, True);
  end Load_Color_Names;

  -- Parse the (optional) definition of color names
  procedure Define_Color_Names (Node : in Xp.Element_Type) is
    Child : Xp.Element_Type;
    Attrs : Xp.Attributes_Array (1 .. 2);
    Id, Color, Default_Background_Name : Asu_Us;
    Index : Con_Io.Effective_Colors;
  begin
    -- Overwrite some colors
    for I in 1 .. Ctx.Get_Nb_Children (Node) loop
      Child := Ctx.Get_Child (Node, I);
      if Ctx.Get_Name (Child) = "Color_Definition" then
        -- Color_Definition: attributes Id and Color
        Attrs := Ctx.Get_Attributes (Child);
        if Attrs(1).Name.Image = "Id" then
          Id := Attrs(1).Value;
          Color := Attrs(2).Value;
        else
          Color := Attrs(1).Value;
          Id := Attrs(2).Value;
        end if;
        Index := Con_Io.Effective_Colors'Value(Id.Image);
        Color_Defs(Index) := As.U.Tus (Memory.Eval (Color.Image));
      else
        -- Default_Background: attribute Color
        -- Last child
        Attrs(1) := Ctx.Get_Attribute (Child, 1);
        Default_Background_Name := Attrs(1).Value;
      end if;
    end loop;

    Color_Names := Afpx_Typ.To_Names (Color_Defs);
    Con_Io.Set_Colors (Color_Defs);
    if not Default_Background_Name.Is_Null then
      Default_Background := Con_Io.Color_Of (Memory.Eval (
                                 Default_Background_Name.Image));
    end if;
    Load_Color_Names (Node);
  exception
    when File_Syntax_Error =>
      raise;
    when others =>
      File_Error (Node, "Invalid colors re-definition");
  end Define_Color_Names;

  -- Check and store upper_left and lower right
  procedure Load_Geometry (Node : in Xp.Node_Type;
                           Fn : in Afpx_Typ.Absolute_Field_Range;
                           Screen_Size : in Con_Io.Square) is
    -- Add a geometry constant, not persistent
    procedure Add_Geo (Name : in String; Value : in Natural) is
    begin
      Add_Variable (Node, Name_Of (Fn) & "." & Name, Geo_Image (Value),
                    False, False);
    end Add_Geo;
    Up, Left, Low, Right : Boolean := False;
    Nb_Verti, Nb_Horiz : Natural := 0;
  begin
    if Node.Kind /= Xp.Element
    or else not Match (Ctx.Get_Name (Node), "Geometry")
    or else Ctx.Get_Nb_Attributes (Node) /= 4
    or else Ctx.Get_Nb_Children (Node) /= 0 then
      File_Error (Node, "Invalid geometry " & String'(Ctx.Get_Name (Node))
                 & ", expected Up, Left, Low, Right, Width, Height");
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Node);
      Err_Val : Asu_Us;
    begin
      for I in Attrs'Range loop
        Err_Val := Attrs(I).Value;
        -- Load upper left and lower right
        if Match (Attrs(I).Name, "Up") then
          Up := True;
          Nb_Verti := Nb_Verti + 1;
          Fields(Fn).Upper_Left.Row :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Up", Fields(Fn).Upper_Left.Row);
        elsif Match (Attrs(I).Name, "Left") then
          Left := True;
          Nb_Horiz := Nb_Horiz + 1;
          Fields(Fn).Upper_Left.Col :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Left", Fields(Fn).Upper_Left.Col);
        elsif Match (Attrs(I).Name, "Low") then
          Low := True;
          Nb_Verti := Nb_Verti + 1;
          Fields(Fn).Lower_Right.Row :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Low", Fields(Fn).Lower_Right.Row);
        elsif Match (Attrs(I).Name, "Right") then
          Right := True;
          Nb_Horiz := Nb_Horiz + 1;
          Fields(Fn).Lower_Right.Col :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Right", Fields(Fn).Lower_Right.Col);
        elsif Match (Attrs(I).Name, "Height") then
          Nb_Verti := Nb_Verti + 1;
          Fields(Fn).Height :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Height", Fields(Fn).Height);
        elsif Match (Attrs(I).Name, "Width") then
          Nb_Horiz := Nb_Horiz + 1;
          Fields(Fn).Width :=
               Memory.Compute (Attrs(I).Value.Image);
          Add_Geo ("Width", Fields(Fn).Width);
        else
          File_Error (Node, "Invalid geometry " & Attrs(I).Name);
        end if;
      end loop;
    exception
      when File_Syntax_Error =>
        raise;
      when Computer.Unknown_Variable =>
        File_Error (Node, "Unknown variable when evaluating "
                        & Err_Val.Image);
      when others =>
        File_Error (Node, "Invalid geometry");
    end;
    -- 2 on each dim must be set
    if Nb_Horiz < 2 or else Nb_Verti < 2 then
      File_Error (Node, "Invalid geometry. Missing some coordinate");
    elsif Nb_Horiz > 2 or else Nb_Verti > 2 then
      File_Error (Node, "Invalid geometry. Too many coordinates");
    end if;

    -- Compute missing data
    if not Up then
      Fields(Fn).Upper_Left.Row :=
       Fields(Fn).Lower_Right.Row - Fields(Fn).Height + 1;
      Add_Geo ("Up", Fields(Fn).Upper_Left.Row);
    elsif not Low then
      Fields(Fn).Lower_Right.Row :=
       Fields(Fn).Upper_Left.Row + Fields(Fn).Height - 1;
      Add_Geo ("Low", Fields(Fn).Lower_Right.Row);
    else
      Fields(Fn).Height :=
       Fields(Fn).Lower_Right.Row - Fields(Fn).Upper_Left.Row + 1;
      Add_Geo ("Height", Fields(Fn).Height);
    end if;
    if not Left then
      Fields(Fn).Upper_Left.Col :=
       Fields(Fn).Lower_Right.Col - Fields(Fn).Width + 1;
      Add_Geo ("Left", Fields(Fn).Upper_Left.Col);
    elsif not Right then
      Fields(Fn).Lower_Right.Col :=
       Fields(Fn).Upper_Left.Col + Fields(Fn).Width - 1;
      Add_Geo ("Right", Fields(Fn).Lower_Right.Col);
    else
      Fields(Fn).Width :=
       Fields(Fn).Lower_Right.Col - Fields(Fn).Upper_Left.Col + 1;
      Add_Geo ("Width", Fields(Fn).Width);
    end if;

    -- Sizes must be positive
    if      Fields(Fn).Upper_Left.Row > Fields(Fn).Lower_Right.Row
    or else Fields(Fn).Upper_Left.Col > Fields(Fn).Lower_Right.Col
    then
      File_Error (Node, "Invalid geometry. Upper_left < lower_right");
    end if;

    -- Must be in screen
    if Fields(Fn).Lower_Right.Row > Screen_Size.Row then
      File_Error (Node, "Invalid geometry. Field is higher than screen");
    end if;
    if Fields(Fn).Lower_Right.Col > Screen_Size.Col then
      File_Error (Node, "Invalid geometry. Field is wider than screen");
    end if;

    -- One Row for Get fields
    if Fields(Fn).Kind = Afpx_Typ.Get and then Fields(Fn).Height /= 1 then
      File_Error (Node, "Invalid geometry. Get fields must have ONE row");
    end if;

  exception
    when Constraint_Error =>
      File_Error (Node, "Invalid geometry");
  end Load_Geometry;

  procedure Load_Colors (Node : in Xp.Node_Type;
                         Fn : in Afpx_Typ.Absolute_Field_Range) is
    Foreground, Background, Selected : Boolean := False;

  begin
    if Node.Kind /= Xp.Element
    or else not Match (Ctx.Get_Name (Node), "Colors")
    or else Ctx.Get_Nb_Children (Node) /= 0 then
      File_Error (Node, "Invalid colors " & String'(Ctx.Get_Name (Node)));
    end if;
    declare
      Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Node);
      Err_Val : Asu_Us;
    begin
      for I in Attrs'Range loop
        Err_Val := Attrs(I).Value;
        if Match (Attrs(I).Name, "Foreground") then
          Foreground := True;
          Fields(Fn).Colors.Foreground := Con_Io.Color_Of (
                 Memory.Eval (Attrs(I).Value.Image));
          Add_Variable (Node, Name_Of (Fn) & "." & "Foreground",
              Color_Image (Fields(Fn).Colors.Foreground), False, False);
        elsif Match (Attrs(I).Name, "Background") then
          Background := True;
          Fields(Fn).Colors.Background := Con_Io.Color_Of (
                 Memory.Eval (Attrs(I).Value.Image));
          Add_Variable (Node, Name_Of (Fn) & "." & "Background",
              Color_Image (Fields(Fn).Colors.Background), False, False);
        elsif Match (Attrs(I).Name, "Selected") then
          Selected := True;
          Fields(Fn).Colors.Selected := Con_Io.Color_Of (
                 Memory.Eval (Attrs(I).Value.Image));
          Add_Variable (Node, Name_Of (Fn) & "." & "Selected",
              Color_Image (Fields(Fn).Colors.Selected), False, False);
        else
          File_Error (Node, "Invalid Color " & Attrs(I).Name);
        end if;
      end loop;

      -- Parse colors
      if Fn = 0 or else Fields(Fn).Kind = Afpx_Typ.Get then
        if Ctx.Get_Nb_Attributes (Node) /= 3
        or else not (Foreground and then Background and then Selected) then
          File_Error (Node,
                      "Expected colors for foreground, background and selected");
        end if;
      elsif Fields(Fn).Kind = Afpx_Typ.Put then
        if Ctx.Get_Nb_Attributes (Node) /= 2
        or else not (Foreground and then Background)
        or else Selected then
          File_Error (Node,
                      "Expected colors for foreground and background");
        end if;
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;
        Add_Variable (Node, Name_Of (Fn) & "." & "Selected",
            Color_Image (Fields(Fn).Colors.Selected), False, False);
      elsif Fields(Fn).Kind = Afpx_Typ.Button then
        if Ctx.Get_Nb_Attributes (Node) /= 2
        or else not (Foreground and then Background)
        or else Selected then
          File_Error (Node,
                      "Expected colors for foreground and background");
        end if;
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;
        Add_Variable (Node, Name_Of (Fn) & "." & "Selected",
            Color_Image (Fields(Fn).Colors.Selected), False, False);
      end if;
    exception
      when File_Syntax_Error =>
        raise;
      when Computer.Unknown_Variable =>
        File_Error (Node, "Unknown variable when evaluating "
                        & Err_Val.Image);
      when others =>
        File_Error (Node, "Invalid colors specification");
    end;

  end Load_Colors;

  procedure Load_List (Node : in Xp.Node_Type;
                       Screen_Size : in Con_Io.Square) is
  begin
    if Node.Kind /= Xp.Element
    or else Ctx.Get_Nb_Attributes (Node) /= 0
    or else Ctx.Get_Nb_Children (Node) /= 2 then
      File_Error (Node, "Invalid list definition");
    end if;
    -- In List
    Fields(0).Kind := Afpx_Typ.Button;
    Fields(0).Activated := True;
    Fields(0).Isprotected := False;
    Load_Geometry (Ctx.Get_Child (Node, 1), 0, Screen_Size);
    Load_Colors (Ctx.Get_Child (Node, 2), 0);
    Fields(0).Char_Index := 1;
  end Load_List;

  procedure Loc_Load_Field (Node : in Xp.Node_Type;
                            Dscr : in Afpx_Typ.Descriptor_Range;
                            No : in Afpx_Typ.Field_Range;
                            Screen_Size : in Con_Io.Square)  is
    Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Node);
    Child : Xp.Node_Type;
    First_Init : Boolean;
    Prev_Init_Square : Con_Io.Square;
    Name : Asu_Us;
    -- Location in field of init string
    Finit_Square : Con_Io.Square;
    -- Init string
    Finit_String : Asu_Us;
    Finit_Length : Positive;
    -- Index in Char_Str of beginning of Init string
    Finit_Index : Afpx_Typ.Char_Str_Range;
  begin
    for I in Attrs'Range loop
      if Match (Attrs(I).Name, "Num") then
        begin
          if Afpx_Typ.Field_Range'Value(Attrs(I).Value.Image) /= No then
            raise Constraint_Error;
          end if;
        exception
          when others =>
            File_Error (Node,
                        "Invalid field number. They must crescent positives");
        end;
      elsif Match (Attrs(I).Name, "Kind") then
        if not Match (Attrs(I).Value, "Put")
        and then not Match (Attrs(I).Value, "Get")
        and then not Match (Attrs(I).Value, "Button") then
          File_Error (Node, "Invalid field kind. Put, Get or Button expected");
        end if;
        Fields(No).Kind := Afpx_Typ.Field_Kind_List'Value(
                                  Attrs(I).Value.Image);
      elsif Match (Attrs(I).Name, "Name") then
        begin
          Name := As.U.Tus (Memory.Eval (Attrs(I).Value.Image));
        exception
          when Computer.Unknown_Variable =>
            File_Error (Node, "Unknown variable when evaluating "
                            & Attrs(I).Value.Image);
        end;
      end if;
    end loop;
    Fields(No).Activated := True;
    Fields(No).Isprotected := False;
    -- Set Field name
    if not Name.Is_Null then
      begin
        Xref.Set_Field_Name (Dscr, No, Name);
      exception
        when Xref.Invalid_Identifier =>
          File_Error (Node, "Invalid field name");
        when Xref.Identifier_Redefined =>
          File_Error (Node,
            "A field with this name already exists in the descriptor");
      end;
    end if;
    Load_Geometry (Ctx.Get_Child (Node, 1), No, Screen_Size);
    Load_Colors (Ctx.Get_Child (Node, 2), No);

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
    for I in 3 .. Ctx.Get_Nb_Children (Node) loop
      Child := Ctx.Get_Child (Node, I);
      if not Match (Ctx.Get_Name (Child), "Init")
      or else Ctx.Get_Nb_Attributes (Child) < 2
      or else Ctx.Get_Nb_Children (Child) > 1 then
        File_Error (Child, "Invalid Init, expected row, col and text");
      end if;

      declare
        Child_Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Child);
        Err_Val : Asu_Us;
      begin
        for I in Child_Attrs'Range loop
          Err_Val := Child_Attrs(I).Value;
          if Match (Child_Attrs(I).Name, "Row") then
            Finit_Square.Row :=
             Memory.Compute(Child_Attrs(I).Value.Image);
          elsif Match (Child_Attrs(I).Name, "Col") then
            Finit_Square.Col :=
             Memory.Compute(Child_Attrs(I).Value.Image);
          elsif Match (Child_Attrs(I).Name, "xml:space") then
            -- Discard
            null;
          else
            File_Error (Child, "Invalid Init attribute "
                             & Child_Attrs(I).Value);
          end if;
        end loop;
      exception
        when File_Syntax_Error =>
          raise;
        when Computer.Unknown_Variable =>
          File_Error (Node, "Unknown variable when evaluating "
                          & Err_Val.Image);
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
      if Ctx.Get_Nb_Children (Child) = 1 then
        declare
          Child_Child : constant Xp.Node_Type := Ctx.Get_Child (Child, 1);
        begin
          if Child_Child.Kind /= Xp.Text then
            File_Error (Child_Child, "Invalid init string");
          end if;
          Finit_String := As.U.Tus (Memory.Eval (
                   Ctx.Get_Text (Child_Child)));
          -- Length in term of put positions (also in term of wide characters)
          Finit_Length := Language.Put_Length (Finit_String.Image);
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
          Language.String_To_Unicode (Finit_String.Image);
      end if;
    end loop;
  end Loc_Load_Field;

  procedure Check_Overlap (Dscr_No  : in Afpx_Typ.Descriptor_Range;
                           Fi1, Fi2 : in Afpx_Typ.Absolute_Field_Range) is
    F1 : Afpx_Typ.Field_Rec renames Fields(Fi1);
    F2 : Afpx_Typ.Field_Rec renames Fields(Fi2);

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

  -- Load a descriptor (at a given index)
  function Dscr_Image is new Int_Image (Afpx_Typ.Descriptor_Range);
  procedure Load_Dscr (Node : in Xp.Element_Type;
                       Dscr_Index : in Afpx_Typ.Descriptor_Range;
                       Screen_Size : in Con_Io.Square)  is
    Attrs : constant Xp.Attributes_Array := Ctx.Get_Attributes (Node);
    Dscr_No : Afpx_Typ.Descriptor_Range;
    Background : Boolean := False;
    Name : Asu_Us;
    Child : Xp.Node_Type;
    List_Allowed : Boolean;
  begin
    for I in Attrs'Range loop
      if Match (Attrs(I).Name, "Num") then
        begin
          Dscr_No := Afpx_Typ.Descriptor_Range'Value (
                      Ctx.Get_Attribute (Node, 1).Value.Image);
        exception
          when others =>
            File_Error (Node, "Invalid descriptor num");
        end;
        -- Dscr no has to be unique
        if Descriptors(Dscr_No).Modified then
          File_Error (Node,
                      "Descriptor " & Afpx_Typ.Descriptor_Range'Image(Dscr_No)
                    & " already defined");
        end if;
        Ada.Text_Io.Put_Line ("   descriptor " &
                          Normal(Integer(Dscr_No), 2, Gap => '0'));
        -- Store default Dscr name if Name not set yet
        if Name.Is_Null then
          Name :=
             As.U.Tus ("Dscr_" &  Normal(Integer(Dscr_No), 2, Gap => '0'));
        end if;
      elsif Match (Attrs(I).Name, "Background") then
        Background := True;
        begin
          Descriptors(Dscr_No).Background :=
              Con_Io.Color_Of (Memory.Eval (Attrs(I).Value.Image));
        exception
          when Computer.Unknown_Variable =>
            File_Error (Node, "Unknown variable when evaluating "
                            & Attrs(I).Value.Image);
          when others =>
            File_Error (Node, "Invalid background specification");
        end;
      elsif Match (Attrs(I).Name, "Name") then
        begin
          Name := As.U.Tus (Memory.Eval (Attrs(I).Value.Image));
        exception
          when Computer.Unknown_Variable =>
            File_Error (Node, "Unknown variable when evaluating "
                            & Attrs(I).Value.Image);
        end;
      end if;
    end loop;
    -- Set default background
    if not Background then
      Descriptors(Dscr_No).Background := Default_Background;
    end if;
    -- Overwrite Dscr name
    if not Name.Is_Null then
      begin
        Xref.Set_Dscr_Name (Dscr_No, Name);
      exception
        when Xref.Invalid_Identifier =>
          File_Error (Node, "Invalid descriptor name");
        when Xref.Identifier_Redefined =>
          File_Error (Node, "A descriptor with this name already exists");
      end;
    end if;
    Add_Variable (Node, "Dscr_" & Dscr_Image (Dscr_No) & ".Background",
        Color_Image (Descriptors(Dscr_No).Background), False, True);
    Add_Variable (Node, "Descriptor.Background",
        Color_Image (Descriptors(Dscr_No).Background), False, False);

    -- Init dscr and fields array. No list at init
    Descriptors(Dscr_No).Modified := True;
    Descriptors(Dscr_No).Redisplay := True;
    Descriptors(Dscr_No).Dscr_Index := Dscr_Index;
    Descriptors(Dscr_No).Nb_Fields := 0;
    Init_Index := 1;
    Init_Str := (others => Con_Io.Space);
    Fields(0).Kind := Afpx_Typ.Put;

    List_Allowed := True;
    for I in 1 .. Ctx.Get_Nb_Children (Node) loop
      -- Var, List or Field
      Child := Ctx.Get_Child (Node, I);
      if Child.Kind /= Xp.Element then
        File_Error (Child, "Expected a Var, a List or a Field");
      end if;
      if List_Allowed and then Match(Ctx.Get_Name (Child), "List") then
        Load_List (Child, Screen_Size);
        List_Allowed := False;
      elsif Match(Ctx.Get_Name (Child), "Var") then
        Load_Variable (Child, False);
      elsif not Match(Ctx.Get_Name (Child), "Field") then
        File_Error (Child, "Expected a Var or a Field");
      else
        Descriptors(Dscr_No).Nb_Fields := Descriptors(Dscr_No).Nb_Fields + 1;
        Loc_Load_Field (Child, Dscr_No, Descriptors(Dscr_No).Nb_Fields,
                        Screen_Size);
        List_Allowed := False;
      end if;
    end loop;

    -- Check no overlapping of fields
    if Fields(0).Kind /= Afpx_Typ.Put then
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

  end Load_Dscr;

  procedure Load_Dscrs (Root : in Xp.Element_Type;
                        Check_Only : in Boolean) is
    Screen_Size : Con_Io.Square;
    Dscr_Index : Afpx_Typ.Descriptor_Range;
    Child : Xp.Node_Type;
    use type Afpx_Typ.Descriptor_Range;

  begin
    -- If not check_only, delete then create binary files
    if not Check_Only then
      begin
        Dscr_Io.Open (Dscr_File, Dscr_Io.In_File,
                      Afpx_Typ.Dest_Path.Image & Afpx_Typ.Dscr_File_Name);
        Dscr_Io.Delete (Dscr_File);
      exception
        when Dscr_Io.Name_Error => null;
      end;
      Dscr_Io.Create (Dscr_File, Dscr_Io.Out_File,
                      Afpx_Typ.Dest_Path.Image & Afpx_Typ.Dscr_File_Name);

      begin
        Fld_Io.Open (Fld_File, Fld_Io.In_File,
                     Afpx_Typ.Dest_Path.Image & Afpx_Typ.Fld_File_Name);
        Fld_Io.Delete (Fld_File);
      exception
        when Fld_Io.Name_Error => null;
      end;
      Fld_Io.Create (Fld_File, Fld_Io.Out_File,
                     Afpx_Typ.Dest_Path.Image & Afpx_Typ.Fld_File_Name);

      begin
        Init_Io.Open (Init_File, Init_Io.In_File,
                      Afpx_Typ.Dest_Path.Image & Afpx_Typ.Init_File_Name);
        Init_Io.Delete (Init_File);
      exception
        when Init_Io.Name_Error => null;
      end;
      Init_Io.Create (Init_File, Init_Io.Out_File,
                      Afpx_Typ.Dest_Path.Image & Afpx_Typ.Init_File_Name);
    end if;

    -- Parse size
    Screen_Size := Load_Size (Root);
    -- Init colors with default
    Color_Defs := Con_Io.Default_Colors;
    Default_Background := Con_Io.Effective_Colors'First;

    -- Initialize the descriptors array as not used
    for I in Afpx_Typ.Descriptor_Range loop
      Descriptors(I).Version  := Afpx_Typ.Afpx_Version;
      Descriptors(I).Size  := Screen_Size;
      Descriptors(I).Modified := False;
      Descriptors(I).Dscr_Index := Afpx_Typ.Descriptor_Range'First;
      Descriptors(I).Nb_Fields := 0;
      Descriptors(I).Colors := Color_Names;
    end loop;

    -- Loop on persistent variables and descriptors
    -- Descriptors are stored in the descriptor file at Dscr_No
    -- Fields and init tables are stored in their files at Dscr_Index
    Colors_Loaded := False;
    Dscr_Index := 1;
    Dscrs:
    for I in 1 .. Ctx.Get_Nb_Children (Root) loop
      -- Color names, persistent variable or descriptor
      Child := Ctx.Get_Child (Root, I);
      if Child.Kind /= Xp.Element then
        File_Error (Child, "Expected Descriptor or Var");
      end if;
      if Match (Ctx.Get_Name (Child), "Var") then
        Load_Variable (Child, True);
      elsif Match (Ctx.Get_Name (Child), "Color_Definitions") then
        Define_Color_Names (Child);
      elsif not  Match (Ctx.Get_Name (Child), "Descriptor") then
        File_Error (Child, "Expected Descriptor or Var");
      else
        Load_Color_Names (Child);
        -- Descriptor
        Load_Dscr (Child, Dscr_Index, Screen_Size);
        -- If not check_only, write fields and init
        if not Check_Only then
          Fld_Io.Write  (Fld_File , Fields,
                         Fld_Io.Positive_Count(Dscr_Index));
          Init_Io.Write (Init_File, Init_Str,
                         Init_Io.Positive_Count(Dscr_Index));
        end if;
        -- Reset volatile variables and constants defined for this descriptor
        Memory.Reset (Not_Persistent => True);
        -- Ready for next descriptor
        Dscr_Index := Dscr_Index + 1;
      end if;

    end loop Dscrs;

    -- Set colors to all descriptors
    for I in Afpx_Typ.Descriptor_Range loop
      Descriptors(I).Colors := Color_Names;
    end loop;

    -- Reset all variables
    Memory.Reset (Not_Persistent => False);

    if Check_Only then
      Xref.Clean;
    else
      Dscr_Io.Write (Dscr_File, Descriptors);
      Xref.Generate;
    end if;

  exception
    when others =>
      Close (True);
      raise;
  end Load_Dscrs;

  -- Root of parsed elements
  Root : Xp.Element_Type;

begin
  -- Help
  begin
    Argument.Get_Parameter (List_File_Name, Param_Key => "h");
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
        & " [ -l<afpx_file> ] [ -d<dest_dir> ] [ -x<cross_ref_package> ]");
    return;
  exception
    when others =>
      null;
  end;

  -- Source file and dest path arguments
  Expected_Args := 0;
  begin
    Argument.Get_Parameter (List_File_Name, Param_Key => "l");
    if List_File_Name.Is_Null then
      raise Argument_Error;
    end if;
    -- Argument found
    Expected_Args := Expected_Args + 1;
  exception
    when Argument.Argument_Not_Found =>
      List_File_Name := As.U.Tus (Default_List_File_Name);
    when others =>
      raise Argument_Error;
  end;

  begin
    Argument.Get_Parameter (Afpx_Typ.Dest_Path, Param_Key => "d");
    if Afpx_Typ.Dest_Path.Is_Null then
      raise Argument_Error;
    end if;
    -- Argument found
    Expected_Args := Expected_Args + 1;
  exception
    when Argument.Argument_Not_Found =>
      Afpx_Typ.Dest_Path := As.U.Tus (".");
    when others =>
      raise Argument_Error;
  end;

  declare
    Xref_Name : Asu_Us;
  begin
    Argument.Get_Parameter (Xref_Name, Param_Key => "x");
    Xref.Set_Package_Name (Xref_Name);
    Expected_Args := Expected_Args + 1;
  exception
    when Argument.Argument_Not_Found =>
      null;
    when others =>
      raise Argument_Error;
  end;

  if Argument.Get_Nbre_Arg /= Expected_Args then
    raise Argument_Error;
  end if;

  Ada.Text_Io.Put_Line ("Reading " & List_File_Name.Image);
  Ada.Text_Io.Put_Line ("Writing in " & Afpx_Typ.Dest_Path.Image);
  Afpx_Typ.Dest_Path.Append ("/");

  -- First check
  Ada.Text_Io.Put_Line ("Parsing:");
  declare
    Parse_Ok : Boolean;
  begin
    Ctx.Parse (List_File_Name.Image, Parse_Ok);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error (Ctx.Get_Parse_Error_Message);
      raise File_Syntax_Error;
    end if;
    Root := Ctx.Get_Root_Element;
  exception
    when Xp.File_Error =>
      Basic_Proc.Put_Line_Error ("Error accessing file "
                            & List_File_Name.Image);
      raise File_Not_Found;
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

