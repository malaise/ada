with Text_Io, Direct_Io;
with Con_Io, Get_Line, Text_Handler, Normal, Argument, Directory;
with Afpx_Typ;
use  Afpx_Typ;
-- Read AFPX.LIS, check
-- Build AFPX.DSC list of Descr_Rec
--       AFPX.FLD list of Fields_Array
--       AFPX.INI list of Char_Str
procedure Afpx_Bld is

  -- Inputs name
  Default_List_File_Name : constant String := "AFPX.LIS";
  List_File_Name : Text_Handler.Text (Directory.Max_Dir_Name_Len * 2);

  -- Get_Line of descriptor file
  package Dscr_Get is new Get_Line (Max_Word_Len => 80, 
                                    Max_Word_Nb  => 45,
                                    Max_Line_Len => 132,
                                    Comment      => "#");
  Dscr_Line : Dscr_Get.Line_Array;
  Dscr_Words : Dscr_Get.Word_Count;

  -- Direct_io of descriptors, fields, init strings
  package Dscr_Io is new Direct_Io (Afpx_Typ.Descriptors_Array);
  Dscr_File : Dscr_Io.File_Type;
  package Fld_Io  is new Direct_Io (Afpx_Typ.Fields_Array);
  Fld_File : Fld_Io.File_Type;
  package Init_Io is new Direct_Io (Afpx_Typ.Char_Str);
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

  procedure Next_Line is
  begin
    Dscr_Get.Read_Next_Line;
    Dscr_Words := Dscr_Get.Get_Word_Number;
    Dscr_Get.Get_Words(Dscr_Line);
  end Next_Line;

  procedure Dump_Line is
  begin
    Text_Io.Put(Text_Io.Positive_Count'Image(Dscr_Get.Get_Line_No) & " : ");
    for I in 1 .. Dscr_Words loop
      Text_Io.Put(">" & Text_Handler.Value(Dscr_Line(I)) & "< ");
    end loop;
    Text_Io.New_Line;
  end Dump_Line;

  procedure Close (On_Error : in Boolean) is
  begin
    begin
      Dscr_Get.Close;
    exception
      when others =>
        null;
    end;
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

  function First_Word return String is
  begin
    return Text_Handler.Value(Dscr_Line(1));
  end;

  function End_Of (Keyword : String) return Boolean is
  begin
    return Dscr_Words = 2 and then
           First_Word = "END" and then
           Text_Handler.Value(Dscr_Line(2)) = Keyword;
  end End_Of;

  procedure File_Error (Msg : in String := "") is
  begin
    if Msg = "" then
      Text_Io.Put_Line ("SYNTAX ERROR.");
    else
      Text_Io.Put_Line ("SYNTAX ERROR : " & Msg & ".");
    end if;
    Text_Io.Put (" In file " & Text_Handler.Value(List_File_Name)
               & " at line ");
    Dump_Line;
    raise File_Syntax_Error;
  end File_Error;

  -- Check and store upper_left (and lower right in size) and colors
  procedure Load_Geo_Color (Fn : in Afpx_Typ.Absolute_Field_Range) is
  begin
    if First_Word /= "GEOMETRY" or else Dscr_Words /= 5 then
      File_Error ("GEOMETRY <upper_row> <left_col> <lower_row> <right_col> expected");
    end if;
    begin
      -- Load upper_right and lower left
      Fields(Fn).Upper_Left.Row :=
       Con_Io.Row_Range'Value(Text_Handler.Value(Dscr_Line(2)));
      Fields(Fn).Upper_Left.Col :=
       Con_Io.Col_Range'Value(Text_Handler.Value(Dscr_Line(3)));
      Fields(Fn).Lower_Right.Row :=
       Con_Io.Row_Range'Value(Text_Handler.Value(Dscr_Line(4)));
      Fields(Fn).Lower_Right.Col :=
       Con_Io.Col_Range'Value(Text_Handler.Value(Dscr_Line(5)));
    exception
      when others =>
        File_Error ("Invalid geometry");
    end;
    if      Fields(Fn).Upper_Left.Row > Fields(Fn).Lower_Right.Row
    or else Fields(Fn).Upper_Left.Col > Fields(Fn).Lower_Right.Col
    then
      File_Error ("Invalid geometry. Upper_left < lower_right");
    end if;

    -- Compute size
    Fields(Fn).Height :=
     Fields(Fn).Lower_Right.Row - Fields(Fn).Upper_Left.Row + 1;
    Fields(Fn).Width :=
     Fields(Fn).Lower_Right.Col - Fields(Fn).Upper_Left.Col + 1;

    -- One Row for Get fields
    if Fields(Fn).Kind = Afpx_Typ.Get and then Fields(Fn).Height /= 1 then
      File_Error ("Invalid geometry. GET fields must have ONE row");
    end if;

    Next_Line;

    -- Parse colors
    if First_Word /= "COLORS" then
      File_Error ("Keyword COLORS expected");
    end if;
    begin
      if Fn = 0 or else Fields(Fn).Kind = Afpx_Typ.Get then
        if Dscr_Words /= 4 then
          File_Error ("COLORS <foreground> <background> <selected> expected");
        end if;
        Fields(Fn).Colors.Foreground :=
         Con_Io.Effective_Colors'Value (Text_Handler.Value(Dscr_Line(2)));
        Fields(Fn).Colors.Blink_Stat := Con_Io.Not_Blink;
        Fields(Fn).Colors.Background :=
         Con_Io.Effective_Basic_Colors'Value (Text_Handler.Value(Dscr_Line(3)));
        Fields(Fn).Colors.Selected :=
         Con_Io.Effective_Basic_Colors'Value (Text_Handler.Value(Dscr_Line(4)));

      elsif Fields(Fn).Kind = Put then
        if Dscr_Words /= 4 then
          File_Error ("COLORS <foreground> <blink> <background> expected");
        end if;
        Fields(Fn).Colors.Foreground :=
         Con_Io.Effective_Colors'Value (Text_Handler.Value(Dscr_Line(2)));
        Fields(Fn).Colors.Blink_Stat :=
         Con_Io.Effective_Blink_Stats'Value(Text_Handler.Value(Dscr_Line(3)));
        Fields(Fn).Colors.Background :=
         Con_Io.Effective_Basic_Colors'Value (Text_Handler.Value(Dscr_Line(4)));
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;

      elsif Fields(Fn).Kind = Button then
        if Dscr_Words /= 3 then
          File_Error ("COLORS <foreground> <background> expected");
        end if;
        Fields(Fn).Colors.Foreground :=
         Con_Io.Effective_Colors'Value (Text_Handler.Value(Dscr_Line(2)));
        Fields(Fn).Colors.Blink_Stat := Con_Io.Not_Blink;
        Fields(Fn).Colors.Background :=
         Con_Io.Effective_Basic_Colors'Value (Text_Handler.Value(Dscr_Line(3)));
        Fields(Fn).Colors.Selected := Fields(Fn).Colors.Background;
      end if;
    exception
      when File_Syntax_Error =>
        raise;
      when others =>
        File_Error ("Invalid colors specification");
    end;

    -- Foreground has to be basic for all but Put fields
    if (Fn = 0 or else Fields(Fn).Kind /= Put)
    and then Fields(Fn).Colors.Foreground
             not in Con_Io.Effective_Basic_Colors then
      -- For list, Get and Button, Foreground has to be basic
      File_Error ("For all but PUT fields, FOREGROUND has to be basic color");
    end if;
    Next_Line;
  end Load_Geo_Color;

  procedure Load_List is
  begin
    if Dscr_Words /= 1 then
      File_Error ("LIST expected");
    end if;
    -- In List
    Fields(0).Kind := Afpx_Typ.Button;
    Fields(0).Activated := True;
    Fields(0).Isprotected := False;
    Next_Line;
    Load_Geo_Color (0);
    if not End_Of ("LIST") then
      File_Error ("END LIST expected");
    end if;
    Fields(0).Char_Index := 1;
    Next_Line;
  end Load_List;

  procedure Loc_Load_Field (No : in Afpx_Typ.Field_Range)  is
    First_Init : Boolean;
    Prev_Init_Square : Con_Io.Square;
    -- Location in field of init string
    Finit_Square : Con_Io.Square;
    -- Whole init line got from get_line
    Finit_Line   : Dscr_Get.Line_Txt;
    -- Init string
    Finit_Str    : String (1 .. Finit_Line.Max_Len);
    Finit_Len    : Natural;
    Finit_Start  : Positive;
    -- Index in Char_Str of beginning of Init string
    Finit_Index : Afpx_Typ.Char_Str_Range;
  begin
    if Dscr_Words /= 3 then
      File_Error ("FIELD <field_no> <field_type> expected");
    end if;
    begin
      if Afpx_Typ.Field_Range'Value(Text_Handler.Value(Dscr_Line(2))) /=
      No then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        File_Error ("Invalid field number. They must crescent positives");
    end;
    begin
      Fields(No).Kind :=
        Afpx_Typ.Field_Kind_List'Value(Text_Handler.Value(Dscr_Line(3)));
    exception
      when others =>
        File_Error ("Invalid field type. PUT, GET or BUTTON expected");
    end;
    Fields(No).Activated := True;
    Fields(No).Isprotected := False;
    Next_Line;
    Load_Geo_Color (No);

    Fields(No).Char_Index := Init_Index;
    Finit_Len := Fields(No).Height * Fields(No).Width;
    begin
      Init_Index := Init_Index + Finit_Len + 1;
    exception
      when others =>
        File_Error ("Too many init characters");
    end;

    if First_Word = "INIT" then

      if Dscr_Words /= 1 then
        File_Error ("INIT expected");
      end if;
      Next_Line;

      First_Init := True;
      Prev_Init_Square := (0, 0);
      while not End_Of ("INIT") loop
        -- Check init syntax and length of string
        if Dscr_Words < 2 then
          File_Error ("Invalid init. <row> <col> [ <str> ] expected");
        end if;
        begin
          Finit_Square.Row :=
           Con_Io.Row_Range'Value(Text_Handler.Value(Dscr_Line(1)));
          Finit_Square.Col :=
           Con_Io.Row_Range'Value(Text_Handler.Value(Dscr_Line(2)));
        exception
          when others =>
            File_Error ("Invalid init row or column");
        end;
        -- Check init squares crescent
        if not First_Init then
          if Finit_Square.Row < Prev_Init_Square.Row then
            File_Error ("Invalid init row. Must be crescent");
          elsif    Finit_Square.Row  = Prev_Init_Square.Row
          and then Finit_Square.Col <= Prev_Init_Square.Col then
            File_Error ("Invalid init col. Must be crescent and not overlap");
          end if;
        end if;
        First_Init := False;
        Prev_Init_Square := Finit_Square;
        -- Check init square in field
        if not Afpx_Typ.In_Field (Fields(No), Finit_Square) then
          File_Error ("Init row or col not in field");
        end if;
        -- Get the whole line to extract init string
        Dscr_Get.Get_Whole_Line (Finit_Line);
        Finit_Len := Text_Handler.Length (Finit_Line);
        Finit_Str (1 .. Finit_Len) := Text_Handler.Value (Finit_Line);
        Finit_Start := 1;
        -- Skeep spaces after string
        while   Finit_Str(Finit_Len) = ' '
        or else Finit_Str(Finit_Len) = Ascii.Ht loop
          Finit_Len := Finit_Len - 1;
        end loop;
        -- Skeep spaces before row
        while   Finit_Str(Finit_Start) = ' '
        or else Finit_Str(Finit_Start) = Ascii.Ht loop
          Finit_Start := Finit_Start + 1;
        end loop;
        -- Skip row
        while    Finit_Str(Finit_Start) /= ' '
        and then Finit_Str(Finit_Start) /= Ascii.Ht loop
          Finit_Start := Finit_Start + 1;
        end loop;
        -- Skeep spaces between row and col
        while   Finit_Str(Finit_Start) = ' '
        or else Finit_Str(Finit_Start) = Ascii.Ht loop
          Finit_Start := Finit_Start + 1;
        end loop;
        -- Skip col
        while    Finit_Start <= Finit_Len
        and then Finit_Str(Finit_Start) /= ' '
        and then Finit_Str(Finit_Start) /= Ascii.Ht loop
          Finit_Start := Finit_Start + 1;
        end loop;
        if Finit_Start /= Finit_Len then
          -- There is a init string. Skeep spaces between col and str
          while   Finit_Start <= Finit_Len
          and then (        Finit_Str(Finit_Start) = ' '
                    or else Finit_Str(Finit_Start) = Ascii.Ht) loop
            Finit_Start := Finit_Start + 1;
          end loop;
          -- Set effective init string
          Finit_Str (1 .. Finit_Len - Finit_Start + 1) :=
                 Finit_Str (Finit_Start .. Finit_Len);
          Finit_Len := Finit_Len - Finit_Start + 1;
          -- Check Finit col + string length compatible with field width
          if not Afpx_Typ.In_Field (Fields(No),
                  (Finit_Square.Row, Finit_Square.Col + Finit_Len - 1)) then
            File_Error ("Init string too long for this col in this field");
          end if;
          -- Update prev_init_square col to last char of init string
          Prev_Init_Square.Col := Finit_Square.Col + Finit_Len - 1;
          -- Copy in init string
          Finit_Index := Fields(No).Char_Index
           + Finit_Square.Row * Fields(No).Width
           + Finit_Square.Col;
          Init_Str (Finit_Index .. Finit_Index + Finit_Len - 1) :=
            Finit_Str (1 .. Finit_Len);
        end if;
        Next_Line;
      end loop;
      Next_Line;
    end if;
    if not End_Of ("FIELD") then
      File_Error ("END FIELD expected");
    end if;
    Next_Line;
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
      Text_Io.Put ("ERROR : LIST");
    else
      Text_Io.Put ("ERROR : Field " & Afpx_Typ.Field_Range'Image(Fi1));
    end if;
    Text_Io.Put_Line (" and Field " & Afpx_Typ.Field_Range'Image(Fi2)
                      & " of descriptor "
                      & Afpx_Typ.Descriptor_Range'Image(Dscr_No)
                      & " overlap.");
    raise File_Syntax_Error;
  end Check_Overlap;

  procedure Load_Dscrs (Check_Only : in Boolean) is
    Dscr_Index : Afpx_Typ.Descriptor_Range;
    Dscr_No    : Afpx_Typ.Descriptor_Range;
    Error_Msg : Text_Handler.Text(60);
    Eof_Allowed : Boolean := False;

  begin
    -- Open list file.
    begin
      Dscr_Get.Open (Text_Handler.Value(List_File_Name));
    exception
      when Text_Io.Name_Error =>
        Text_Io.Put_Line ("ERROR : File " & Text_Handler.Value(List_File_Name)
                          & " not found.");
        raise File_Not_Found;
    end;
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
      Descriptors(I).Modified := False;
      Descriptors(I).Dscr_Index := Afpx_Typ.Descriptor_Range'First;
      Descriptors(I).Nb_Fields := 0;
    end loop;

    Text_Handler.Set (Error_Msg, "DESCRIPTOR <descriptor_number> expected");
    Dscr_Words := Dscr_Get.Get_Word_Number;
    Dscr_Get.Get_Words(Dscr_Line);

    -- Loop on descriptors
    -- Descriprors are stored in the descriptor file at Dscr_No
    -- Fields and init tables are stored in their files at Dscr_Index
    Dscr_Index := 1;
    Dscrs:
    loop
      Eof_Allowed := False;
      -- Descriptor line
      Text_Handler.Set (Error_Msg, "DESCRIPTOR <descriptor_number> expected");
      if Dscr_Words /= 2 then
        File_Error (Text_Handler.Value (Error_Msg));
      end if;
      if First_Word /= "DESCRIPTOR" then
        File_Error (Text_Handler.Value (Error_Msg));
      end if;
      begin
        Dscr_No := Afpx_Typ.Descriptor_Range'Value (
                               Text_Handler.Value(Dscr_Line(2)));
      exception
        when others =>
        File_Error (Text_Handler.Value (Error_Msg));
      end;
      Text_Io.Put_Line ("   descriptor " &
                        Normal(Integer(Dscr_No), 2, Gap => '0'));
      -- Dscr no has to be unique
      if Descriptors(Dscr_No).Modified then
        File_Error ("DESCRIPTOR " & Afpx_Typ.Descriptor_Range'Image(Dscr_No)
                                  & " already defined");
      end if;
      -- Init dscr and fields array. No list at init
      Descriptors(Dscr_No).Modified := True;
      Descriptors(Dscr_No).Dscr_Index := Dscr_Index;
      Descriptors(Dscr_No).Nb_Fields := 0;
      Init_Index := 1;
      Init_Str := (others => ' ');
      Next_Line;

      if First_Word = "LIST" then
        Load_List;
      else
        Fields(0).Kind := Put;
      end if;
      while First_Word = "FIELD" loop
        if Descriptors(Dscr_No).Nb_Fields /= Afpx_Typ.Field_Range 'Last then
          Descriptors(Dscr_No).Nb_Fields := Descriptors(Dscr_No).Nb_Fields + 1;
        else
          File_Error ("Too many fields. Maximum is"
           & Field_Range'Image(Afpx_Typ.Field_Range'Last) & " per descriptor");
        end if;
        Loc_Load_Field (Descriptors(Dscr_No).Nb_Fields);
      end loop;

      if not End_Of ("DESCRIPTOR") then
        if Descriptors(Dscr_No).Nb_Fields = 0
        and then Fields(0).Kind = Afpx_Typ.Put then
          -- No list nor field
          File_Error ("LIST, FIELD, or END DESCRIPTOR expected");
        else
          -- Some list or fields
          File_Error ("FIELD, or END DESCRIPTOR expected");
        end if;
      end if;

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

      -- if not check_only, write fields and init
      if not Check_Only then
        Fld_Io.Write  (Fld_File , Fields,   Fld_Io.Positive_Count(Dscr_Index));
        Init_Io.Write (Init_File, Init_Str, Init_Io.Positive_Count(Dscr_Index));
      end if;

      Dscr_Index := Dscr_Index + 1;
      Eof_Allowed := True;
      Next_Line;
    end loop Dscrs;

    -- Should never go there.
    -- Exit loop Dscrs on exception Dscr_Get.No_More_Line
    raise Constraint_Error;

  exception
    when Dscr_Get.No_More_Line =>
      if Eof_Allowed then
        -- if not check_only, write descriptors and close files
        if not Check_Only then
          Dscr_Io.Write (Dscr_File, Descriptors);
        end if;
        Close(False);
      else
        File_Error ("Unexpected end of file");
        Close (True);
        raise File_Syntax_Error;
      end if;
    when Dscr_Get.Too_Many_Words =>
      File_Error ("Too many words");
      Close (True);
      raise File_Syntax_Error;
    when Dscr_Get.Line_Too_Long =>
      File_Error ("Line too long");
      Close (True);
      raise File_Syntax_Error;
    when Dscr_Get.Word_Too_Long =>
      File_Error ("Word too long");
      Close (True);
      raise File_Syntax_Error;
    when others =>
      Close (True);
      raise;
  end Load_Dscrs;


begin
  -- Help
  begin
    Argument.Get_Parameter (List_File_Name, Param_Key => "h");
    Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
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

  Text_Io.Put_Line ("Reading " & Text_Handler.Value(List_File_Name));
  Text_Io.Put_Line ("Writing in " & Text_Handler.Value(Afpx_Typ.Dest_Path));
  Text_Handler.Append (Afpx_Typ.Dest_Path, "/");

  -- First check
  Text_Io.Put_Line ("Checking:");
  Load_Dscrs(True);
  -- Then write
  Text_Io.Put_Line ("Building:");
  Load_Dscrs(False);
  Text_Io.Put_Line ("Done.");
exception
  when Argument_Error =>
    Close (True);
    Text_Io.Put_Line ("Argument error. Try -h option.");
  when File_Not_Found =>
    Close (True);
    Text_Io.Put_Line ("Directory or file not found. Try -h option.");
  when File_Syntax_Error =>
    Close (True);
    Text_Io.Put_Line ("Syntax error.");
  when others =>
    Close (True);
    Text_Io.Put_Line ("Unexpected exception.");
    raise;
end Afpx_Bld;

