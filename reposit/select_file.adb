with Ada.Characters.Latin_1;
with Text_Handler, Con_Io, Afpx, Directory, Dir_Mng, String_Mng, Language;
function Select_File (Descriptor   : Afpx.Descriptor_Range;
                      Current_File : String;
                      For_Read     : Boolean) return String is

  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;

  -- Text/get fields
  -- 1 is the fixed title
  Title_Fld    : constant Afpx.Field_Range := 2;
  File_Fld     : constant Afpx.Field_Range := 4;
  Get_Fld      : constant Afpx.Field_Range := 5;
  Info_Fld     : constant Afpx.Field_Range := 6;
  -- The scroll buttons
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range 7 .. 13;
  Center_Fld     : constant Afpx.Field_Range := 10;
  -- Action buttons
  Reread_Fld   : constant Afpx.Field_Range := 14;
  Ok_Fld       : constant Afpx.Field_Range := 15;
  Cancel_Fld   : constant Afpx.Field_Range := 16;

  type Error_List is (E_File_Not_Found, E_Io_Error, E_File_Name);

  Get_Width    : Natural := 0;
  Get_Content  : Afpx.Str_Txt;
  Get_Ok       : Boolean;
  Title_Width  : Natural := 0;
  Dir_List     : Dir_Mng.File_List_Mng.List_Type;
  File_Rec     : Dir_Mng.File_Entry_Rec;
  Valid        : Boolean;
  Pos_In_List  : Natural;
  Is_A_Dir     : Boolean;


  -- Return width of Get field
  function Get_Get_Width return Afpx.Width_Range is
    Height : Afpx.Height_Range;
  begin
    if Get_Width = 0 then
      Afpx.Get_Field_Size (Get_Fld, Height, Get_Width);
    end if;
    return Get_Width;
  end Get_Get_Width;

  -- Return width of Title field
  function Get_Title_Width return Afpx.Width_Range is
    Height : Afpx.Height_Range;
  begin
    if Title_Width = 0 then
      Afpx.Get_Field_Size (Title_Fld, Height, Title_Width);
    end if;
    return Title_Width;
  end Get_Title_Width;

  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  procedure Parse_Spaces (Txt : in out Text_Handler.Text;
                          Ok : out Boolean) is
    Str : constant String := Text_Handler.Value(Txt);
    L : Natural;
  begin
    L := 0;
    for I in reverse Str'Range loop
      if Str(I) /= ' ' and then Str(I) /= Ada.Characters.Latin_1.Ht then
        -- Significant char
        if L = 0 then
          L := I;
        end if;
      else
        -- space
        if L /= 0 then
          -- Space before significant char
          Ok := False;
          return;
        end if;
      end if;
    end loop;
    -- If all spaces, L = 0 => empty
    Text_Handler.Set (Txt, Str(1 .. L));
    Ok := True;
  end Parse_Spaces;

  -- Put file name
  procedure Put_File (File_Name : in String) is
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
  begin
    Afpx.Get_Field_Size(File_Fld, Height, Width);
    Afpx.Encode_Field(File_Fld, (0, 0),
      String_Mng.Procuste(File_Name, Width));
  end Put_File;

  -- Encode in info field
  procedure Encode_Info (Str : in String) is
  begin
    Afpx.Clear_Field (Info_Fld);
    Afpx.Encode_Field (Info_Fld, (0, 0), Str);
  end Encode_Info;

  -- Scroll the list
  procedure Scroll (Fld_No : in List_Scroll_Fld_Range) is
  begin
    case Fld_No is
      when 07 => Afpx.Update_List(Afpx.Top);
      when 08 => Afpx.Update_List(Afpx.Page_Up);
      when 09 => Afpx.Update_List(Afpx.Up);
      when Center_Fld => Afpx.Update_List(Afpx.Center);
      when 11 => Afpx.Update_List(Afpx.Down);
      when 12 => Afpx.Update_List(Afpx.Page_Down);
      when 13 => Afpx.Update_List(Afpx.Bottom);
    end case;
  end Scroll;

  -- Ptg on Ok (and cancel) buttons
  Exit_Request : exception;
  function Confirm return Boolean is
    Cursor_Field : Afpx.Field_Range := 1;
    Cursor_Col : Con_Io.Col_Range := 0;
    Insert : Boolean := False;
    Redisplay : Boolean := False;
    Ptg_Result : Afpx.Result_Rec;
    Get_Prot : Boolean;
    Get_Act : Boolean;
    Res : Boolean;
  begin
    -- Protect get field
    Get_Prot := Afpx.Get_Field_Protection (Get_Fld);
    if not Get_Prot then
      Afpx.Set_Field_Protection (Get_Fld, True);
    end if;
    Afpx.Set_Field_Colors(Get_Fld,
         Background => Con_Io.Color_Of ("Black"));
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay);
      Redisplay := False;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Res := True;
              exit;
            when Afpx.Escape_Key =>
              Res := False;
              exit;
            when Afpx.Break_Key =>
              raise Exit_Request;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when List_Scroll_Fld_Range'First .. List_Scroll_Fld_Range'Last =>
              Scroll(Ptg_Result.Field_No);
            when Ok_Fld =>
              Res := True;
              exit;
            when Cancel_Fld =>
              Res := False;
              exit;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event =>
          Fd_Callback;
        when Afpx.Timer_Event =>
          Timer_Callback;
        when Afpx.Signal_Event =>
          Signal_Callback;
        when Afpx.Refresh =>
          Redisplay := True;
      end case;
    end loop;
    -- Restore Get field colors
    Get_Act := Afpx.Get_Field_Activation (Get_Fld);
    Afpx.Reset_Field (Get_Fld, Reset_String => False);
    Afpx.Set_Field_Activation (Get_Fld, Get_Act);
    -- Restore Get field protection
    if not Get_Prot then
      Afpx.Set_Field_Protection (Get_Fld, False);
    end if;
    return Res;
  end Confirm;


  procedure Error (Msg : in Error_List) is
  begin
    Afpx.Bell(1);
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Set_Field_Activation (Center_Fld, False);
    Afpx.Set_Field_Activation (Reread_Fld, False);
    Afpx.Set_Field_Activation (Cancel_Fld, False);
    Afpx.Set_Field_Colors(Info_Fld, Foreground => Con_Io.Color_Of ("Orange"));
    case Msg is
      when E_File_Not_Found     => Encode_Info ("File not found");
      when E_Io_Error           => Encode_Info ("Error accessing file");
      when E_File_Name          => Encode_Info ("Error invalid file name");
    end case;

    while not Confirm loop
      null;
    end loop;

    Afpx.Reset_Field(Info_Fld);
    Afpx.Set_Field_Activation (Cancel_Fld, True);
    Afpx.Set_Field_Activation (Reread_Fld, True);
    Afpx.Set_Field_Activation (Center_Fld, True);
    Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
    Cursor_Field := Get_Fld;
  end Error;

  function Is_Dir (File : String) return Boolean is
    Kind : Directory.File_Kind_List;
    File_Txt : Text_Handler.Text(Directory.Max_Dir_Name_Len);
    use Directory;
  begin
    Text_Handler.Set (File_Txt, File);
    Kind := Directory.File_Kind (Text_Handler.Value(File_Txt));
    if Kind = Directory.Link then
      Directory.Read_Link(Text_Handler.Value(File_Txt), File_Txt);
      Kind := Directory.File_Kind (Text_Handler.Value(File_Txt));
    end if;
    return Kind = Directory.Dir;
  end Is_Dir;

  procedure Change_Dir (New_Dir : in String) is
    Height : Afpx.Height_Range;
    Width  : Afpx.Width_Range;
    Dir_Item : Dir_Mng.File_Entry_Rec;
    Char : Character;
    Afpx_Item : Afpx.Line_Rec;
  begin
    -- Clear get field
    Cursor_Col := 0;
    Insert := False;

    -- change dir
    Directory.Change_Current(New_Dir);
    -- Title
    if Directory.Get_Current = "/" then
      Put_File ("/*");
    else
      Put_File (Directory.Get_Current & "/*");
    end if;

    -- Set Afpx list
    -- Get list width
    Afpx.Get_Field_Size(Afpx.List_Field_No, Height, Width);
    -- Read dir and move to first
    Dir_List.Delete_List;
    Dir_Mng.List_Dir (Dir_List, ".");
    Dir_Mng.File_Sort (Dir_List);
    Dir_List.Rewind;
    -- Clear Afpx list
    Afpx.Line_List_Mng.Delete_List(Afpx.Line_List);
    loop
      Dir_List.Read (Dir_Item, Dir_Mng.File_List_Mng.Current);
      case Dir_Item.Kind is
        when Directory.File =>
          Char := ' ';
        when Directory.Dir =>
          Char := '/';
        when Directory.Link =>
          Char := '@';
        when Directory.Block_Device | Directory.Character_Device =>
          Char := '>';
        when Directory.Pipe =>
          Char := '|';
        when Directory.Socket =>
          Char := '=';
        when Directory.Unknown =>
          -- device, fifo ...
          Char := '?';
      end case;
      Afpx_Item.Len := Width;
      Afpx_Item.Str (1 .. Width) :=
        Language.String_To_Wide (
        String_Mng.Procuste (Dir_Item.Name (1 .. Dir_Item.Len) & ' ' & Char,
                             Width) );
      Afpx.Line_List_Mng.Insert (Afpx.Line_List, Afpx_Item);
      exit when not Dir_List.Check_Move;
      Dir_List.Move_To;
    end loop;
    -- Move to beginning of Afpx list
    Afpx.Line_List_Mng.Rewind (Afpx.Line_List);
    Afpx.Update_List(Afpx.Top);

  end Change_Dir;

begin
  Afpx.Use_Descriptor(Descriptor);

  -- Call client specific init
  Init_Procedure;

  Afpx.Encode_Field (Info_Fld, (0, 0), "Select or enter file name");
  -- Title
  if For_Read then
    if Read_Title = "" or else Read_Title'Length > Get_Title_Width then
      Afpx.Encode_Field (Title_Fld, (0, 0), "Load a file");
    else
      Afpx.Encode_Field (Title_Fld, (0, 0), Read_Title);
    end if;
  else
    if Write_Title = "" or else Write_Title'Length > Get_Title_Width then
      Afpx.Encode_Field (Title_Fld, (0, 0), "Save in a file");
    else
      Afpx.Encode_Field (Title_Fld, (0, 0), Write_Title);
    end if;
  end if;

  -- Encode current file name in get field
  if Current_File'Length > Get_Get_Width then
    Text_Handler.Empty(Get_Content);
  else
    Text_Handler.Set (Get_Content,
      String_Mng.Procuste(Current_File, Get_Get_Width));
  end if;
  Afpx.Encode_Field (Get_Fld, (0, 0), Get_Content);

  -- File name
  Change_Dir (".");

  Cursor_Field := Get_Fld;
  Redisplay := False;
  loop

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Afpx.Decode_Field (Get_Fld, 0, Get_Content);
            Parse_Spaces (Get_Content, Get_Ok);
            Get_Ok := Get_Ok and then not Text_Handler.Empty(Get_Content);
            if not Get_Ok then
              Error (E_File_Name);
            else
              -- Value to return if not dir
              File_Rec.Len := Text_Handler.Length(Get_Content);
              File_Rec.Name(1 .. File_Rec.Len) :=
                        Text_Handler.Value(Get_Content);
              begin
                Is_A_Dir := Is_Dir (Text_Handler.Value(Get_Content));
                if Is_A_Dir then
                  -- Change dir
                  Afpx.Clear_Field (Get_Fld);
                  Change_Dir(Text_Handler.Value(Get_Content));
                else
                  -- Valid file entered
                  Valid := True;
                  exit;
                end if;
              exception
                when Directory.Name_Error =>
                  -- File not found
                  if For_Read then
                    -- Read non existing file
                    Error (E_File_Not_Found);
                  else
                    -- Save on new file
                    Valid := True;
                    exit;
                  end if;
                when others =>
                  Error (E_Io_Error);
              end;
            end if;
          when Afpx.Escape_Key =>
            Valid := False;
            exit;
          when Afpx.Break_Key =>
            raise Exit_Request;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when List_Scroll_Fld_Range'First .. List_Scroll_Fld_Range'Last =>
            Scroll(Ptg_Result.Field_No);

          -- Ok button or double click in list
          when Ok_Fld | Afpx.List_Field_No =>
            Pos_In_List := Afpx.Line_List_Mng.Get_Position(Afpx.Line_List);
            Dir_List.Move_At(Pos_In_List);
            Dir_List.Read(File_Rec, Dir_Mng.File_List_Mng.Current);
            begin
              if Is_Dir (File_Rec.Name(1 .. File_Rec.Len)) then
                Afpx.Clear_Field (Get_Fld);
                Change_Dir(File_Rec.Name(1 .. File_Rec.Len));
              else
                -- File selected
                Valid := True;
                exit;
              end if;
            exception
              when others =>
                Error (E_Io_Error);
            end;

          when Cancel_Fld =>
            Valid := False;
            exit;
          when Reread_Fld =>
            -- Reread current directory
            Change_Dir(".");
          when others => null;
        end case;
      when Afpx.Fd_Event =>
        Fd_Callback;
      when Afpx.Timer_Event =>
        Timer_Callback;
      when Afpx.Signal_Event =>
        Signal_Callback;
      when Afpx.Refresh =>
        Redisplay := True;
    end case;
  end loop;

  Dir_List.Delete_List;
  Afpx.Line_List_Mng.Delete_List(Afpx.Line_List);
  if Valid then
    return File_Rec.Name(1 .. File_Rec.Len);
  else
    return "";
  end if;
exception
  when Exit_Request =>
    return "";
end Select_File;

