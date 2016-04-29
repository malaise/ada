with As.U, Con_Io, Directory, Dir_Mng, Str_Util, Language;
package body Select_File is

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

  -- Use Afpx descriptor
  procedure Use_Descriptor is
    use type Afpx.Descriptor_Range;
  begin
    if not Afpx.Is_Descriptor_Set
    or else Afpx.Get_Descriptor /= Descriptor then
      Afpx.Use_Descriptor (Descriptor);
    end if;
  end Use_Descriptor;

  -- Scroll the list
  procedure Scroll (Fld_No : in List_Scroll_Fld_Range) is
  begin
    case Fld_No is
      when 07 => Afpx.Update_List (Afpx.Top);
      when 08 => Afpx.Update_List (Afpx.Page_Up);
      when 09 => Afpx.Update_List (Afpx.Up);
      when Center_Fld => Afpx.Update_List (Afpx.Center_Selected);
      when 11 => Afpx.Update_List (Afpx.Down);
      when 12 => Afpx.Update_List (Afpx.Page_Down);
      when 13 => Afpx.Update_List (Afpx.Bottom);
    end case;
  end Scroll;

  -- Encode in info field
  procedure Encode_Info (Str : in String) is
  begin
    Afpx.Clear_Field (Info_Fld);
    Afpx.Encode_Field (Info_Fld, (0, 0),
      Str_Util.Procuste (Str, Afpx.Get_Field_Width (Info_Fld),
                         Trunc_Head => False));
  end Encode_Info;

  -- Ptg on Ok (and cancel) buttons
  function Confirm return Boolean is
    Handle : Afpx.Get_Handle_Rec;
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
    Afpx.Set_Field_Colors (Get_Fld,
         Background => Con_Io.Color_Of ("Black"));
    loop
      Afpx.Put_Then_Get (Handle, Ptg_Result);
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
              raise Exit_Requested;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when List_Scroll_Fld_Range =>
              Scroll (Ptg_Result.Field_No);
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
          null;
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

  -- Handle for getting file
  Get_Handle  : Afpx.Get_Handle_Rec;

  function Get_File (Current_File   : String;
                     For_Read       : Boolean;
                     Select_Current : Boolean) return String is

    Ptg_Result  : Afpx.Result_Rec;


    type Error_List is (E_File_Not_Found, E_Io_Error, E_File_Name);

    Get_Width    : Natural := 0;
    Get_Content  : As.U.Asu_Us;
    Get_Ok       : Boolean;
    Title_Width  : Natural := 0;
    Dir_List     : Dir_Mng.File_List_Mng.List_Type;
    File_Rec     : Dir_Mng.File_Entry_Rec;
    Valid        : Boolean;
    Pos_In_List  : Natural;


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
    procedure Parse_Spaces (Txt : in out As.U.Asu_Us;
                            Ok : out Boolean) is
      Str : constant String := Txt.Image;
      L : Natural;
    begin
      L := 0;
      for I in reverse Str'Range loop
        if not Str_Util.Is_Separator (Str(I)) then
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
      Txt := As.U.Tus (Str(1 .. L));
      Ok := True;
    end Parse_Spaces;

    -- Put file name
    procedure Put_File (File_Name : in String) is
      Height : Afpx.Height_Range;
      Width  : Afpx.Width_Range;
    begin
      Afpx.Get_Field_Size (File_Fld, Height, Width);
      Afpx.Encode_Field (File_Fld, (0, 0),
        Str_Util.Procuste (File_Name, Width));
    end Put_File;

    -- Report an internal error
    procedure Error (Msg : in Error_List) is
    begin
      Report_Error (case Msg is
                      when E_File_Not_Found     => "File not found",
                      when E_Io_Error           => "Error accessing file",
                      when E_File_Name          => "Error invalid file name");
    end Error;

    function Is_Dir (File : String) return Boolean is
      Kind : Directory.File_Kind_List;
      File_Txt : As.U.Asu_Us;
      use type Directory.File_Kind_List;
    begin
      File_Txt := As.U.Tus (File);
      Kind := Directory.File_Kind (File_Txt.Image);
      if Kind = Directory.Link then
        File_Txt := As.U.Tus (Directory.Read_Link (File_Txt.Image));
        Kind := Directory.File_Kind (File_Txt.Image);
      end if;
      return Kind = Directory.Dir;
    end Is_Dir;

    procedure Change_Dir (New_Dir : in String; Current : in String) is
      Height : Afpx.Height_Range;
      Width  : Afpx.Width_Range;
      Dir_Item : Dir_Mng.File_Entry_Rec;
      Char : Character;
      Afpx_Item : Afpx.Line_Rec;
      Selected : Natural := 0;
    begin
      -- Clear get field
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;

      -- Change dir
      Directory.Change_Current (New_Dir);
      -- Title
      Put_File (if Directory.Get_Current = "/" then "/*"
                else Directory.Get_Current & "/*");

      -- Set Afpx list
      -- Get list width
      Afpx.Get_Field_Size (Afpx.List_Field_No, Height, Width);
      -- Read dir and move to first
      Dir_List.Delete_List;
      Dir_Mng.List_Dir (Dir_List, ".");
      Dir_Mng.File_Sort (Dir_List);
      Dir_List.Rewind;
      -- Clear Afpx list then build it
      Afpx.Line_List.Delete_List;
      loop
        Dir_List.Read (Dir_Item, Dir_Mng.File_List_Mng.Current);
        Char := (case Dir_Item.Kind is
                   when Directory.File =>             ' ',
                   when Directory.Dir =>              '/',
                   when Directory.Link =>             '@',
                   when Directory.Block_Device
                      | Directory.Character_Device => '>',
                   when Directory.Pipe =>             '|',
                   when Directory.Socket =>           '=',
                   -- device, fifo ...
                   when Directory.Unknown =>          '?');
        Afpx_Item.Len := Width;
        Afpx_Item.Str (1 .. Width) :=
            Language.String_To_Unicode (
                Str_Util.Procuste (
                    Dir_Item.Name.Image & ' ' & Char, Width) );
        Afpx.Line_List.Insert (Afpx_Item);
        -- A file/dir name cannot be empty, so empty Current will never match
        if Dir_Item.Name.Image = Current then
          Selected := Afpx.Line_List.Get_Position;
        end if;
        exit when not Dir_List.Check_Move;
        Dir_List.Move_To;
      end loop;

      -- Move to Selected or beginning
      if Selected /= 0 then
        Afpx.Line_List.Move_At (Selected);
      else
        Afpx.Line_List.Rewind;
      end if;
      Afpx.Update_List(Afpx.Center_Selected);

    end Change_Dir;

    -- To find current position back
    function Match (Current, Criteria : Dir_Mng.File_Entry_Rec) return Boolean is
      use type Dir_Mng.File_Kind_List, As.U.Asu_Us;
    begin
      return Current.Kind = Criteria.Kind
      and then Current.Name = Criteria.Name;
    end Match;
    function File_Search is new Dir_Mng.File_List_Mng.Search (Match);

    -- Reread current directory, try to restore current
    procedure Reread is
      Dir_Item : Dir_Mng.File_Entry_Rec;
    begin
      -- Save current entry
      Dir_List.Move_At (Afpx.Line_List.Get_Position);
      Dir_List.Read (Dir_Item, Dir_Mng.File_List_Mng.Current);
      -- Rebuild list
      Change_Dir(".", "");
      -- Search position back and move Afpx to it
      if File_Search (Dir_List, Dir_Item,
                      From => Dir_Mng.File_List_Mng.Current_Absolute) then
        Afpx.Line_List.Move_At (Dir_List.Get_Position);
        Afpx.Update_List (Afpx.Center_Selected);
      end if;

    end Reread;

    -- Get text from file get field and check it
    procedure Handle_File_Text (Allow_Empty : in Boolean;
                                Name : out As.U.Asu_Us;
                                Ok : out Boolean) is
    begin
      Afpx.Decode_Field (Get_Fld, 0, Name);
      Parse_Spaces (Get_Content, Ok);
      if Ok and then Name.Is_Null then
        if Allow_Empty then
          Name.Set_Null;
          return;
        else
          Ok := False;
        end if;
      end if;
      if not Ok then
        Error (E_File_Name);
        return;
      end if;
      begin
        -- Value to return if not dir
        if not Is_Dir (Name.Image) then
          -- Valid file entered
          return;
        end if;
        -- Change dir
        Afpx.Clear_Field (Get_Fld);
        Change_Dir (Name.Image, "");
        Ok := False;
      exception
        when Directory.Name_Error =>
          -- File not found
          if For_Read then
            -- Read non existing file
            Ok := False;
            Error (E_File_Not_Found);
          else
            -- Save on new file
            return;
          end if;
        when others =>
          Ok := False;
          Error (E_Io_Error);
      end;
    end Handle_File_Text;

  begin
    Use_Descriptor;

    -- Call client specific init
    Init_Procedure;

    Afpx.Encode_Field (Info_Fld, (0, 0), "Select or enter file name");
    -- Title
    Afpx.Clear_Field (Title_Fld);
    if For_Read then
      Afpx.Encode_Field (Title_Fld, (0, 0),
          (if Read_Title'Length > Get_Title_Width then
             Str_Util.Normalize (Read_Title)(1 .. Get_Title_Width)
           else Read_Title));
    else
      Afpx.Encode_Field (Title_Fld, (0, 0),
          (if Write_Title'Length > Get_Title_Width then
            Str_Util.Normalize (Write_Title)(1 .. Get_Title_Width)
           else Write_Title));
    end if;

    -- Encode current file name in get field
    if Current_File'Length > Get_Get_Width then
      Get_Content.Set_Null;
    else
      Get_Content := As.U.Tus (
        Str_Util.Procuste (Current_File, Get_Get_Width));
    end if;
    Afpx.Encode_Field (Get_Fld, (0, 0), Get_Content);

    -- Build list
    Change_Dir (".", (if Select_Current then Current_File else ""));

    Get_Handle.Cursor_Field := Get_Fld;
    loop

      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Handle_File_Text (False, Get_Content, Get_Ok);
              if Get_Ok then
                -- Valid file
                File_Rec.Name := Get_Content;
                Valid := True;
                exit;
              end if;
            when Afpx.Escape_Key =>
              Valid := False;
              exit;
            when Afpx.Break_Key =>
              raise Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when List_Scroll_Fld_Range =>
              Scroll (Ptg_Result.Field_No);

            when Ok_Fld =>
              -- Ok button
              Handle_File_Text (True, Get_Content, Get_Ok);
              if Get_Ok then
                if not Get_Content.Is_Null then
                  -- Valid file
                  File_Rec.Name := Get_Content;
                  Valid := True;
                  exit;
                end if;
                Pos_In_List := Afpx.Line_List_Mng.Get_Position(Afpx.Line_List);
                Dir_List.Move_At (Pos_In_List);
                Dir_List.Read (File_Rec, Dir_Mng.File_List_Mng.Current);
                begin
                  if Is_Dir (File_Rec.Name.Image) then
                    Afpx.Clear_Field (Get_Fld);
                    Change_Dir (File_Rec.Name.Image, "");
                  else
                    -- File selected
                    Valid := True;
                    exit;
                  end if;
                exception
                  when others =>
                    Error (E_Io_Error);
                end;
              end if;

            when Afpx.List_Field_No =>
              -- Double click in list
              Pos_In_List := Afpx.Line_List_Mng.Get_Position(Afpx.Line_List);
              Dir_List.Move_At (Pos_In_List);
              Dir_List.Read (File_Rec, Dir_Mng.File_List_Mng.Current);
              begin
                if Is_Dir (File_Rec.Name.Image) then
                  Afpx.Clear_Field (Get_Fld);
                  Change_Dir (File_Rec.Name.Image, "");
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
              Reread;
            when others => null;
          end case;
        when Afpx.Fd_Event =>
          Fd_Callback;
        when Afpx.Timer_Event =>
          Timer_Callback;
        when Afpx.Signal_Event =>
          Signal_Callback;
        when Afpx.Refresh =>
          null;
      end case;
    end loop;

    Dir_List.Delete_List;
    Afpx.Line_List_Mng.Delete_List (Afpx.Line_List);
    if Valid then
      return File_Rec.Name.Image;
    else
      return "";
    end if;
  end Get_File;

  procedure Report_Error (Message : in String) is
  begin
    Use_Descriptor;
    Afpx.Bell (1);
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Set_Field_Activation (Center_Fld, False);
    Afpx.Set_Field_Activation (Reread_Fld, False);
    Afpx.Set_Field_Activation (Cancel_Fld, False);
    Afpx.Set_Field_Colors(Info_Fld, Foreground => Con_Io.Color_Of ("Orange"));
    Encode_Info (Message);

    while not Confirm loop
      null;
    end loop;

    Afpx.Reset_Field (Info_Fld);
    Afpx.Set_Field_Activation (Cancel_Fld, True);
    Afpx.Set_Field_Activation (Reread_Fld, True);
    Afpx.Set_Field_Activation (Center_Fld, True);
    Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
    Get_Handle.Cursor_Field := Get_Fld;
  end Report_Error;

end Select_File;
