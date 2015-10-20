with As.U, Directory, Afpx.Utils, Environ, Computer, Ada_Words, Aski;
with Utils.X, Config, Afpx_Xref, Confirm, Error;
package body Bookmarks is

  -- The internal variables (none so far)
  Mem : Computer.Memory_Type;

  -- Set external resolver of variables to getenv returning empty is not set
  procedure Init is
  begin
    Mem.Set_External_Resolver (Environ.Getenv'Access);
  end Init;

  -- Set an internal variable
  procedure Set_Var (Name, Value : in String) is
  begin
    Mem.Set (Name, Value, Modifiable => True, Persistent => False);
  end Set_Var;

  -- Expand Env variables from path of bookmark
  function Dir_Of (Index : in Positive) return String is
  begin
    return Mem.Eval (As.U.Image (Config.Get_Bookmarks (Index).Path));
  end Dir_Of;

  -- Insert in Afpx list the image of a bookmark (normalized if necessary)
  List_Width : Afpx.Width_Range;
  procedure Insert_List (Bookmark : Config.Bookmark_Rec) is
    Line : Afpx.Line_Rec;
    Head, Text : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    if not Bookmark.Path.Is_Null then
      -- Regular bookmark
      if not Bookmark.Name.Is_Null then
        -- Regular bookmark with name
        Head := "(" & Bookmark.Name & ") ";
      end if;
      Text := Bookmark.Path;
    elsif not Bookmark.Name.Is_Null then
      -- Separator with name
      Text := "----- " & Bookmark.Name & " -----";
    end if;
    Afpx.Utils.Encode_Line (Head.Image, Text.Image, "", List_Width, Line);
    Afpx.Line_List.Insert (Line);
  end Insert_List;

  -- Re- set the list of bookmarks
  procedure Load_List is
    Bookmarks : constant Config.Bookmark_Array := Config.Get_Bookmarks;
  begin
    Afpx.Line_List.Delete_List;
    for I in Bookmarks'Range loop
      Insert_List (Bookmarks(I));
    end loop;
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;
  end Load_List;

  -- Init screen
  Get_Handle : Afpx.Get_Handle_Rec;
  procedure Init_Screen is
  begin
    -- Init Afpx
    Afpx.Use_Descriptor (Afpx_Xref.Bookmarks.Dscr_Num);
    Get_Handle := (others => <>);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode current dir
    Utils.X.Encode_Field (Directory.Get_Current, Afpx_Xref.Bookmarks.Dir);

    -- Encode Bookmarks
    Load_List;
  end Init_Screen;

  -- Get and check bookmark name or get edited path (from Get field)
  -- Handle error and return Aski.Nul on error
  Get_Error : constant String := Aski.Nul_S;
  function Get_Name (Name : Boolean := True) return String is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Afpx_Xref.Bookmarks.Name,
                               0, False));
  begin
    -- No check on Path (edit), or empty name (anonymous) or valid
    if not Name or else Str = "" or else Ada_Words.Is_Identifier (Str) then
      Afpx.Clear_Field (Afpx_Xref.Bookmarks.Name);
      Afpx.Reset (Get_Handle);
      return Str;
    else
      Error ("Bookmark name", Str, "Invalid bookmark name");
      Init_Screen;
      Afpx.Encode_Field (Afpx_Xref.Bookmarks.Name, (0, 0), Str);
      return Get_Error;
    end if;
  end Get_Name;

  -- Modify current bookmark
  In_Edit : Boolean := False;
  procedure Start_Edit (Bookmark : out Config.Bookmark_Rec) is
    Getfld : constant Afpx.Field_Range := Afpx_Xref.Bookmarks.Name;
    Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (Getfld);
    New_Name : constant String := Get_Name;
  begin
    if New_Name = Get_Error then
      return;
    end if;
    -- Get bookmark
    Bookmark := Config.Get_Bookmark (Afpx.Line_List.Get_Position);
    if New_Name /= "" then
      -- Change name
      Bookmark.Name := As.U.Tus (New_Name);
    end if;
    -- Encode path to edit
    Utils.X.Encode_Field (Bookmark.Path.Image, Getfld);
     -- Move cursor col on last significant char
    declare
      Str : constant Afpx.Unicode_Sequence := Afpx.Decode_Field (Getfld, 0);
    begin
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Offset := 0;
      Get_Handle.Insert := False;
      Get_Handle.Cursor_Col := Afpx.Last_Index (Str, True);
      -- String is longer that field width
      if Get_Handle.Cursor_Col >= Width then
        -- Width + Offset = Data_Len
        Get_Handle.Offset := Get_Handle.Cursor_Col - Width;
        Get_Handle.Cursor_Col := Width - 1;
      end if;
    end;
    -- Change titles
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Clear_Field (Afpx_Xref.Bookmarks.Get_Title);
    Utils.X.Encode_Field ("Path:", Afpx_Xref.Bookmarks.Get_Title);
    Utils.X.Center_Field ("OK", Afpx_Xref.Bookmarks.Addcurr);
    Utils.X.Center_Field ("Cancel", Afpx_Xref.Bookmarks.Addsep);
    -- Disable list and all buttons
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Go, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Moveup, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Movedown, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Edit, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Del, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Bookmarks.Back, False);
    In_Edit := True;
  end Start_Edit;

  procedure End_Edit (Bookmark : in out Config.Bookmark_Rec;
                      Ok : in Boolean) is
  begin
    -- Restore list and buttons
    Afpx.Set_Field_Protection (Afpx.List_Field_No, False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Get_Title, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Addcurr, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Addsep, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Go, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Moveup, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Movedown, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Edit, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Del, Reset_Colors => False);
    Afpx.Reset_Field (Afpx_Xref.Bookmarks.Back, Reset_Colors => False);

    -- Get and clear path
    Bookmark.Path := As.U.Tus (Get_Name (False));
    if Ok then
      -- Insert new bookmark after current and delete current
      Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
      Config.Del_Bookmark (Afpx.Line_List.Get_Position);
    end if;
    In_Edit := False;
  end End_Edit;

  -- Update the list status
  procedure List_Change (Unused_Action : in Afpx.List_Change_List;
                         Unused_Status : in Afpx.List_Status_Rec) is
    Bookmark : Config.Bookmark_Rec;
  begin
     if Afpx.Line_List.Is_Empty then
      return;
    end if;
    -- No Goto on separator (nor in Edit)
    Bookmark := Config.Get_Bookmark (Afpx.Line_List.Get_Position);
    Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Go,
                              In_Edit or else Bookmark.Path.Is_Null);
  end List_Change;


  -- Handle Bookmarks screen
  -- Returns new dir to change to, or "" if unchanged
  function Handle return String is
    -- Afpx stuff
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range, Afpx.Descriptor_Range;

    -- Current position in Afpx list
    Position : Positive;

    -- Bookmark
    Bookmark : Config.Bookmark_Rec;

    -- In Edit mode
    In_Edit : Boolean := False;

    Dummy : Boolean;

  begin
    -- Init screen
    Init_Screen;

    -- Main loop
    loop
      if not In_Edit then
        -- No Goto nor Del nor Move if no Bookmark
        Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Go,
                                  Afpx.Line_List.Is_Empty);
        Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Moveup,
                                  Afpx.Line_List.Is_Empty);
        Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Movedown,
                                  Afpx.Line_List.Is_Empty);
        Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Edit,
                                  Afpx.Line_List.Is_Empty);
        Afpx.Utils.Protect_Field (Afpx_Xref.Bookmarks.Del,
                                  Afpx.Line_List.Is_Empty);
      end if;

      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                         List_Change_Cb => List_Change'Access);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              if In_Edit then
                -- End edition OK
                End_Edit (Bookmark, True);
                In_Edit := False;
                Position := Afpx.Line_List.Get_Position;
                Load_List;
                Afpx.Line_List.Move_At (Position);
              end if;
            when Afpx.Escape_Key =>
              if In_Edit then
                -- End edition Cancel
                End_Edit (Bookmark, False);
                In_Edit := False;
              else
                -- Back
                return "";
              end if;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No | Afpx_Xref.Bookmarks.Go =>
              -- Double click or Goto => move to bookmark
              declare
                Dir : constant String := Dir_Of (Afpx.Line_List.Get_Position);
              begin
                if Dir /= "" then
                  return Dir;
                end if;
              end;
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.Utils.Scroll(
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.Bookmarks.Addcurr =>
              if In_Edit then
                -- End edition OK
                End_Edit (Bookmark, True);
                In_Edit := False;
                Position := Afpx.Line_List.Get_Position;
                Load_List;
                Afpx.Line_List.Move_At (Position);
              else
                -- Add current
                Bookmark := (As.U.Tus (Get_Name),
                             As.U.Tus (Directory.Get_Current));
                if Bookmark.Name.Image /= Get_Error then
                  if Afpx.Line_List.Is_Empty then
                    Config.Add_Bookmark (0, Bookmark);
                  else
                    Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
                  end if;
                  Insert_List (Bookmark);
                end if;
              end if;
            when Afpx_Xref.Bookmarks.Addsep =>
              if In_Edit then
                -- End edition Cancel
                End_Edit (Bookmark, False);
                In_Edit := False;
              else
                -- Add separator
                Bookmark := (As.U.Tus (Get_Name), As.U.Asu_Null);
                if Bookmark.Name.Image /= Get_Error then
                  if Afpx.Line_List.Is_Empty then
                    Config.Add_Bookmark (0, Bookmark);
                  else
                    Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
                  end if;
                  Insert_List (Bookmark);
                end if;
              end if;

            when Afpx_Xref.Bookmarks.Moveup =>
              -- Move bookmark up
              Position := Afpx.Line_List.Get_Position;
              if Position /= 1 then
                Config.Move_Bookmark (Position, Up => True);
                Load_List;
                Afpx.Line_List.Move_At (Position - 1);
              end if;
            when Afpx_Xref.Bookmarks.Movedown =>
              -- Move bookmark down
              Position := Afpx.Line_List.Get_Position;
              if Position /= Afpx.Line_List.List_Length then
                Config.Move_Bookmark (Position, Up => False);
                Load_List;
                Afpx.Line_List.Move_At (Position + 1);
              end if;

            when Afpx_Xref.Bookmarks.Edit =>
              -- Edit bookmark
              Start_Edit (Bookmark);
              In_Edit := True;
            when Afpx_Xref.Bookmarks.Del =>
              -- Del bookmark
              Bookmark := Config.Get_Bookmark (Afpx.Line_List.Get_Position);
              -- Confirm bookmark deletion (no confirmation for separators)
              if Bookmark.Path.Is_Null
              or else Confirm ("Remove bookmark", Bookmark.Path.Image) then
                Config.Del_Bookmark (Afpx.Line_List.Get_Position);
                Afpx.Line_List.Delete (Moved => Dummy);
              end if;
              -- Restore descriptor
              if Afpx.Get_Descriptor /= Afpx_Xref.Bookmarks.Dscr_Num then
                Init_Screen;
              end if;
            when Afpx_Xref.Bookmarks.Back =>
              -- Back
              return "";
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
    end loop;

  end Handle;

end Bookmarks;

