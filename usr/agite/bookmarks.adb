with As.U, Directory, Con_Io, Afpx.List_Manager;
with Utils.X, Config, Afpx_Xref, Confirm;
package body Bookmarks is

  -- Strip potential leading "(name) " of bookmark
  function Dir_Of (Index : in Positive) return String is
  begin
    return As.U.Image (Config.Get_Bookmarks (Index).Path);
  end Dir_Of;

  -- Insert in Afpx list
  procedure Insert_List (Str : in String) is
    Line : Afpx.Line_Rec;
  begin
    Afpx.Encode_Line (Line,
         Utils.Normalize (Str, Afpx.Get_Field_Width (Afpx.List_Field_No)) );
    Afpx.Line_List.Insert (Line);
  end Insert_List;

  -- Image of a bookmark
  function Image (Bookmark : Config.Bookmark_Rec) return String is
    Res : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    if not Bookmark.Path.Is_Null then
      -- Regular bookmark
      if not Bookmark.Name.Is_Null then
        -- Regular bookmark with name
        Res := "(" & Bookmark.Name & ") " & Bookmark.Path;
      else
        -- Regular bookmark without name
        Res := Bookmark.Path;
      end if;
    elsif not Bookmark.Name.Is_Null then
      -- Separator with name
      Res := "----- " & Bookmark.Name & " -----";
    else
      -- Empty separator
      null;
    end if;
    return Res.Image;
  end Image;

  -- Re- set the list of bookmarks
  procedure Load_List is
    Bookmarks : constant Config.Bookmark_Array := Config.Get_Bookmarks;
  begin
    Afpx.Line_List.Delete_List;
    for I in Bookmarks'Range loop
      Insert_List (Image (Bookmarks(I)));
    end loop;
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;
  end Load_List;

  -- Bookmark name (from Get field)
  Cursor_Col : Con_Io.Col_Range;
  function Get_Name return String is
    Str : constant String
        := Utils.Parse_Spaces (Afpx.Decode_Field (Afpx_Xref.Bookmarks.Name,
                               0, False));
  begin
    Afpx.Clear_Field (Afpx_Xref.Bookmarks.Name);
    Cursor_Col := 0;
    return Str;
  end Get_Name;

  -- Handle Bookmarks screen
  -- Returns new dir to change to, or "" if unchanged
  function Handle return String is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range, Afpx.Descriptor_Range;

    -- Current dir
    Curr_Dir : constant String := Directory.Get_Current;
    Dir_Width : Afpx.Width_Range;

    -- Current position in Afpx list
    Position : Positive;

    -- Bookmark
    Bookmark : Config.Bookmark_Rec;

    Dummy : Boolean;

  begin
    -- Init Afpx
    Afpx.Use_Descriptor (Afpx_Xref.Bookmarks.Dscr_Num);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;
    Dir_Width := Afpx.Get_Field_Width (10);

    -- Encode dir
    Afpx.Clear_Field (Afpx_Xref.Bookmarks.Dir);
    Afpx.Encode_Field (Afpx_Xref.Bookmarks.Dir, (0, 0),
                       Utils.Normalize (Curr_Dir, Dir_Width));

    -- Encode Bookmarks
    Load_List;

    -- Main loop
    loop
      -- No Goto nor Del nor Move if no Bookmark
      if Afpx.Line_List.Is_Empty then
        Utils.X.Protect_Field (Afpx_Xref.Bookmarks.Go);
        Utils.X.Protect_Field (Afpx_Xref.Bookmarks.Moveup);
        Utils.X.Protect_Field (Afpx_Xref.Bookmarks.Movedown);
        Utils.X.Protect_Field (Afpx_Xref.Bookmarks.Del);
      else
        Afpx.Reset_Field (Afpx_Xref.Bookmarks.Go);
        Afpx.Reset_Field (Afpx_Xref.Bookmarks.Moveup);
        Afpx.Reset_Field (Afpx_Xref.Bookmarks.Movedown);
        Afpx.Reset_Field (Afpx_Xref.Bookmarks.Del);
      end if;

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Redisplay, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return "";
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
              Afpx.List_Manager.Scroll(
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.Bookmarks.Addcurr =>
              -- Add current
              Bookmark := (As.U.Tus (Get_Name),
                           As.U.Tus (Directory.Get_Current));
              if Afpx.Line_List.Is_Empty then
                Config.Add_Bookmark (0, Bookmark);
              else
                Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
              end if;
              Insert_List (Image (Bookmark));
            when Afpx_Xref.Bookmarks.Addsep =>
              -- Add separator
              Bookmark := (As.U.Tus (Get_Name), As.U.Asu_Null);
              if Afpx.Line_List.Is_Empty then
                Config.Add_Bookmark (0, Bookmark);
              else
                Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
              end if;
              Insert_List (Image (Bookmark));

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

            when Afpx_Xref.Bookmarks.Del =>
              -- Del
              Bookmark := Config.Get_Bookmark (Afpx.Line_List.Get_Position);
              -- Confirm bookmark deletion (no confirmation for separators)
              if Bookmark.Path.Is_Null
              or else Confirm ("Remove bookmark", Image (Bookmark)) then
                Config.Del_Bookmark (Afpx.Line_List.Get_Position);
                Afpx.Line_List.Delete (Moved => Dummy);
              end if;
              -- Restore descriptor
              if Afpx.Get_Descriptor /= Afpx_Xref.Bookmarks.Dscr_Num then
                Afpx.Use_Descriptor (Afpx_Xref.Bookmarks.Dscr_Num);
              end if;
            when Afpx_Xref.Bookmarks.Back =>
              -- Back
              return "";
            when others =>
              -- Other button?
              null;
          end case;

        when Afpx.Fd_Event =>
          null;
        when Afpx.Timer_Event =>
          null;
        when Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          Redisplay := True;
      end case;
    end loop;

  end Handle;

end Bookmarks;

