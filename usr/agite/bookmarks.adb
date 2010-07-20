with As.U; use As.U;
with Directory, Con_Io, Afpx.List_Manager;
with Utils, Config;
package body Bookmarks is

  -- Strip potential leading "(name) " of bookmark
  function Dir_Of (Index : in Positive) return String is
  begin
    return Asu_Ts (Config.Get_Bookmarks (Index).Path);
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
    Res : Asu_Us;
    use type Asu_Us;
  begin
    if not Asu_Is_Null (Bookmark.Path) then
      -- Regular bookmark
      if not Asu_Is_Null (Bookmark.Name) then
        -- Regular bookmark with name
        Res := "(" & Bookmark.Name & ") " & Bookmark.Path;
      else
        -- Regular bookmark without name
        Res := Bookmark.Path;
      end if;
    elsif not Asu_Is_Null (Bookmark.Name) then
      -- Separator with name
      Res := "----- " & Bookmark.Name & " -----";
    else
      -- Emty separator
      null;
    end if;
    return Asu_Ts (Res);
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
        := Utils.Parse_Spaces (Afpx.Decode_Field (12, 0, False));
  begin
    Afpx.Clear_Field (12);
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
    use type Afpx.Absolute_Field_Range;

    -- Current dir
    Curr_Dir : constant String := Directory.Get_Current;
    Dir_Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (10);

    -- Current position in Afpx list
    Position : Positive;

    -- Bookmark
    Bookmark : Config.Bookmark_Rec;

    Dummy : Boolean;

  begin
    -- Init Afpx
    Afpx.Use_Descriptor (2);
    Cursor_Field := Afpx.Next_Cursor_Field (0);
    Cursor_Col := 0;
    Insert := False;
    Redisplay := True;

    -- Encode dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0), Utils.Normalize (Curr_Dir, Dir_Width));

    -- Encode Bookmarks
    Load_List;

    -- Main loop
    loop
      -- No Goto nor Del nor Move if no Bookmark
      if Afpx.Line_List.Is_Empty then
        Utils.Protect_Field (15);
        Utils.Protect_Field (16);
        Utils.Protect_Field (17);
        Utils.Protect_Field (18);
      else
        Afpx.Reset_Field (15);
        Afpx.Reset_Field (16);
        Afpx.Reset_Field (17);
        Afpx.Reset_Field (18);
      end if;

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay);

      Redisplay := False;
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
            when Afpx.List_Field_No | 15 =>
              -- Double click or Goto => move to bookmark
              declare
                Dir : constant String := Dir_Of (Afpx.Line_List.Get_Position);
              begin
                if Dir /= "" then
                  return Dir;
                end if;
              end;
            when Utils.List_Scroll_Fld_Range'First ..
                 Utils.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(Ptg_Result.Field_No
                                     - Utils.List_Scroll_Fld_Range'First + 1);
            when 13 =>
              -- Add current
              Bookmark := (Asu_Tus (Get_Name), Asu_Tus (Directory.Get_Current));
              if Afpx.Line_List.Is_Empty then
                Config.Add_Bookmark (0, Bookmark);
              else
                Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
              end if;
              Insert_List (Image (Bookmark));
            when 14 =>
              -- Add separator
              Bookmark := (Asu_Tus (Get_Name), Asu_Null);
              if Afpx.Line_List.Is_Empty then
                Config.Add_Bookmark (0, Bookmark);
              else
                Config.Add_Bookmark (Afpx.Line_List.Get_Position, Bookmark);
              end if;
              Insert_List (Image (Bookmark));

            when 16 =>
              -- Move bookmark up
              Position := Afpx.Line_List.Get_Position;
              if Position /= 1 then
                Config.Move_Bookmark (Position, Up => True);
                Load_List;
                Afpx.Line_List.Move_At (Position - 1);
              end if;
            when 17 =>
              -- Move bookmark down
              Position := Afpx.Line_List.Get_Position;
              if Position /= Afpx.Line_List.List_Length then
                Config.Move_Bookmark (Position, Up => False);
                Load_List;
                Afpx.Line_List.Move_At (Position + 1);
              end if;

            when 18 =>
              -- Del
              Config.Del_Bookmark (Afpx.Line_List.Get_Position);
              Afpx.Line_List.Delete (Moved => Dummy);
            when 19 =>
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

