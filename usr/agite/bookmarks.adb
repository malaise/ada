with Directory, Con_Io, Afpx.List_Manager, String_Mng;
with Utils, Config;
package body Bookmarks is

  -- Strip potential leading "(name) " of bookmark
  function Dir_Of (Index : in Positive) return String is
    Str : constant String := Utils.Asu_Ts (Config.Get_Bookmarks (Index));
    I : Natural;
  begin
    if Str(1) /= '(' then
      return Str;
    else
      I := String_Mng.Locate (Str, ") ");
      if I = 0 then
        -- Badly formated
        return "";
      else
        return Str (I + 2 .. Str'Last);
      end if;
    end if;
  end Dir_Of;

  procedure Insert_List (Str : in String) is
    Line : Afpx.Line_Rec;
  begin
    Afpx.Encode_Line (Line,
         Utils.Normalize (Str, Afpx.Get_Field_Width (Afpx.List_Field_No)) );
    Afpx.Line_List.Insert (Line);
  end Insert_List;

  -- Handle Bookmarks screen
  -- Returns new dir to change to, or "" if unchanged
  function Handle return String is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- Current dir
    Curr_Dir : constant String := Directory.Get_Current;
    Dir_Width : constant Afpx.Width_Range := Afpx.Get_Field_Width (10);

    Dummy : Boolean;

  begin
    -- Init Afpx
    Afpx.Use_Descriptor (2);
    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := True;

    -- Encode dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0), Utils.Normalize (Curr_Dir, Dir_Width));

    -- Encode Bookmarks
    Afpx.Line_List.Delete_List;
    declare
      Bookmarks : constant Config.Bookmark_Array := Config.Get_Bookmarks;
    begin
      for I in Bookmarks'Range loop
        Insert_List (Utils.Asu_Ts (Bookmarks(I)));
      end loop;
    end;
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;

    loop
      -- Main loop
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
            when Afpx.List_Field_No | 11 =>
              -- Double click or Goto => move to bookmark
              return Dir_Of (Afpx.Line_List.Get_Position);
            when Utils.List_Scroll_Fld_Range'First ..
                 Utils.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(Ptg_Result.Field_No
                                     - Utils.List_Scroll_Fld_Range'First + 1);
            when 12 =>
              -- Del
              Config.Del_Bookmark (Afpx.Line_List.Get_Position);
              Afpx.Line_List.Delete (Done => Dummy);
            when 13 =>
              -- Add current
              Config.Add_Bookmark (Directory.Get_Current);
              Afpx.Line_List.Rewind (Afpx.Line_List_Mng.Prev);
              Insert_List (Directory.Get_Current);
            when 14 =>
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

