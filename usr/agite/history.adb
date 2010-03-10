with Con_Io, Afpx.List_Manager, String_Mng;
with Utils, Config, Details, View;
package body History is

  -- Cut string if too long for list
  -- Width after "YY-MM-DD HH:MM:SS "
  Width : Afpx.Width_Range;
  function Procuste (Str : String) return String is
  begin
  if Str'Length <= Width then
    -- String fits. OK
    return Str;
  else
    -- Trunk tail and show " <" at the end
    return String_Mng.Procuste (Str, Width, Trunc_Head => False);
  end if;
  end Procuste;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in  Git_If.Log_Entry_Rec) is
  begin
    Afpx.Encode_Line (Line,
        -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD HH:MM "
        From.Date(03 .. 04) & From.Date(06 .. 07) & From.Date(09 .. 10) & '-'
      & From.Date(12 .. 13) & From.Date(15 .. 16) & ' '
      & Procuste (Utils.Asu_Ts (From.Comment(1))) );
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.Log_Entry_Rec, Git_If.Log_Mng, Set);

  -- To search matching hash in Log
  function Hash_Match (Current, Criteria : Git_If.Log_Entry_Rec) return Boolean is
  begin
    return Current.Hash = Criteria.Hash;
  end Hash_Match;
  procedure Hash_Search is new Git_If.Log_Mng.Dyn_List.Search (Hash_Match);

  -- Handle the history of a file or dir
  procedure Handle (Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    -- The log
    Logs : Git_If.Log_List;
    Log : Git_If.Log_Entry_Rec;

    -- Search found
    Found : Boolean;

    -- Init Afpx
    procedure Init is
      Background : Con_Io.Effective_Basic_Colors;
    begin
      Afpx.Use_Descriptor (3);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;
      -- Encode file/dir
      Afpx.Clear_Field (10);
      if Is_File then
        Afpx.Encode_Field (10, (0, 0),
               Utils.Normalize (Name, Afpx.Get_Field_Width (10)));
      else
        Afpx.Encode_Field (10, (0, 0),
               Utils.Normalize (Name & "/", Afpx.Get_Field_Width (10)));
        -- Lock buttons Edit and Diff (only leave history)
        Afpx.Get_Descriptor_Background (Background);
        Afpx.Set_Field_Protection (11, True);
        Afpx.Set_Field_Colors (11, Foreground => Con_Io.Black,
                                   Background => Background);
        Afpx.Set_Field_Protection (12, True);
        Afpx.Set_Field_Colors (12, Foreground => Con_Io.Black,
                                   Background => Background);

      end if;
    end Init;

    -- Show delta from current in list to comp
    procedure Show_Delta (Comp : in Positive) is
      Ref : Positive;
      Ref_Hash, Comp_Hash : Git_If.Git_Hash;
    begin
      -- Save position in List
      Ref := Afpx.Line_List.Get_Position;

      -- Read reference hash in Logs
      Logs.Move_To (Number => Ref - 1, From_Current => False);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      Ref_Hash := Log.Hash;

      -- Move to Comp and read comp hash in Logs
      Logs.Move_To (Number => Comp - 1, From_Current => False);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      Comp_Hash := Log.Hash;

      -- Restore position in List
      Afpx.Line_List.Move_To (Number => Ref - 1, From_Current => False);

      -- Call delta
      Git_If.Launch_Delta (Config.Differator, Name, Ref_Hash, Comp_Hash);
    end Show_Delta;

    -- View file of commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Ref : Positive;
    begin
      -- Read reference hash in Logs
      Ref := Afpx.Line_List.Get_Position;
      Logs.Move_To (Number => Ref - 1, From_Current => False);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      case What is
        when Show_View =>
          View (Path & Name, Log.Hash);
          Redisplay := True;
        when Show_Details =>
          Details.Handle (Log.Hash);
          Init;
          Init_List (Logs);
          Afpx.Update_List (Afpx.Center);
      end case;
    end Show;

  begin
    -- Init Afpx
    Init;

    -- Width after "YY-MM-DD HH:MM:SS "
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 13;

    -- Get history
    Afpx.Suspend;
    Redisplay := True;
    begin
      Git_If.List_Log (Name, Logs);
      Afpx.Resume;
    exception
      when others =>
        Afpx.Resume;
        raise;
    end;

    -- Encode history
    if Logs.Is_Empty then
      return;
    end if;
    if Hash /= Git_If.No_Hash then
      -- Set current to Hash provided
      Log.Hash := Hash;
      Hash_Search (Logs, Found, Log, From => Git_If.Log_Mng.Dyn_List.Absolute);
    end if;
    Init_List (Logs);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay, True);

      Redisplay := False;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx.List_Field_No | 11 =>
              -- Double click or View => View if file
              if Is_File then
                Show (Show_View);
              end if;
            when Utils.List_Scroll_Fld_Range'First ..
                 Utils.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(Ptg_Result.Field_No
                                     - Utils.List_Scroll_Fld_Range'First + 1);

            when 12 =>
              -- Diff, if file and Right selection
              if Is_File and then Ptg_Result.Id_Selected_Right /= 0 then
                Show_Delta (Ptg_Result.Id_Selected_Right);
              end if;
            when 13 =>
              -- Details
              Show (Show_Details);
            when 14 =>
              -- Back
              return;
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

end History;

