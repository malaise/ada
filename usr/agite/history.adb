with Con_Io, Afpx.List_Manager, String_Mng;
with Utils, Git_If, Config, Details, View;
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

  -- Handle the history of a file or dir
  procedure Handle (Path, Name : in String; Is_File : in Boolean) is
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

    -- Init Afpx
    procedure Init is
    begin
      Afpx.Use_Descriptor (3);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;
      Redisplay := True;
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
        when Show_Details =>
          Details.Handle (Log.Hash);
          Init;
      end case;
    end Show;

  begin
    -- Init Afpx
    Init;

    -- Width after "YY-MM-DD HH:MM:SS "
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 13;

    -- Encode file/dir
    Afpx.Clear_Field (10);
    Afpx.Encode_Field (10, (0, 0),
               Utils.Normalize (Name, Afpx.Get_Field_Width (10)));

    -- Get history
    Afpx.Suspend;
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
              Init_List (Logs);
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

