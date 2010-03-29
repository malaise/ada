with Ada.Exceptions;
with Con_Io, Afpx.List_Manager, String_Mng, Basic_Proc, Normal;
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
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised on history of " & From.Hash);
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
  procedure Handle (Root, Path, Name : in String;
                    Is_File : in Boolean;
                    Hash : in Git_If.Git_Hash := Git_If.No_Hash) is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    List_Height  : Afpx.Height_Range;
    List_Width   : Afpx.Width_Range;
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
      -- List characteristics
      Afpx.Get_Field_Size (Afpx.List_Field_No, List_Height, List_Width);
      -- Encode file/dir
      Afpx.Clear_Field (10);
      if Is_File then
        Afpx.Encode_Field (10, (0, 0),
               Utils.Normalize (Path & Name, Afpx.Get_Field_Width (10)));
      else
        if Name /= "" then
          Afpx.Encode_Field (10, (0, 0),
                 Utils.Normalize (Path & Name & "/", Afpx.Get_Field_Width (10)));
        elsif Path /= "" then
          Afpx.Encode_Field (10, (0, 0),
                 Utils.Normalize (Path , Afpx.Get_Field_Width (10)));
        else
          Afpx.Encode_Field (10, (0, 0),
                 Utils.Normalize ("/" , Afpx.Get_Field_Width (10)));
        end if;
        -- Lock button View
        Afpx.Get_Descriptor_Background (Background);
        Afpx.Set_Field_Protection (17, True);
        Afpx.Set_Field_Colors (17, Foreground => Con_Io.Black,
                                   Background => Background);

      end if;
    end Init;

    -- Show delta from current in list to comp
    procedure Show_Delta (Ref : in Natural) is
      Comp : Positive;
      Ref_Hash, Comp_Hash : Git_If.Git_Hash;
    begin
      -- Save position in List
      Comp := Afpx.Line_List.Get_Position;

      -- Read reference hash in Logs
      if Ref = 0 then
        -- Only Left selection
        Ref_Hash := Git_If.No_Hash;
      else
        Logs.Move_At (Ref);
        Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
        Ref_Hash := Log.Hash;
      end if;

      -- Move to Comp and read comp hash in Logs
      Logs.Move_At (Comp);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      Comp_Hash := Log.Hash;

      -- Restore position in List
      Afpx.Line_List.Move_At (Comp);

      -- Call delta
      if Ref_Hash = Git_If.No_Hash then
        -- Only Left selection: Hash^ and Hash
        Git_If.Launch_Delta (Config.Differator, Root & Path & Name,
                             Comp_Hash & "^", Comp_Hash);
      else
        Git_If.Launch_Delta (Config.Differator, Root & Path & Name,
                             Ref_Hash, Comp_Hash);
      end if;
    end Show_Delta;

    -- View file of commit details
    type Show_List is (Show_View, Show_Details);
    procedure Show (What : in Show_List) is
      Ref : Positive;
    begin
      -- Read reference hash in Logs
      Ref := Afpx.Line_List.Get_Position;
      Logs.Move_At (Ref);
      Logs.Read (Log, Git_If.Log_Mng.Dyn_List.Current);
      case What is
        when Show_View =>
          View (Path & Name, Log.Hash);
          Redisplay := True;
        when Show_Details =>
          Details.Handle (Root, Log.Hash);
          Init;
          Init_List (Logs);
          Afpx.Update_List (Afpx.Center);
      end case;
    end Show;

    -- Update the list status
    procedure List_Change (Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      pragma Unreferenced (Action);
      Percent : Natural;
      Last_Top : Integer;
    begin
      -- Compute %
      if Status.Id_Top = 0 then
        -- Empty list
        Percent := 0;
      else
        -- At which percent is the bottom shown
        -- Top index when at bottom
        Last_Top := Afpx.Line_List.List_Length - List_Height + 1;
        -- Factor = (100 - 1) / (LastTop - 1)
        -- Percent - 1 = (Top - 1) * Factor
        Percent := (Status.Id_Top - 1) * (100 - 1) / (Last_Top - 1)  + 1;
      end if;
      Afpx.Encode_Field (11, (0, 0), Normal (Percent, 3, True));
      Afpx.Encode_Field (14, (0, 0),
           Normal (Status.Ids_Selected(Afpx.List_Left),
                   Afpx.Get_Field_Width (14), False));
      Afpx.Encode_Field (16, (0, 0),
           Normal (Status.Ids_Selected(Afpx.List_Right),
                   Afpx.Get_Field_Width (16), False));
    end List_Change;

  begin
    -- Init Afpx
    Init;

    -- Width after "YY-MM-DD HH:MM:SS "
    Width := Afpx.Get_Field_Width (Afpx.List_Field_No) - 13;

    -- Get history
    Afpx.Suspend;
    Redisplay := True;
    begin
      Git_If.List_Log (Root & Path & Name, Logs);
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
                         Ptg_Result, Redisplay, True,
                         List_Change_Cb => List_Change'Access);

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
            when Afpx.List_Field_No | 17 =>
              -- Double click or View => View if file
              if Is_File then
                Show (Show_View);
              end if;
            when Utils.List_Scroll_Fld_Range'First ..
                 Utils.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(Ptg_Result.Field_No
                                     - Utils.List_Scroll_Fld_Range'First + 1);

            when 18 =>
              -- Diff
              Show_Delta (Ptg_Result.Id_Selected_Right);
            when 19 =>
              -- Details
              Show (Show_Details);
            when 20 =>
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

