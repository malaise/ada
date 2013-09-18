with As.U.Utils, Con_Io, Afpx.List_Manager, Unicode;
with Utils.X, Git_If, Afpx_Xref, Error;
package body Push is

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Utils.X.Encode_Line ("", From.Image, "",
                         Afpx.Get_Field_Width (Afpx.List_Field_No), Line);
  end Set;

  procedure Init_List is new Afpx.List_Manager.Init_List (
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set, False);

  -- Afpx line list with "origin"
  Origin : Afpx.Line_Rec;
  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  -- The references
  References : Git_If.Reference_Mng.List_Type;

  -- Push
  function Do_Push return Boolean is
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    if Git_If.Do_Push (References.Access_Current.Image) then
      return True;
    else
      Error ("Pushing to", References.Access_Current.Image, "");
      return False;
    end if;
  end Do_Push;

  -- Pull current branch
  function Do_Pull return Boolean is
    Branch : As.U.Asu_Us;
  begin
    -- Get current branch
    Afpx.Suspend;
    Branch := As.U.Tus (Git_If.Current_Branch);
    Afpx.Resume;

    -- Pulll current branch from current remote
    References.Move_At (Afpx.Line_List.Get_Position);
    if Git_If.Do_Pull (References.Access_Current.Image, Branch.Image) then
      return True;
    else
      Error ("Pulling from", References.Access_Current.Image, Branch.Image);
      return False;
    end if;
  end Do_Pull;

  -- Handle the Push
  function Handle (Root : String; Pull : Boolean) return Boolean is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    -- Result of Push or Pull
    Result : Boolean;
    use type Afpx.Absolute_Field_Range;

    procedure Init is
    begin
      -- Afpx stuff
      Afpx.Use_Descriptor (Afpx_Xref.Push.Dscr_Num);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;

      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Push.Root);

      -- Encode current branch
      Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                            Afpx_Xref.Push.Branch);

      -- Change title and Push pbutton if Pull
      if Pull then
        Utils.X.Center_Field ("Pull", Afpx_Xref.Push.Title);
        Utils.X.Center_Field ("Pull", Afpx_Xref.Push.Push);
      end if;

      -- Get list of references
      Git_If.List_References (References);
      Init_List (References);
      if References.Is_Empty then
        Afpx.Set_Field_Activation (Afpx_Xref.Push.Push, False);
      else
        -- Move to "origin" or top
        Afpx.Encode_Line (Origin, "origin");
        if not Search (Afpx.Line_List, Origin,
                       From => Afpx.Line_List_Mng.Absolute) then
          Afpx.Line_List.Rewind;
        end if;
      end if;

    end Init;

  begin
    -- Reset Afpx list

    -- Init Afpx
    Init;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return False;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx_Xref.Push.Push =>
              if Pull then
                Result := Do_Pull;
              else
                Result := Do_Push;
              end if;
              if Result then
                -- Push/Pull OK
                return True;
              else
                -- Push/Pull KO
                Init;
              end if;
            when Afpx_Xref.Push.Back =>
              -- Back button
              return False;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
          | Afpx.Refresh =>
         null;
      end case;
    end loop;

  end Handle;

end Push;

