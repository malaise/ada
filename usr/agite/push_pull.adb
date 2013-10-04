with As.U.Utils, Afpx.List_Manager, Unicode;
with Utils.X, Git_If, Afpx_Xref, Error;
package body Push_Pull is

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
    Log : As.U.Asu_Us;
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    Afpx.Suspend;
    Log := As.U.Tus (Git_If.Do_Push (References.Access_Current.Image));
    Afpx.Resume;
    if Log.Is_Null then
      return True;
    else
      Error ("Pushing to", References.Access_Current.Image, Log.Image);
      return False;
    end if;
  end Do_Push;

  -- Pull current branch
  function Do_Pull return Boolean is
    Branch : As.U.Asu_Us;
    Index : Natural;
    Log : As.U.Asu_Us;
  begin
    -- Get current branch
    Afpx.Suspend;
    Branch := As.U.Tus (Git_If.Current_Branch);
    Afpx.Resume;

    -- Pull current branch:remote
    References.Move_At (Afpx.Line_List.Get_Position);
    References.Read (Branch, Git_If.Reference_Mng.Current);
    Index := Branch.Locate (Git_If.Separator & "");
    if Index < 2 or else Index > Branch.Length - 1 then
      Log.Set ("Invalid remote brach format");
    else
      Afpx.Suspend;
      Log := As.U.Tus (
          Git_If.Do_Pull (Branch.Slice (1, Index - 1),
                          Branch.Slice (Index + 1, Branch.Length)));
      Afpx.Resume;
    end if;
    if Log.Is_Null then
      return True;
    else
      Error ("Pulling branch", References.Access_Current.Image, Log.Image);
      return False;
    end if;
  end Do_Pull;

  -- Handle the Push
  function Handle (Root : String; Pull : Boolean) return Boolean is
    -- Afpx stuff
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result   : Afpx.Result_Rec;
    -- Current branch
    Branch : As.U.Asu_Us;
    -- Result of Push or Pull
    Result : Boolean;
    use type Afpx.Absolute_Field_Range;

    procedure Init is
    begin
      -- Afpx stuff
      Afpx.Use_Descriptor (Afpx_Xref.Push_Pull.Dscr_Num);
      Get_Handle := (others => <>);

      -- Encode Root
      Utils.X.Encode_Field (Root, Afpx_Xref.Push_Pull.Root);

      -- Encode current branch
      Afpx.Suspend;
        Branch := As.U.Tus (Git_If.Current_Branch);
      Afpx.Resume;
      Utils.X.Encode_Field (Utils.X.Branch_Image (Branch.Image),
                            Afpx_Xref.Push_Pull.Branch);

      -- Change title and Push button if Pull
      if Pull then
        Utils.X.Center_Field ("Pull", Afpx_Xref.Push_Pull.Title);
        Utils.X.Center_Field ("Pull", Afpx_Xref.Push_Pull.Push);
        Afpx.Clear_Field (Afpx_Xref.Push_Pull.Entries);
        Afpx.Encode_Field (Afpx_Xref.Push_Pull.Entries, (0, 0),
                           "remote branches");
      end if;

      -- Get list of references
      Afpx.Suspend;
      if Pull then
        Git_If.List_Branches (Local => False, Branches => References);
      else
        Git_If.List_References (References);
      end if;
      Afpx.Resume;
      Init_List (References);

      Utils.X.Protect_Field (Afpx_Xref.Push_Pull.Push, References.Is_Empty);
      if not References.Is_Empty then
        -- Move to "origin" for push, or "origin":<branch> for pull, or top
        if not Pull then
          Afpx.Encode_Line (Origin, "origin");
        elsif not Branch.Is_Null then
          Afpx.Encode_Line (Origin,
                            "origin" & Git_If.Separator & Branch.Image);
        end if;
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
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

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
            when Afpx.List_Field_No | Afpx_Xref.Push_Pull.Push =>
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
            when Afpx_Xref.Push_Pull.Back =>
              -- Back button
              return False;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
         null;
      when Afpx.Refresh =>
        -- Encode current branch
        Afpx.Suspend;
        Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                              Afpx_Xref.Push_Pull.Branch);
        Afpx.Resume;
      end case;
    end loop;

  end Handle;

end Push_Pull;

