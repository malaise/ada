with Ada.Exceptions;
with As.U, Directory, Afpx.Utils, Basic_Proc, Unicode;
with Git_If, Utils.X, Afpx_Xref, Details, Checkout, Reset, Confirm;
package body Reflog is

  -- List width
  List_Width : Afpx.Width_Range;

  -- Root path and current branch
  Root, Target_Branch : As.U.Asu_Us;

  -- Current list o resf
  Refs : Git_If.Reflog_List;

  -- Init Afpx list from Refs
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Reflog_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line ("", From.Comment.Image, "", List_Width, Line,
                            False);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in reflog on " & From.Comment.Image);
  end Set;
  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Reflog_Entry_Rec, Git_If.Set, Git_If.Reflog_Mng, Set, False);
  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(1 .. Current.Len) = Criteria.Str(1 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Re assess the list of refs
  procedure Reread is
    Line : Afpx.Line_Rec;
  begin
    -- Save position
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    else
      Line.Len := 0;
    end if;
    -- Encode the Afpx list
    -- Get the list of local and remote branches
    Git_If.List_Reflog (Target_Branch.Image, Refs);
    Init_List (Refs);
    -- Move back to the same entry as before (if possible)
    if not Afpx.Line_List.Is_Empty then
      if Search (Afpx.Line_List, Line,
                 From => Afpx.Line_List_Mng.Absolute) then
        Afpx.Line_List.Move_At (Afpx.Line_List.Get_Position);
      else
        Afpx.Line_List.Rewind;
      end if;
    end if;
    -- Center
    Afpx.Update_List (Afpx.Top_Selected);
  end Reread;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Reflog.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Reflog.Root);
    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Reflog.Branch);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
    -- Encode target branch
    Utils.X.Encode_Field (
        (if  Target_Branch.Is_Null then
           Utils.X.Branch_Image (Git_If.Current_Branch)
         else
           Target_Branch.Image),
        Afpx_Xref.Reflog.Target_Branch);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
    -- Reread
  end Init;

  -- Actions on refs
  type Action_List is (Detail, Chkout, Reset, Delete);
  function Do_Action (Action : in Action_List) return Boolean is
    Position : Afpx.Line_List_Mng.Ll_Positive;
    Result : Boolean;
  begin
    -- Save current position and move to proper ref
    Position := Afpx.Line_List.Get_Position;
    Refs.Move_At (Position);
    case Action is
      when Detail =>
        Details.Handle (Root.Image, Target_Branch.Image,
                        Refs.Access_Current.Hash.Image, False, False);
        Result := False;
      when Chkout =>
        Result := Checkout.Handle (Root.Image,
                                   "From reflog of branch",
                                   Target_Branch.Image,
                                   Refs.Access_Current.Hash);
      when Reset =>
        Result := Reset (Root.Image, Refs.Access_Current.Hash.Image,
                         Only_Hard => True, Allow_Clean => False,
                         Comment => "from reflog");
      when Delete =>
        if Confirm ("Deleting reference", Refs.Access_Current.Hash.Image,
                    Warning => "This cannot be undone") then
          Git_If.Delete_Ref (Refs.Access_Current.Id.Image);
        end if;
        -- Remain in reflog menu
        Result := False;
    end case;
    Init;
    Reread;
    Afpx.Line_List.Move_At (Position);
    return Result;
  end Do_Action;

  -- Handle the reflogs
  function Handle (Root : String; Branch: String) return Boolean  is
    Ptg_Result : Afpx.Result_Rec;
    Result  : Boolean;
    use type Afpx.Field_Range;
  begin

    Reflog.Root := As.U.Tus (Root);
    Target_Branch := As.U.Tus (Branch);

    -- Move to root
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;
    Afpx.Line_List.Delete_List (False);

    -- Encode refs
    Reread;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                         Right_Select => True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              exit;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx_Xref.Reflog.Detail =>
              Result := Do_Action (Detail);
            when Afpx_Xref.Reflog.Checkout =>
              Result := Do_Action (Chkout);
              if Result then
                -- Back to Dir menu
                return True;
              end if;
            when Afpx_Xref.Reflog.Reset =>
              Result := Do_Action (Reset);
              if Result then
                -- Back to Dir menu
                return True;
              end if;
            when Afpx_Xref.Reflog.Delete =>
              Result := Do_Action (Delete);
            when Afpx_Xref.Reflog.Back =>
              -- Back button
              exit;
            when others =>
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Reflog.Branch);
      end case;
    end loop;

    -- No big change
    return False;
  end Handle;

end Reflog;

