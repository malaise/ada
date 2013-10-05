with Ada.Exceptions;
with As.U.Utils, Directory, Afpx.List_Manager, Basic_Proc, Unicode;
with Git_If, Utils.X, Afpx_Xref, Confirm, Error;
package body Branch is

  -- List width
  List_Width : Afpx.Width_Range;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Utils.X.Encode_Line ("", From.Image, "", List_Width, Line);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in branches on " & From.Image);
  end Set;

  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  procedure Init_List is new Afpx.List_Manager.Init_List (
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set, False);

  -- Root path
  Root : As.U.Asu_Us;

  -- The Branches
  Branches : Git_If.Branches_Mng.List_Type;

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Branches.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Branches.Root);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
  end Init;

  -- Re assess the list of branches
  procedure Reread (Restore : in Boolean) is
    Line : Afpx.Line_Rec;
  begin
    -- Save current selection
    if Restore and then not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
    else
      Line.Len := 0;
    end if;

    -- Get list of changes
    Afpx.Suspend;
    Git_If.List_Branches (Local => True, Branches => Branches);
    if not Restore then
      -- Set default to current branch
      Set (Line, As.U.Tus (Git_If.Current_Branch));
    end if;
    Afpx.Resume;

    -- Encode the list
    Init_List (Branches);
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
    Afpx.Update_List (Afpx.Center_Selected);

    -- Encode current branch
    Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                          Afpx_Xref.Branches.Branch);
    -- Set field activity
    Utils.X.Protect_Field (Afpx_Xref.Branches.Rename, Afpx.Line_List.Is_Empty);
    Utils.X.Protect_Field (Afpx_Xref.Branches.Delete, Afpx.Line_List.Is_Empty);
    Utils.X.Protect_Field (Afpx_Xref.Branches.Checkout, Afpx.Line_List.Is_Empty);
   exception
     when others =>
       if Afpx.Is_Suspended then
         Afpx.Resume;
       end if;
       raise;
  end Reread;

  -- Actions on branches
  type Action_List is (Create, Rename, Delete, Checkout);
  function Do_Action (Action : in Action_List) return Boolean is
    Curr_Name, New_Name : As.U.Asu_Us;
    Message, Result : As.U.Asu_Us;
  begin
    -- Retrieve current name
    if Action /= Create then
      Branches.Move_At (Afpx.Line_List.Get_Position);
      Curr_Name := Branches.Access_Current.all;
    end if;
    if Action = Create or else Action = Rename then
      Afpx.Decode_Field (Afpx_Xref.Branches.Name, 0, New_Name);
    end if;

    -- Cancel if not confirm
    if Action = Delete or else Action = Checkout then
      if not Confirm (
          (if Action = Delete then "Delete" else "Checkout") & " branch",
          Curr_Name.Image) then
        Init;
        Reread (True);
        return False;
      else
        Init;
      end if;
    end if;

    -- Git_If
    Afpx.Suspend;
    case Action is
      when Create =>
        Message := As.U.Tus ("Creating branch " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Create_Branch (New_Name.Image));
        end if;
      when Rename =>
        Message := As.U.Tus ("Renaming branch " & Curr_Name.Image
                           & " to " & New_Name.Image);
        if not New_Name.Is_Null then
          Result := As.U.Tus (Git_If.Rename_Branch (Curr_Name.Image,
                                                    New_Name.Image));
        end if;
      when Delete =>
        Message := As.U.Tus ("Deleting branch " & Curr_Name.Image);
        Result := As.U.Tus (Git_If.Delete_Branch (Curr_Name.Image));
      when Checkout =>
        Message := As.U.Tus ("Cehcking out branch " & Curr_Name.Image);
        Result := As.U.Tus (Git_If.Do_Checkout (Curr_Name.Image));
    end case;
    Afpx.Resume;

    -- Handle error
    if not Result.Is_Null then
      Error (Message.Image, "", Result.Image);
      Init;
      Reread (False);
      return False;
    end if;

    -- Done
    Reread (False);
    -- Successful checkout leads to return
    return Action = Checkout;
  end Do_Action;

  -- Handle the Branches
  procedure Handle (Root : in String) is
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Field_Range;
  begin

    -- Move to root
    Branch.Root := As.U.Tus (Root);
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- List width
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode Branches
    Reread (False);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Move to next line of comment
              if Get_Handle.Cursor_Field = Afpx_Xref.Commit.Comment7 then
                Get_Handle.Cursor_Field := Afpx_Xref.Commit.Comment1;
              else
                Get_Handle.Cursor_Field := Afpx.Next_Cursor_Field (
                    Get_Handle.Cursor_Field);
              end if;
              Get_Handle.Cursor_Col := 0;
            when Afpx.Escape_Key =>
              -- Back
              return;
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
            when Afpx.List_Field_No | Afpx_Xref.Branches.Checkout =>
              if Do_Action (Checkout) then
                return;
              end if;
            when Afpx_Xref.Branches.Create =>
              if Do_Action (Create) then
                return;
              end if;
            when Afpx_Xref.Branches.Rename =>
              if Do_Action (Rename) then
                return;
              end if;
            when Afpx_Xref.Branches.Delete =>
              if Do_Action (Delete) then
                return;
              end if;
            when Afpx_Xref.Branches.Back =>
              -- Back button
              return;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
       when Afpx.Refresh =>
         -- Reread branches
         Reread (True);
      end case;
    end loop;

  end Handle;

end Branch;

