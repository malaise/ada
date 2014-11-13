with As.U, Afpx.List_Manager, Str_Util;
with Utils.X, Afpx_Xref, Details, Confirm, Checkout;
separate (Tags)

procedure List (Root : in String) is

   -- List Width
  List_Width : Afpx.Width_Range;

  -- Image of a tag
  function Image (Tag : Git_If.Tag_Entry_Rec) return String is
  begin
    return Tag.Name.Image
         & (if Tag.Annotated then " " & Tag.Comment.Image else "");
  end Image;

  -- Encode a line of list
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Tag_Entry_Rec) is
  begin
    Utils.X.Encode_Line ("", Image (From), "", List_Width, Line, False);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    Git_If.Tag_Entry_Rec, Git_If.Tag_Mng, Set, False);

  -- Afpx stuff
  Get_Handle  : Afpx.Get_Handle_Rec;
  Ptg_Result  : Afpx.Result_Rec;
  use type Afpx.Absolute_Field_Range;

  -- List of matching tags
  Tags_List : Git_If.Tag_List;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.List_Tags.Dscr_Num);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    Utils.X.Encode_Field (Root, Afpx_Xref.List_Tags.Root);
    Utils.X.Encode_Branch (Afpx_Xref.List_Tags.Branch);
    Get_Handle := (others => <>);

    -- Clear the list
    Afpx.Line_List.Delete_List;
  end Init;

  -- Get the tags and encode in list
  procedure Read_Tags is
    Template : As.U.Asu_Us;
  begin
    Template := As.U.Tus (Str_Util.Strip (
          Afpx.Decode_Field (Afpx_Xref.List_Tags.Template, 0, False)));
    -- Get tags list
    Git_If.List_Tags (Template.Image, Tags_List);
    Init_List (Tags_List);
    Afpx.Update_List (Afpx.Center_Selected);
  end Read_Tags;

  -- Checkout current tag
  function Do_Checkout return Boolean is
    Pos : Positive;
    Tag : Git_If.Tag_Entry_Rec;
  begin
    -- Save position in List and read it
    Pos := Afpx.Line_List.Get_Position;
    Tags_List.Move_At (Pos);
    Tags_List.Read (Tag, Git_If.Tag_Mng.Dyn_List.Current);

    -- Checkout (success will lead to return to Directory)
    if Checkout.Handle (Root, "tag: " & Tag.Name.Image,
                        Image (Tag), Tag.Hash) then
      return True;
    else
      -- Restore screen
      Init;
      Init_List (Tags_List);
      Afpx.Line_List.Move_At (Pos);
      Afpx.Update_List (Afpx.Center_Selected);
      return False;
    end if;
  end Do_Checkout;

  -- Current tag, template, position
  Current_Tag : As.U.Asu_Us;
  Current_Tmpl : As.U.Asu_Us;
  Current_Pos : Natural;

  -- Save current context
  procedure Save is
  begin
    if Afpx.Line_List.Is_Empty then
      Current_Pos := 0;
    else
      Current_Pos := Afpx.Line_List.Get_Position;
    end if;
    Current_Tmpl := As.U.Tus (Str_Util.Strip (
      Afpx.Decode_Field (Afpx_Xref.List_Tags.Template, 0, False)));
  end Save;

  -- Restore context
  procedure Restore (Position : in Boolean) is
  begin
    -- Reset Afpx and reread tags
    Init;
    Afpx.Encode_Field (Afpx_Xref.List_Tags.Template, (0, 0),
                       Current_Tmpl.Image);
    Read_Tags;
    -- Restore position
    if Position
    and then Current_Pos > 0
    and then Current_Pos <= Afpx.Line_List.List_Length then
       Afpx.Line_List.Move_At (Current_Pos);
       Afpx.Update_List (Afpx.Center_Selected);
    else
      Afpx.Update_List (Afpx.Bottom);
    end if;
  end Restore;

begin

  -- Init Afpx
  Init;
  Init_List (Tags_List);

  -- Main loop
  loop

    -- Activate Checkout and Delete if list is not empty
    Afpx.Set_Field_Activation (Afpx_Xref.List_Tags.Details,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.List_Tags.Checkout,
                               not Afpx.Line_List.Is_Empty);
    Afpx.Set_Field_Activation (Afpx_Xref.List_Tags.Delete,
                               not Afpx.Line_List.Is_Empty);

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result, True);

    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Read_Tags;
          when Afpx.Escape_Key =>
            -- Back
            return;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Afpx.List_Field_No =>
            -- Double click => No action
            null;
          when Utils.X.List_Scroll_Fld_Range'First ..
               Utils.X.List_Scroll_Fld_Range'Last =>
            -- Scroll list
            Afpx.List_Manager.Scroll (
               Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
          when Afpx_Xref.List_Tags.List =>
            Read_Tags;
          when Afpx_Xref.List_Tags.Details =>
            -- Details of tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Current_Tag := Tags_List.Access_Current.Name;
            Save;
            Details.Handle (Root, Current_Tag.Image);
            Restore (True);
          when Afpx_Xref.List_Tags.Checkout =>
            -- Checkout current tag
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Save;
            if Do_Checkout then
              return;
            end if;
            Restore (True);
          when Afpx_Xref.List_Tags.Delete =>
            -- Delete tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Current_Tag := Tags_List.Access_Current.Name;
            Save;
            if Confirm  ("Delete Tag", Current_Tag.Image) then
              Git_If.Delete_Tag (Str_Util.Strip (
                  Tags_List.Access_Current.Name.Image));
              Restore (False);
            else
              Restore (True);
            end if;
          when Afpx_Xref.List_Tags.Back =>
            return;
          when others =>
            null;
        end case;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
        null;
      when Afpx.Refresh =>
        -- Encode current branch
        begin
          Utils.X.Encode_Branch (Afpx_Xref.List_Tags.Branch);
        exception
          when others =>
            Afpx.Resume;
        end;
    end case;
  end loop;

end List;

