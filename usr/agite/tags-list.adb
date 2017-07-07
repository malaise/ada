with Afpx.Utils, Str_Util;
with Utils.X, Utils.Store, Afpx_Xref, Config, History, Details, Confirm,
     Checkout, Push_Pull;
separate (Tags)

procedure List (Root : in String) is

   -- List Width
  List_Width : Afpx.Width_Range;

  -- Image of a tag
  function Image (Tag : Git_If.Tag_Entry_Rec) return String is
    (Tag.Name.Image & (if Tag.Annotated then " " & Tag.Comment.Image else ""));

  -- Encode a line of list
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in Git_If.Tag_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line ("", Image (From), "", List_Width, Line, False);
  end Set;
  procedure Init_List is new Afpx.Utils.Init_List (
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
    -- Restore template from previous invocation
    Utils.X.Encode_Field (Template.Image, Afpx_Xref.List_Tags.Template);

    Get_Handle := (others => <>);

    -- Clear the list
    Afpx.Line_List.Delete_List;
  end Init;

  -- To sort tags by date, return false if A before B so newest tags first
  function Less_Than (A, B : in Git_If.Tag_Entry_Rec) return Boolean is
    (if A.Date /= Git_If.No_Date and then B.Date /= Git_If.No_Date then
       (if A.Date /= B.Date then A.Date > B.Date
        else A.Name.Image > B.Name.Image)
     elsif A.Date /= Git_If.No_Date then True
     elsif B.Date /= Git_If.No_Date then False
     else A.Name.Image > B.Name.Image);
  procedure Sort is new Git_If.Tag_Mng.Dyn_List.Sort (Less_Than);

  -- Read the template
  procedure Read_Template is
  begin
    Template := As.U.Tus (Str_Util.Strip (
            Afpx.Decode_Field (Afpx_Xref.List_Tags.Template, 0, False)));
  end Read_Template;

  -- Get the tags and encode in list
  procedure Read_Tags (Reread : in Boolean) is
  begin
    if Reread then
      Read_Template;
      -- Get tags list
      Git_If.List_Tags (Template.Image, Tags_List);
      Sort (Tags_List);
    end if;
    Init_List (Tags_List);
    if not Afpx.Line_List.Is_Empty then
      Afpx.Line_List.Rewind;
    end if;
    Afpx.Update_List (Afpx.Top);
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
  Current_Tag : Git_If.Tag_Entry_Rec;
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
  procedure Restore (Position, Reread : in Boolean) is
  begin
    -- Reset Afpx and reread tags
    Init;
    Afpx.Encode_Field (Afpx_Xref.List_Tags.Template, (0, 0),
                       Current_Tmpl.Image);
    Read_Tags (Reread);
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

  Dummy_Res : Boolean;
begin

  -- Init Afpx
  Init;
  -- Read all tags if a template is set or if configured to read all
  if not Template.Is_Null or else Config.List_Tags then
    Read_Tags (True);
  else
    Init_List (Tags_List);
  end if;

  -- Main loop
  loop

    -- Activate Detail, Checkout, Delete and Push only if list is not empty
    Afpx.Utils.Protect_Field (Afpx_Xref.List_Tags.Details,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.List_Tags.Checkout,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.List_Tags.Delete,
                              Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.List_Tags.Push,
                              Afpx.Line_List.Is_Empty);

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Read_Tags (True);
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
          when Utils.X.List_Scroll_Fld_Range =>
            -- Scroll list
            Afpx.Utils.Scroll (
               Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
          when Afpx_Xref.List_Tags.List =>
            Read_Tags (True);
          when Afpx_Xref.List_Tags.Create =>
            -- Forbid modif but allow tagging
            History.List (Root, "", "", "", False, False, True);
            Restore (False, True);
          when Afpx_Xref.List_Tags.Details =>
            -- Details of tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Tags_List.Read (Current_Tag, Git_If.Tag_Mng.Dyn_List.Current);
            Save;
            -- No modif nor tagging
            Details.Handle (Root, "", Current_Tag.Name.Image, False, False,
                (if Current_Tag.Annotated then Current_Tag.Date
                 else ""),
                (if Current_Tag.Annotated then Current_Tag.Comment.Image
                 else ""));
            Restore (True, False);
          when Afpx_Xref.List_Tags.Checkout =>
            -- Checkout current tag
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Save;
            if Do_Checkout then
              return;
            end if;
            Restore (True, False);
          when Afpx_Xref.List_Tags.Delete =>
            -- Delete tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Tags_List.Read (Current_Tag, Git_If.Tag_Mng.Dyn_List.Current);
            Save;
            if Confirm  ("Delete Tag", Current_Tag.Name.Image) then
              Git_If.Delete_Tag (Str_Util.Strip (Current_Tag.Name.Image));
              Restore (False, True);
            else
              Restore (True, False);
            end if;
          when Afpx_Xref.List_Tags.Push =>
            -- Push tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Tags_List.Read (Current_Tag, Git_If.Tag_Mng.Dyn_List.Current);
            Save;
            Dummy_Res := Push_Pull.Handle (Root, Pull => False,
                                           Tag => Current_Tag.Name.Image);
            Restore (True, False);
          when Afpx_Xref.List_Tags.Mark =>
            -- Store hash of current tag
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Tags_List.Read (Current_Tag, Git_If.Tag_Mng.Dyn_List.Current);
            Utils.Store.Hash := Current_Tag.Hash;
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

