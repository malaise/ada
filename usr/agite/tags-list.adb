with As.U.Utils, Afpx.List_Manager, Str_Util;
with Utils.X, Afpx_Xref, Confirm;
separate (Tags)

procedure List (Root : in String) is

   -- List Width
  List_Width : Afpx.Width_Range;
  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Utils.X.Encode_Line (From.Image, "", "", List_Width, Line, False);
  end Set;
  procedure Init_List is new Afpx.List_Manager.Init_List (
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set, False);

  -- Afpx stuff
  Get_Handle  : Afpx.Get_Handle_Rec;
  Ptg_Result  : Afpx.Result_Rec;
  use type Afpx.Absolute_Field_Range;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.List_Tags.Dscr_Num);
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    Utils.X.Encode_Field (Root, Afpx_Xref.List_Tags.Root);
    Get_Handle := (others => <>);
    Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                          Afpx_Xref.List_Tags.Branch);
    Afpx.Line_List.Delete_List;
  end Init;

  -- Get the tags and encode in list
  Tags_List : Git_If.Tags_Mng.List_Type;
  procedure Read_Tags is
    Template : As.U.Asu_Us;
  begin
    Template := As.U.Tus (Str_Util.Strip (
          Afpx.Decode_Field (Afpx_Xref.List_Tags.Template, 0, False)));
    -- Get tags list
    Git_If.List_Tags (Template.Image, Tags_List);
  end Read_Tags;

  Current_Tag : As.U.Asu_Us;
begin

  -- Init Afpx
  Init;

  -- Main loop
  loop
   -- Set the list
   Init_List (Tags_List);

    -- Activate Checkout and Delete if list is not empty
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
          when Afpx_Xref.List_Tags.Checkout =>
            -- Call Checkout on current tag
            -- @@@
            null;
          when Afpx_Xref.List_Tags.Delete =>
            -- Delete tag selected
            Tags_List.Move_At (Afpx.Line_List.Get_Position);
            Current_Tag := As.U.Tus (Tags_List.Access_Current.Image);
            if Confirm  ("Delete Tag", Current_Tag.Image) then
              begin
                Afpx.Suspend;
                Git_If.Delete_Tag (Str_Util.Strip (
                    Tags_List.Access_Current.Image));
                Afpx.Resume;
              exception
                when others =>
                  Afpx.Resume;
              end;
              Init;
              -- Rewind
              Read_Tags;
              if not Afpx.Line_List.Is_Empty then
                Afpx.Line_List.Rewind;
                Afpx.Update_List (Afpx.Top);
              end if;
            else
              Init;
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
          Afpx.Suspend;
          Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                                Afpx_Xref.List_Tags.Branch);
          Afpx.Resume;
        exception
          when others =>
            Afpx.Resume;
        end;
    end case;
  end loop;

end List;

