with Afpx.List_Manager;
with Utils.X, Afpx_Xref;
separate (Tags)

procedure List (Root : in String) is
  -- Afpx stuff
  Get_Handle  : Afpx.Get_Handle_Rec;
  Ptg_Result  : Afpx.Result_Rec;
  use type Afpx.Absolute_Field_Range;
  
  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.List_Tags.Dscr_Num);
    Utils.X.Encode_Field (Root, Afpx_Xref.List_Tags.Root);
    Get_Handle := (others => <>);
    Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                          Afpx_Xref.List_Tags.Branch);
    Afpx.Line_List.Delete_List;
  end init;

  -- Get the tags and encode in list
  procedure Read_Tags is
  begin
    -- @@@
    null;
    -- Decode Crit
    -- Get tags list
    -- Encode List
  end Read_Tags;
    
begin
  
  -- Init Afpx
  Init;

  -- Main loop
  loop
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
            Afpx.List_Manager.Scroll(
               Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
          when Afpx_Xref.List_Tags.List =>
            Read_Tags;
          when Afpx_Xref.List_Tags.Checkout =>
            -- Call Checkout on current tag
            -- @@@
            null;
          when Afpx_Xref.List_Tags.Delete =>
            -- Delete 
            -- @@@
            null;
          when Afpx_Xref.List_Tags.Back =>
            return;
          when others =>
            null;
        end case;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
        null;
      when Afpx.Refresh =>
        -- Encode current branch
        Utils.X.Encode_Field (Utils.X.Branch_Image (Git_If.Current_Branch),
                              Afpx_Xref.List_Tags.Branch);
    end case;
  end loop;

end List;

