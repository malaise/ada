with As.U, Afpx, Str_Util;
with Afpx_Xref, Utils.X, Error;
package body Checkout is

  -- Root path
  Root : As.U.Asu_Us;
  -- Info and Text of the Commit/tag
  Info : As.U.Asu_Us;
  Text : As.U.Asu_Us;

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Init Afpx
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Checkout.Dscr_Num);
    -- Encode Root and branch
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Checkout.Root);
    Utils.X.Encode_Branch (Afpx_Xref.Checkout.Branch);
    -- Commit or tag
    Afpx.Encode_Field (Afpx_Xref.Checkout.Commit_Tag, (0, 0), Info.Image);
    -- Info
    Afpx.Encode_Field (Afpx_Xref.Checkout.Info, (0, 0), Text.Image);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
  end Init;

  -- Do a checkout
  function Do_Checkout (Hash : Git_If.Git_Hash) return Boolean is
    Result : As.U.Asu_Us;
    Branch : As.U.Asu_Us;
  begin
    -- Read target branch
    Branch := As.U.Tus (
      Str_Util.Strip (Afpx.Decode_Field (Afpx_Xref.Checkout.Into_Branch,
                                         0, False)));
    begin
      Result := As.U.Tus (Git_If.Do_Checkout (Hash, Branch.Image));
    exception
      when others =>
        Afpx.Resume;
        raise;
    end;
    -- Handle error
    if not Result.Is_Null then
      Error ("Checkout", Hash, Result.Image);
      -- Restore screen
      Init;
    end if;
    return Result.Is_Null;
  end Do_Checkout;

  -- Checkout a commit or tag. True if success
  function Handle (Root : String;
                   Info : String;
                   Text : String;
                   Hash : Git_If.Git_Hash) return Boolean is
    Ptg_Result   : Afpx.Result_Rec;
  begin
    Checkout.Root := As.U.Tus (Root);
    Checkout.Info := As.U.Tus (Info);
    Checkout.Text := As.U.Tus (Text);
    Init;

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
            when Afpx_Xref.Checkout.Checkout =>
              -- Checkout
              return Do_Checkout (Hash);
            when Afpx_Xref.Checkout.Cancel =>
              -- Back
              return False;
            when others =>
              -- Other button?
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Checkout.Branch);
      end case;
    end loop;

  end Handle;

end Checkout;

