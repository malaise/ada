with Afpx;
with Utils.X, Afpx_Xref, Git_If, Con_Io;
-- Reset (soft/mixed/hard) or clean
function Reset (Root : String;
                Ref : String;
                Only_Hard : Boolean := False;
                Allow_Clean : Boolean := False;
                Comment : String := "";
                Pushed : in Boolean := True) return Boolean is
  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result   : Afpx.Result_Rec;

begin
  -- Common info
  Afpx.Use_Descriptor (Afpx_Xref.Reset.Dscr_Num);
  Utils.X.Encode_Branch (Afpx_Xref.Reset.Branch);
  Utils.X.Encode_Field (Root, Afpx_Xref.Reset.Root);

  -- Tuning
  Afpx.Set_Field_Activation (Afpx_Xref.Reset.Clean, Allow_Clean);
  Afpx.Set_Field_Activation (Afpx_Xref.Reset.Mixed, not Only_Hard);
  Afpx.Set_Field_Activation (Afpx_Xref.Reset.Soft,  not Only_Hard);

  if Ref /= "" then
    -- A ref => Hard / Soft / Mixed reset to ref
    Utils.X.Center_Field (Ref, Afpx_Xref.Reset.Title);
    if Comment /= "" then
      -- Comment of target commit
      Utils.X.Encode_Field (Comment, Afpx_Xref.Reset.Comment);
    end if;
    if Pushed then
      Utils.X.Center_Field (
        "ALERT: This operation will alter the pushed history",
        Afpx_Xref.Reset.Warning);
    else
      Afpx.Set_Field_Colors (Afpx_Xref.Reset.Warning,
                             Foreground => Con_Io.Color_Of ("Orange"));
      Utils.X.Center_Field (
        "WARNING: This operation will alter the local history",
        Afpx_Xref.Reset.Warning);
    end if;
  else
    -- No ref => Hard / Soft / Mixed reset to head or clean
    Utils.X.Center_Field ("HEAD", Afpx_Xref.Reset.Title);
  end if;

  -- Main loop
  loop

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            null;
          when Afpx.Escape_Key =>
            return False;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Afpx_Xref.Reset.Soft =>
            -- Soft reset
            Git_If.Do_Reset (Ref, True);
            return True;
          when Afpx_Xref.Reset.Mixed =>
            -- Mixed reset
            Git_If.Do_Reset (Ref, False);
            return True;
          when Afpx_Xref.Reset.Hard =>
            -- Hard reset
            Git_If.Do_Reset_Hard (Ref);
            return True;
          when Afpx_Xref.Reset.Clean =>
            -- Clean
            Git_If.Do_Clean;
            return True;
          when Afpx_Xref.Reset.Cancel =>
            -- Cancel
            return False;
        when others =>
            -- Other button?
            null;
        end case;

      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Refresh =>
        Utils.X.Encode_Branch (Afpx_Xref.Reset.Branch);
    end case;
  end loop;

end Reset;

