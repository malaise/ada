with Command;
separate (History)
procedure Patch (Logs : in out Git_If.Log_List;
                 From_Hash, To_Hash : in Git_If.Git_Hash) is

  -- Afpx stuff
  Get_Handle  : Afpx.Get_Handle_Rec;
  Ptg_Result  : Afpx.Result_Rec;

  -- Command info
  Hash : Git_If.Git_Hash;
  Cmd, Name : As.U.Asu_Us;

   Out_Flow : Command.Flow_Rec (Command.Str);
   Err_Flow : Command.Flow_Rec (Command.Str);
   Exit_Code : Command.Exit_Code_Range;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Patch.Dscr_Num);
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
    Afpx.Set_Field_Activation (Afpx_Xref.Patch.Center, False);
    -- Encode branch and list
    Utils.X.Encode_Branch (Afpx_Xref.Patch.Branch);
    Init_List (Logs);
    Get_Handle := (others => <>);
  end Init;

  use type Afpx.Absolute_Field_Range;

begin
  -- Get and check Patch command
  Cmd := As.U.Tus (Config.Patch);
  if Cmd.Is_Null then
    return;
  end if;

  -- Move at top
  Move_At (Logs, Git_If.No_Hash);
  Hash := Hash_Of (Logs);

  -- Main loop until OK or cancel
  loop
    -- Init screen
    Init;
    Move_At (Logs, Hash);
    -- Put_then get
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Do patch
              exit;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                 Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
            when Afpx_Xref.Patch.Ok =>
              -- Do patch
              exit;
            when Afpx_Xref.Patch.Cancel =>
              -- Back
              return;
            when others =>
              -- Other button?
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
          -- Encode current branch
          Utils.X.Encode_Branch (Afpx_Xref.Patch.Branch);
      end case;
    end loop;

    -- Get patch name
    Name := As.U.Tus (Str_Util.Strip (
        Afpx.Decode_Field (Afpx_Xref.Patch.Name, 0, True)));
    Hash:= Hash_Of (Logs);

    -- Check no space
    exit when not Name.Is_Null and then Name.Locate (" ") = 0;
    Error ("patch", Name.Image,
        (if Name.Is_Null then "File name is empty"
         else "File name contains illegal character"));
  end loop;

  -- Build command and launch
  Cmd.Append (" " & Name.Image);
  Cmd.Append (" " & From_Hash & " " & To_Hash);
  Utils.Execute (Cmd.Image, Out_Flow'Unrestricted_Access,
                            Err_Flow'Unrestricted_Access, Exit_Code);

  -- Display result / error
  if Exit_Code = 0 then
    Error ("Patch", Name.Image, Out_Flow.Str.Image, Info => True);
  else
    Error ("Patch", Name.Image, Err_Flow.Str.Image, Info => False);
  end if;

end Patch;

