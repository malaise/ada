with Afpx;
with Utils.X, Afpx_Xref;
-- Confirm: OK/Cancel or Yes/No, show (protected) of hide the list
function Confirm (Title, Action : String;
                  Msg1, Msg2, Msg_Below : String := "";
                  Warning : String := "";
                  Ok_Cancel : Boolean := True;
                  Show_List : Boolean := False) return Boolean is
  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result   : Afpx.Result_Rec;

begin
  Afpx.Use_Descriptor (Afpx_Xref.Confirm.Dscr_Num);
  Utils.X.Center_Field (Title, Afpx_Xref.Confirm.Title);
  Utils.X.Center_Field (Action, Afpx_Xref.Confirm.Action);
  if Msg1 /= "" then
    Utils.X.Center_Field (Msg1, Afpx_Xref.Confirm.Msg1);
  end if;
  if Msg2 /= "" then
    Utils.X.Center_Field (Msg2, Afpx_Xref.Confirm.Msg2);
  end if;
  if Msg_Below /= "" then
    Utils.X.Center_Field (Msg_Below, Afpx_Xref.Confirm.Msg_Below);
  end if;
  if Warning /= "" then
    Utils.X.Center_Field ("WARNING: " & Warning, Afpx_Xref.Confirm.Warning);
  end if;
  -- Yes / No
  if not Ok_Cancel then
    Utils.X.Center_Field ("Yes", Afpx_Xref.Confirm.Ok);
    Utils.X.Center_Field ("No",  Afpx_Xref.Confirm.Cancel);
  end if;
  -- Show/Hide list
  if Show_List then
    -- Show list protected
    Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
  else
    -- Hide list
    Afpx.Set_Field_Activation (Afpx.List_Field_No, False);
  end if;

  -- Main loop
  loop

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            -- OK
            return True;
          when Afpx.Escape_Key =>
            return False;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Afpx_Xref.Confirm.Ok =>
            -- OK
            return True;
          when Afpx_Xref.Confirm.Cancel =>
            -- Cancel
            return False;
        when others =>
            -- Other button?
            null;
        end case;

      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Refresh =>
        null;
    end case;
  end loop;

end Confirm;

