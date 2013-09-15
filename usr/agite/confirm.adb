with Con_Io, Afpx, Str_Util;
with Utils, Afpx_Xref;
function Confirm (Title, Msg : String;
                  Ok_Cancel : Boolean := True;
                  Show_List : Boolean := False) return Boolean is
  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;

begin
  Afpx.Use_Descriptor (Afpx_Xref.Confirm.Dscr_Num);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Afpx.Encode_Field (Afpx_Xref.Confirm.Action, (0, 0),
      Str_Util.Center (Title, Afpx.Get_Field_Width (Afpx_Xref.Confirm.Action)));
  Afpx.Encode_Field (Afpx_Xref.Confirm.Name, (0, 0),
      Str_Util.Center (Msg, Afpx.Get_Field_Width (Afpx_Xref.Confirm.Name)));
  -- Yes / No
  if not Ok_Cancel then
    Afpx.Clear_Field (Afpx_Xref.Confirm.Ok);
    Afpx.Clear_Field (Afpx_Xref.Confirm.Cancel);
    Afpx.Encode_Field (Afpx_Xref.Confirm.Ok, (1, 1), "Yes");
    Afpx.Encode_Field (Afpx_Xref.Confirm.Cancel, (1, 3), "No");
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

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);
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

