with Con_Io, Afpx, Str_Util;
with Utils, Afpx_Xref;
function Confirm (Title, Msg : String) return Boolean is
  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;

begin
  Afpx.Use_Descriptor (Afpx_Xref.Confirm.Dscr_Num);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Redisplay := False;
  Afpx.Encode_Field (Afpx_Xref.Confirm.Action, (0, 0),
        Str_Util.Center (Title, Afpx.Get_Field_Width (2)));
  Afpx.Encode_Field (Afpx_Xref.Confirm.Name, (0, 0),
          Str_Util.Center (Msg, Afpx.Get_Field_Width (2)));

  -- Main loop
  loop

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                       Ptg_Result, Redisplay);
    Redisplay := False;
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

      when Afpx.Fd_Event =>
        null;
      when Afpx.Timer_Event =>
        null;
      when Afpx.Signal_Event =>
        null;
      when Afpx.Refresh =>
        Redisplay := True;
    end case;
  end loop;

end Confirm;

