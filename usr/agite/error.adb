with Con_Io, Afpx, Str_Util;
with Utils, Afpx_Xref;
procedure Error (Action, Target, Text : String) is
  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;

begin
  Afpx.Use_Descriptor (Afpx_Xref.Error.Dscr_Num);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Redisplay := False;
  Afpx.Encode_Field (Afpx_Xref.Error.Action, (0, 0),
      Str_Util.Center (Action, Afpx.Get_Field_Width (Afpx_Xref.Error.Action)));
  Afpx.Encode_Field (Afpx_Xref.Error.Target, (0, 0),
      Str_Util.Center (Target, Afpx.Get_Field_Width (Afpx_Xref.Error.Target)));
  Afpx.Encode_Field (Afpx_Xref.Error.Text, (0, 0),
      Str_Util.Center (Text, Afpx.Get_Field_Width (Afpx_Xref.Error.Text)));

  -- Main loop
  loop

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                       Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key | Afpx.Escape_Key =>
            -- Done
            return;
          when Afpx.Break_Key =>
            raise Utils.Exit_Requested;
        end case;

      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when Afpx_Xref.Error.Ok =>
            -- OK
            return;
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

end Error;

