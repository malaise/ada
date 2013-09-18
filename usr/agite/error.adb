with Con_Io, Afpx;
with Utils.X, Afpx_Xref;
procedure Error (Action, Target, Text : String) is
  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;

begin
  Afpx.Use_Descriptor (Afpx_Xref.Error.Dscr_Num);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Utils.X.Center_Field (Action, Afpx_Xref.Error.Action);
  Utils.X.Center_Field (Target, Afpx_Xref.Error.Target);
  Utils.X.Center_Field (Text,   Afpx_Xref.Error.Text);

  -- Main loop
  loop

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);
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

      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Refresh =>
        null;
    end case;
  end loop;

end Error;

