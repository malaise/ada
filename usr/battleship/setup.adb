with Con_Io, Afpx;
with Afpx_Xref, Communication, Utils;
package body Setup is

  function Init (Addr : String; Server : Boolean) return Boolean is
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Result       : Afpx.Result_Rec;
    Redisplay    : Boolean;
    use type Afpx.Absolute_Field_Range;
  begin
    -- Init Afpx
    Afpx.Use_Descriptor (Afpx_Xref.Setup.Dscr_Num);
    for Fld in Afpx_Xref.Setup.Start .. Afpx_Xref.Setup.Delete loop
      Afpx.Set_Field_Activation (Fld, False);
    end loop;
    Afpx.Set_Field_Activation (Afpx_Xref.Setup.Done, False);

    -- Initiate connection
    Communication.Connect (Addr, Server);

    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Result, Redisplay);
      case Result.Event is
        when Afpx.Signal_Event =>
          -- Aborted by signal
          if Communication.Sig_Received then
            raise Utils.Abort_Game;
          end if;
        when Afpx.Fd_Event =>
          -- Connected
          exit when Communication.Is_Connected;
        when Afpx.Mouse_Button =>
          if Result.Field_No = Afpx_Xref.Setup.Cancel then
            -- Cancelled by user
            return False;
          end if;
        when Afpx.Refresh =>
          Redisplay := True;
        when others =>
          -- Other event
          null;
      end case;
    end loop;
    return True;
  end Init;

  procedure Define is
  begin
    null;
  end Define;

end Setup;

