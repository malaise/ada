with Afpx, As.U.Utils, Parser, Split_Lines;
with Utils.X, Afpx_Xref;
procedure Error (Action, Target, Text : String) is
  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result   : Afpx.Result_Rec;

  -- To split text
  Iter : Parser.Iterator;
  Texts : As.U.Utils.Asu_Ua.Unb_Array;
  Fld : Afpx.Field_Range;

begin
  -- Use descriptor en encode action and target
  Afpx.Use_Descriptor (Afpx_Xref.Error.Dscr_Num);
  Utils.X.Center_Field (Action, Afpx_Xref.Error.Action);
  Utils.X.Center_Field (Target, Afpx_Xref.Error.Target);
  -- Split text and encode
  Iter.Set (Text, Utils.Separator'Access);
  Texts := Split_Lines (Iter, Afpx.Get_Field_Width (Afpx_Xref.Error.Text1), -4);
  Fld := Afpx_Xref.Error.Text1;
  for I in 1 .. Texts.Length loop
    Utils.X.Center_Field (Texts.Element(I).Image, Fld);
    Fld := Afpx.Field_Range'Succ (Fld);
  end loop;

  -- Main loop
  loop

    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
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

