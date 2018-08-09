with Con_Io, Afpx.Utils, As.U.Utils, Parser, Aski, Split_Lines;
with Utils.X, Afpx_Xref;
procedure Error (Action, Target, Text : in String;
                 Info : Boolean := False) is
  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result   : Afpx.Result_Rec;
  List_Width : Afpx.Width_Range;
  Af_Line : Afpx.Line_Rec;

  -- Action and Target are reduced by LmWidth at the right (in Afpx.xml)
  Offset : constant := 5;

  -- To split text
  function Is_Lf (C : Character) return Boolean is (C = Aski.Lf_C);
  Line_Iter, Word_Iter : Parser.Iterator;
  Line : As.U.Asu_Us;
  Texts : As.U.Utils.Asu_Ua.Unb_Array;

  use type Afpx.Absolute_Field_Range;

begin
  -- Use descriptor en encode action and target
  Afpx.Use_Descriptor (Afpx_Xref.Error.Dscr_Num);
  List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);
  Afpx.Set_Field_Protection (Afpx.List_Field_No, True);
  if Info then
    Afpx.Set_Field_Colors (Afpx_Xref.Error.Title,
        Foreground => Con_Io.Color_Of ("Black"));
    Utils.X.Center_Field ("Info", Afpx_Xref.Error.Title);
  end if;
  Utils.X.Center_Field (Action, Afpx_Xref.Error.Action, Offset => Offset);
  Utils.X.Center_Field (Target, Afpx_Xref.Error.Target, Offset => Offset);

  -- Split text in lines and encode in list
  Afpx.Line_List.Delete_List;
  Line_Iter.Set (Text, Is_Lf'Unrestricted_Access);
  loop
    -- Next line
    Line := As.U.Tus (Line_Iter.Next_Word);
    exit when Line.Is_Null;
    -- Split lines
    Word_Iter.Set (Line.Image, Utils.Separator'Access);
    Texts := Split_Lines (Word_Iter, List_Width, "  ");
    for I in 1 .. Texts.Length loop
      Afpx.Utils.Encode_Line ("", Texts.Element(I).Image, "",
                              List_Width, Af_Line, Keep_Tail => False);
      Afpx.Line_List.Insert (Af_Line);
    end loop;
  end loop;

  -- Cleanup
  if Word_Iter.Is_Set then
    Word_Iter.Del;
  end if;
  Line_Iter.Del;
  Afpx.Line_List.Rewind (Check_Empty => False);

  -- De-activate list if empty list
  if Afpx.Line_List.Is_Empty then
    Afpx.Set_Field_Activation (Afpx.List_Field_No, False);
  end if;

  -- De-activate navigation if list is short enough
  if Natural (Afpx.Line_List.List_Length)
      <= Afpx.Get_Field_Height (Afpx.List_Field_No)
  then
    for I in  Utils.X.List_Scroll_Fld_Range loop
      Afpx.Set_Field_Activation (I, False);
    end loop;
  end if;

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
           when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                Ptg_Result.Field_No - Utils.X.List_Scroll_Fld_Range'First + 1);
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

