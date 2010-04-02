with Argument, Afpx, Con_Io, Dir_Mng, Timers, Language, Basic_Proc;
procedure T_Afpx is

  procedure Dir_Sort is new Dir_Mng.File_List_Mng.Sort (Dir_Mng.Less_Than);
  Dir_List : Dir_Mng.File_List_Mng.List_Type;
  Dir_Item : Dir_Mng.File_Entry_Rec;
  Afpx_Item : Afpx.Line_Rec;

  Height : Afpx.Height_Range;
  Width  : Afpx.Width_Range;
  Background   : Con_Io.Effective_Colors;
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean;
  Flip_Flop : Boolean;

  Timer_Ss, Timer_Per, Timer_Tmp : Timers.Timer_Id;
  pragma Unreferenced (Timer_Ss);
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Flip_Flop := not Flip_Flop;
    return True;
  end Timer_Cb;

  use Afpx;

  procedure Next_Field (Cursor_Field : in out Afpx.Field_Range) is
    Loc : Afpx.Absolute_Field_Range;
  begin
    Loc := Afpx.Next_Cursor_Field (Cursor_Field);
    if Loc = 0 then
      Cursor_Field := 1;
    else
      Cursor_Field := Loc;
    end if;
  end Next_Field;

begin
  Afpx.Get_Screen_Size (Height, Width);
  Basic_Proc.Put_Line_Output ("Screen geometry is "
                            & Width'Img & " x" & Height'Img);
  Afpx.Use_Descriptor(1);

  Afpx.Get_Descriptor_Background (Background);
  Basic_Proc.Put_Line_Output ("Dscr background is " & Background'Img);

  -- List directory and store it in Afpx list
  if Argument.Get_Nbre_Arg = 0 then
    Dir_Mng.List_Dir (Dir_List, "");
  else
    Dir_Mng.List_Dir (Dir_List,
                      Argument.Get_Parameter (Occurence => 1));
  end if;

  Dir_Sort (Dir_List);
  Dir_List.Rewind;

  -- Start a temporary silly timer
  Timer_Tmp := Timers.Create ( (Timers.Delay_Sec, null, 0.1, 0.1), null);
  -- Start a single shot timer in 10 secs
  Timer_Ss := Timers.Create ( (Timers.Delay_Sec, null, Timers.No_Period, 10.0),
                              null);
  -- Start a 10 sec periodical timer in 20 secs
  Timer_Per := Timers.Create ( (Timers.Delay_Sec, null, 10.0, 20.0),
                               Timer_Cb'Unrestricted_Access);
  -- Delete the temporary silly timer
  Timers.Delete (Timer_Tmp);

  loop
    Dir_List.Read (Dir_Item, Dir_Mng.File_List_Mng.Current);
    Afpx_Item.Len := Dir_Item.Len;
    Afpx_Item.Str := (others => ' ');
    Afpx_Item.Str(1 .. Afpx_Item.Len) :=
          Language.String_To_Wide (Dir_Item.Name (1 .. Dir_Item.Len));
    Afpx_Item.Str(Afpx_Item.Len+1) := '>';
    Afpx_Item.Str(Afpx_Item.Len+2) :=
       Language.Char_To_Wide (Dir_Mng.File_Kind_List'Image(Dir_Item.Kind)(1));
    Afpx_Item.Len := Afpx_Item.Len + 2;
    Afpx.Line_List.Insert (Afpx_Item);
    exit when not Dir_List.Check_Move;
    Dir_List.Move_To;
  end loop;
  Afpx.Line_List.Rewind;

  Afpx.Line_List.Read (Afpx_Item, Afpx.Line_List_Mng.Current);

  Afpx.Encode_Wide_Field (2, (1, 0),
                     ">" & Afpx_Item.Str (1 .. Afpx_Item.Len) & "<");

  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Flip_Flop := True;
  Redisplay := False;

  loop
    Afpx.Set_Field_Activation (5, Flip_Flop);
    Afpx.Set_Field_Protection (0, not Flip_Flop);

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, Redisplay,
                       Right_Select => True);
    Redisplay := False;

    Afpx.Clear_Field (15);
    Afpx.Encode_Field (15, (0, 0), Ptg_Result.Id_Selected_Right'Img);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            if Afpx.Decode_Field(Cursor_Field, 0)(1 .. 4) = "exit" then
              Afpx.Set_Field_Activation (Cursor_Field, False);
              Afpx.Clear_Field (2);
              Afpx.Encode_Wide_Field (2, (1, 0), ">" &
                           Afpx.Decode_Wide_Field(Cursor_Field, 0) & "<");
              Next_Field (Cursor_Field);
              Cursor_Col := 0;
              Insert := False;
            else
              Afpx.Clear_Field (2);
              Afpx.Encode_Field (2, (1, 0), ">" &
                                 Afpx.Decode_Field(Cursor_Field, 0) & "<");
            end if;
          when Afpx.Escape_Key =>
            Afpx.Clear_Field (Cursor_Field);
            Afpx.Clear_Field (2);
            Afpx.Encode_Field (2, (1, 0), ">" &
                               Afpx.Decode_Field(Cursor_Field, 0) & "<");
            Cursor_Col := 0;
            Insert := False;
          when Afpx.Break_Key =>
            exit;
        end case;
        Flip_Flop := not Flip_Flop;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when 4 =>
            exit;
          when 5 | Afpx.List_Field_No =>
            Afpx.Line_List.Read (Afpx_Item, Afpx.Line_List_Mng.Current);
            Afpx.Clear_Field (2);
            Afpx.Encode_Wide_Field (2, (1, 0),
                        ">" & Afpx_Item.Str (1 .. Afpx_Item.Len) & "<");
          when 8 =>
            Afpx.Update_List(Afpx.Up);
          when 9 =>
            Afpx.Update_List(Afpx.Down);
          when 10 =>
            Afpx.Update_List(Afpx.Page_Up);
          when 11 =>
            Afpx.Update_List(Afpx.Page_Down);
          when 12 =>
            Afpx.Update_List(Afpx.Top);
          when 13 =>
            Afpx.Update_List(Afpx.Bottom);
          when 14 =>
            Afpx.Update_List(Afpx.Center);
          when others =>
            null;
        end case;
      when Afpx.Fd_Event =>
        Afpx.Clear_Field (2);
        Afpx.Encode_Field (2, (1, 0), ">> Fd Event <<");
      when Afpx.Timer_Event =>
        Afpx.Clear_Field (2);
        Afpx.Encode_Field (2, (1, 0), ">> Timer Event <<");
      when Afpx.Signal_Event =>
        Afpx.Clear_Field (2);
        Afpx.Encode_Field (2, (1, 0), ">> Signal Event <<");
      when Afpx.Refresh =>
        Redisplay := True;
    end case;

  end loop;

  Timers.Delete (Timer_Per);
  Afpx.Release_Descriptor;

end T_Afpx;

