with Argument, Afpx, Con_Io, Dir_Mng, Timers, Language, Basic_Proc, Mixed_Str;
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
  Redisplay    : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Flip_Flop : Boolean;

  Timer_Ss, Timer_Per, Timer_Tmp : Timers.Timer_Id;
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Flip_Flop := not Flip_Flop;
    return True;
  end Timer_Cb;

  procedure Next_Field (Cursor_Field : in out Afpx.Field_Range) is
    Loc : Afpx.Absolute_Field_Range;
    use type Afpx.Absolute_Field_Range;
  begin
    Loc := Afpx.Next_Cursor_Field (Cursor_Field);
    if Loc = 0 then
      Cursor_Field := 1;
    else
      Cursor_Field := Loc;
    end if;
  end Next_Field;

  procedure Encode_Status (U : in Language.Unicode_Sequence) is
    use type Language.Unicode_Sequence;
  begin
    Afpx.Encode_Field (2, (1, 0),
      Language.Char_To_Unicode ('>') & U & Language.Char_To_Unicode ('<'));
  end Encode_Status;
  procedure Encode_Status (S : in String) is
  begin
    Encode_Status (Language.String_To_Unicode (S));
  end Encode_Status;

  function Cursor_Col_Cb (Cursor_Field : Afpx.Field_Range;
                          New_Field : Boolean;
                          Cursor_Col : Con_Io.Col_Range;
                          Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
                          Str : Afpx.Unicode_Sequence)
           return Con_Io.Col_Range is
    Last_Index : Con_Io.Col_Range;
    use type Afpx.Enter_Field_Cause_List, Afpx.Absolute_Field_Range;
  begin
    Basic_Proc.Put_Line_Output ("Cursor_Set_Col_Cb --> "
     & "Cursor_Field:" & Cursor_Field'Img
     & ", New: " & Mixed_Str (New_Field'Img)
     & ", Cursor_Col:" & Cursor_Col'Img
     & ", Cause: " & Mixed_Str (Enter_Field_Cause'Img)
     & ", Content: " & Language.Unicode_To_String (Str));
    Last_Index := Afpx.Last_Index (Afpx.Decode_Field (Cursor_Field, 0), True);
    case Enter_Field_Cause is
      when Afpx.Mouse =>
        if Cursor_Col > Last_Index then
          return Last_Index;
        else
          return Cursor_Col;
        end if;
      when Afpx.Right_Full =>
        return Con_Io.Col_Range'First;
      when Afpx.Left | Afpx.Tab | Afpx.Stab=>
        if Cursor_Field = 6 then
          return Con_Io.Col_Range'First;
        else
          return Last_Index;
        end if;
    end case;
  end Cursor_Col_Cb;

  procedure List_Change_Cb (Action : in Afpx.List_Change_List;
                            Status : in Afpx.List_Status_Rec) is
  begin
    Basic_Proc.Put_Line_Output ("List_Change_Cb --> "
     & "Action: " & Mixed_Str (Action'Img)
     & ", Nb_Rows:" & Status.Nb_Rows'Img
     & ", Id_Top: " & Status.Id_Top'Img
     & ", Id_Bottom: " & Status.Id_Bottom'Img
     & ", Id_Selected Left: "  & Status.Ids_Selected(Afpx.List_Left)'Img
     & ", Id_Selected Right: " & Status.Ids_Selected(Afpx.List_Right)'Img);
  end List_Change_Cb;

begin
  Afpx.Get_Screen_Size (Height, Width);
  Basic_Proc.Put_Line_Output ("Screen geometry is "
                            & Width'Img & " x" & Height'Img);
  Afpx.Use_Descriptor(1);

  Background := Afpx.Get_Descriptor_Background;
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
  Timer_Tmp.Create ( (Timers.Delay_Sec, null, 0.1, 0.1), null);
  -- Start a single shot timer in 10 secs
  Timer_Ss.Create ( (Timers.Delay_Sec, null, Timers.No_Period, 10.0),
                              null);
  -- Start a 1 min periodical timer in 30 secs
  Timer_Per.Create ( (Timers.Delay_Sec, null, 60.0, 30.0),
                               Timer_Cb'Unrestricted_Access);
  -- Delete the temporary silly timer
  Timer_Tmp.Delete;

  loop
    Dir_List.Read (Dir_Item, Dir_Mng.File_List_Mng.Current);
    Afpx_Item.Len := Dir_Item.Name.Length;
    Afpx_Item.Str := (others => Con_Io.Space);
    Afpx_Item.Str(1 .. Afpx_Item.Len) :=
          Language.String_To_Unicode (Dir_Item.Name.Image);
    Afpx_Item.Str(Afpx_Item.Len+1) := Language.Char_To_Unicode ('>');
    Afpx_Item.Str(Afpx_Item.Len+2) :=
      Language.Char_To_Unicode (Dir_Mng.File_Kind_List'Image(Dir_Item.Kind)(1));
    Afpx_Item.Len := Afpx_Item.Len + 2;
    Afpx.Line_List.Insert (Afpx_Item);
    exit when not Dir_List.Check_Move;
    Dir_List.Move_To;
  end loop;
  Afpx.Line_List.Rewind;

  Afpx.Line_List.Read (Afpx_Item, Afpx.Line_List_Mng.Current);

  Encode_Status (Afpx_Item.Str (1 .. Afpx_Item.Len));

  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Redisplay := False;
  Flip_Flop := True;

  loop
    Afpx.Set_Field_Activation (5, Flip_Flop);
    Afpx.Set_Field_Protection (0, not Flip_Flop);

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Redisplay, Ptg_Result,
                       Right_Select => True,
                       Cursor_Col_Cb  => Cursor_Col_Cb'Unrestricted_Access,
                       List_Change_Cb => List_Change_Cb'Unrestricted_Access);

    Afpx.Clear_Field (18);
    Afpx.Encode_Field (18, (0, 0), Ptg_Result.Id_Selected_Right'Img);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            if Afpx.Decode_Field(Cursor_Field, 0)(1 .. 4) = "exit" then
              Afpx.Set_Field_Activation (Cursor_Field, False);
              Afpx.Clear_Field (2);
              Encode_Status (U => Afpx.Decode_Field(Cursor_Field, 0));
              Next_Field (Cursor_Field);
              Cursor_Col := 0;
              Insert := False;
            else
              Afpx.Clear_Field (2);
              Encode_Status (U => Afpx.Decode_Field(Cursor_Field, 0));
            end if;
          when Afpx.Escape_Key =>
            Afpx.Clear_Field (Cursor_Field);
            Afpx.Clear_Field (2);
            Encode_Status (U => Afpx.Decode_Field(Cursor_Field, 0));
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
            Encode_Status (U => Afpx_Item.Str (1 .. Afpx_Item.Len));
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
            Afpx.Update_List(Afpx.Top_Selected);
          when 15 =>
            Afpx.Update_List(Afpx.Center_Selected);
          when 16 =>
            Afpx.Update_List(Afpx.Bottom_Selected);
          when others =>
            null;
        end case;
      when Afpx.Fd_Event =>
        Afpx.Clear_Field (2);
        Encode_Status ("> Fd Event <");
      when Afpx.Timer_Event =>
        Afpx.Clear_Field (2);
        Encode_Status ("> Timer Event <");
      when Afpx.Signal_Event =>
        Afpx.Clear_Field (2);
        Encode_Status ("> Signal Event <");
      when Afpx.Refresh =>
        Redisplay := True;
    end case;

  end loop;

  Timer_Per.Delete;
  Afpx.Release_Descriptor;

end T_Afpx;

