-- Test X_Mng. Fixed Line and dump of input events
with Ada.Calendar;
with C_Types, X_Mng, Event_Mng, Timers, Argument, As.B, Null_Procedure,
     Language;
procedure T_X is

  Line_Def : constant X_Mng.Line_Definition_Rec := (
    Screen_Id => 0,
    Row => 10,
    Column => 20,
    Height => 50,
    Width => 80,
    Background => 0,
    Border => 1,
    No_Font => 1);

  Id : X_Mng.Line;
  Font_Width, Font_Height, Font_Offset : Natural;

  Timeout : Duration;
  Exp : Timers.Delay_Rec;
  Kind : X_Mng.Event_Kind;

  Control, Shift, Code : Boolean;
  Kbd_Codes : X_Mng.Kbd_Tab_Code;
  Tid_Button : X_Mng.Button_List;
  Tid_Row, Tid_Col, Sub_Row, Sub_Col : Integer;
  Char_Offset : X_Mng.Byte;
  Wchar : Wide_Character;

  Font, Txt : As.B.Asb_Bs(80);
  subtype Row_Range is Natural range 10 .. 30;
  Row : Row_Range := Row_Range'First;

  procedure Put (Str : in String) is
  begin
    X_Mng.X_Put_String (Id, " ", Row, 8);
    if Row /= Row_Range'Last then
      Row := Row_Range'Succ(Row);
    else
      Row := Row_Range'First;
    end if;
    X_Mng.X_Put_String (Id, "> " & Str & "                 ",
                            Row, 8);
  end Put;

  use type Ada.Calendar.Time, C_Types.Byte, X_Mng.Event_Kind;

begin
  Char_Offset := 0;
  if Argument.Get_Nbre_Arg = 0 then
    X_Mng.X_Initialise ("");
  elsif Argument.Get_Nbre_Arg = 1 then
    X_Mng.X_Initialise (Argument.Get_Parameter(1));
  elsif Argument.Get_Nbre_Arg = 2 then
    X_Mng.X_Initialise (Argument.Get_Parameter(1));
    Char_Offset := X_Mng.Byte'Value(Argument.Get_Parameter(2));
  end if;
  X_Mng.X_Get_Font_Geometry (Line_Def.No_Font,
      Font_Width, Font_Height, Font_Offset);
  Font.Set ("Font:" & Font_Width'Img  & " x" & Font_Height'Img
           & " offset" & Font_Offset'Img);

  Timeout := 1.0;
  X_Mng.X_Open_Line (Line_Def, Id);

  -- Enable signal event
  Event_Mng.Set_Sig_Term_Callback (Null_Procedure'Access);

  Kind := X_Mng.Refresh;
  Exp := (Delay_Kind      => Timers.Delay_Exp,
          Period          => Timers.No_Period,
          Clock           => null,
          Expiration_Time => Ada.Calendar.Clock + Timeout);
  Main_Loop:
  loop
    if Kind = X_Mng.Refresh then
      X_Mng.X_Clear_Line (Id);
      X_Mng.X_Set_Attributes (Id, 0, 5, True, False, False);
      X_Mng.X_Put_String (Id, "Offset " & Char_Offset'Img, 7, 60);
      for I in 0 .. 15 loop
        for J in 0 .. 15 loop
          Wchar := Wide_Character'Val(Integer(Char_Offset) * 256 + I * 16 + J);
          X_Mng.X_Put_String (Id, Language.Wide_To_String (Wchar & ""),
                              8 + I, 60 + J);
        end loop;
      end loop;
      X_Mng.X_Set_Attributes (Id, 0, 3, False, False, False);
      X_Mng.X_Put_String (Id, Font.Image, 5, 10);
      X_Mng.X_Set_Attributes (Id, 1, 4);
      X_Mng.X_Draw_Area (Id, 50, 2, 7, 10);
      X_Mng.X_Set_Attributes (Id, 0, 3, False, False, False);
      X_Mng.X_Bell (Id, 1);
    end if;
    X_Mng.X_Wait_Event (Id, Exp, Kind);
    case Kind is
      when X_Mng.Refresh | X_Mng.Fd_Event
         | X_Mng.Timer_Event =>
        X_Mng.X_Set_Attributes (Id, 0, 3, False, False, False);
        Put (X_Mng.Event_Kind'Image(Kind));
      when X_Mng.Signal_Event =>
        exit Main_Loop;
      when X_Mng.Tid_Press | X_Mng.Tid_Release =>
        X_Mng.X_Read_Tid (Id, True, Tid_Button, Tid_Row, Tid_Col,
                          Sub_Row, Sub_Col);
        Put (X_Mng.Event_Kind'Image(Kind) & " "
           & X_Mng.Button_List'Image(Tid_Button)
           & " " & Integer'Image(Tid_Row)  & " " & Integer'Image(Tid_Col)
           & " " & Integer'Image(Sub_Row)  & " " & Integer'Image(Sub_Col));
        exit Main_Loop when Tid_Row = 1 and then Tid_Col = 1;
      when X_Mng.Tid_Motion | X_Mng.Tid_Enter | X_Mng.Tid_Leave
         | X_Mng.Selection =>
        null;
      when X_Mng.Keyboard =>
        X_Mng.X_Read_Key(Id, Control, Shift, Code, Kbd_Codes);
        Txt.Set (X_Mng.Event_Kind'Image(Kind));
        Txt.Append (" " & Control'Img & " " & Shift'Img & " " & Code'Img);
        for I in 1 .. Kbd_Codes.Nbre loop
          Txt.Append (" " & X_Mng.Byte'Image(Kbd_Codes.Tab(I)));
        end loop;
        Put (Txt.Image);
        -- Ctrl C
        exit Main_Loop when Kbd_Codes.Nbre = 2
             and then Kbd_Codes.Tab(1) = 255 and then Kbd_Codes.Tab(2) = 27;
      when X_Mng.Timeout =>
        Put (X_Mng.Event_Kind'Image(Kind));
        Exp.Expiration_Time := Exp.Expiration_Time + Timeout;
      when X_Mng.Exit_Request =>
        exit Main_Loop;
    end case;
  end loop Main_Loop;

  X_Mng.X_Close_Line (Id);
end T_X;

