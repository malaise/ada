with My_Io, X_Mng, Event_Mng, Argument, Text_Handler, Null_Procedure;
use X_Mng;
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

  Timeout_Ms, Var_Timeout_Ms : Integer;
  X_Event : Boolean;
  Kind : X_Mng.Event_Kind;
  Next : Boolean;

  Kbd_Codes : X_Mng.Kbd_Tab_Code;
  Tid_Button : X_Mng.Button_List;
  Tid_Row, Tid_Col : Integer;

  Txt : Text_Handler.Text(80);
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
    X_Mng.X_Put_String (Id, "> " & Str & "                       ",
                            Row, 8);
  end Put;

begin
  if Argument.Get_Nbre_Arg = 0 then
    X_Mng.X_Initialise ("");
    Timeout_Ms := 1_000;
  elsif Argument.Get_Nbre_Arg = 1 then
    X_Mng.X_Initialise (Argument.Get_Parameter(1));
    Timeout_Ms := 1_000;
  elsif Argument.Get_Nbre_Arg = 2 then
    X_Mng.X_Initialise (Argument.Get_Parameter(1));
    Timeout_Ms := Integer'Value (Argument.Get_Parameter(2));
  end if;

  X_Mng.X_Open_Line (Line_Def, Id);
  
  -- Enable signal event
  Event_Mng.Set_Sig_Term_Callback (Null_Procedure'Access);

  X_Event := True;
  Kind := X_Mng.Refresh;
  Main_Loop:
  loop
    if X_Event and then Kind = X_Mng.Refresh then
      X_Mng.X_Clear_Line (Id);
      X_Mng.X_Set_Attributes (Id, 0, 5, True, False, False, False);
      for I in 0 .. 15 loop
        for J in 0 .. 15 loop
          X_Mng.X_Put_Char (Id, X_Mng.Byte(16 * I + J), 8 + I, 60 + J);
        end loop;
      end loop;
      X_Mng.X_Set_Attributes (Id, 0, 3, False, False, True, False);
      X_Mng.X_Put_String (Id, "Ah que coucou", 5, 10);
      X_Mng.X_Set_Attributes (Id, 1, 4);
      X_Mng.X_Draw_Area (Id, 50, 2, 7, 10);
      X_Mng.X_Set_Attributes (Id, 0, 3, False, False, False, False);
      X_Mng.X_Bell (Id, 1);
    end if;
    Var_Timeout_Ms := Timeout_Ms;
    X_Mng.X_Select (Id, Var_Timeout_Ms, X_Event);
    if X_Event then
      loop
        X_Mng.X_Process_Event (Id, Kind, Next);
        case Kind is
          when X_Mng.Discard =>
            Put (X_Mng.Event_Kind'Image(Kind));
          when X_Mng.Refresh | X_Mng.Fd_Event 
             | X_Mng.Timer_Event | X_Mng.Signal_Event =>
            X_Mng.X_Set_Attributes (Id, 0, 3, False, False, True, False);
            Put (X_Mng.Event_Kind'Image(Kind));
          when X_Mng.Tid_Press | X_Mng.Tid_Release =>
            X_Mng.X_Read_Tid (Id, True, Tid_Button, Tid_Row, Tid_Col);
            Put (X_Mng.Event_Kind'Image(Kind) & " " & X_Mng.Button_List'Image(Tid_Button)
                              & " " & Integer'Image(Tid_Row)  & " " & Integer'Image(Tid_Col));
            exit Main_Loop when Tid_Row = 1 and then Tid_Col = 1;
          when X_Mng.Tid_Motion =>
            null;
          when X_Mng.Keyboard =>
            X_Mng.X_Read_Key(Id, Kbd_Codes);
            Text_Handler.Set (Txt, X_Mng.Event_Kind'Image(Kind));
            for I in 1 .. Kbd_Codes.Nbre loop
               Text_Handler.Append (Txt, " " & X_Mng.Byte'Image(Kbd_Codes.Tab(I)));
            end loop;
            Put (Text_Handler.Value(Txt));
            exit Main_Loop when Kbd_Codes.Nbre = 2
                 and then Kbd_Codes.Tab(1) = 255 and then Kbd_Codes.Tab(2) = 27;
          end case;
        exit when not Next;
      end loop;
    else
--    Put ("Timeout");
      null;
    end if;
    My_Io.Put_Line (Integer'Image(Var_Timeout_Ms));
  end loop Main_Loop;

  X_Mng.X_Close_Line (Id);
end T_X;
