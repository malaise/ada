with String_Mng, Normal, Con_Io, Afpx;
package body Screen is
  type Modes_List is (Default, Confirm, Ack);

  Edit_Allowed : Boolean := False;
  Sublist_Active : Boolean := False;

  procedure Update_To_Unit is
    use type Unit_Format.Units_List;
  begin
    if Unit_Format.Get_Current_Unit = Unit_Format.Euros then
      Afpx.Reset_Field(36);
    else
      Afpx.Clear_Field(36);
      Afpx.Set_Field_Colors(36, Foreground => Con_Io.Blue);
      Afpx.Encode_Field(36, (0, 1), "TO EUROS");
    end if;
  end Update_To_Unit;

  procedure Set_Mode (Mode : in Modes_List) is
  begin
    -- List unprotected in default
    Afpx.Set_Field_Protection(Afpx.List_Field_No, Mode /= Default);
    -- Oper buttons
    for F in Afpx.Field_Range'(23) .. 24 loop
      Afpx.Set_Field_Activation(F, Mode = Default);
    end loop;
    for F in Afpx.Field_Range'(25) .. 28 loop
      Afpx.Set_Field_Activation(F, Mode = Default and then Edit_Allowed);
    end loop;
    if Sublist_Active then
      Afpx.Encode_Field(29, (0, 1), "UN SEL");
    else
      Afpx.Reset_Field(29, Reset_Colors => False);
    end if;
    Afpx.Set_Field_Activation(29, Mode = Default and then Edit_Allowed);
    Afpx.Set_Field_Activation(30, Mode = Default and then Sublist_Active);
    -- Account buttons
    for F in Afpx.Field_Range'(31) .. 38 loop
       Afpx.Set_Field_Activation(F, Mode = Default);
    end loop;
    -- To francs/Euros button
    if Mode = Default then
      Update_To_Unit;
    end if;
    -- Message
    Afpx.Clear_Field(39);
    -- Confirm Ack
    Afpx.Set_Field_Activation(40, Mode /= Default);
    if Mode = Confirm then
      Afpx.Reset_Field(40, Reset_Colors => False);
    elsif Mode = Ack then
      Afpx.Encode_Field(40, (0, 1), "ACK");
    end if;

    Afpx.Set_Field_Activation(41, Mode = Confirm);
  end Set_Mode;

  procedure Allow_Edit (Allow : in Boolean) is
  begin
    Edit_Allowed := Allow;
    for F in Afpx.Field_Range'(25) .. 29 loop
      Afpx.Set_Field_Activation(F, Edit_Allowed);
    end loop;
  end Allow_Edit;

  procedure Sublist (Active : in Boolean) is
  begin
    Sublist_Active := Active;
    if Sublist_Active then
      Afpx.Encode_Field(29, (0, 1), "UN SEL");
    else
       Afpx.Reset_Field(29, Reset_Colors => False);
    end if;
    Afpx.Set_Field_Activation(30, Sublist_Active);
  end Sublist;

  -- Reset to default screen
  procedure Reset is
  begin
    Afpx.Use_Descriptor(1);
    Afpx.Line_List_Mng.Delete_List(Afpx.Line_List);
    Set_Mode(Default);
  end Reset;


  procedure Encode_File_Name (File_Name : in String) is
  begin
    Afpx.Encode_Field(1, (0, 0),
         String_Mng.Procuste(File_Name,
                             Afpx.Get_Field_Width(1),
                             Align_Left => False));
  end Encode_File_Name;

  procedure Encode_Nb_Oper (Oper : in Natural; Selected : in Natural) is
  begin
    -- Set account number
    Afpx.Encode_Field(3, (0, 0),
        Normal(Oper, Afpx.Get_Field_Width(3)));
    if Oper <= 1 then
      Afpx.Encode_Field(4, (0, 0), "operation ");
    else
      Afpx.Encode_Field(4, (0, 0), "operations");
    end if;
    Afpx.Set_Field_Activation(6, Sublist_Active);
    Afpx.Encode_Field(6, (0, 0),
               Normal(Selected, Afpx.Get_Field_Width(6)));
    Afpx.Set_Field_Activation(7, Sublist_Active);
  end Encode_Nb_Oper;

  procedure Encode_Saved (Saved : in Boolean) is
  begin
     if Saved then
       Afpx.Reset_Field(5, Reset_Colors => True, Reset_String => True);
     else
       Afpx.Encode_Field(5, (0, 0), "NOT SAVED");
       Afpx.Set_Field_Colors (5, Con_Io.Red);
     end if;
  end Encode_Saved;

  procedure Encode_Summary(Real_Amount, Account_Amount,
                           Defered_Amount, Margin_Amount :
                                    in Oper_Def.Amount_Range) is
  begin
    Afpx.Encode_Field (10, (0, 0), Unit_Format.Image(Real_Amount, True));
    Afpx.Encode_Field (12, (0, 0), Unit_Format.Image(Account_Amount, True));
    Afpx.Encode_Field (14, (0, 0), Unit_Format.Image(Defered_Amount, True));
    Afpx.Encode_Field (16, (0, 0), Unit_Format.Image(Margin_Amount, True));
  end Encode_Summary;

  function My_Ptg return Boolean is
    -- Afpx put_then_get stuff
    Cursor_Field : Afpx.Absolute_Field_Range := 1;
    Cursor_Col   : Con_Io.Col_Range := 0;
    Ptg_Result   : Afpx.Result_Rec;
    Redisplay    : Boolean := False;
  begin
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
      Redisplay := False;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              return True;
            when Afpx.Escape_Key =>
              return False;
            when Afpx.Break_Key =>
              null;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when 40 =>
              return True;
            when 41 =>
              return False;
            when others =>
              null;
          end case;
        when Afpx.Refresh =>
          Redisplay := True;
        when Afpx.Fd_Event | Afpx.Timer_Event =>
          Redisplay := True;
      end case;
    end loop;
  end My_Ptg;

  function Confirm_Action (Action : Action_List) return Boolean is
    Result : Boolean;
  begin
    Set_Mode(Confirm);
    case Action is
      when Overwrite_Account =>
        Ring(False);
        Afpx.Encode_Field (39, (0, 0),
          "Account is not saved and will be overwritten. Confirm?");
      when Overwrite_File =>
        Afpx.Encode_Field (39, (0, 0),
          "Account file exists and will be overwritten. Confirm?");
      when Quit_Unsaved =>
        Ring(False);
        Afpx.Encode_Field (39, (0, 0),
          "Account is not saved and will be lost. Confirm?");
    end case;
    -- Get answer
    Result := My_Ptg;
    Set_Mode(Default);
    return Result;
  end Confirm_Action;

  procedure Ack_Error (Error : in Error_List) is
  begin
    Set_Mode(Ack);
    Ring(True);
    case Error is
      when File_Access =>
        Afpx.Encode_Field (39, (0, 0),
          "File not found or not an account or access denied");
      when File_Io =>
        Afpx.Encode_Field (39, (0, 0), "File read or write error");
      when File_Read_Only =>
        Afpx.Encode_Field (39, (0, 0), "File is read-only");
      when File_Name_Too_Long =>
        Afpx.Encode_Field (39, (0, 0), "File name too long");
      when Account_Full =>
        Afpx.Encode_Field (39, (0, 0), "Sorry, the account is full");
      when Not_Implemented =>
        Afpx.Encode_Field (39, (0, 0), "Sorry, not implmeneted yet");
      when Internal_Error =>
        Afpx.Encode_Field (39, (0, 0), "Internal error. Saving in Tmp");
    end case;
    -- Loop until ack
    while not My_Ptg loop
      null;
    end loop;
    Set_Mode(Default);
  end Ack_Error;
 
  -- Ring alarm / question bell
  procedure Ring (Alarm : in Boolean) is
  begin
    if Alarm then
      Con_Io.Bell(3);
    else
      Con_Io.Bell(1);
    end if;
  end Ring;

end Screen;

