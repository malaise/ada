with Ada.Calendar;
with Con_Io;
separate (Mng)
package body Edition is

  -- For checking unicity of kind and reference
  function Same_Kind_And_Ref (Op1, Op2 : Oper_Def.Oper_Rec) return Boolean is
    use type Oper_Def.Kind_List;
  begin
    return Op1.Kind = Op2.Kind and then Op1.Reference = Op2.Reference;
  end Same_Kind_And_Ref;
  procedure Search_Kind_Ref is new Oper_List_Mng.Search(Same_Kind_And_Ref);


  -- Affectation of kind and status buttons
  Kind_Buttons : constant array (Oper_Def.Kind_List) of Afpx.Field_Range :=
    (Oper_Def.Cheque => 22,
     Oper_Def.Credit => 23,
     Oper_Def.Transfer => 24,
     Oper_Def.Withdraw => 25);

  Status_Buttons : constant array (Oper_Def.Status_List) of Afpx.Field_Range :=
    (Oper_Def.Entered => 27,
     Oper_Def.Not_Entered => 28,
     Oper_Def.Defered => 29);

  -- Protect get and button fields &  set colors.
  procedure Protect_Data is
  begin
    Afpx.Set_Field_Protection(13, True);
    Afpx.Set_Field_Colors(13, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(15, True);
    Afpx.Set_Field_Colors(15, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(17, True);
    Afpx.Set_Field_Colors(17, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(19, True);
    Afpx.Set_Field_Colors(19, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(20, True);
    Afpx.Set_Field_Colors(20, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(22, True);
    Afpx.Set_Field_Colors(22, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(23, True);
    Afpx.Set_Field_Colors(23, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(24, True);
    Afpx.Set_Field_Colors(24, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(25, True);
    Afpx.Set_Field_Colors(25, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(27, True);
    Afpx.Set_Field_Colors(27, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(28, True);
    Afpx.Set_Field_Colors(28, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(29, True);
    Afpx.Set_Field_Colors(29, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(31, True);
    Afpx.Set_Field_Colors(31, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(33, True);
    Afpx.Set_Field_Colors(33, Con_Io.Light_Gray, Background => Con_Io.Black);
    Afpx.Set_Field_Protection(35, True);
    Afpx.Set_Field_Colors(35, Con_Io.Light_Gray, Background => Con_Io.Black);
  end Protect_Data;


  -- Title, data protection,
  procedure Prepare (Edit_Type : in Edit_List) is
  begin
    Afpx.Use_Descriptor(3);
    Screen.Encode_File_Name(Text_Handler.Value(Account_Name));
    Screen.Encode_Nb_Oper(Oper_List_Mng.List_Length(Oper_List),
                          Sel_List_Mng.List_Length(Sel_List));
    Screen.Encode_Saved(Account_Saved);
    case Edit_Type is
      when Create =>
        Afpx.Encode_Field(9, (0, 0), "creation");
        -- Disable No
        Afpx.Set_Field_Activation(10, False);
        Afpx.Set_Field_Activation(11, False);
      when Modify =>
        Afpx.Encode_Field(9, (0, 0), "modification");
      when Copy =>
        Afpx.Encode_Field(9, (0, 0), "copy");
        -- Disable No
        Afpx.Set_Field_Activation(10, False);
        Afpx.Set_Field_Activation(11, False);
      when Delete =>
        Afpx.Encode_Field(9, (0, 0), "deletion");
        Protect_Data;
    end case;
  end Prepare;

  -- Set buttons colors according to current kind/status
  procedure Set_Buttons(Allow_Edit : in Boolean;
                        Kind : in Oper_Def.Kind_List;
                        Status : in Oper_Def.Status_List) is

    -- Set one button color according to active or not and modifiable or not
    procedure Set_Active (Button : in Afpx.Field_Range;
                          Active : in Boolean) is
    begin
      -- Default (except unselected and view/delete)
      Afpx.Set_Field_Activation(Button, True);
      if Allow_Edit then
        if Active then
          -- Active and modifiable
          Afpx.Set_Field_Colors(Button,
                                Foreground => Con_Io.Magenta);
        else
          -- Inactive and modifiable
          Afpx.Set_Field_Colors(Button,
                                Foreground => Con_Io.Cyan);
        end if;
      else
        if Active then
          -- Inactive and modifiable
          Afpx.Set_Field_Colors(Button,
                                Foreground => Con_Io.Light_Gray);
        else
          Afpx.Set_Field_Activation(Button, False);
        end if;
      end if;
    end Set_Active;

    use type Oper_Def.Kind_List, Oper_Def.Status_List;
  begin
    for K in Oper_Def.Kind_List loop
      Set_Active(Kind_Buttons(K), K = Kind);
    end loop;
    for S in Oper_Def.Status_List loop
      Set_Active(Status_Buttons(S), S = Status);
    end loop;
    -- Allow defered (check done in update_buttons)
    if Status = Oper_Def.Defered
    and then not Oper_Def.Kind_Can_Be_Defered(Kind) then
      raise Program_Error;
    end if;
    Afpx.Set_Field_Activation(Status_Buttons(Oper_Def.Defered),
                              Oper_Def.Kind_Can_Be_Defered(Kind));
  end Set_Buttons;

  -- Update buttons after a click
  procedure Update_Buttons (Field  : in Afpx.Field_Range;
                            Kind   : in out Oper_Def.Kind_List;
                            Status : in out Oper_Def.Status_List) is
    use type Afpx.Field_Range, Oper_Def.Kind_List, Oper_Def.Status_List;
  begin
    for K in Oper_Def.Kind_List loop
      if Field = Kind_Buttons(K) then
        -- New kind
        Kind := K;
        -- Update Status if defered and not allowed
        if Status = Oper_Def.Defered
        and then not Oper_Def.Kind_Can_Be_Defered(Kind) then
          Status := Oper_Def.Not_Entered;
        elsif Status = Oper_Def.Not_Entered
        and then Oper_Def.Kind_Can_Be_Defered(Kind) then
          Status := Oper_Def.Defered;
        end if;
        Set_Buttons(True, Kind, Status);
        return;
      end if;
    end loop;
    for S in Oper_Def.Status_List loop
      if Field = Status_Buttons(S) then
        -- New status
        Status := S;
        Set_Buttons(True, Kind, Status);
        return;
      end if;
    end loop;
    -- Never reached
    raise Program_Error;
  end Update_Buttons;

  procedure Protect_Movements (Edit_Type : in Edit_List) is
  begin
    if Edit_Type = Create or else Edit_Type = Copy then
      -- Always and only allow Ok, Cancel,
      --  and also Ok_And_Next if create
      Afpx.Set_Field_Activation(37, False);
      Afpx.Set_Field_Activation(38, False);
      Afpx.Set_Field_Activation(42, False);
      if Edit_Type = Copy then
        Afpx.Set_Field_Activation(41, False);
      end if;
      return;
    end if;

    -- Now list cannot be empty: protect out-of-list
    Afpx.Set_Field_Activation(37,
            Sel_List_Mng.Check_Move(Sel_List, Sel_List_Mng.Prev));
    Afpx.Set_Field_Activation(38,
            Sel_List_Mng.Check_Move(Sel_List, Sel_List_Mng.Prev));
    Afpx.Set_Field_Activation(41,
            Sel_List_Mng.Check_Move(Sel_List, Sel_List_Mng.Next));
    Afpx.Set_Field_Activation(42,
            Sel_List_Mng.Check_Move(Sel_List, Sel_List_Mng.Next));

    if Edit_Type = Delete then
      -- Only allow back
      Afpx.Set_Field_Activation(37, False);
      Afpx.Set_Field_Activation(38, False);
      Afpx.Set_Field_Activation(41, False);
      Afpx.Set_Field_Activation(42, False);
    end if;
  end Protect_Movements;

  -- Set unit button according to current unit
  procedure Set_Unit is
    use type Unit_Format.Units_List;
  begin
    -- Forbid switch
    Afpx.Set_Field_Protection(19, Protect => True);
    if Unit_Format.Get_Current_Unit = Unit_Format.Euros then
      Afpx.Reset_Field(19);
      Afpx.Encode_Field(19, (0, 1), "e");
    else
      Afpx.Set_Field_Colors(19, Con_Io.Blue);
      Afpx.Encode_Field(19, (0, 1), "F");
    end if;
  end Set_Unit;

  -- Adjust operation after copy
  procedure Adjust_Copy (Oper : in out Oper_Def.Oper_Rec) is
    use type Oper_Def.Kind_List;
    Cur_Date : constant Oper_Def.Date_Rec := Oper_Def.Current_Date;
    Prev : Boolean;
  begin
    -- Adjust Status: Credit is defered, others are not entered
    if Oper.Kind = Oper_Def.Credit then
      Oper.Status := Oper_Def.Defered;
    else
      Oper.Status := Oper_Def.Not_Entered;
    end if;

    -- Set Date: If source of copy is at previous month
    --  and is a Trnasfer then set to current month
    begin
      Prev := (Cur_Date.Month = Ada.Calendar.Month_Number'First
               and then Oper.Date.Month = Ada.Calendar.Month_Number'Last
               and then Cur_Date.Year = Oper.Date.Year + 1)
              or else (Cur_Date.Month = Oper.Date.Month + 1
                and then Cur_Date.Year = Oper.Date.Year);
    exception
      when others =>
        Prev := False;
    end;
    if Oper.Kind = Oper_Def.Transfer and then Prev then
      Oper.Date.Month := Cur_Date.Month;
    end if;
  end Adjust_Copy;

  -- Encode operation
  procedure Encode_Oper (Edit_Type : in Edit_List;
                         Oper : in Oper_Def.Oper_Rec;
                         Deleted : in Boolean) is
    Date_Str : Unit_Format.Short_Date_Str;
    use type Oper_Def.Amount_Range;
  begin
    if Edit_Type /= Create then
      -- No
      Afpx.Encode_Field(11, (0, 0),
          Normal(Oper_List_Mng.Get_Position(Oper_List), 4));
    end if;
    -- Date
      Date_Str := Unit_Format.Short_Date_Image(Oper.Date);
    Afpx.Encode_Field(13, (0, 0), Date_Str(1 .. 2));
    Afpx.Encode_Field(15, (0, 0), Date_Str(4 .. 5));
    Afpx.Encode_Field(17, (0, 0), Date_Str(7 .. 8));
    -- Amount
    if Oper.Amount /= 0.0 then
      Afpx.Encode_Field(20, (0, 0), Unit_Format.Image(Oper.Amount, True));
    else
      Afpx.Clear_Field(20);
    end if;
    Set_Unit;
    -- Kind and status (modifiable or not)
    Set_Buttons(Edit_Type in Create .. Copy, Oper.Kind, Oper.Status);
    -- 3 strings
    Afpx.Encode_Field(31, (0, 0), Oper.Destination);
    Afpx.Encode_Field(33, (0, 0), Oper.Comment);
    Afpx.Encode_Field(35, (0, 0), Oper.Reference);
    -- Deleted
    Afpx.Set_Field_Activation(36, Deleted);
  end Encode_Oper;

  procedure Update is
  begin
    Account_Saved := False;
    Screen.Encode_Nb_Oper(Oper_List_Mng.List_Length(Oper_List)
                             - Deletion.Get_Nb_Deleted,
                          Sel_List_Mng.List_Length(Sel_List)
                             - Deletion.Get_Nb_Deleted);
    Screen.Encode_Saved(Account_Saved);

    Compute_Amounts;
    List_Util.Move_To_Current;
  end Update;

  -- Cancel (a deletion)
  procedure Cancel (Edit_Type : in Edit_List) is
  begin
    if Edit_Type = Delete then
      Deletion.Flag_Undeleted;
      Update;
    end if;
  end Cancel;

  -- Check data. If Ok, write/delete it and return 0,
  --   otherwise return the field of error
  function Validate (Edit_Type : in Edit_List;
                     Kind : Oper_Def.Kind_List;
                     Status : Oper_Def.Status_List)
           return Afpx.Absolute_Field_Range is

    Oper, Saved_Oper : Oper_Def.Oper_Rec;
    Saved_Movement : Oper_List_Mng.Movement;
    Date_Str : Unit_Format.Date_Str;
    Field : Afpx.Absolute_Field_Range := 0;
    Found : Boolean;
    Pos : Positive;
    Sel : Sel_Rec;
    use type Oper_Def.Status_List, Oper_Def.Kind_List,
             Afpx.Absolute_Field_Range;
  begin
    if Edit_Type = Delete then
      -- Flag selected operation as deleted
      Deletion.Flag_Deleted;
      Update;
      return 0;
    end if;

    -- Check data, return error field
    -- Date
    Field := 13;
    Date_Str := Afpx.Decode_Field(13,0) & '/'
              & Afpx.Decode_Field(15,0) & '/'
       & "20" & Afpx.Decode_Field(17,0);
    Oper.Date := Unit_Format.Date_Value(Date_Str);
    -- Amount
    Field := 20;
    Oper.Amount := Unit_Format.Value(Afpx.Decode_Field(20, 0));
    -- Kind and status
    Oper.Kind := Kind;
    Oper.Status := Status;
    if Oper.Status = Oper_Def.Defered
    and then not Oper_Def.Kind_Can_Be_Defered(Oper.Kind) then
      -- Should be protected at buttons level
      raise Program_Error;
    end if;

    -- Strings
    Oper.Destination := Afpx.Decode_Field(31,0);
    Oper.Comment     := Afpx.Decode_Field(33,0);
    Oper.Reference   := Afpx.Decode_Field(35,0);

    -- Non empty reference unique for cheque
    Field := 35;
    if Oper.Kind = Oper_Def.Cheque
    and then Oper.Reference /= Oper_Def.Reference_Str'(others => ' ') then

      -- Remove current temporaly (just for search)
      if Edit_Type = Modify then
        -- Save current oper and position before deletion
        Pos := Oper_List_Mng.Get_Position(Oper_List);
        Oper_List_Mng.Read(Oper_List, Saved_Oper, Oper_List_Mng.Current);
        -- Remove current oper so we won't find it
        --  move to next if possible.
        if Oper_List_Mng.List_Length(Oper_List) = 1
        or else Oper_List_Mng.Check_Move(Oper_List) then
          -- Last item of the list or not end of list
          Saved_Movement := Oper_List_Mng.Next;
        else
          Saved_Movement := Oper_List_Mng.Prev;
        end if;
        Oper_List_Mng.Delete(Oper_List, Saved_Movement);
        if Oper_List_Mng.Is_Empty(Oper_List) then
          -- List becomes empty
          Saved_Movement := Oper_List_Mng.Current;
        end if;
      end if;

      -- Search same kind (cheque) and same reference
      Search_Kind_Ref(Oper_List, Found, Oper, From => Oper_List_Mng.Absolute);
      -- Found another one?
      if not Found then
        -- Ok. Ref is unique
        Field := 0;
      end if;

      -- Restore 
      if Edit_Type = Modify then
        case Saved_Movement is
          when Oper_List_Mng.Next =>
            -- Not last elem was removed, and we moved to next
            -- Move to this element and insert before
            Oper_List_Mng.Move_To(Oper_List, Oper_List_Mng.Next, Pos-1, False);
            Oper_List_Mng.Insert(Oper_List, Saved_Oper, Oper_List_Mng.Prev);
          when Oper_List_Mng.Prev =>
            -- Last elem was removed, and we moved to previous
            -- Insert at end
            Oper_List_Mng.Rewind(Oper_List, Oper_List_Mng.Prev);
            Oper_List_Mng.Insert(Oper_List, Saved_Oper, Oper_List_Mng.Next);
          when Oper_List_Mng.Current =>
            -- List is empty
            Oper_List_Mng.Insert(Oper_List, Saved_Oper, Oper_List_Mng.Next);
        end case;
      end if;
      if Field /= 0 then
        -- Generate error
        raise Unit_Format.Format_Error;
      end if;
    end if; -- Cheque ref is unique

    -- Data ok: insert or modify
    if Edit_Type = Modify then
      Oper_List_Mng.Modify(Oper_List, Oper, Oper_List_Mng.Current);
    else
      -- Insert at the end to keep selection accurate
      if not Oper_List_Mng.Is_Empty(Oper_List) then
        Oper_List_Mng.Rewind(Oper_List, Oper_List_Mng.Prev);
      end if;
      Oper_List_Mng.Insert(Oper_List, Oper);
      -- Insert at the end of selection and go back to current (for next copy)
      if not Sel_List_Mng.Is_Empty(Sel_List) then
        List_Util.Save_Pos;
        Sel_List_Mng.Rewind(Sel_List, Sel_List_Mng.Prev);
        Sel_List_Mng.Insert(Sel_List, (No => Oper_List_Mng.List_Length(Oper_List),
                                       Deleted => False) );
        List_Util.Restore_Pos;
        List_Util.Move_To_Current;
      else
        Sel_List_Mng.Insert(Sel_List, (No => Oper_List_Mng.List_Length(Oper_List),
                                       Deleted => False) );
      end if;
    end if;
    Update;
    return 0;
  exception
    when Unit_Format.Format_Error =>
      return Field;
  end Validate;

  -- Cursor is set after the last significant (non  space) character
  function Set_Cursor (Cause : Afpx.Enter_Field_Cause_List;
                       Str   : String) return Con_Io.Col_Range is
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Cause = Afpx.Right_Full then
      return 0;
    end if;
    for I in reverse Str'Range loop
      if Str(I) /= ' ' then
        if I = Str'Last then
          -- Last char is significant.
          -- Ok Afpx takes care of excessive col, but let's be clean
          return Str'Length - 1;
        else
          -- Afpx takes care of us with a Str (1 .. N), but let's be clean
          return I - Str'First + 1;
        end if;
      end if;
    end loop;
    return 0;
  end Set_Cursor;

  -- Do the edition  
  procedure Edit (Edit_Type : in Edit_List) is
    -- Original unit to restore
    Orig_Unit : constant Unit_Format.Units_List
              := Unit_Format.Get_Current_Unit;
    Oper : Oper_Def.Oper_Rec;
    -- Afpx put_then_get stuff
    Cursor_Field : Afpx.Absolute_Field_Range := 0;
    Cursor_Col   : Con_Io.Col_Range := 0;
    Ptg_Result   : Afpx.Result_Rec;
    Redisplay    : Boolean := False;
    -- Current Kind and Status
    Kind : Oper_Def.Kind_List;
    Status : Oper_Def.Status_List;
    -- Deleted flag read from selection list
    Sel : Sel_Rec;
    Deleted : Boolean;
    -- Is Ok_And_Next active when keyboard Return
    Oknext_Active : Boolean;
    use type Afpx.Absolute_Field_Range, Oper_Def.Kind_List;
  begin

    -- Set title, fields protections & buttons
    Prepare(Edit_Type);
    All_Edit:
    loop
      -- Move to current for copy, edit, view, delete
      if not Sel_List_Mng.Is_Empty(Sel_List) then
        List_Util.Move_To_Current;
      end if;
      -- Set data
      case Edit_Type is
        when Create =>
          -- Default operation, credit defered.
          Oper.Date := Oper_Def.Current_Date;
          Oper.Amount := 0.0;
          Oper.Kind := Oper_Def.Credit;
          Oper.Status := Oper_Def.Defered;
          Oper.Reference := (others => ' ');
          Oper.Destination := (others => ' ');
          Oper.Comment := (others => ' ');
          Deleted := False;
        when Copy =>
          Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
          Adjust_Copy (Oper);
          Deleted := False;
        when Modify | Delete =>
          Sel_List_Mng.Read(Sel_List, Sel, Sel_List_Mng.Current);
          Deleted := Sel.Deleted;
          -- Current operation
          Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
      end case;
      -- Encode data
      Encode_Oper(Edit_Type, Oper, Deleted);
      Kind := Oper.Kind;
      Status := Oper.Status;
      -- Prepare Ptg
      Cursor_Field := Afpx.Next_Cursor_Field(0);
      if Cursor_Field = 0 then
        -- No get field
        Cursor_Field := 1;
      else
        Cursor_Col := 0;
      end if;
      -- Protect movements
      Protect_Movements(Edit_Type);

      -- Ptgs
      One_Edit:
      loop
        Afpx.Put_Then_Get(Cursor_Field, Cursor_Col, Ptg_Result, Redisplay,
                          Set_Cursor'Access);
        Redisplay := False;
        case Ptg_Result.Event is

          when Afpx.Keyboard =>
            case Ptg_Result.Keyboard_Key is
              when Afpx.Return_Key =>
                -- Ok and back or next
                Cursor_Field := Validate(Edit_Type, Kind, Status);
                Cursor_Col := 0;
                if Cursor_Field = 0 then
                  -- Check that Ok_And_Next button is active
		  Afpx.Get_Field_Activation(41, Oknext_Active);
                  if not Oknext_Active then
                    -- Ok and back
                    exit All_Edit;
                  else
                    if Edit_Type /= Create then
                      -- Next oper
                      Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next);
                    end if;
                    exit One_Edit;
                  end if;
                end if;
                Screen.Ring(True);
                -- Return = Ok
                Screen.Ring(True);
              when Afpx.Escape_Key =>
                -- Escape = Cancel
                Cancel(Edit_Type);
                exit All_Edit;
              when Afpx.Break_Key =>
                -- Break = Cancel
                exit All_Edit;
            end case;

          when Afpx.Mouse_Button =>
            case Ptg_Result.Field_No is
              when 19 =>
                -- Change unit
                Unit_Format.Switch_Unit;
                Set_Unit;

              when 22 .. 25 | 27 .. 29 =>
                -- Kind and status buttons
                Update_Buttons(Ptg_Result.Field_No, Kind, Status);
                if Edit_Type = Create then
                  Cursor_Field := Afpx.Next_Cursor_Field(Ptg_Result.Field_No);
                  Cursor_Col := 0;
                end if;

              when 37 =>
                -- Ok and prev
                Cursor_Field := Validate(Edit_Type, Kind, Status);
                Cursor_Col := 0;
                if Cursor_Field = 0 then
                  -- Prev oper
                  Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Prev);
                  exit One_Edit;
                end if;
                Screen.Ring(True);
              when 38 =>
                -- Cancel and prev
                Cancel(Edit_Type);
                Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Prev);
                exit One_Edit;
              when 39 =>
                -- Ok and back
                Cursor_Field := Validate(Edit_Type, Kind, Status);
                Cursor_Col := 0;
                exit All_Edit when Cursor_Field = 0;
                Screen.Ring(True);
              when 40 =>
                -- Cancel and back
                Cancel(Edit_Type);
                exit All_Edit;
              when 41 =>
                -- Ok and next
                Cursor_Field := Validate(Edit_Type, Kind, Status);
                Cursor_Col := 0;
                if Cursor_Field = 0 then
                  if Edit_Type /= Create and then Edit_Type /= Copy then
                    -- Next oper
                    Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next);
                  end if;
                  exit One_Edit;
                end if;
                Screen.Ring(True);
              when 42 =>
                -- Cancel and next
                Cancel(Edit_Type);
                Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next);
                exit One_Edit;

              when others =>
                null;
            end case;

          when Afpx.Refresh =>
            Redisplay := True;
          when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
             | Afpx.Wakeup_Event =>
            null;
        end case;

      end loop One_Edit;

    end loop All_Edit;

    if Edit_Type = Delete then
      Deletion.Commit_Deletions;
    elsif Edit_Type = Create
    and then not Sel_List_Mng.Is_Empty(Sel_List) then
      -- Move to bottom
      Sel_List_Mng.Rewind(Sel_List, Sel_List_Mng.Prev);
    end if;

    -- Restore original unit
    Unit_Format.Set_Unit_To(Orig_Unit);

  end Edit;

end Edition;

