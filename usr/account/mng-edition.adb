with Con_Io;
separate (Mng)
package body Edition is

  -- For checking unicity of kind and reference
  function Same_Kind_And_Ref (Op1, Op2 : Oper_Def.Oper_Rec) return Boolean is
    use type Oper_Def.Kind_List;
  begin
    return Op1.Kind = Op2.Kind and then Op1.Reference = Op2.Reference;
  end Same_Kind_And_Ref;
  function Search_Kind_Ref is new Oper_List_Mng.Search(Same_Kind_And_Ref);


  -- Affectation of kind and status buttons
  Kind_Buttons : constant array (Oper_Def.Kind_List) of Afpx.Field_Range :=
    (Oper_Def.Cheque => Afpx_Xref.Edition.Cheque,
     Oper_Def.Credit => Afpx_Xref.Edition.Credit_Card,
     Oper_Def.Transfer => Afpx_Xref.Edition.Transfer,
     Oper_Def.Withdraw => Afpx_Xref.Edition.Withdraw,
     Oper_Def.Savings => Afpx_Xref.Edition.Savings);

  Status_Buttons : constant array (Oper_Def.Status_List) of Afpx.Field_Range :=
    (Oper_Def.Entered => Afpx_Xref.Edition.Entered,
     Oper_Def.Not_Entered => Afpx_Xref.Edition.Not_Entered,
     Oper_Def.Defered => Afpx_Xref.Edition.Defered);

  -- Protect get and button fields & set colors.
  procedure Protect_Data is
    Back : constant Con_Io.Effective_Colors
               := Afpx.Get_Descriptor_Background;
    Black : constant Con_Io.Effective_Colors
               := Con_Io.Color_Of ("Black");
  begin
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Day, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Day, Black, Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Month, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Month, Black, Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Year, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Year, Black, Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Amount, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Amount, Black, Background => Back);

    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Cheque, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Cheque, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Credit_Card, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Credit_Card, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Transfer, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Transfer, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Withdraw, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Withdraw, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Savings, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Savings, Black,
                           Background => Back);

    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Entered, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Entered, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Not_Entered, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Not_Entered, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Defered, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Defered, Black,
                           Background => Back);

    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Destination, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Destination, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Comment, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Comment, Black,
                           Background => Back);
    Afpx.Set_Field_Protection (Afpx_Xref.Edition.Reference, True);
    Afpx.Set_Field_Colors (Afpx_Xref.Edition.Reference, Black,
                           Background => Back);
  end Protect_Data;

  -- Set unit button according to current unit
  procedure Set_Unit is
    use type Unit_Format.Units_List;
  begin
    if Unit_Format.Get_Current_Unit = Unit_Format.Euros then
      Afpx.Reset_Field (Afpx_Xref.Edition.Unit);
      begin
        Afpx.Encode_Field (Afpx_Xref.Edition.Unit, (0, 0), "€"); --## rule line off Char
      exception
        when Afpx.String_Too_Long =>
          -- We are not in UTF-8
          Afpx.Encode_Field (Afpx_Xref.Edition.Unit, (0, 0), "E");
      end;
    else
      Afpx.Set_Field_Colors (Afpx_Xref.Edition.Unit,
                             Con_Io.Color_Of ("Brown4"));
      Afpx.Encode_Field (Afpx_Xref.Edition.Unit, (0, 0), "F");
    end if;
  end Set_Unit;

  -- Title, data protection,
  procedure Prepare (Edit_Type : in Edit_List) is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Edition.Dscr_Num);
    Screen.Encode_File_Name (Account_Name.Image);
    Screen.Encode_Nb_Oper (Oper_List.List_Length,
                           Sel_List.List_Length);
    Screen.Encode_Saved (Account_Saved);
    Set_Unit;
    case Edit_Type is
      when Create =>
        Afpx.Encode_Field (Afpx_Xref.Edition.Action, (0, 0), "creation");
        -- Disable No
        Afpx.Set_Field_Activation (Afpx_Xref.Edition.No_Title, False);
        Afpx.Set_Field_Activation (Afpx_Xref.Edition.No_Value, False);
      when Modify =>
        Afpx.Encode_Field (Afpx_Xref.Edition.Action, (0, 0), "modification");
      when Copy =>
        Afpx.Encode_Field (Afpx_Xref.Edition.Action, (0, 0), "copy");
        -- Disable No
        Afpx.Set_Field_Activation (Afpx_Xref.Edition.No_Title, False);
        Afpx.Set_Field_Activation (Afpx_Xref.Edition.No_Value, False);
      when Delete =>
        Afpx.Encode_Field (Afpx_Xref.Edition.Action, (0, 0), "deletion");
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
      Afpx.Reset_Field (Button);
      if Allow_Edit then
        -- All buttons shown and active one is protected
        Afpx.Set_Field_Activation (Button, True);
        Afpx.Set_Field_Protection (Button, Active);
        if Active then
          Afpx.Set_Field_Colors (Button,
                                 Foreground => Con_Io.Color_Of("Dark_Green"),
                                 Background => Afpx.Get_Descriptor_Background);
        else
          -- Not the active one => modifiable
          Afpx.Set_Field_Colors (Button,
                                 Foreground => Con_Io.Color_Of ("Black"));
        end if;
      else
        -- Only active button shown, and protected
        if Active then
          Afpx.Set_Field_Protection (Button, True);
          Afpx.Set_Field_Colors (Button,
                                 Foreground => Con_Io.Color_Of ("Black"),
                                 Background => Afpx.Get_Descriptor_Background);
        else
          -- Hide
          Afpx.Set_Field_Activation (Button, False);
        end if;
      end if;
    end Set_Active;

    use type Oper_Def.Kind_List, Oper_Def.Status_List;
  begin
    for K in Oper_Def.Kind_List loop
      Set_Active (Kind_Buttons(K), K = Kind);
    end loop;
    for S in Oper_Def.Status_List loop
      Set_Active (Status_Buttons(S), S = Status);
    end loop;
    -- Allow some new statuses
    if not Oper_Def.Kind_Can_Be (Kind, Status) then
      raise Program_Error;
    end if;
    -- Hide status that are not possible for this kind
    for Stat in Oper_Def.Status_List loop
      if Afpx.Get_Field_Activation (Status_Buttons(Stat)) then
        Afpx.Set_Field_Activation (Status_Buttons(Stat),
                                   Oper_Def.Kind_Can_Be (Kind, Stat));
      end if;
    end loop;
  end Set_Buttons;

  -- Update buttons after a click
  procedure Update_Buttons (Field  : in Afpx.Field_Range;
                            Kind   : in out Oper_Def.Kind_List;
                            Status : in out Oper_Def.Status_List) is
    use type Afpx.Field_Range, Oper_Def.Status_List;
  begin
    for K in Oper_Def.Kind_List loop
      if Field = Kind_Buttons(K) then
        -- New kind
        Kind := K;
        -- Update Status when not allowed for current (new) kind
        if not Oper_Def.Kind_Can_Be (Kind, Status) then
          -- Try Not_Entered then Defered then Entered
          if Oper_Def.Kind_Can_Be (Kind, Oper_Def.Not_Entered) then
            Status := Oper_Def.Not_Entered;
          elsif Oper_Def.Kind_Can_Be (Kind, Oper_Def.Defered) then
            Status := Oper_Def.Defered;
          elsif Oper_Def.Kind_Can_Be (Kind, Oper_Def.Entered) then
            Status := Oper_Def.Entered;
          else
            raise Program_Error;
          end if;
        end if;
        Set_Buttons (True, Kind, Status);
        return;
      end if;
    end loop;
    for S in Oper_Def.Status_List loop
      if Field = Status_Buttons(S) then
        -- New status
        Status := S;
        Set_Buttons (True, Kind, Status);
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
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Prev, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Cancel_Prev, False);
      if Edit_Type = Copy then
        Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Next, False);
      end if;
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Cancel_Next, False);
      return;
    end if;

    -- Now list cannot be empty: protect out-of-list
    Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Prev,
                               Sel_List.Check_Move (Sel_List_Mng.Prev));
    Afpx.Set_Field_Activation (Afpx_Xref.Edition.Cancel_Prev,
                               Sel_List.Check_Move (Sel_List_Mng.Prev));
    Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Next,
                               Sel_List.Check_Move (Sel_List_Mng.Next));
    Afpx.Set_Field_Activation(Afpx_Xref.Edition.Cancel_Next,
                              Sel_List.Check_Move (Sel_List_Mng.Next));

    if Edit_Type = Delete then
      -- Only allow back
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Prev, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Cancel_Prev, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Ok_Next, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Edition.Cancel_Next, False);
    end if;
  end Protect_Movements;

  -- Encode operation
  procedure Encode_Oper (Edit_Type : in Edit_List;
                         Oper : in Oper_Def.Oper_Rec;
                         Deleted : in Boolean) is
    Date_Str : Unit_Format.Short_Date_Str;
    use type Oper_Def.Amount_Range;
  begin
    if Edit_Type /= Create then
      -- No
      Afpx.Encode_Field (Afpx_Xref.Edition.No_Value, (0, 0),
                         Normal (Oper_List.Get_Position, 4));
    end if;
    -- Date
    Date_Str := Unit_Format.Short_Date_Image (Oper.Date);
    Afpx.Encode_Field (Afpx_Xref.Edition.Day, (0, 0), Date_Str(1 .. 2));
    Afpx.Encode_Field (Afpx_Xref.Edition.Month, (0, 0), Date_Str(4 .. 5));
    Afpx.Encode_Field (Afpx_Xref.Edition.Year, (0, 0), Date_Str(7 .. 8));
    -- Amount
    if Oper.Amount /= 0.0 then
      Afpx.Encode_Field (Afpx_Xref.Edition.Amount, (0, 0),
                         Unit_Format.Image(Oper.Amount, True));
    else
      Afpx.Clear_Field (Afpx_Xref.Edition.Amount);
    end if;
    Set_Unit;
    -- Kind and status (modifiable or not)
    Set_Buttons (Edit_Type in Create .. Copy, Oper.Kind, Oper.Status);
    -- 3 strings
    Afpx.Encode_Wide_Field (Afpx_Xref.Edition.Destination, (0, 0),
                            Oper.Destination);
    Afpx.Encode_Wide_Field (Afpx_Xref.Edition.Comment, (0, 0), Oper.Comment);
    Afpx.Encode_Wide_Field (Afpx_Xref.Edition.Reference, (0, 0),
                            Oper.Reference);
    -- Deleted
    Afpx.Set_Field_Activation (37, Deleted);
  end Encode_Oper;

  procedure Update is
  begin
    Account_Saved := False;
    Screen.Encode_Nb_Oper (Oper_List.List_Length - Deletion.Get_Nb_Deleted,
                           Sel_List.List_Length - Deletion.Get_Nb_Deleted);
    Screen.Encode_Saved (Account_Saved);

    Compute_Amounts;
    List_Util.Move_To_Current;
  end Update;

  -- Cancel (a deletion)
  procedure Cancel (Edit_Type : in Edit_List) is
  begin
    if Edit_Type = Delete then
      Deletion.Flag_Undeleted;
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
    Pos : Positive;
    use type Oper_Def.Kind_List, Oper_Def.Amount_Range,
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
    Field := Afpx_Xref.Edition.Day;
    Date_Str := Afpx.Decode_Field (Afpx_Xref.Edition.Day, 0) & '/'
              & Afpx.Decode_Field (Afpx_Xref.Edition.Month, 0) & '/'
       & "20" & Afpx.Decode_Field (Afpx_Xref.Edition.Year, 0);
    Oper.Date := Unit_Format.Date_Value (Date_Str);
    -- Amount
    Field := 20;
    Oper.Amount := Unit_Format.Value(Afpx.Decode_Field(
                        Afpx_Xref.Edition.Amount, 0));
    -- Kind and status
    Oper.Kind := Kind;
    Oper.Status := Status;
    -- Negative (or nul) amount for savings
    if Oper.Kind = Oper_Def.Savings and then Oper.Amount > 0.0 then
      raise Unit_Format.Format_Error;
    end if;
    -- Sanity check, should be protected at buttons level
    if not Oper_Def.Kind_Can_Be (Oper.Kind, Oper.Status) then
      raise Program_Error;
    end if;

    -- Strings
    Field := Afpx_Xref.Edition.Destination;
    Oper.Destination := Afpx.Decode_Wide_Field (32,0);
    Field := Afpx_Xref.Edition.Comment;
    Oper.Comment     := Afpx.Decode_Wide_Field (34,0);
    Field := Afpx_Xref.Edition.Reference;
    Oper.Reference   := Afpx.Decode_Wide_Field (36,0);

    -- Non empty reference unique for cheque
    Field := Afpx_Xref.Edition.Reference;
    if Oper.Kind = Oper_Def.Cheque
    and then Oper.Reference /= Oper_Def.Reference_Str'(others => ' ') then

      -- Remove current temporaly (just for search)
      if Edit_Type = Modify then
        -- Save current oper and position before deletion
        Pos := Oper_List.Get_Position;
        Oper_List.Read (Saved_Oper, Oper_List_Mng.Current);
        -- Remove current oper so we won't find it
        --  move to next if possible.
        if Oper_List.List_Length = 1
        or else Oper_List.Check_Move then
          -- Last item of the list or not end of list
          Saved_Movement := Oper_List_Mng.Next;
        else
          Saved_Movement := Oper_List_Mng.Prev;
        end if;
        Oper_List.Delete (Saved_Movement);
        if Oper_List.Is_Empty then
          -- List becomes empty
          Saved_Movement := Oper_List_Mng.Current;
        end if;
      end if;

      -- Search same kind (cheque) and same reference
      -- Found another one?
      if not Search_Kind_Ref (Oper_List, Oper,
                              From => Oper_List_Mng.Absolute) then
        -- Ok. Ref is unique
        Field := 0;
      end if;

      -- Restore
      if Edit_Type = Modify then
        case Saved_Movement is
          when Oper_List_Mng.Next =>
            -- Not last elem was removed, and we moved to next
            -- Move to this element and insert before
            Oper_List.Move_At (Pos);
            Oper_List.Insert (Saved_Oper, Oper_List_Mng.Prev);
          when Oper_List_Mng.Prev =>
            -- Last elem was removed, and we moved to previous
            -- Insert at end
            Oper_List.Rewind (Oper_List_Mng.Prev);
            Oper_List.Insert (Saved_Oper, Oper_List_Mng.Next);
          when Oper_List_Mng.Current =>
            -- List is empty
            Oper_List.Insert (Saved_Oper, Oper_List_Mng.Next);
        end case;
      end if;
      if Field /= 0 then
        -- Generate error
        raise Unit_Format.Format_Error;
      end if;
    end if; -- Cheque ref is unique

    -- Data ok: insert or modify
    if Edit_Type = Modify then
      Oper_List.Modify (Oper, Oper_List_Mng.Current);
    else
      -- Insert at the end to keep selection accurate
      Oper_List.Rewind (Oper_List_Mng.Prev, False);
      Oper_List.Insert (Oper);
      -- Insert at the end of selection and go back to current (for next copy)
      if not Sel_List.Is_Empty then
        List_Util.Save_Pos;
        Sel_List.Rewind (Sel_List_Mng.Prev);
        Sel_List.Insert ( (No => Oper_List.List_Length, Deleted => False) );
        List_Util.Restore_Pos;
        List_Util.Move_To_Current;
      else
        Sel_List.Insert ( (No => Oper_List.List_Length, Deleted => False) );
      end if;
    end if;
    Update;
    return 0;
  exception
    when Unit_Format.Format_Error | Constraint_Error =>
      return Field;
  end Validate;

  -- Cursor is set after the last significant (non space) character on Left
  --  and on first char in all other cases
  function Set_Cursor (Unused_Field     : Afpx.Field_Range;
                       Unused_New_Field : Boolean;
                       Unused_Col       : Con_Io.Col_Range;
                       Unused_Offset    : Con_Io.Col_Range;
                       Cause : Afpx.Enter_Field_Cause_List;
                       Str   : Afpx.Unicode_Sequence)
           return Con_Io.Col_Range is
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Cause = Afpx.Left then
      return Afpx.Last_Index (Str, True);
    else
      return Con_Io.Col_Range'First;
    end if;
  end Set_Cursor;

  -- Do the edition
  procedure Edit (Edit_Type : in Edit_List) is
    -- Original unit to restore
    Orig_Unit : constant Unit_Format.Units_List
              := Unit_Format.Get_Current_Unit;
    Oper : Oper_Def.Oper_Rec;
    -- Afpx put_then_get stuff
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;
    -- Current Kind and Status
    Kind : Oper_Def.Kind_List;
    Status : Oper_Def.Status_List;
    -- Deleted flag read from selection list
    Sel : Sel_Rec;
    Deleted : Boolean;
    use type Afpx.Absolute_Field_Range, Oper_Def.Kind_List;
  begin

    -- Set title, fields protections & buttons
    Prepare (Edit_Type);
    All_Edit:
    loop
      -- Move to current for copy, edit, view, delete
      if not Sel_List.Is_Empty then
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
          Oper_List.Read (Oper, Oper_List_Mng.Current);
          Adjust_Copy (Oper);
          Deleted := False;
        when Modify | Delete =>
          Sel_List.Read(Sel, Sel_List_Mng.Current);
          Deleted := Sel.Deleted;
          -- Current operation
          Oper_List.Read (Oper, Oper_List_Mng.Current);
      end case;
      -- Encode data
      Encode_Oper (Edit_Type, Oper, Deleted);
      Kind := Oper.Kind;
      Status := Oper.Status;
      -- Prepare Ptg
      Get_Handle.Cursor_Field := 0;
      Protect_Movements (Edit_Type);

      -- Ptgs
      One_Edit:
      loop
        Afpx.Put_Then_Get (Get_Handle, Ptg_Result, False, Set_Cursor'Access);
        case Ptg_Result.Event is

          when Afpx.Keyboard =>
            case Ptg_Result.Keyboard_Key is
              when Afpx.Return_Key =>
                -- Ok and back or next
                Get_Handle.Cursor_Field := Validate (Edit_Type, Kind, Status);
                Get_Handle.Cursor_Col := 0;
                Get_Handle.Insert := False;
                if Get_Handle.Cursor_Field = 0 then
                  -- Check that Ok_And_Next button is active
                  exit All_Edit when not Afpx.Get_Field_Activation (42);
                  if Edit_Type /= Create then
                    -- Next oper
                    Sel_List.Move_To;
                  end if;
                  exit One_Edit;
                end if;
                Screen.Ring (True);
                Screen.Ring (True);
                -- Return = Ok
              when Afpx.Escape_Key =>
                -- Escape = Cancel
                Cancel (Edit_Type);
                exit All_Edit;
              when Afpx.Break_Key =>
                -- Break = Cancel
                exit All_Edit;
            end case;

          when Afpx.Mouse_Button =>
            case Ptg_Result.Field_No is
              when Afpx_Xref.Edition.Unit =>
                -- Change unit
                Unit_Format.Switch_Unit;
                Set_Unit;

              when Afpx_Xref.Edition.Cheque .. Afpx_Xref.Edition.Savings
                 | Afpx_Xref.Edition.Entered .. Afpx_Xref.Edition.Defered =>
                -- Kind and status buttons
                Update_Buttons (Ptg_Result.Field_No, Kind, Status);
                if Edit_Type = Create then
                  Get_Handle.Cursor_Field := Afpx.Next_Cursor_Field (
                      Ptg_Result.Field_No);
                  Get_Handle.Cursor_Col := 0;
                  Get_Handle.Insert := False;
                end if;

              when Afpx_Xref.Edition.Ok_Prev =>
                -- Ok and prev
                Get_Handle.Cursor_Field := Validate (Edit_Type, Kind, Status);
                Get_Handle.Cursor_Col := 0;
                Get_Handle.Insert := False;
                if Get_Handle.Cursor_Field = 0 then
                  -- Prev oper
                  Sel_List.Move_To (Sel_List_Mng.Prev);
                  exit One_Edit;
                end if;
                Screen.Ring (True);
              when Afpx_Xref.Edition.Cancel_Prev =>
                -- Cancel and prev
                Cancel (Edit_Type);
                Sel_List.Move_To (Sel_List_Mng.Prev);
                exit One_Edit;
              when Afpx_Xref.Edition.Ok =>
                -- Ok and back
                Get_Handle.Cursor_Field := Validate (Edit_Type, Kind, Status);
                Get_Handle.Cursor_Col := 0;
                Get_Handle.Insert := False;
                exit All_Edit when Get_Handle.Cursor_Field = 0;
                Screen.Ring (True);
              when Afpx_Xref.Edition.Quit =>
                -- Cancel and back
                Cancel (Edit_Type);
                exit All_Edit;
              when Afpx_Xref.Edition.Ok_Next =>
                -- Ok and next
                Get_Handle.Cursor_Field := Validate (Edit_Type, Kind, Status);
                Get_Handle.Cursor_Col := 0;
                Get_Handle.Insert := False;
                if Get_Handle.Cursor_Field = 0 then
                  if Edit_Type /= Create and then Edit_Type /= Copy then
                    -- Next oper
                    Sel_List.Move_To;
                  end if;
                  exit One_Edit;
                end if;
                Screen.Ring (True);
              when Afpx_Xref.Edition.Cancel_Next =>
                -- Cancel and next
                Cancel (Edit_Type);
                Sel_List.Move_To;
                exit One_Edit;

              when others =>
                null;
            end case;

          when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
             | Afpx.Refresh =>
            null;
        end case;

      end loop One_Edit;

    end loop All_Edit;

    if Edit_Type = Delete then
      Deletion.Commit_Deletions;
    elsif Edit_Type = Create then
      -- Move to bottom
      Sel_List.Rewind (Sel_List_Mng.Prev, Check_Empty => False);
    end if;

    -- Restore original unit
    Unit_Format.Set_Unit_To (Orig_Unit);

  end Edit;

end Edition;

