with Con_Io;
separate(Mng)

procedure Search is
  In_Sublist : Boolean;

  -- Unselect current oper
  procedure Unsel is
    Done : Boolean;
  begin
    Sel_List_Mng.Delete(Sel_List, Sel_List_Mng.Prev, Done => Done);
  end Unsel;

  -- Search criteria
  type Status_Array is array (Oper_Def.Status_List) of Boolean;
  type Kind_Array   is array (Oper_Def.Kind_List) of Boolean;
  type Criteria_Rec is record
    Status : Status_Array;
    Kind   : Kind_Array;
    Reference_Set : Boolean;
    Reference : Oper_Def.Reference_Str;
  end record;

  -- Set_Criteria Match for search
  procedure Set (To : out Criteria_Rec; Val : in Criteria_Rec) is
  begin
    To := Val;
  end Set;

  -- Unselect all non matching oper
  procedure Unsel_All (Crit : in Criteria_Rec) is
    function Match (Current  : Oper_Def.Oper_Rec;
                    Criteria : Criteria_Rec) return Boolean is
    begin
      -- Match if Cur kind is set in Crit and Cur status is set in Crit
      -- Or reference Crit is set and Cur matches
      return Criteria.Kind(Current.Kind)
      and then Criteria.Status(Current.Status)
      and then (not Criteria.Reference_Set
                or else Criteria.Reference = Current.Reference);
    end Match;
    Oper : Oper_Def.Oper_Rec;
  begin
    if Sel_List_Mng.Is_Empty(Sel_List) then
      return;
    end if;

    -- Scan from first
    Sel_List_Mng.Rewind(Sel_List);
    loop
      List_Util.Move_To_Current;
      Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
      if not Match(Oper, Crit) then
        -- Remove current and move to next or remove last
        Unsel;
        exit when Sel_List_Mng.Is_Empty(Sel_List);
      else
        -- Move to next
        exit when not Sel_List_Mng.Check_Move(Sel_List);
        Sel_List_Mng.Move_To(Sel_List);
      end if;
    end loop;
  end Unsel_All;

  -- Afpx put_then_get stuff
  Cursor_Field : Afpx.Absolute_Field_Range := 0;
  Cursor_Col   : Con_Io.Col_Range := 0;
  Insert       : Boolean := False;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean := False;
  -- The search criteria
  Criteria : Criteria_Rec;

  -- Update all Afpx fields according to criteria
  procedure Update_Fields is
    -- Update the color of a field
    One_Set : Boolean;
    Status_Set, Kind_Set : Boolean;
    procedure Update_Color (Fld : in Afpx.Field_Range; Value : in Boolean) is
    begin
      if Value then
        Afpx.Set_Field_Colors (Fld, Foreground => Con_Io.Red);
        One_Set := True;
      else
        Afpx.Reset_Field (Fld, Reset_String => False);
      end if;
    end Update_Color;
  begin
    -- Set all colors
    One_Set := False;
    Update_Color (10, Criteria.Status(Oper_Def.Entered));
    Update_Color (11, Criteria.Status(Oper_Def.Not_Entered));
    Update_Color (12, Criteria.Status(Oper_Def.Defered));
    Status_Set := One_Set;
    One_Set := False;
    Update_Color (15, Criteria.Kind(Oper_Def.Cheque));
    Update_Color (16, Criteria.Kind(Oper_Def.Credit));
    Update_Color (17, Criteria.Kind(Oper_Def.Transfer));
    Update_Color (18, Criteria.Kind(Oper_Def.Withdraw));
    Update_Color (19, Criteria.Kind(Oper_Def.Savings));
    Kind_Set := One_Set;
    Update_Color (20, Criteria.Reference_Set);
    -- Update reference
    if not Criteria.Reference_Set then
     Criteria.Reference := (others => ' ');
     Afpx.Clear_Field (21);
    end if;
    Afpx.Set_Field_Activation (21, Criteria.Reference_Set);
    -- Update SEARCH button
    Afpx.Set_Field_Activation (24, Status_Set and then Kind_Set);
  end Update_Fields;

  -- Update the Criteria and fields according to clicked field
  procedure Switch_Field (Fld : in Afpx.Field_Range) is
    procedure Switch (Val : in out Boolean) is
    begin
      Val := not Val;
    end Switch;
  begin
    -- Update Criteria booleans according to field clicked
    case Fld is
      when 10 =>
        Switch (Criteria.Status(Oper_Def.Entered));
      when 11 =>
        Switch (Criteria.Status(Oper_Def.Not_Entered));
      when 12 =>
        Switch (Criteria.Status(Oper_Def.Defered));
      when 15 =>
        Switch (Criteria.Kind(Oper_Def.Cheque));
      when 16 =>
        Switch (Criteria.Kind(Oper_Def.Credit));
      when 17 =>
        Switch (Criteria.Kind(Oper_Def.Transfer));
      when 18 =>
        Switch (Criteria.Kind(Oper_Def.Withdraw));
      when 19 =>
        Switch (Criteria.Kind(Oper_Def.Savings));
      when 20 =>
        Switch (Criteria.Reference_Set);
      when others =>
        raise Program_Error;
    end case;
  end Switch_Field;

begin

  if Screen.Is_Sublist then
    -- Unselect
    Unsel;
    Refresh_Screen(Unchanged);
    return;
  end if;

  -- Not in sublist: get criteria
  -- Init screen
  Afpx.Use_Descriptor(4);
  Screen.Encode_File_Name(Text_Handler.Value(Account_Name));
  Screen.Encode_Nb_Oper(Oper_List_Mng.List_Length(Oper_List),
                        Sel_List_Mng.List_Length(Sel_List));
  Screen.Encode_Saved(Account_Saved);
  Cursor_Field := Afpx.Next_Cursor_Field(0);

  -- Init criteria and fields accordingly
  Criteria.Status := (others => False);
  Criteria.Kind   := (others => False);
  Criteria.Reference_Set := False;
  Update_Fields;

  loop
    Afpx.Put_Then_Get(Cursor_Field, Cursor_Col, Insert,
                      Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is

      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            -- Return = Search if allowed
            if Afpx.Get_Field_Activation (24) then
              Criteria.Reference := Afpx.Decode_Wide_Field(21, 0);
              Unsel_All(Criteria);
              In_Sublist := True;
              exit;
            end if;
          when Afpx.Escape_Key | Afpx.Break_Key =>
            -- Escape/Break = Cancel
            In_Sublist := True;
            exit;
        end case;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when 10 | 11 | 12 | 15 | 16 | 17 | 18 | 19 | 20 =>
            -- Switch a button
            Switch_Field (Ptg_Result.Field_No);
            Update_Fields;
          when 13 =>
            -- Select all statuses
            Criteria.Status := (others => True);
            Update_Fields;
          when 22 =>
            -- Select all
            Criteria.Status := (others => True);
            Criteria.Kind := (others => True);
            Update_Fields;
          when 23 =>
            -- Select none
            Criteria.Status := (others => False);
            Criteria.Kind := (others => False);
            Criteria.Reference_Set := False;
            Update_Fields;
          when 24 =>
            -- Search
            Criteria.Reference := Afpx.Decode_Wide_Field(21, 0);
            Unsel_All(Criteria);
            In_Sublist := True;
            exit;
          when 25 =>
            -- Cancel
            In_Sublist := False;
            exit;
          when others =>
            null;
        end case;
      when Afpx.Refresh =>
        Redisplay := True;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Wakeup_Event =>
        null;
    end case;

  end loop;

  Screen.Reset;
  Screen.Set_Sublist(In_Sublist);
  Refresh_Screen(Bottom);

end Search;

