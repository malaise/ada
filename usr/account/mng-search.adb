with Timers;
separate(Mng)

procedure Search is
  In_Sublist : Boolean;

  -- Current date for searching and displaying
  Today : Oper_Def.Date_Rec;
  -- Update current date if it changes
  function Set_Today (Id : Timers.Timer_Id;
                      Data : Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
    Newday : Oper_Def.Date_Rec;
    Seconds : Ada.Calendar.Day_Duration;
    use type Oper_Def.Date_Rec;
  begin
    -- Encode current day
    Ada.Calendar.Split (Ada.Calendar.Clock,
                        Newday.Year, Newday.Month, Newday.Day, Seconds);
    if Newday /= Today then
      -- New day
      Today := Newday;
      Afpx.Encode_Field (Screen.Selected_Fld, (0,0),
          Unit_Format.Short_Date_Image(Today));
      return True;
    else
      -- No change
      return False;
    end if;
  end Set_Today;

  -- The timer id
  Timer : Timers.Timer_Id;

  -- Cancel timer if it is active
  procedure Cancel_Timer is
  begin
    Timer.Delete_If_Exists;
  end Cancel_Timer;

  -- Unselect current oper
  procedure Unsel is
    Moved : Boolean;
  begin
    Sel_List.Delete(Sel_List_Mng.Prev, Moved => Moved);
  end Unsel;

  -- Search criteria
  type Status_Array is array (Oper_Def.Status_List) of Boolean;
  type Kind_Array   is array (Oper_Def.Kind_List) of Boolean;
  type Date_List is (Prev_Month, Curr_Month, All_Dates);
  type Criteria_Rec is record
    Status : Status_Array;
    Kind   : Kind_Array;
    Reference_Set : Boolean;
    Date : Date_List;
    Reference : Oper_Def.Reference_Str;
  end record;

  -- Unselect all non matching oper
  procedure Unsel_All (Crit : in Criteria_Rec) is
    -- Previous month
    Prev_Month_Date : Oper_Def.Date_Rec;
    -- Does Current date match Criteria
    function Match (Current  : Oper_Def.Oper_Rec;
                    Criteria : Criteria_Rec) return Boolean is

      function Date_Match (Current  : Oper_Def.Oper_Rec;
                           Criteria : Criteria_Rec) return Boolean is
      begin
        if Criteria.Date = All_Dates then
          return True;
        end if;
        if Criteria.Date = Curr_Month
        and then Current.Date.Year = Today.Year
        and then Current.Date.Month = Today.Month then
          return True;
        end if;
        if Criteria.Date = Prev_Month
        and then Current.Date.Year = Prev_Month_Date.Year
        and then Current.Date.Month = Prev_Month_Date.Month then
          return True;
        end if;
        return False;
      end Date_Match;
    begin
      -- Match if Cur kind is set in Crit and Cur status is set in Crit
      -- Or reference Crit is set and Cur matches
      return Criteria.Kind(Current.Kind)
      and then Criteria.Status(Current.Status)
      and then Date_Match (Current, Criteria)
      and then (not Criteria.Reference_Set
                or else Criteria.Reference = Current.Reference);
    end Match;
    Oper : Oper_Def.Oper_Rec;
  begin
    if Sel_List.Is_Empty then
      return;
    end if;

    -- Compute previous month
    if Crit.Date = Prev_Month then
      declare
        Now : constant Ada.Calendar.Time
            := Ada.Calendar.Time_Of (Today.Year, Today.Month, Today.Day, 0.0);
        use type Perpet.Duration_Rec;
        Prev_Month_Time : constant Ada.Calendar.Time
                        := Now - (Years => 0, Months => 1);
        Seconds : Ada.Calendar.Day_Duration;
      begin
        Ada.Calendar.Split (Prev_Month_Time,
           Prev_Month_Date.Year, Prev_Month_Date.Month, Prev_Month_Date.Day,
           Seconds);
      end;
    end if;

    -- Scan from first
    Sel_List.Rewind;
    loop
      List_Util.Move_To_Current;
      Oper_List.Read(Oper, Oper_List_Mng.Current);
      if not Match(Oper, Crit) then
        -- Remove current and move to next or remove last
        Unsel;
        exit when Sel_List.Is_Empty;
      else
        -- Move to next
        exit when not Sel_List.Check_Move;
        Sel_List.Move_To;
      end if;
    end loop;
  end Unsel_All;

  -- Afpx put_then_get stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result : Afpx.Result_Rec;
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
        Afpx.Set_Field_Colors (Fld, Foreground => Con_Io.Color_Of ("Blue"));
        One_Set := True;
      else
        Afpx.Reset_Field (Fld, Reset_String => False);
      end if;
    end Update_Color;
  begin
    -- Set all colors
    One_Set := False;
    Update_Color (Afpx_Xref.Search.Entered,
                  Criteria.Status(Oper_Def.Entered));
    Update_Color (Afpx_Xref.Search.Not_Entered,
                  Criteria.Status(Oper_Def.Not_Entered));
    Update_Color (Afpx_Xref.Search.Defered,
                  Criteria.Status(Oper_Def.Defered));
    Status_Set := One_Set;
    One_Set := False;
    Update_Color (Afpx_Xref.Search.Cheque, Criteria.Kind(Oper_Def.Cheque));
    Update_Color (Afpx_Xref.Search.Credit_Card,
                  Criteria.Kind(Oper_Def.Credit));
    Update_Color (Afpx_Xref.Search.Transfer, Criteria.Kind(Oper_Def.Transfer));
    Update_Color (Afpx_Xref.Search.Withdraw, Criteria.Kind(Oper_Def.Withdraw));
    Update_Color (Afpx_Xref.Search.Savings, Criteria.Kind(Oper_Def.Savings));
    Kind_Set := One_Set;
    Update_Color (Afpx_Xref.Search.Prev_Month, Criteria.Date = Prev_Month);
    Update_Color (Afpx_Xref.Search.Curr_Month, Criteria.Date = Curr_Month);
    Update_Color (Afpx_Xref.Search.Ref_Title, Criteria.Reference_Set);
    -- Update reference
    if not Criteria.Reference_Set then
     Criteria.Reference := (others => ' ');
     Afpx.Clear_Field (Afpx_Xref.Search.Reference);
     Get_Handle.Cursor_Col := 0;
     Get_Handle.Insert := False;
    end if;
    Afpx.Set_Field_Activation (Afpx_Xref.Search.All_Dates,
                               Criteria.Date /= All_Dates);
    Afpx.Set_Field_Activation (Afpx_Xref.Search.Reference,
                               Criteria.Reference_Set);
    -- Update SEARCH button
    Afpx.Set_Field_Activation (Afpx_Xref.Search.Search,
                               Status_Set and then Kind_Set);
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
      when Afpx_Xref.Search.Entered =>
        Switch (Criteria.Status(Oper_Def.Entered));
      when Afpx_Xref.Search.Not_Entered =>
        Switch (Criteria.Status(Oper_Def.Not_Entered));
      when Afpx_Xref.Search.Defered =>
        Switch (Criteria.Status(Oper_Def.Defered));
      when Afpx_Xref.Search.Cheque =>
        Switch (Criteria.Kind(Oper_Def.Cheque));
      when Afpx_Xref.Search.Credit_Card =>
        Switch (Criteria.Kind(Oper_Def.Credit));
      when Afpx_Xref.Search.Transfer =>
        Switch (Criteria.Kind(Oper_Def.Transfer));
      when Afpx_Xref.Search.Withdraw =>
        Switch (Criteria.Kind(Oper_Def.Withdraw));
      when Afpx_Xref.Search.Savings =>
        Switch (Criteria.Kind(Oper_Def.Savings));
      when Afpx_Xref.Search.Ref_Title =>
        Switch (Criteria.Reference_Set);
      when others =>
        raise Program_Error;
    end case;
  end Switch_Field;

  Dummy_Bool : Boolean;
  pragma Unreferenced (Dummy_Bool);
begin

  if Screen.Is_Sublist then
    -- Unselect
    Unsel;
    Refresh_Screen(Unchanged);
    return;
  end if;

  -- Not in sublist: get criteria
  -- Init screen
  Afpx.Use_Descriptor(Afpx_Xref.Search.Dscr_Num);
  Screen.Encode_File_Name(Account_Name.Image);
  Screen.Encode_Nb_Oper(Oper_List.List_Length, Sel_List.List_Length);
  Afpx.Set_Field_Activation(Screen.Selected_Fld, True);
  Screen.Encode_Saved(Account_Saved);

  -- Arm timer
  Dummy_Bool := Set_Today (Timers.No_Timer);
  Timer.Create ( (Delay_Kind => Timers.Delay_Sec,
                  Clock  => null,
                  Period => 1.0,
                  Delay_Seconds => 1.0),
                     Set_Today'Unrestricted_Access);

  -- Init criteria and fields accordingly
  Criteria.Status := (others => False);
  Criteria.Kind   := (others => False);
  Criteria.Date := All_Dates;
  Criteria.Reference_Set := False;
  Update_Fields;

  loop
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
    case Ptg_Result.Event is

      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            -- Return = Search if allowed
            if Afpx.Get_Field_Activation (29) then
              Criteria.Reference := Afpx.Decode_Wide_Field(26, 0);
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
          when Afpx_Xref.Search.Entered
             | Afpx_Xref.Search.Not_Entered
             | Afpx_Xref.Search.Defered
             | Afpx_Xref.Search.Cheque
             | Afpx_Xref.Search.Credit_Card
             | Afpx_Xref.Search.Transfer
             | Afpx_Xref.Search.Withdraw
             | Afpx_Xref.Search.Savings
             | Afpx_Xref.Search.Ref_Title =>
            -- Switch a button
            Switch_Field (Ptg_Result.Field_No);
            Update_Fields;
          when Afpx_Xref.Search.All_Status =>
            -- Select all statuses
            Criteria.Status := (others => True);
            Update_Fields;
          when Afpx_Xref.Search.All_Kind =>
            -- Select all kinds
            Criteria.Kind := (others => True);
            Update_Fields;
          when Afpx_Xref.Search.Prev_Month =>
            -- Select previous month or all dates
            if Criteria.Date /= Prev_Month then
              Criteria.Date := Prev_Month;
            else
              Criteria.Date := All_Dates;
            end if;
            Update_Fields;
          when Afpx_Xref.Search.Curr_Month =>
            -- Select current month or all dates
            if Criteria.Date /= Curr_Month then
              Criteria.Date := Curr_Month;
            else
              Criteria.Date := All_Dates;
            end if;
            Update_Fields;
          when Afpx_Xref.Search.All_Dates =>
            -- Select all dates
            Criteria.Date := All_Dates;
            Update_Fields;
          when Afpx_Xref.Search.All_Oper =>
            -- Select all
            Criteria.Status := (others => True);
            Criteria.Kind := (others => True);
            Update_Fields;
          when Afpx_Xref.Search.None =>
            -- Select none
            Criteria.Status := (others => False);
            Criteria.Kind := (others => False);
            Update_Fields;
          when Afpx_Xref.Search.Search =>
            -- Search
            Criteria.Reference := Afpx.Decode_Wide_Field(26, 0);
            Unsel_All(Criteria);
            In_Sublist := True;
            exit;
          when Afpx_Xref.Search.Cancel =>
            -- Cancel
            In_Sublist := False;
            exit;
          when others =>
            null;
        end case;
      when Afpx.Fd_Event | Afpx.Signal_Event | Afpx.Timer_Event
         | Afpx.Refresh =>
        null;
    end case;

  end loop;

  Cancel_Timer;
  Screen.Reset;
  Screen.Set_Sublist(In_Sublist);
  Refresh_Screen(Bottom);

exception
  when others =>
    Cancel_Timer;
    raise;
end Search;

