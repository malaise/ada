with Con_Io;
separate(Mng)

procedure Search is
  -- Afpx put_then_get stuff
  Cursor_Field : Afpx.Absolute_Field_Range := 0;
  Cursor_Col   : Con_Io.Col_Range := 0;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean := False;
  -- Operation to match
  Oper : Oper_Def.Oper_Rec;

  -- Unselect current oper
  procedure Unsel is
  begin
    if Sel_List_Mng.Get_Position(Sel_List) /= 1 then
      Sel_List_Mng.Delete(Sel_List, Sel_List_Mng.Prev);
    else
      Sel_List_Mng.Delete(Sel_List, Sel_List_Mng.Next);
    end if;
  end Unsel;

  type Match_Prot is access
       function(Cur, Crit : Oper_Def.Oper_Rec) return Boolean;

  function Ref_Match(Cur, Crit : Oper_Def.Oper_Rec) return Boolean is
    use type Oper_Def.Kind_List;
  begin
    return Cur.Kind = Oper_Def.Cheque
           and then Cur.Reference = Crit.Reference;
  end Ref_Match;

  function Status_Match(Cur, Crit : Oper_Def.Oper_Rec) return Boolean is
    use type Oper_Def.Status_List;
  begin
    return Cur.Status = Crit.Status;
  end Status_Match;

  -- Unselect all non matching oper
  procedure Unsel_All(Match : in Match_Prot; Crit : in Oper_Def.Oper_Rec) is
     Oper : Oper_Def.Oper_Rec;
  begin
    if Sel_List_Mng.Is_Empty(Sel_List) then
      return;
    end if;
    -- Scan from first
    Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next, 0, False);
    loop
      List_Util.Move_To_Current;
      Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
      if not Match(Oper, Crit) then
        -- Remove current and move to next or remove last
        Unsel;
        exit when Sel_List_Mng.Is_Empty(Sel_List);
      else
        -- Move to next
        exit when Sel_List_Mng.Get_Position(Sel_List)
                = Sel_List_Mng.List_Length(Sel_List);
        Sel_List_Mng.Move_To(Sel_List);
      end if;
    end loop;
  end Unsel_All;
    

begin

  if In_Sublist then
    -- Unselect
    Unsel;
    Refresh_Screen(Unchanged);
    return;
  end if; 

  -- Not in sublist: get criteria
  Afpx.Use_Descriptor(4);
  Screen.Encode_File_Name(Text_Handler.Value(Account_Name));
  Screen.Encode_Nb_Oper(Oper_List_Mng.List_Length(Oper_List),
                        Sel_List_Mng.List_Length(Sel_List));
  Screen.Encode_Saved(Account_Saved);
  Cursor_Field := Afpx.Next_Cursor_Field(0);

  loop
    Afpx.Put_Then_Get(Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
    Redisplay := False;
    case Ptg_Result.Event is

      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            -- Return = Search ref
            Oper.Reference := Afpx.Decode_Field(12, 0);
            Unsel_All(Ref_Match'access, Oper);
            In_Sublist := True;
            exit;
          when Afpx.Escape_Key | Afpx.Break_Key =>
            -- Escape/Break = Cancel
            In_Sublist := False;
            exit;
        end case;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          when 9 =>
            -- Defered
            Oper.Status := Oper_Def.Defered;
            Unsel_All(Status_Match'access, Oper);
            In_Sublist := True;
            exit;
          when 10 =>
            -- Not entered
            Oper.Status := Oper_Def.Not_Entered;
            Unsel_All(Status_Match'access, Oper);
            In_Sublist := True;
            exit;
          when 13 =>
            -- Cancel
            In_Sublist := False;
            exit;
          when others =>
            null;
        end case;
      when Afpx.Refresh =>
        Redisplay := True;
      when Afpx.Fd_Event | Afpx.Timer_Event =>
        null;
    end case;

  end loop;

  Screen.Reset;
  Screen.Sublist(In_Sublist);
  Refresh_Screen(Bottom);

end Search;

