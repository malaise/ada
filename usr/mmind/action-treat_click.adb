separate (Action)
procedure Treat_Click is
begin

  if not Playing then
    -- Only Start/Level/Exit
    case History(Curr_Status).Selection_Kind is

      when Screen.Menu =>
        Screen.Put_Start_Giveup (Start => True, Selected => True);

      when Screen.Level =>
        Screen.Put_Level (History(Curr_Status).Level_No,
                          Selected => True);

      when Screen.Exit_Game =>
        Screen.Put_Exit (Selected => True);

      when others =>
        History(Curr_Status) := Discard;
    end case;

    return;
  end if;

  -- Playing
  case History(Curr_Status).Selection_Kind is

    when Screen.Menu =>
      -- Give up
      Screen.Put_Start_Giveup (Start => False, Selected => True);

    when Screen.Exit_Game =>
      -- Exit
      Screen.Put_Exit (Selected => True);

    when Screen.Try =>
      declare
        Try_State : constant Common.Try_List
            := Common.Get_Propal_State (History(Curr_Status).Try_No).Try;
        use type Common.Try_List;
      begin
        -- Check that this propal is completed and not already answered
        if Try_State = Common.Can_Try then
          -- Highlight try
          Screen.Put_Try (Propal => History(Curr_Status).Try_No,
                          Try_State => Screen.Selected);
        else
          History(Curr_Status) := Discard;
        end if;
      end;

    when Screen.Color =>
      -- Highlight color
      Screen.Put_Selected_Color (Color => History(Curr_Status).Color_No,
                                 Selected => True);
    when Screen.Propal =>
      declare
        Propal_State : constant Common.Propal_State_Rec
            := Common.Get_Propal_State (History(Curr_Status).Propal_No);
        Color : constant Common.Color_Range
              := Propal_State.Propal_Color(History(Curr_Status).Column_No);
        use type Common.Color_Range;
      begin
        -- Check that this propal is filled if as a source
        if (Curr_Status = Click_Orig
            and then Color /= Common.Color_Range'First)
        or else Curr_Status = Click_Dest then
          -- Attempt to move a color in or to propal. Highlight dest square
          Screen.Put_Default_Pos (History(Curr_Status).Propal_No,
                                  History(Curr_Status).Column_No,
                                  Show => True);
        else
          History(Curr_Status) := Discard;
        end if;
      end;

    when others =>
      -- Level or other
      History(Curr_Status) := Discard;
  end case;

end Treat_Click;

