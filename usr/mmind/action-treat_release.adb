separate (Action)
procedure Treat_Release (Go_On, Exit_Game, Color_Move : out Boolean) is

  procedure Put_Secret is
    Secret : constant Response.Color_Rec := Response.Get_Code;
  begin
    for I in Common.Level_Range
     range Common.Level_Range'First .. Level loop
      Screen.Put_Secret_Color(I, Secret.Color(I));
    end loop;
  end Put_Secret;

  procedure Answer is
    Free_State : Common.Propal_State_Rec;
    Color : Response.Color_Rec (Level);
    Resp : Response.Response_Rec;
    use type Common.Propal_Range, Common.Try_List;
  begin
    if History(Curr_Status).Try_No /= First_Free then
      declare
        Prop_State : Common.Propal_State_Rec;
      begin
        Free_State := Common.Get_Propal_State (History(Curr_Status).Try_No);
        Prop_State := Common.Get_Propal_State (First_Free);
        -- Switch propals
        Common.Set_Propal_State (
         Propal => History(Curr_Status).Try_No,
         State  => Prop_State);
        Common.Set_Propal_State (
         Propal => First_Free,
         State  => Free_State);
        -- Update screen of propals and try
        for I in Common.Level_Range
         range Common.Level_Range'First .. Level loop
          Screen.Put_Color (
           Propal => History(Curr_Status).Try_No,
           Level => I,
           Color => Prop_State.Propal_Color(I) );
          Screen.Put_Color (
           Propal => First_Free,
           Level => I,
           Color => Free_State.Propal_Color(I) );
        end loop;
        -- Answered impossible because of Next_Free
        if Prop_State.Try = Common.Can_Try then
          Screen.Put_Try (History(Curr_Status).Try_No, Screen.Can_Try);
        else
          Screen.Put_Try (History(Curr_Status).Try_No, Screen.Cannot_Try);
        end if;
      end;
    else
      Free_State := Common.Get_Propal_State (First_Free);
    end if;

    -- Build color rec and update screen
    for I in Common.Level_Range
     range Common.Level_Range'First .. Level loop
      Color.Color(I) := Free_State.Propal_Color(I);
    end loop;

    -- Respond
    Resp := Response.Respond (Color);
    Screen.Put_Answer (
     Propal => First_Free,
     Placed_Ok => Resp.Placed_Ok,
     Colors_Ok => Resp.Colors_Ok);

    -- Answered
    Common.Set_Try_State (First_Free, Common.Answered);
    Common.Set_Answer (First_Free, Resp.Placed_Ok, Resp.Colors_Ok);

    -- Check end of game
    if First_Free = Common.Max_Number_Propal or else
       Resp.Placed_Ok = Natural (Level) then
      Playing := False;
      Screen.Put_Start_Giveup (Start => True, Selected => False);
      Put_Secret;
    else
      First_Free := Common.Propal_Range'Succ(First_Free);
    end if;
  end Answer;

  Propal : Common.Propal_State_Rec(Level);
  Valid : Boolean;

  use type Ada.Calendar.Time,
           Common.Propal_Range, Common.Color_Range, Common.Level_Range,
           Common.Try_List,
           Screen.Selection_List, Screen.Selection_Rec;

begin
  -- Valid if release in same item as clicked
  case History(Prev_Status).Selection_Kind is
    when Screen.Menu | Screen.Exit_Game =>
      Valid := History(Curr_Status).Selection_Kind
             = History(Prev_Status).Selection_Kind;
    when Screen.Level =>
      Valid := History(Curr_Status).Selection_Kind = Screen.Level
      and then History(Curr_Status).Level_No = History(Prev_Status).Level_No;
    when Screen.Try =>
      Valid := History(Curr_Status).Selection_Kind = Screen.Try
      and then History(Curr_Status).Try_No = History(Prev_Status).Try_No;
    when Screen.Color =>
      Valid := History(Curr_Status).Selection_Kind = Screen.Color
      and then History(Curr_Status).Color_No = History(Prev_Status).Color_No;
    when Screen.Propal =>
      Valid := History(Curr_Status).Selection_Kind = Screen.Propal
      and then History(Curr_Status).Propal_No = History(Prev_Status).Propal_No
      and then History(Curr_Status).Column_No = History(Prev_Status).Column_No;
    when Screen.Nothing =>
      Valid := False;
  end case;

  -- Cancel click except when selecting a origin color (in colors or propal)
  case History(Prev_Status).Selection_Kind is
    when Screen.Menu =>
      Screen.Put_Start_Giveup (Start => not Playing, Selected => False);
    when Screen.Level =>
      Screen.Put_Level (History(Prev_Status).Level_No, Selected => False);
    when Screen.Exit_Game =>
      Screen.Put_Exit (Selected => False);
    when Screen.Try =>
      Screen.Put_Try (Propal => History(Prev_Status).Try_No,
                      Try_State => Screen.Can_Try);
    when Screen.Color =>
      if not Valid then
        Screen.Put_Selected_Color (Color => History(Prev_Status).Color_No,
                                   Selected => False);
      end if;
    when Screen.Propal =>
      if not Valid then
        Screen.Put_Default_Pos (History(Prev_Status).Propal_No,
                                History(Prev_Status).Column_No,
                                Show => False);
      end if;
    when Screen.Nothing =>
      -- Discard invalid click
      Go_On := True;
      -- Keep selection
      Color_Move := Curr_Status = Release_Dest;
      return;
  end case;

  -- Valid or not: Cancel selected origin color in colors or propal
  if Curr_Status = Release_Dest then
    if History(Release_Orig).Selection_Kind = Screen.Color then
      Screen.Put_Selected_Color (Color => History(Release_Orig).Color_No,
                                 Selected => False);
    elsif History(Release_Orig).Selection_Kind = Screen.Propal then
      Screen.Put_Default_Pos (History(Release_Orig).Propal_No,
                              History(Release_Orig).Column_No,
                              Show => False);
    end if;
  end if;

  -- Invalid release
  if not Valid then
    -- Discard
    if History(Prev_Status).Selection_Kind = Screen.Color then
      Screen.Put_Selected_Color (Color => History(Prev_Status).Color_No,
                                 Selected => False);
    elsif History(Prev_Status).Selection_Kind = Screen.Propal then
      Screen.Put_Default_Pos (History(Prev_Status).Propal_No,
                              History(Prev_Status).Column_No,
                              Show => False);
    end if;
    Go_On := True;
    Color_Move := False;
    return;
  end if;

  -- Treat release
  case History(Curr_Status).Selection_Kind is
    when Screen.Exit_Game =>
      Go_On := False;
      Exit_Game := True;

    when Screen.Level =>
      Common.Store_Level (History(Curr_Status).Level_No);
      Screen.Put_Current_Level (History(Curr_Status).Level_No);
      Go_On := True;
      Color_Move := False;

    when Screen.Menu =>
      if Playing then
        -- Give up
        Put_Secret;
        Screen.Put_Start_Giveup (Start => True, Selected => False);
        Go_On := True;
        Color_Move := False;
      else
        -- Restart
        Common.Set_Level_To_Stored;
        Screen.Put_Start_Giveup (Start => False, Selected => False);
        Go_On := False;
        Exit_Game := False;
      end if;
      Playing := not Playing;

    when Screen.Try =>
      if History(Prev_Status).Try_No = History(Curr_Status).Try_No then
        -- Valid try (already tested on click)
        if Common.Get_Propal_State (History(Curr_Status).Try_No).Try
           = Common.Can_Try then
          Answer;
        elsif Ada.Calendar.Clock - Release_Orig_Date
                     <= Double_Click_Delay then
          -- Copy previous propal
          Propal := Common.Get_Propal_State (History(Curr_Status).Try_No - 1);
          Common.Set_Propal_State (History(Curr_Status).Try_No, Propal);
          for I in 1 .. Level loop
            Screen.Put_Color (History(Curr_Status).Try_No, I,
                              Propal.Propal_Color(I));
          end loop;
          Common.Set_Try_State (History(Curr_Status).Try_No, Common.Can_Try);
          Screen.Put_Try (History(Curr_Status).Try_No, Screen.Can_Try);
        end if;
      end if;
      Go_On := True;
      Color_Move := False;

    when Screen.Color =>
      -- Move color if origin or if a different color
      --  otherwise it is a unselect
      Go_On := True;
      if Curr_Status = Release_Orig then
        Color_Move := True;
      elsif History(Release_Orig).Selection_Kind = Screen.Color then
        -- Selecting another color or unselecting the color
        Screen.Put_Selected_Color (Color => History(Release_Orig).Color_No,
                                   Selected => False);
        Color_Move := History(Curr_Status).Color_No
                    /= History(Release_Orig).Color_No;
        if Color_Move then
          -- In fact, we are setting a new origin
          History(Release_Orig) := History(Curr_Status);
         end if;
      else
        -- Selecting a color instead of a propal
        Screen.Put_Default_Pos (History(Release_Orig).Propal_No,
                                History(Release_Orig).Column_No,
                                Show => False);
        History(Release_Orig) := History(Curr_Status);
        Color_Move := True;
      end if;

    when Screen.Propal =>
      Go_On := True;
      if Curr_Status = Release_Orig then
        -- Selecting a origin color in the propal
        Color_Move := True;
      elsif History(Release_Orig).Selection_Kind = Screen.Color then
        if Common.Is_Answered (History(Curr_Status).Propal_No) then
          -- Selecting an anwsered propal instead of a color
          Screen.Put_Selected_Color (Color => History(Release_Orig).Color_No,
                                     Selected => False);
          History(Release_Orig) := History(Curr_Status);
          Color_Move := True;
        else
          -- Moving from color to propoal
          Common.Set_Color (Propal => History(Curr_Status).Propal_No,
                            Level  => History(Curr_Status).Column_No,
                            Color  => History(Release_Orig).Color_No);
          Screen.Put_Color(Propal => History(Curr_Status).Propal_No,
                           Level  => History(Curr_Status).Column_No,
                           Color  => History(Release_Orig).Color_No);
          Screen.Put_Default_Pos (History(Curr_Status).Propal_No,
                                  History(Curr_Status).Column_No,
                                  Show => False);
          Update_Try (History(Curr_Status).Propal_No);
          Color_Move := False;
        end if;
      else
        -- Moving color within propal
        declare
          -- Move what, where
          Orig_State : constant Common.Propal_State_Rec
                     := Common.Get_Propal_State (
                          History(Release_Orig).Propal_No);
          Moved_Color : constant Common.Eff_Color_Range
                      := Orig_State.Propal_Color(
                          History(Release_Orig).Column_No);
        begin
          if History(Curr_Status) = History(Release_Orig) then
            -- Same propal cell => unselect.
            -- Clear if not answered and double click
            Screen.Put_Default_Pos (History(Curr_Status).Propal_No,
                                    History(Curr_Status).Column_No,
                                    Show => False);
            if not Common.Is_Answered (History(Curr_Status).Propal_No)
            and then Ada.Calendar.Clock - Release_Orig_Date
                     <= Double_Click_Delay then
              -- Clear
              Common.Set_Color (History(Curr_Status).Propal_No,
                                History(Curr_Status).Column_No,
                                Common.No_Color);
              Screen.Put_Color(History(Curr_Status).Propal_No,
                               History(Curr_Status).Column_No,
                               Common.No_Color);
              Update_Try (History(Curr_Status).Propal_No);
            end if;
            Color_Move := False;
          elsif Common.Is_Answered (History(Curr_Status).Propal_No) then
            -- Dest is answered: replace selection
            Screen.Put_Default_Pos (History(Release_Orig).Propal_No,
                                    History(Release_Orig).Column_No,
                                    Show => False);
            -- Dest may be adjacent with orig => Redraw
            Screen.Put_Default_Pos (History(Curr_Status).Propal_No,
                                    History(Curr_Status).Column_No,
                                    Show => True);
            History(Release_Orig) := History(Curr_Status);
            Color_Move := True;
          else
            -- Dest is free: copy if source is answered, otherwise move color
            if not Common.Is_Answered (History(Release_Orig).Propal_No) then
              -- Move
              Common.Set_Color (History(Release_Orig).Propal_No,
                                History(Release_Orig).Column_No,
                                Common.No_Color);
              Screen.Put_Color(History(Release_Orig).Propal_No,
                               History(Release_Orig).Column_No,
                               Common.No_Color);
              Update_Try (History(Release_Orig).Propal_No);
            end if;
            Screen.Put_Default_Pos (History(Curr_Status).Propal_No,
                                    History(Curr_Status).Column_No,
                                    Show => False);
            Common.Set_Color (History(Curr_Status).Propal_No,
                              History(Curr_Status).Column_No,
                              Moved_Color);
            Screen.Put_Color (History(Curr_Status).Propal_No,
                              History(Curr_Status).Column_No,
                              Moved_Color);
            Update_Try (History(Curr_Status).Propal_No);
            Color_Move := False;
          end if;
        end;
      end if;

    when Screen.Nothing =>
      Go_On := True;
      Color_Move := False;
  end case;

  -- Valid release => Store release orig date
  if Curr_Status = Release_Orig then
    Release_Orig_Date := Ada.Calendar.Clock;
  end if;

end Treat_Release;

