separate (Action)
procedure Treat_Release (Go_On, Exit_Game : out Boolean) is

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
    use type Common.Propal_Range, Common.Color_Range, Common.Try_List;
  begin
    if Cur_Selection.Try_No /= First_Free then
      declare
        Prop_State : Common.Propal_State_Rec;
      begin
        Free_State := Common.Get_Propal_State (Cur_Selection.Try_No);
        Prop_State := Common.Get_Propal_State (First_Free);
        -- Switch propals
        Common.Set_Propal_State (
         Propal => Cur_Selection.Try_No,
         State  => Prop_State);
        Common.Set_Propal_State (
         Propal => First_Free,
         State  => Free_State);
        -- Update screen of propals and try
        for I in Common.Level_Range
         range Common.Level_Range'First .. Level loop
          Screen.Put_Color (
           Propal => Cur_Selection.Try_No,
           Level => I,
           Color => Prop_State.Propal_Color(I) );
          Screen.Put_Color (
           Propal => First_Free,
           Level => I,
           Color => Free_State.Propal_Color(I) );
        end loop;
        -- Answered impossible because of Next_Free
        if Prop_State.Try = Common.Can_Try then
          Screen.Put_Try (Cur_Selection.Try_No, Screen.Can_Try);
        else
          Screen.Put_Try (Cur_Selection.Try_No, Screen.Cannot_Try);
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

  use type Common.Propal_Range, Common.Color_Range, Common.Try_List,
           Common.Level_Range,
           Screen.Selection_List;

begin
  -- Cancel click
  case Last_Click.Selection_Kind is
    when Screen.Menu =>
      if Playing then
        Screen.Put_Start_Giveup (Start => False, Selected => False);
        -- Debug
        -- if Cur_Selection.Selection_Kind = Screen.Exit_Game then
        --   Put_Secret;
        -- end if;
      else
        Screen.Put_Start_Giveup (Start => True, Selected => False);
      end if;
    when Screen.Level =>
      Screen.Put_Level (Last_Click.Level_No, Selected => False);
    when Screen.Exit_Game =>
      Screen.Put_Exit (Selected => False);
    when Screen.Try =>
      Screen.Put_Try (Propal => Last_Click.Try_No,
                      Try_State => Screen.Can_Try);
    when Screen.Color =>
      Screen.Put_Selected_Color (Color => Last_Click.Color_No,
                                 Selected => False);
      Screen.Set_Mouse_Default_Color;
    when Screen.Propal =>
      Screen.Put_Default_Pos (Last_Click.Propal_No,
                              Last_Click.Column_No,
                              Show => False);
      Screen.Set_Mouse_Default_Color;
    when others=>
      null;
  end case;

  -- Treat release
  if Last_Click.Selection_Kind = Cur_Selection.Selection_Kind then
    case Cur_Selection.Selection_Kind is
      when Screen.Exit_Game =>
        Go_On := False;
        Exit_Game := True;

      when Screen.Level =>
        Common.Store_Level (Cur_Selection.Level_No);
        Screen.Put_Current_Level (Cur_Selection.Level_No);
        Go_On := True;
        Exit_Game := False;

      when Screen.Menu =>
        if Playing then
          -- Give up
          Put_Secret;
          Screen.Put_Start_Giveup (Start => True, Selected => False);
          Go_On := True;
          Exit_Game := False;
        else
          -- Restart
          Common.Set_Level_To_Stored;
          Screen.Put_Start_Giveup (Start => False, Selected => False);
          Go_On := False;
          Exit_Game := False;
        end if;
        Playing := not Playing;

      when Screen.Try =>
        if Last_Click.Try_No = Cur_Selection.Try_No then
          -- Valid try (already tested on click)
          Answer;
        else
          Screen.Console.Bell;
        end if;
        Go_On := True;
        Exit_Game := False;
      when Screen.Color =>
        Screen.Console.Bell;
        Go_On := True;
        Exit_Game := False;
      when Screen.Propal =>
        -- Move color in propal
        declare
          Prop_State : Common.Propal_State_Rec;
          Prev_State : Common.Propal_State_Rec;
          Moved_Color : Common.Eff_Color_Range;
        begin
          Prop_State := Common.Get_Propal_State (Cur_Selection.Propal_No);
          Prev_State := Common.Get_Propal_State (Last_Click.Propal_No);
          Moved_Color := Prev_State.Propal_Color(Last_Click.Column_No);
          if Prop_State.Propal_Color(Cur_Selection.Column_No) =
           Common.Color_Range'First then
            -- Dest is free : move color
            Common.Set_Color (Last_Click.Propal_No,
                              Last_Click.Column_No,
                              Common.Color_Range'First);
            Screen.Put_Color(Last_Click.Propal_No,
                             Last_Click.Column_No,
                             Common.Color_Range'First);
            Update_Try (Last_Click.Propal_No);

            Common.Set_Color (Cur_Selection.Propal_No,
                              Cur_Selection.Column_No,
                              Moved_Color);
            Screen.Put_Color (Cur_Selection.Propal_No,
                              Cur_Selection.Column_No,
                              Moved_Color);
            Update_Try (Cur_Selection.Propal_No);
          else
            -- Dest is used
            if Last_Click.Propal_No /= Cur_Selection.Propal_No
            and then Last_Click.Column_No /= Cur_Selection.Column_No then
              Screen.Console.Bell;
            end if;
          end if;
        end;
        Go_On := True;
        Exit_Game := False;
      when Screen.Nothing =>
        Go_On := True;
        Exit_Game := False;
    end case;

  elsif Last_Click.Selection_Kind = Screen.Color and then
        Cur_Selection.Selection_Kind = Screen.Propal then
    -- Check that propal is valid
    if Cur_Selection.Propal_No >= First_Free then
      Common.Set_Color (Propal => Cur_Selection.Propal_No,
                        Level  => Cur_Selection.Column_No,
                        Color  => Last_Click.Color_No);
      Screen.Put_Color(Propal => Cur_Selection.Propal_No,
                       Level  => Cur_Selection.Column_No,
                       Color  => Last_Click.Color_No);
      Update_Try (Cur_Selection.Propal_No);

    else
      Screen.Console.Bell;
    end if;
    Go_On := True;
    Exit_Game := False;

  elsif Last_Click.Selection_Kind = Screen.Propal then
    if Cur_Selection.Selection_Kind = Screen.Nothing and then
       Cur_Selection.Selection = Screen.Propal then
      -- Restore color
      Screen.Console.Bell;
    else
      -- Remove a color from propal (already cleared)
      Common.Set_Color (Last_Click.Propal_No,
                        Last_Click.Column_No,
                        Common.Color_Range'First);
      Screen.Put_Color (Last_Click.Propal_No,
                        Last_Click.Column_No,
                        Common.Color_Range'First);
      Update_Try (Last_Click.Propal_No);
    end if;
    Go_On := True;
    Exit_Game := False;
  else
    if Last_Click.Selection_Kind /= Screen.Nothing then
      Screen.Console.Bell;
    end if;
    Go_On := True;
    Exit_Game := False;
  end if;

  if Playing then
    declare
      Can_Try, Can_Propose : Boolean;
    begin
      Common.Possible_Selections (Can_Try, Can_Propose);
      Screen.Put_Help (Screen.Released, Can_Try, Can_Propose);
    end;
  else
    Screen.Put_Help (Screen.Start);
  end if;
end Treat_Release;

