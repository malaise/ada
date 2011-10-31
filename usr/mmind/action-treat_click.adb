separate (Action)
procedure Treat_Click is
begin

  if not Playing then

    case Cur_Selection.Selection_Kind is

      when Screen.Menu =>
        Screen.Put_Help (Screen.Click_Other);
        Screen.Put_Start_Giveup (Start => True, Selected => True);
        Last_Click := Cur_Selection;

      when Screen.Level =>
        Screen.Put_Help (Screen.Click_Other);
        Screen.Put_Level (Cur_Selection.Level_No,
                          Selected => True);
        Last_Click := Cur_Selection;

      when Screen.Exit_Game =>
        Screen.Put_Help (Screen.Click_Other);
        Screen.Put_Exit (Selected => True);
        Last_Click := Cur_Selection;

      when others =>
        Last_Click := (Selection_Kind => Screen.Nothing,
                       Selection => Screen.Nothing);
        Screen.Put_Help (Screen.Discarded);
        Screen.Console.Bell;
    end case;

  else

    case Cur_Selection.Selection_Kind is

      when Screen.Menu =>
        Screen.Put_Help (Screen.Click_Other);
        Screen.Put_Start_Giveup (Start => False, Selected => True);
        Last_Click := Cur_Selection;

      when Screen.Exit_Game =>
        Screen.Put_Help (Screen.Click_Other);
        Screen.Put_Exit (Selected => True);
        Last_Click := Cur_Selection;

      when Screen.Try =>
        declare
          Try_State : constant Common.Try_List
                    := Common.Get_Propal_State (Cur_Selection.Try_No).Try;
          use Common;
        begin
          -- Check that this propal is completed and not already answered
          if Try_State = Common.Can_Try then
            Screen.Put_Help (Screen.Click_Other);
            Screen.Put_Try (Propal => Cur_Selection.Try_No,
                            Try_State => Screen.Selected);
            Last_Click := Cur_Selection;
          else
            Screen.Put_Help (Screen.Discarded);
            Last_Click := (Selection_Kind => Screen.Nothing,
                           Selection => Screen.Try);
            Screen.Console.Bell;
          end if;
        end;

      when Screen.Color =>
        Screen.Put_Help (Screen.Click_Color);
        Screen.Put_Selected_Color (Color => Cur_Selection.Color_No,
                                   Selected => True);
        Screen.Set_Mouse_Color (Color => Cur_Selection.Color_No);
        Last_Click := Cur_Selection;

      when Screen.Propal =>
        declare
          Propal_State : constant Common.Propal_State_Rec
                       := Common.Get_Propal_State (Cur_Selection.Propal_No);
          use Common;
        begin
          -- Check that this propal is completed and not already answered
          if Propal_State.Try /= Common.Answered and then
             Propal_State.Propal_Color(Cur_Selection.Column_No)
              /= Common.Color_Range'First then
            Screen.Put_Help (Screen.Click_Propal);
            -- Attempt to move a color in propal. Clear square
            Screen.Put_Default_Pos (Cur_Selection.Propal_No,
                                    Cur_Selection.Column_No,
                                    Show => True);
            Screen.Set_Mouse_Color (
             Color => Propal_State.Propal_Color(Cur_Selection.Column_No));
            Last_Click := Cur_Selection;
          else
            Screen.Put_Help (Screen.Discarded);
            Last_Click := (Selection_Kind => Screen.Nothing,
                           Selection => Screen.Propal);
            Screen.Console.Bell;
          end if;
        end;

      when others =>
        Screen.Put_Help (Screen.Discarded);
        Last_Click := (Selection_Kind => Screen.Nothing,
                       Selection => Screen.Nothing);
        Screen.Console.Bell;

    end case;

  end if;


end Treat_Click;

