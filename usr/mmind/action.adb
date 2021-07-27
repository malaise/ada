with Ada.Calendar;
with Rnd, Con_Io;
with Common, Screen, Response;
package body Action is

  -- Current level
  Level : Common.Last_Level_Range;

  -- True as long as not end of game
  Playing : Boolean;

  -- Current and previous status
  type Status_List is (Click_Orig, Release_Orig,
                       Click_Dest, Release_Dest);
  Default_Status : constant Status_List := Click_Orig;

  Curr_Status, Prev_Status : Status_List;
  Discard : constant Screen.Selection_Rec
          := (Selection_Kind => Screen.Nothing,
              Selection => Screen.Nothing);
  History : array (Status_List) of Screen.Selection_Rec
          := (others => Discard);
  Release_Orig_Date : Ada.Calendar.Time;
  Double_Click_Delay : constant Duration := 0.500;

  -- The first row not answered (for next propal and answer)
  First_Free : Common.Propal_Range;

  -- Init the game
  procedure Init is
  begin
    Screen.Init;
    Rnd.Gen.Randomize;
    Level := Common.Get_Level;
  end Init;

  -- Terminate the game
  procedure End_Action is
  begin
    null;
  end End_Action;

  -- Criteria for a Try area to allow copy from previous propal:
  -- Empty and first Unanswered
  function Can_Copy_Propal (Propal : in Common.Propal_Range) return Boolean is
    State : constant Common.Propal_State_Rec
          := Common.Get_Propal_State (Propal);
    No_Colors : constant Common.Propal_Color_Array(1 .. Level)
              := (others => Common.No_Color);
    use type Common.Try_List, Common.Propal_Range, Common.Propal_Color_Array;
  begin
    return   Propal /= Common.Propal_Range'First
    and then State.Propal_Color = No_Colors
    and then Common.Get_Propal_State (Propal - 1).Try = Common.Answered;
  end Can_Copy_Propal;

  -- Check and update wether current propal is complete and can be tried
  procedure Update_Try (Propal : in Common.Propal_Range) is
    Prop_State : constant Common.Propal_State_Rec
               := Common.Get_Propal_State (Propal);
    use type Common.Color_Range;
  begin
    for I in Common.Level_Range
     range Common.Level_Range'First .. Level loop
      if Prop_State.Propal_Color(I) = 0 then
        Common.Set_Try_State (Propal, Common.Not_Set);
        Screen.Put_Try (Propal, Screen.Cannot_Try);
        return;
      end if;
    end loop;
    Common.Set_Try_State (Propal, Common.Can_Try);
    Screen.Put_Try (Propal, Screen.Can_Try);
  end Update_Try;

  -- Put accurate help
  procedure Update_Help is
    use type Screen.Selection_List;
  begin
    if Curr_Status = Release_Orig or else Curr_Status = Release_Dest then
      -- Help after Click: Invalid or "Release in same"
      if History(Prev_Status).Selection_Kind = Screen.Nothing then
        Screen.Put_Help (Screen.Invalid);
      else
        Screen.Put_Help (Screen.Clicked);
      end if;
      return;
    end if;

    if Playing then
      if Curr_Status = Click_Orig  then
        Screen.Put_Help (Screen.Play);
      elsif History(Prev_Status).Selection_Kind = Screen.Color then
        Screen.Put_Help (Screen.Released_Color);
      elsif History(Prev_Status).Selection_Kind = Screen.Propal then
        Screen.Put_Help (Screen.Released_Propal,
            Can_Clear => not Common.Is_Answered (
                              History(Prev_Status).Propal_No));
      end if;
    else
      Screen.Put_Help (Screen.Stopped);
    end if;
  end Update_Help;

  -- Handle a click or a release
  procedure Treat_Click is separate;
  -- End of game?
  --  If no: Exit or not (restart, new level...)
  --  If yes: Step or remain on color moving
  procedure Treat_Release (Go_On, Exit_Game, Color_Move : out Boolean)
            is separate;

  -- True if start again, False if exit
  function Play return Boolean is

    Clicked : Boolean := False;

    -- Redraw
    procedure Handle_Refresh is
      Propal : Common.Propal_State_Rec(Level);
      Placed_Ok, Colors_Ok : Natural;
      Code : Response.Color_Rec(Level);
      use type Common.Try_List, Screen.Selection_List;
    begin
      Screen.Init (False, Level);
      -- Put colors
      for I in Common.Propal_Range loop
        Propal := Common.Get_Propal_State(I);
        for J in 1 .. Level loop
          Screen.Put_Color (I, J, Propal.Propal_Color(J));
        end loop;
        if Propal.Try = Common.Can_Try then
          Screen.Put_Try (I, Screen.Can_Try);
        elsif Propal.Try = Common.Answered then
          Common.Get_Answer (I, Placed_Ok, Colors_Ok);
          Screen.Put_Answer (I, Placed_Ok, Colors_Ok);
        end if;
      end loop;
      if Playing then
        Screen.Put_Start_Giveup (Start => False, Selected => False);
        if Clicked then
          Treat_Click;
        end if;
        Screen.Put_Current_Level (Level);
        if Curr_Status = Click_Dest then
          if History(Release_Orig).Selection_Kind = Screen.Color then
            -- Selected color
            Screen.Put_Selected_Color (Color => History(Release_Orig).Color_No,
                                       Selected => True);
          else
            -- Selected propal
            Screen.Put_Default_Pos (History(Release_Orig).Propal_No,
                                    History(Release_Orig).Column_No,
                                    Show => True);
          end if;
        end if;
      else
        -- Not playing
        Code := Response.Get_Code;
        for J in 1 .. Level loop
          Screen.Put_Secret_Color(J, Code.Color(J));
        end loop;
        Screen.Put_Start_Giveup (Start => True, Selected => False);
        Screen.Put_Current_Level (Common.Get_Stored_Level);
      end if;
      Update_Help;
    end Handle_Refresh;

    -- Result of release
    Go_On, Exit_Game, Color_Move : Boolean;
    Scr : constant Con_Io.Window := Con_Io.Get_Screen (Screen.Console'Access);

  begin

    -- Start new game - playing
    Level := Common.Get_Level;
    Common.Reset_State;
    Curr_Status := Default_Status;
    Prev_Status := Default_Status;
    Screen.Init (True, Level);

    First_Free := Common.Propal_Range'First;
    Response.New_Code;

    Playing := True;
    Screen.Put_Start_Giveup (Start => False, Selected => False);
    Update_Help;
    Screen.Put_Current_Level (Level);

    Main:
    loop

      -- Loop until valid event
      declare
        Str : Con_Io.Unicode_Sequence (1 .. 0);
        Last : Natural;
        Stat : Con_Io.Curs_Mvt;
        Pos : Positive;
        Ins : Boolean;
        Mouse_Status : Con_Io.Mouse_Event_Rec;
        use type Con_Io.Curs_Mvt, Con_Io.Mouse_Button_List,
                 Con_Io.Mouse_Button_Status_List;
      begin
        Wait_Event:
        loop
          Scr.Get (Str, Last, Stat, Pos, Ins, Echo => False);
          if Stat = Con_Io.Mouse_Button then
            Screen.Console.Get_Mouse_Event (Mouse_Status);
            if Mouse_Status.Button = Con_Io.Left then
              -- Exit on new event
              exit Wait_Event when Clicked
                 xor Mouse_Status.Status = Con_Io.Pressed;
            end if;
          elsif Stat = Con_Io.Refresh then
            Handle_Refresh;
          elsif Stat = Con_Io.Break then
            Screen.Clear;
            return False;
          end if;
        end loop Wait_Event;
        Clicked := not Clicked;

        -- Store action
        History (Curr_Status) := Screen.Get_Selected (
           (Row => Mouse_Status.Row,
            Col => Mouse_Status.Col));
      end;

      -- Handle click or release
      if Clicked then
        Treat_Click;
        Go_On := True;
        Prev_Status := Curr_Status;
        Curr_Status := Status_List'Succ (Curr_Status);
      else
        Treat_Release (Go_On, Exit_Game, Color_Move);
        Prev_Status := Curr_Status;
        Curr_Status := (if Color_Move then Click_Dest else Default_Status);
      end if;
      Update_Help;
      exit Main when not Go_On;

    end loop Main;

    -- End?
    if Exit_Game then
      Screen.Clear;
    end if;

    return not Exit_Game;
  exception
    when others =>
      Scr.Move;
      return Exit_Game;
  end Play;

end Action;

