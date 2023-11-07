with Con_Io, Trace.Loggers;
with Common, Screen, Response;
package body Action is

  -- Trace logger
  Logger : Trace.Loggers.Logger;

  -- Current level
  Level : Common.Last_Level_Range;

  -- True as long as not end of game
  Playing : Boolean;

  -- Do we show the codes
  Show_Codes : Boolean;

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
  Double_Click : Boolean;

  -- False as long as no move has been done
  Moved : Boolean;

  -- The first row not answered (for next propal and answer)
  First_Free : Common.Propal_Range;

  -- Handle the clock
  package Clock is
    -- Reset  and restart the clock
    procedure Start;
    -- Stop (freeze)
    procedure Stop;
    -- Reset
    procedure Reset;
    -- Timer expiration (show current clock)
    procedure Expire;
  end Clock;
  package body Clock is separate;

  -- Init the game
  procedure Init (Show_Codes : in Boolean) is
  begin
    Screen.Init;
    Level := Common.Get_Level;
    Action.Show_Codes := Show_Codes;
    Logger.Init ("Action");
  end Init;

  -- Terminate the game
  procedure End_Action is
  begin
    Screen.Clear;
  end End_Action;

  -- Check and update wether current propal is complete and can be tried
  --  (not already answered)
  procedure Update_Try (Prop_No : in Common.Propal_Range) is
    Prop_State : constant Common.Propal_State_Rec
               := Common.Get_Propal_State (Prop_No);
    Tmp_State : Common.Propal_State_Rec(Level);
    procedure Set_No is
    begin
      Common.Set_Try_State (Prop_No, Common.Not_Set);
      Screen.Put_Try (Prop_No, Screen.Cannot_Try, False);
    end Set_No;
    use type Common.Propal_Range, Common.Try_List, Common.Color_Range,
             Common.Propal_Color_Array;

  begin
    -- If not complete => Cannot try
    for I in Common.Level_Range
             range Common.Level_Range'First .. Level loop
      if Prop_State.Propal_Color(I) = 0 then
        Set_No;
        return;
      end if;
    end loop;
    -- Compare to answered
    for I in 1 .. Prop_No - 1 loop
      Tmp_State := Common.Get_Propal_State (I);
      if Tmp_State.Try = Common.Answered then
        if Tmp_State.Propal_Color = Prop_State.Propal_Color then
          -- This answer exists
          Set_No;
          return;
        end if;
      else
        -- No more answer
        exit;
      end if;
    end loop;
    -- This propal is new
    Common.Set_Try_State (Prop_No, Common.Can_Try);
    Screen.Put_Try (Prop_No, Screen.Can_Try, False);
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

    -- Put secret
    procedure Put_Secret is
      Code : Response.Color_Rec(Level);
    begin
      Code := Response.Get_Code;
      for J in 1 .. Level loop
        Screen.Put_Secret_Color(J, Code.Color(J));
      end loop;
    end Put_Secret;

    -- Redraw
    procedure Handle_Refresh is
      Propal : Common.Propal_State_Rec(Level);
      Placed_Ok, Colors_Ok : Natural;
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
          Screen.Put_Try (I, Screen.Can_Try, False);
        elsif Propal.Try = Common.Answered then
          Common.Get_Answer (I, Placed_Ok, Colors_Ok);
          Screen.Put_Answer (I, Placed_Ok, Colors_Ok, False);
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
        if Show_Codes then
          Put_Secret;
        end if;
      else
        -- Not playing
        Put_Secret;
        Screen.Put_Start_Giveup (Start => True, Selected => False);
        Screen.Put_Current_Level (Common.Get_Stored_Level);
      end if;
      Update_Help;
    end Handle_Refresh;

    -- Result of release
    Go_On, Exit_Game, Color_Move : Boolean;
    Scr : constant Con_Io.Window := Con_Io.Get_Screen (Screen.Console'Access);

  begin

    -- Init state - playing
    Level := Common.Get_Level;
    Common.Reset_State;
    First_Free := Common.Propal_Range'First;
    Playing := True;
    Curr_Status := Default_Status;
    Prev_Status := Default_Status;
    Moved := False;

    -- Init screen
    Screen.Init (True, Level);
    Screen.Put_Start_Giveup (Start => False, Selected => False);
    Update_Help;
    Screen.Put_Current_Level (Level);
    Clock.Reset;
    Clock.Expire;
    if Show_Codes then
      Put_Secret;
    end if;

    -- Start new game - playing
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
            if Mouse_Status.Valid
            and then Mouse_Status.Button = Con_Io.Left then
              -- Exit on new event
              exit Wait_Event when Clicked
                 xor Mouse_Status.Status = Con_Io.Pressed;
            end if;
          elsif Stat = Con_Io.Timer_Event then
            Clock.Expire;
          elsif Stat = Con_Io.Refresh then
            Handle_Refresh;
            Clock.Expire;
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
        if Clicked then
          Double_Click := Mouse_Status.Double_Click;
          if Double_Click then
            Logger.Log_Debug ("Double click");
          end if;
        end if;
      end;

      -- Handle click or release
      if Clicked then
        Logger.Log_Debug ("Click");
        Treat_Click;
        Go_On := True;
        Prev_Status := Curr_Status;
        Curr_Status := Status_List'Succ (Curr_Status);
      else
        Logger.Log_Debug ("Release");
        Treat_Release (Go_On, Exit_Game, Color_Move);
        Prev_Status := Curr_Status;
        Curr_Status := (if Color_Move then Click_Dest else Default_Status);
        Double_Click := False;
      end if;
      Update_Help;
      exit Main when not Go_On;

    end loop Main;

    return not Exit_Game;
  end Play;

end Action;

