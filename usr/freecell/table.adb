with Ada.Calendar;
with X_Mng, Timers, Long_Long_Limited_Pool, Trace.Loggers, Images;
package body Table is
  -- Debug logger
  Logger : Trace.Loggers.Logger;

  -- FIFO Pool of pending events
  procedure Set (To : out Event_Rec; Val : in Event_Rec) is
  begin
    To := Val;
  end Set;
  package Event_Pool_Mng is new Long_Long_Limited_Pool (
      Data_Type => Event_Rec, Lifo => False, Set => Set);
  Event_Pool : Event_Pool_Mng.Pool_Type;

  -- Static information about the console
  Font_Height : Natural;
  Last_Col : Con_Io.Col_Range;
  Last_Row : constant Con_Io.Row_Range := 53;
  Background : constant Con_Io.Colors :=  Con_Io.Color03;

  -- Done and Tmp stacks
  X_Gap : constant Con_Io.X_Range := 4;
  Play_X : Con_Io.Y_Range;
  Y_Gap : constant Con_Io.X_Range := 10;
  Done_Tmp_Y : Con_Io.Y_Range;

  -- Play stacks and cards positions
  Nb_Y_Top : constant := 7;
  Y_Gap_Top : constant Con_Io.Y_Range := 31;
  Y_Gap_Bot : constant Con_Io.Y_Range := 25;
  Play_Y : Con_Io.Y_Range;

  -- Dummy window for blind Get
  Get_Window : Con_Io.Window;

  -- Window for the menu
  Menu_Window : Con_Io.Window;
  Menu_Back : constant Con_Io.Colors :=  Con_Io.Color_Of ("Light_Blue");
  Menu_Fore : constant Con_Io.Colors :=  Con_Io.Color_Of ("Black");
  Menu_Row : constant Con_Io.Row_Range := 1;

  -- Buttons of the menu
  Start_Exit    : constant :=  2; Stop_Exit    : constant :=  7;
  Start_New     : constant := 10; Stop_New     : constant := 15;
  Start_Restart : constant := 17; Stop_Restart : constant := 25;
  Start_Purge   : constant := 71; Stop_Purge   : constant := 77;
  Start_Undo    : constant := 80; Stop_Undo    : constant := 85;
  Start_Redo    : constant := 88; Stop_Redo    : constant := 93;

  -- Put the menu
  procedure Put_Menu is
  begin
    Menu_Window.Move (0, Start_Exit);
    Menu_Window.Put (" Exit ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_New);
    Menu_Window.Put (" New ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_Restart);
    Menu_Window.Put (" Restart ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_Purge);
    Menu_Window.Put (" Purge ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_Undo);
    Menu_Window.Put (" Undo ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_Redo);
    Menu_Window.Put (" Redo ", Menu_Fore, Menu_Back, False);
  end Put_Menu;

  -- Needs to be called only once, create the table, move the stacks, cards, menu...
  procedure Init is
  begin
    if Console.Is_Open then
      return;
    end if;
    Logger.Init ("Events");
    -- Create Console at proper size
    Con_Io.Initialise;
    declare
      Font_No : constant := 0;
      Font_Width, Font_Offset : Natural;
    begin
      Con_Io.Get_Font_Geometry (Font_No, Font_Width, Font_Height, Font_Offset);
      Last_Col := ((Deck.Width + X_Gap) * Cards.Play_Stack_Range'Last + X_Gap)
                  / Font_Width;
      Logger.Log_Debug ("Last col:" & Last_Col'Img);
      Console.Open (Font_No, Last_Row, Last_Col, Def_Back => Background);
    end;
    Console.Set_Y_Mode (Con_Io.X_Mng_Mode);
    Cards.Init (Console.Get_Line);

    -- Compute offset of play stacks and tmp and done stacks and put them
    Done_Tmp_Y := (Menu_Row + 1) * Font_Height + Y_Gap;
    Play_Y := Done_Tmp_Y + Cards.Deck.Height + Y_Gap;
    Play_X := (Console.X_Max - Cards.Play_Stack_Range'Last
                               * (Deck.Width + X_Gap)
               + X_Gap) / 2;
    for I in Cards.Play_Stack_Range loop
      Cards.The_Xplay(I).Move (Play_Of (I, 1));
      Cards.The_Xplay(I).Show (True);
      Cards.The_Xplay(I).Do_Raise;
    end loop;
    for I in Cards.Deck.Suit_List loop
      Cards.The_Xdone(I).Move (Done_Of (I));
      Cards.The_Xdone(I).Show (True);
      Cards.The_Xdone(I).Do_Raise;
    end loop;
    for I in Cards.Tmp_Stack_Range loop
      Cards.The_Xtmp(I).Move (Tmp_Of (I));
      Cards.The_Xtmp(I).Show (True);
      Cards.The_Xtmp(I).Do_Raise;
    end loop;

    -- Create a dummy window for blind get
    Get_Window.Open (Console'Unchecked_Access, (0, 0), (0, 0));
    Get_Window.Set_Foreground (Background);
    Get_Window.Set_Background (Background);

    -- Put menus
    Menu_Window.Open (Console'Unchecked_Access,
                      (Menu_Row, 0), (Menu_Row, Last_Col));
    Put_Menu;
  end Init;

  -- Set game num
  procedure Set_Game_Num (Num : in Memory.Game_Range) is
  begin
    Console.Set_Name ("Freecell " & "(" & Images.Integer_Image (Num) & ")");
  end Set_Game_Num;

  -- Position (X, Y) of card within a play stack
  function Play_Of (Play : Cards.Play_Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec is
    Y : Con_Io.Y_Range;
  begin
    if Depth <= Nb_Y_Top then
      Y := (Depth - 1) * Y_Gap_Top;
    else
      Y := (Nb_Y_Top - 1) * Y_Gap_Top + (Depth - Nb_Y_Top) * Y_Gap_Bot;
    end if;
    return (X => Play_X + (Play - 1) * (Deck.Width + X_Gap),
            Y => Play_Y + Y);
  end Play_Of;

  -- Position (X, Y) of card within a done stack
  function Done_Of (Suit : Deck.Suit_List) return Deck.Position_Rec is
  begin
    if Deck.Suit_List'Pos (Suit) < 2 then
      -- Red on left part of Tmp stacks
      return (X => Play_X + (Deck.Suit_List'Pos (Suit) + 0)
                            * (Deck.Width + X_Gap),
              Y => Done_Tmp_Y);
    else
      -- Black on right part of Tmp stacks
      return (X => Play_X + (Deck.Suit_List'Pos (Suit) + 4)
                            * (Deck.Width + X_Gap),
              Y => Done_Tmp_Y);
    end if;
  end Done_Of;

  -- Position (X, Y) of card within a tmp stack
  function Tmp_Of (Tmp : Cards.Tmp_Stack_Range) return Deck.Position_Rec is
  begin
    return (X => Play_X + (Cards.Tmp_Stack_Range'Pos (Tmp) + 1)
                          * (Deck.Width + X_Gap),
            Y => Done_Tmp_Y);
  end Tmp_Of;

  -- Is the pointer currently above a card
  --  Based only on positions (the card might be covered or hidden)
  function Is_Pointer_Above (Acard : Cards.Card_Access) return Boolean is
    Pos : constant Cards.Deck.Position_Rec := Acard.Xcard.Get_Position;
    X, Y : Integer;
  begin
    X_Mng.X_Get_Current_Pointer_Position (Console.Get_Line, X, Y);
    return   Pos.X <= X and then X <= Pos.X + Cards.Deck.Width - 1
    and then Pos.Y <= Y and then Y <= Pos.Y + Cards.Deck.Height;
  end Is_Pointer_Above;


  -- Local: Decode a card event
  function Decode_Card_Event (Mouse_Event : Con_Io.Mouse_Event_Rec;
                              Event : out Event_Rec) return Boolean is
    Acc : Cards.Card_Access;
    use type Con_Io.Mouse_Button_List, Cards.Card_Access;
  begin
    if not Mouse_Event.Valid then
      return False;
    end if;
    Acc := Cards.X_To_Card (Mouse_Event.Xref);
    if Acc = null then
      return False;
    end if;
    case Mouse_Event.Status is
      when Con_Io.Pressed =>
        if Mouse_Event.Button = Con_Io.Left then
          Event := (Left_Pressed, Acc);
        elsif Mouse_Event.Button = Con_Io.Right then
          Event := (Right_Pressed, Acc);
        else
          return False;
        end if;
        return True;
      when Con_Io.Released =>
        if Mouse_Event.Button = Con_Io.Left then
          Event := (Left_Released, Acc);
        elsif Mouse_Event.Button = Con_Io.Right then
          Event := (Right_Released, Acc);
        else
          return False;
        end if;
        return True;
      when Con_Io.Enter =>
        Event := (Enter, Acc);
        return True;
      when Con_Io.Leave =>
        Event := (Leave, Acc);
        return True;
      when others =>
        -- Release, motion?
        return False;
    end case;
  end Decode_Card_Event;

  -- Local: Decode a menu event
  -- Last pressed. Leave for none
  subtype Extended_Menu_List is Event_List range Leave .. Menu_Event_List'Last;
  Last_Pressed : Extended_Menu_List := Leave;
  function Decode_Menu_Event (Mouse_Event : in Con_Io.Mouse_Event_Rec;
                              Event : out Event_Rec) return Boolean is
    Square : Con_Io.Square;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List;
  begin
    Event := (Kind => Quit);
    if not Mouse_Event.Valid
    or else Mouse_Event.Button /= Con_Io.Left
    or else (Mouse_Event.Status /= Con_Io.Pressed
             and then Mouse_Event.Status /= Con_Io.Released) then
      return False;
    end if;
    -- Decode click
    Square := Console.To_Square (Mouse_Event.X, Mouse_Event.Y);
    Event := (Leave, null);
    if Square.Row = Menu_Row then
      case Square.Col is
        when Start_Exit    .. Stop_Exit    => Event := (Kind => Quit);
        when Start_New     .. Stop_New     => Event := (Kind => New_Game);
        when Start_Restart .. Stop_Restart => Event := (Kind => Restart);
        when Start_Purge   .. Stop_Purge   => Event := (Kind => Purge);
        when Start_Undo    .. Stop_Undo    => Event := (Kind => Undo);
        when Start_Redo    .. Stop_Redo    => Event := (Kind => Redo);
        when others                        => null;
      end case;
    end if;
    -- Flip / flop menu entry on press
    if Mouse_Event.Status = Con_Io.Pressed then
      case Event.Kind is
        when Leave =>
          -- Drop invalid press
          null;
        when Quit =>
          Menu_Window.Move (0, Start_Exit);
          Menu_Window.Put (" Exit ", Menu_Back, Menu_Fore, False);
        when New_Game =>
          Menu_Window.Move (0, Start_New);
          Menu_Window.Put (" New ", Menu_Back, Menu_Fore, False);
        when Restart =>
          Menu_Window.Move (0, Start_Restart);
          Menu_Window.Put (" Restart ", Menu_Back, Menu_Fore, False);
        when Purge =>
          Menu_Window.Move (0, Start_Purge);
          Menu_Window.Put (" Purge ", Menu_Back, Menu_Fore, False);
        when Undo =>
          Menu_Window.Move (0, Start_Undo);
          Menu_Window.Put (" Undo ", Menu_Back, Menu_Fore, False);
        when Redo =>
          Menu_Window.Move (0, Start_Redo);
          Menu_Window.Put (" Redo ", Menu_Back, Menu_Fore, False);
        when others =>
          null;
      end case;
      Last_Pressed := Event.Kind;
      return False;
    else
      Put_Menu;
      -- Validate on release in same entry, drop if release anywhere else
      return Event.Kind /= Leave and then Event.Kind = Last_Pressed;
    end if;
  end Decode_Menu_Event;

  -- Wait for an event (Dur < 0) or a delay
  function Wait (Dur : Duration) return Event_Rec is
    -- For blind get
    Str : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Insert : Boolean;
    -- Mouse event
    Mouse_Event : Con_Io.Mouse_Event_Rec;
    -- Expiration
    Expiration : Con_Io.Delay_Rec;
    -- Result
    Event : Event_Rec := (Kind => Quit);
    use type Ada.Calendar.Time, X_Mng.External_Reference;
  begin
    if Dur = Timers.Infinite_Seconds then
      -- Wait for an event
      Expiration := Con_Io.Infinite_Delay;
    else
      -- Wiat until the delay expiration, store intermediate events
      Expiration := (Delay_Kind => Timers.Delay_Exp,
                     Expiration_Time => Ada.Calendar.Clock + Dur,
                     others => <>);
    end if;

    loop
      if Dur = Timers.Infinite_Seconds and then not Event_Pool.Is_Empty then
        -- Event expected and present
        Event := Event_Pool.Pop;
        Logger.Log_Debug ("  Popping " & Event.Kind'Img &
            (if Event.Kind in Card_Event_List then
             " " & Event.Card.Image else "") );
        return Event;
      end if;
      Get_Window.Get (Str, Last, Stat, Pos, Insert, Time_Out => Expiration);
      case Stat is
        when Con_Io.Break =>
          Event := (Kind => Quit);
          Event_Pool.Push (Event);
        when Con_Io.Mouse_Button =>
          Console.Get_Mouse_Event (Mouse_Event, Con_Io.X_Y);
          if Mouse_Event.Xref /= X_Mng.Null_Reference then
            if Decode_Card_Event (Mouse_Event, Event) then
              -- Valid card event
              Logger.Log_Debug ("  Pushing " & Event.Kind'Img
                              & " " & Event.Card.Image);
              Event_Pool.Push (Event);
            end if;
          elsif Decode_Menu_Event (Mouse_Event, Event) then
            -- Valid menu event
            Logger.Log_Debug ("  Pushing " & Event.Kind'Img);
            Event_Pool.Push (Event);
          end if;
          Console.Get_Mouse_Event (Mouse_Event, Con_Io.X_Y);
        when Con_Io.Refresh =>
          Put_Menu;
        when Con_Io.Timeout =>
          return Event;
        when others =>
          null;
      end case;
    end loop;
  end Wait;

  -- Decode an event, on card, on menu or Break
  procedure Next_Event (Event : out Event_Rec) is
  begin
    Logger.Log_Debug ("Start next event");
    Event := Wait (Timers.Infinite_Seconds);
    Logger.Log_Debug ("Stop next event " & Event.Kind'Img);
  end Next_Event;

  -- Wait some milliseconds
  procedure Wait (Dur : in Duration) is
    Dummy_Event : Event_Rec;
  begin
    Logger.Log_Debug ("Start waiting");
    Dummy_Event := Wait (Dur);
    Logger.Log_Debug ("Stop waiting");
  end Wait;

end Table;
