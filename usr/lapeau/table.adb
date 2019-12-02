with X_Mng;
package body Table is

  -- Static information about the console
  Font_Height : Natural;
  Last_Col : Con_Io.Col_Range;
  Last_Row : constant Con_Io.Row_Range := 51;
  Background : constant Con_Io.Colors :=  Con_Io.Color03;

  -- Done stacks
  Done_X_Offset : constant := 5;
  Done_X_Gap : constant Con_Io.X_Range := 31;
  Y_Gap_Done : constant Con_Io.Y_Range := 4;
  Done_X : Con_Io.X_Range ;
  Done_Y : constant Con_Io.Y_Range := Y_Gap_Done;

  -- Stacks and cards positions
  Menu_Row : constant Con_Io.Row_Range := 1;
  X_Gap : constant Con_Io.X_Range := 4;
  Nb_Y_Top : constant := 6;
  Y_Gap_Top : constant Con_Io.Y_Range := 31;
  Y_Gap_Bot : constant Con_Io.Y_Range := 25;
  Stack_X : Con_Io.X_Range;
  Stack_Y : constant Con_Io.Y_Range := Deck.Height + Y_Gap_Done * 2;

  -- Dummy window for blind Get
  Get_Window : Con_Io.Window;

  -- Window for the menu
  Menu_Window : Con_Io.Window;
  Menu_Back : constant Con_Io.Colors :=  Con_Io.Color_Of ("Light_Blue");
  Menu_Fore : constant Con_Io.Colors :=  Con_Io.Color_Of ("Black");

  -- Put the menu
  Start_Exit    : constant :=  1; Stop_Exit    : constant :=  6;
  Start_New     : constant :=  8; Stop_New     : constant := 12;
  Start_Restart : constant := 14; Stop_Restart : constant := 22;
  Start_Undo    : constant := 24; Stop_Undo    : constant := 29;
  Start_Redo    : constant := 31; Stop_Redo    : constant := 36;
  procedure Put_Menu is
  begin
    Menu_Window.Move (0, Start_Exit);
    Menu_Window.Put (" Exit ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_New);
    Menu_Window.Put (" New ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, Start_Restart);
    Menu_Window.Put (" Restart ", Menu_Fore, Menu_Back, False);
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
    -- Create Console at proper size
    Con_Io.Initialise;
    declare
      Font_No : constant := 0;
      Font_Width, Font_Offset : Natural;
    begin
      Con_Io.Get_Font_Geometry (Font_No, Font_Width, Font_Height, Font_Offset);
      Last_Col := ((Deck.Width + X_Gap) * Cards.Stack_Range'Last + X_Gap)
                  / Font_Width;
      Console.Open (Font_No, Last_Row, Last_Col, Def_Back => Background);
    end;
    Console.Set_Name ("La peau");
    Console.Set_Y_Mode (Con_Io.X_Mng_Mode);
    Cards.Init (Console.Get_Line);

    -- Compute offset of stacks and done stacks and put them
    Stack_X := (Console.X_Max - Stack_Range'Last * (Deck.Width + X_Gap) + X_Gap)
               / 2;
    Done_X := Stack_Of (Done_X_Offset, 1).X;
    for I in Stack_Range loop
      Cards.The_Xstacks(I).Move (Stack_Of (I, 1));
    end loop;
    for I in Cards.Deck.Suit_List loop
      Cards.The_Xdones(I).Move (Done_Of (I));
    end loop;

    -- Create a dummy window for blind get
    Get_Window.Open (Console'Unchecked_Access, (0, 0), (0, 0));
    Get_Window.Set_Foreground (Background);
    Get_Window.Set_Background (Background);

    -- Put menu
    Menu_Window.Open (Console'Unchecked_Access,
                      (Menu_Row, 0), (Menu_Row, Last_Col));
    Put_Menu;
  end Init;

  -- Position (X, Y) of card within a stack
  function Stack_Of (Stack : Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec is
    Y : Con_Io.Y_Range;
  begin
    if Depth <= Nb_Y_Top then
      Y := Depth * Y_Gap_Top;
    else
      Y := Nb_Y_Top * Y_Gap_Top + (Depth - Nb_Y_Top) * Y_Gap_Bot;
    end if;
    return (X => Stack_X + (Stack - 1) * (Deck.Width + X_Gap),
            Y => Stack_Y + Y);
  end Stack_Of;

  -- Position (X, Y) of card within a done stack
  function Done_Of (Suit : Deck.Suit_List) return Deck.Position_Rec is
  begin
    return (X => Done_X + Deck.Suit_List'Pos (Suit) * (Deck.Width + Done_X_Gap),
            Y => Done_Y);
  end Done_Of;


  -- Local: Decode a card event
  function Decode_Card_Event (Mouse_Event : Con_Io.Mouse_Event_Rec;
                              Event : out Event_Rec) return Boolean is
    Acc : Cards.Card_Access;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List,
             Cards.Card_Access;
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
        if Mouse_Event.Button /= Con_Io.Left then
          return False;
        end if;
        Event := (Pressed, Acc);
        return True;
      when Con_Io.Released =>
        if Mouse_Event.Button /= Con_Io.Left then
          return False;
        end if;
        Event := (Released, Acc);
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
  subtype Menu_List is Event_List range Leave .. Redo;
  Last_Pressed : Menu_List := Leave;
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

  -- Decode an event, on card, on menu or Break
  procedure Next_Event (Event : out Event_Rec) is
    -- For the blind get
    Str : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Insert : Boolean;
    -- The mouse event
    Mouse_Event : Con_Io.Mouse_Event_Rec;
    use type Con_Io.Curs_Mvt, X_Mng.External_Reference;
  begin
    loop
      Get_Window.Get (Str, Last, Stat, Pos, Insert);
      case Stat is
        when Con_Io.Break =>
          Event := (Kind => Quit);
          return;
        when Con_Io.Mouse_Button =>
          Console.Get_Mouse_Event (Mouse_Event, Con_Io.X_Y);
          if Mouse_Event.Xref /= X_Mng.Null_Reference then
            if Decode_Card_Event (Mouse_Event, Event) then
              -- valid card event
              return;
            end if;
          elsif Decode_Menu_Event (Mouse_Event, Event) then
            -- valid menu event
            return;
          end if;
        when Con_Io.Refresh =>
          Put_Menu;
        when others =>
          null;
      end case;
    end loop;
  end Next_Event;

end Table;

