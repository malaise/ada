with As.U, Basic_Proc, Con_Io, X_Mng.Cards;
procedure T_Cards is
  -- Console
  Last_Col : Con_Io.Col_Range;
  Last_Row : constant Con_Io.Row_Range := 53;
  Console : aliased Con_Io.Console;
  Background : constant Con_Io.Colors :=  Con_Io.Color03;

  -- Dummy window for blind Get
  Get_Window : Con_Io.Window;
  Str : Con_Io.Unicode_Sequence (1 .. 0);
  Last : Natural;
  Stat : Con_Io.Curs_Mvt;
  Pos : Positive;
  Insert : Boolean;

  -- Window for the menu
  Menu_Window : Con_Io.Window;
  Menu_Back : constant Con_Io.Colors :=  Con_Io.Color_Of ("Light_Blue");
  Menu_Fore : constant Con_Io.Colors :=  Con_Io.Color_Of ("Black");

  -- Mouse event
  Mouse_Event : Con_Io.Mouse_Event_Rec;

  -- Cards
  package Deck is new X_Mng.Cards;
  The_Cards : array (Deck.Suit_List, Deck.Name_Range) of Deck.Card;
  The_Names : array (Deck.Suit_List, Deck.Name_Range) of As.U.Asu_Us;
  Got_Card : Deck.Card_Access;

  -- Put the menu
  procedure Put_Menu is
  begin
    Menu_Window.Move (0, 1);
    Menu_Window.Put (" Exit ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, 8);
    Menu_Window.Put (" New ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, 14);
    Menu_Window.Put (" Restart ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, 24);
    Menu_Window.Put (" Undo ", Menu_Fore, Menu_Back, False);
    Menu_Window.Move (0, 31);
    Menu_Window.Put (" Redo ", Menu_Fore, Menu_Back, False);
  end Put_Menu;

  use type X_Mng.External_Reference, Con_Io.Curs_Mvt;
begin

  -- Initialize with proper background color
  declare
  begin
    Con_Io.Initialise;
  end;

  -- Create Console at proper size
  declare
    Font_No : constant := 0;
    Font_Width, Font_Height, Font_Offset : Natural;
  begin
    Con_Io.Get_Font_Geometry (Font_No, Font_Width, Font_Height, Font_Offset);
    Last_Col := ((Deck.Width + 4) * 13 + 4) / Font_Width;
    Console.Open (Font_No, Last_Row, Last_Col, Def_Back => Background);
    Basic_Proc.Put_Line_Output ("Last col" & Last_Col'Img
        & "  font: " & Console.Font_Name
        & " size: " & Font_Width'Img & " x" & Font_Height'Img);
  end;
  Console.Enable_Motion_Events (True);
  Console.Set_Y_Mode (Con_Io.X_Mng_Mode);
  Deck.Set_Line (Console.Get_Line);

  -- Create a dummy window for blind get
  Get_Window.Open (Console'Unchecked_Access, (0, 0), (0, 0));
  Get_Window.Set_Foreground (Background);
  Get_Window.Set_Background (Background);

  -- Put menu
  Menu_Window.Open (Console'Unchecked_Access, (0, 0), (0, Last_Col));
  Put_Menu;

  -- Create the cards
  Basic_Proc.Put_Line_Output ("Creating the cards");
  for Suit in Deck.Suit_List loop
    for Name in Deck.Name_Range loop
      The_Names(Suit, Name).Set (Suit'Img & Name'Img);
      The_Cards(Suit, Name).Create_Card (Suit, Name);
    end loop;
  end loop;

  -- Display some cards
  Basic_Proc.Put_Line_Output ("Mapping the cards");
  The_Cards(Deck.Heart, 1).Move ((5, 15));
  The_Cards(Deck.Heart, 1).Show (True);
  The_Cards(Deck.Spade, 13).Move ((100, 15));
  The_Cards(Deck.Spade, 13).Show (True);

  -- Main loop
  Basic_Proc.Put_Line_Output ("Entering main loop");
  loop
    Get_Window.Get (Str, Last, Stat, Pos, Insert);
    exit when Stat = Con_Io.Break;
    if Stat = Con_Io.Mouse_Button then
      Console.Get_Mouse_Event (Mouse_Event, Con_Io.X_Y);
      if Mouse_Event.Valid then
        Basic_Proc.Put_Line_Output (Mouse_Event.Button'Img
            & " " & Mouse_Event.Status'Img
            & " X" & Mouse_Event.X'Img & ", Y" & Mouse_Event.Y'Img);
        if Mouse_Event.Xref /= X_Mng.Null_Reference then
          Got_Card := Deck.Ref_To_Access (Mouse_Event.Xref);
          Basic_Proc.Put_Line_Output ("Xref: "
              & The_Names(Got_Card.Get_Suit, Got_Card.Get_Name).Image);
        end if;
      end if;
    elsif Stat = Con_Io.Refresh then
      Console.Clear_Screen;
      Put_Menu;
    end if;
  end loop;

end T_Cards;

