with As.U, Basic_Proc, Argument, Con_Io, X_Mng.Cards, Dynamic_List, Rnd;
procedure T_Cards is

  -- Cards
  package Deck is new X_Mng.Cards;
  The_Cards : array (Deck.Suit_List, Deck.Name_Range) of aliased Deck.Card;
  The_Names : array (Deck.Suit_List, Deck.Name_Range) of As.U.Asu_Us;
  Got_Card : Deck.Card_Access;
  package Cards_Dyn_List_Mng is new Dynamic_List (Deck.Card_Access);
  package Cards_List_Mng renames Cards_Dyn_List_Mng.Dyn_List;
  Cards_List : Cards_List_Mng.List_Type;

  -- Console
  Motion : Boolean := False;
  Last_Col : Con_Io.Col_Range;
  Last_Row : constant Con_Io.Row_Range := 49;
  Console : aliased Con_Io.Console;
  Font_Height : Natural;
  Background : constant Con_Io.Colors :=  Con_Io.Color03;

  -- Stacks and cards positions
  Menu_Row : constant Con_Io.Row_Range := 1;
  X_Gap : constant Con_Io.X_Range := 4;
  Y_Gap : constant Con_Io.Y_Range := 31;
  Stack_X : Con_Io.X_Range;
  Stack_Y : Con_Io.Y_Range;
  subtype Stack_Range is Deck.Name_Range;
  -- 4 Cards, the last one being the Ace of a complete color
  subtype Depth_Range is Natural range 1 .. 3 + Deck.Name_Range'Last;

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

  -- Position (X, Y) of card within a stack
  function Pos_Of (Stack : Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec is
  begin
    return (X => Stack_X + (Stack - 1) * (Deck.Width + X_Gap),
            Y => Stack_Y + (Depth - 1) * Y_Gap);
  end Pos_Of;

  use type X_Mng.External_Reference, Con_Io.Curs_Mvt,
           Con_Io.Mouse_Button_Status_List;
begin

  -- Create Console at proper size
  Con_Io.Initialise;
  declare
    Font_No : constant := 0;
    Font_Width, Font_Offset : Natural;
  begin
    Con_Io.Get_Font_Geometry (Font_No, Font_Width, Font_Height, Font_Offset);
    Last_Col := ((Deck.Width + X_Gap) * Stack_Range'Last + X_Gap) / Font_Width;
    Console.Open (Font_No, Last_Row, Last_Col, Def_Back => Background);
    Basic_Proc.Put_Line_Output ("Last col" & Last_Col'Img
        & "  font: " & Console.Font_Name
        & " size: " & Font_Width'Img & " x" & Font_Height'Img);
    Basic_Proc.Put_Line_Output ("Last X:" & Integer'Image (Console.X_Max)
        & ", Last Y:" & Integer'Image (Console.Y_Max));
  end;
  Console.Set_Y_Mode (Con_Io.X_Mng_Mode);
  Motion := Argument.Get_Nbre_Arg = 1
            and then Argument.Get_Parameter = "--motion";
  Deck.Set_Line (Console.Get_Line, Motion);
  if Motion then
    Console.Enable_Motion_Events (True);
  end if;

  -- Compute offset of stacks
  Stack_X := (Console.X_Max - Stack_Range'Last * (Deck.Width + X_Gap) + X_Gap)
             / 2;
  Stack_Y := (Menu_Row + 1) * Font_Height + Y_Gap;

  -- Create a dummy window for blind get
  Get_Window.Open (Console'Unchecked_Access, (0, 0), (0, 0));
  Get_Window.Set_Foreground (Background);
  Get_Window.Set_Background (Background);

  -- Put menu
  Menu_Window.Open (Console'Unchecked_Access,
                    (Menu_Row, 0), (Menu_Row, Last_Col));
  Put_Menu;

  -- Create the cards
  Basic_Proc.Put_Line_Output ("Creating the cards");
  for Suit in Deck.Suit_List loop
    for Name in Deck.Name_Range loop
      The_Names(Suit, Name).Set (Suit'Img & Name'Img);
      The_Cards(Suit, Name).Create_Card (Suit, Name);
      Cards_List.Insert (The_Cards(Suit, Name)'Access);
    end loop;
  end loop;

  -- Display the cards randomly
  declare
    R : Positive;
    Acc : Deck.Card_Access;
    Moved : Boolean;
  begin
    Basic_Proc.Put_Line_Output ("Putting the cards"
        & Integer'Image (Cards_List.List_Length));
    Depths :
    for Depth in 1 .. 4 loop
      for Stack in Stack_Range loop
        R := Rnd.Gen.Int_Random (1, Cards_List.List_Length);
        Cards_List.Move_At (R);
        Cards_List.Get (Acc, Moved => Moved);
        Acc.Move (Pos_Of (Stack, Depth));
        Acc.Show (True);
      end loop;
    end loop Depths;
  end;

  -- Test depth: the initial 4 including an ace at top, then Ring to 2
  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "--depth" then
    for Name in reverse Deck.Name_Range range 2 .. 13 loop
      The_Cards (Deck.Heart, Name).Move (Pos_Of (1, 18 - Name));
      The_Cards (Deck.Heart, Name).Show (True);
    end loop;
  end if;

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
          if Mouse_Event.Status = Con_Io.Enter then
            Console.Set_Pointer_Shape (Con_Io.Hand);
          elsif Mouse_Event.Status = Con_Io.Leave then
            Console.Set_Pointer_Shape (Con_Io.Arrow);
          end if;
        end if;
      end if;
    elsif Stat = Con_Io.Refresh then
      Console.Clear_Screen;
      Put_Menu;
    end if;
  end loop;

end T_Cards;

