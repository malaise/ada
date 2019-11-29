package body Table is

  -- Static information about the console
  Font_Height : Natural;
  Last_Col : Con_Io.Col_Range;
  Last_Row : constant Con_Io.Row_Range := 49;
  Background : constant Con_Io.Colors :=  Con_Io.Color03;

  -- Stacks and cards positions
  Menu_Row : constant Con_Io.Row_Range := 1;
  X_Gap : constant Con_Io.X_Range := 4;
  Y_Gap : constant Con_Io.Y_Range := 31;
  Stack_X : Con_Io.X_Range;
  Stack_Y : Con_Io.Y_Range;

  -- Dummy window for blind Get
  Get_Window : Con_Io.Window;

  -- Window for the menu
  Menu_Window : Con_Io.Window;
  Menu_Back : constant Con_Io.Colors :=  Con_Io.Color_Of ("Light_Blue");
  Menu_Fore : constant Con_Io.Colors :=  Con_Io.Color_Of ("Black");

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

    -- Compute offset of stacks
    Stack_X := (Console.X_Max - Cards.Stack_Range'Last * (Deck.Width + X_Gap)
                + X_Gap) / 2;
    Stack_Y := (Menu_Row + 1) * Font_Height + Y_Gap;

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
  function Pos_Of (Stack : Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec is
  begin
    return (X => Stack_X + (Stack - 1) * (Deck.Width + X_Gap),
            Y => Stack_Y + (Depth - 1) * Y_Gap);
  end Pos_Of;

  -- Wait for next event, return False when exiting the game
  function Wait_Event return Boolean is
    Str : Con_Io.Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Insert : Boolean;
    use type Con_Io.Curs_Mvt;
  begin
    Get_Window.Get (Str, Last, Stat, Pos, Insert);
    if Stat = Con_Io.Break then
      return False;
    end if;
    return True;
  end Wait_Event;

end Table;

