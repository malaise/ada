with As.U, Normal;
package body Screen is

  -------------------------------
  -- GLOBAL SCREEN DEFINITIONS --
  -------------------------------
  Screen_Win, Global_Win, Secret_Win, Propal_Win, Try_Win,
   Color_Win, Help_Win, Menu_Win, Level_Win, Exit_Win : Con_Io.Window;

  -- Fixed geometry
  Propal_Col_Width : constant Con_Io.Col_Range :=  2;
  Propal_Last_Row  : constant Con_Io.Row_Range := 22;
  Propal_First_Row : constant Con_Io.Row_Range :=
   Propal_Last_Row - (Con_Io.Row_Range(Common.Max_Number_Propal)-1) * 2;
  Propal_Last_Col  : constant Con_Io.Col_Range := 15;
  Try_First_Col    : constant Con_Io.Col_Range := 22;
  Color_Col_Width  : constant Con_Io.Col_Range :=  2;
  Color_First_Row  : constant Con_Io.Row_Range :=  7;
  Color_Last_Row   : constant Con_Io.Row_Range :=
   Color_First_Row + (Con_Io.Row_Range(Common.Max_Number_Color)-1) * 2;
  Color_First_Col  : constant Con_Io.Col_Range := 36;
  Color_Last_Col   : constant Con_Io.Col_Range :=
   Color_First_Col + Color_Col_Width - 1;
  Menu_Row : constant Con_Io.Row_Range := 22;
  Menu_First_Col : constant Con_Io.Col_Range := 46;
  Menu_Last_Col : constant Con_Io.Col_Range := 56;
  Level_First_Col : constant Con_Io.Col_Range := 58;
  Level_Last_Col : constant Con_Io.Col_Range := 66;
  Exit_First_Col : constant Con_Io.Col_Range := 68;
  Exit_Last_Col : constant Con_Io.Col_Range := 76;

  -- Level dependant gemetry
  Current_Level : Common.Last_Level_Range;
  Propal_First_Col : Con_Io.Col_Range;
  Try_Last_Col     : Con_Io.Col_Range;

  -- Color definitions
  Colors : array (Common.Color_Range) of Con_Io.Effective_Colors;
  Color_Names : constant array (Common.Color_Range) of As.U.Asu_Us :=
      (0 => As.U.Tus ("Brown"),
       1 => As.U.Tus ("Blue"),
       2 => As.U.Tus ("Dark_Green"),
       3 => As.U.Tus ("Cyan"),
       4 => As.U.Tus ("Red"),
       5 => As.U.Tus ("Magenta"),
       6 => As.U.Tus ("Light_Grey"),
       7 => As.U.Tus ("Chartreuse"),
       8 => As.U.Tus ("Yellow"));
  White : Con_Io.Effective_Colors;

  -- Screen foreground and backgound
  Foreground_Color  : Con_Io.Effective_Colors;
  Background_Color  : Con_Io.Effective_Colors;

  -- When possible to try
  Try_Color : Con_Io.Effective_Colors;
  -- When click in try or menu window
  Background_Select : Con_Io.Effective_Colors;
  -- Used to answer
  Ok_Color  : Con_Io.Effective_Colors;
  Nok_Color : Con_Io.Effective_Colors;

  Pin : constant Character := '!';

  -- Dummy for upward compatibility
  procedure Set_Mouse_Default_Color is
  begin
    null;
  end Set_Mouse_Default_Color;

  procedure Set_Mouse_Color (Color : in Common.Eff_Color_Range) is
  begin
    null;
  end Set_Mouse_Color;

  -- Square, in Propal_Win for a propal & level
  function Propal_Square (Propal : Common.Propal_Range;
                          Level  : Common.Level_Range) return Con_Io.Square is
    Lower_Right : constant Con_Io.Square :=
     Propal_Win.Get_Relative_Lower_Right;
  begin
    return (Lower_Right.Row - (Con_Io.Row_Range(Propal)-1) * 2,
            (Con_Io.Col_Range(Level)-1) * (Propal_Col_Width+1) );
  end Propal_Square;


  -- Draw a frame (+---+) around a window
  procedure Frame (Name : in Con_Io.Window) is
    Upper_Left  : constant Con_Io.Square := Name.Get_Absolute_Upper_Left;
    Lower_Right : constant Con_Io.Square := Name.Get_Absolute_Lower_Right;
    -- A line will be "+---   ---+". Here, define the "---  ---"
    Line : constant String (1 .. Name.Get_Relative_Lower_Right.Col + 1)
         :=  (others => '-');
  begin
    -- Set attributes
    Screen_Win.Set_Foreground (Con_Io.Get_Foreground (Name));
    Screen_Win.Set_Background (Con_Io.Get_Background (Name));
    -- First and last rows
    Screen_Win.Move (Upper_Left.Row - 1,  Upper_Left.Col - 1);
    Screen_Win.Put ("+" & Line & "+", Move => False);
    Screen_Win.Move (Lower_Right.Row + 1, Upper_Left.Col - 1);
    Screen_Win.Put ("+" & Line & "+", Move => False);
    -- All cols
    for Row in Upper_Left.Row .. Lower_Right.Row loop
      Screen_Win.Move (Row, Upper_Left.Col  - 1);
      Screen_Win.Put ('|', Move => False);
      Screen_Win.Move (Row, Lower_Right.Col + 1);
      Screen_Win.Put ('|', Move => False);
    end loop;
  end Frame;

  -- Init the screen, the windows, draw borders
  procedure Init is
    Color_Def : Con_Io.Colors_Definition := Con_Io.Default_Colors;
    Coli : Con_Io.Colors;
  begin
    -- Set Color names, start with background and colors to search
    Coli := Con_Io.Effective_Colors'First;
    for I in Common.Color_Range loop
      Coli := Con_Io.Effective_Colors'Succ (Coli);
      Color_Def(Coli) := Color_Names(I);
      Colors(I) := Coli;
    end loop;
    -- Add other colors
    Coli := Con_Io.Effective_Colors'Succ (Coli);
    Color_Def(Coli) := As.U.Tus ("Light_Grey");
    Coli := Con_Io.Effective_Colors'Succ (Coli);
    Color_Def(Coli) := As.U.Tus ("White");
    Coli := Con_Io.Effective_Colors'Succ (Coli);
    Color_Def(Coli) := As.U.Tus ("Dark_Grey");
    Coli := Con_Io.Effective_Colors'Succ (Coli);
    Color_Def(Coli) := As.U.Tus ("Black");
    Con_Io.Set_Colors (Color_Def);

    -- Store other colors
    Background_Color := Colors(0);
    Background_Select := Con_Io.Color_Of ("Light_Grey");
    White := Con_Io.Color_Of ("White");
    Foreground_Color := Con_Io.Color_Of ("Dark_Grey");
    Try_Color := White;
    Ok_Color  := Con_Io.Color_Of ("Black");
    Nok_Color := White;
    -- Open console
    Console.Open (Def_Fore => Foreground_Color,
                  Def_Back => Background_Color);
    Screen_Win.Set_To_Screen (Console'Access);

  end Init;

  procedure Init (Level : in Common.Last_Level_Range) is
    Square : Con_Io.Square;
    use Common;
  begin

    if not Global_Win.Is_Open then
      -- Open windows
      Global_Win.Open (Console'Access, (1, 1), (23, 78));
      Color_Win.Open (Console'Access, (Color_First_Row,  Color_First_Col),
                                      (Color_Last_Row,   Color_Last_Col) );
      Help_Win.Open (Console'Access, (4, Menu_First_Col),(Menu_Row-3, 76) );
      Menu_Win.Open (Console'Access, (Menu_Row,  Menu_First_Col),
                                     (Menu_Row,  Menu_Last_Col) );
      Level_Win.Open (Console'Access, (Menu_Row,  Level_First_Col),
                                      (Menu_Row,  Level_Last_Col) );
      Exit_Win.Open (Console'Access, (Menu_Row,  Exit_First_Col),
                                     (Menu_Row,  Exit_Last_Col) );
    else
      Secret_Win.Close;
      Propal_Win.Close;
      Try_Win.Close;
    end if;

    -- Compute level dependant geometry
    Current_Level := Level;
    Propal_First_Col := Propal_Last_Col
     - (Con_Io.Col_Range(Current_Level)-1) * (Propal_Col_Width+1)
     - (Propal_Col_Width-1);
    Try_Last_Col := Try_First_Col + (Con_Io.Col_Range(Current_Level)-1);

    Secret_Win.Open (Console'Access, (1, Propal_First_Col),
                                     (1, Propal_Last_Col) );
    Propal_Win.Open (Console'Access, (Propal_First_Row, Propal_First_Col),
                                     (Propal_Last_Row,  Propal_Last_Col) );
    Try_Win.Open (Console'Access, (Propal_First_Row, Try_First_Col),
                                  (Propal_Last_Row,  Try_Last_Col) );

    -- Redraw and frames
    Screen_Win.Clear;
    Global_Win.Clear;
    -- Frame (Name => Global_Win);
    Frame (Name => Secret_Win);
    Frame (Name => Propal_Win);
    Frame (Name => Try_Win);
    Frame (Name => Color_Win);
    Frame (Name => Help_Win);
    Frame (Name => Level_Win);
    Frame (Name => Menu_Win);
    Frame (Name => Exit_Win);

    -- Draw lines in propal and try frames
    for J in Common.Level_Range'First .. Level loop
      for I in Common.Propal_Range loop
        Square := Propal_Square (I, J);
        if J /= Current_Level then
          -- Draw | of propal
          Propal_Win.Move (Square.Row, Square.Col + Propal_Col_Width);
          Propal_Win.Put ('|');
        end if;
        if I /= Common.Propal_Range'Last then
          -- Draw -- of propal
          for K in 1 .. Propal_Col_Width loop
            Propal_Win.Move (Square.Row-1, Square.Col+K-1);
            Propal_Win.Put ('-');
          end loop;
          if J /= Current_Level then
            -- Draw + of propal
            Propal_Win.Move (Square.Row-1, Square.Col+Propal_Col_Width);
            Propal_Win.Put ('+');
          end if;

          -- Draw - of try
          Try_Win.Move (Square.Row-1, Con_Io.Col_Range(J)-1);
          Try_Win.Put ('-');
        end if;

      end loop;

    end loop;

    -- Adapt propal and try frames
    for I in Common.Propal_Range'First ..
             Common.Propal_Range'Pred(Common.Propal_Range'Last) loop
      -- |- in propal
      Screen_Win.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                       Propal_First_Col - 1);
      Screen_Win.Put ('+');
      -- -| in propal
      Screen_Win.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                       Propal_Last_Col + 1);
      Screen_Win.Put ('+');
      -- |- in try
      Screen_Win.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                       Try_First_Col - 1);
      Screen_Win.Put ('|');
      -- -| in try
      Screen_Win.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                       Try_Last_Col + 1);
      Screen_Win.Put ('|');
    end loop;

   for J in Common.Level_Range'First .. Current_Level - 1 loop
      -- T in propal
      Screen_Win.Move (Propal_First_Row - 1,
                       Propal_First_Col + Propal_Col_Width +
                        (Con_Io.Col_Range(J)- 1) * (Propal_Col_Width+1) );
      Screen_Win.Put ('+');
      -- L in propal
      Screen_Win.Move (Propal_Last_Row + 1,
                       Propal_First_Col + Propal_Col_Width +
                        (Con_Io.Col_Range(J)- 1) * (Propal_Col_Width+1) );
      Screen_Win.Put ('+');
    end loop;


    -- Draw propal numbers
    for I in Common.Propal_Range loop
      Screen_Win.Move (Propal_Last_Row - (Con_Io.Row_Range(I)-1) * 2,
                       Propal_Last_Col + 3);
      Screen_Win.Put (Normal (Integer(I), 2));
    end loop;

    -- Draw colors
    for I in Common.Eff_Color_Range loop
      Color_Win.Move ((Con_Io.Row_Range(I)-1) * 2, 0);
      Color_Win.Put (Pin, Foreground => Colors(I));
      Color_Win.Move ((Con_Io.Row_Range(I)-1) * 2, 1);
      Color_Win.Put (Pin, Foreground => Colors(I));
      if I /= Common.Eff_Color_Range'Last then
        Color_Win.Move ((Con_Io.Row_Range(I)-1) * 2 + 1, 0);
        Color_Win.Put ('-');
        Color_Win.Move ((Con_Io.Row_Range(I)-1) * 2 + 1, 1);
        Color_Win.Put ('-');

        Screen_Win.Move (Color_First_Row + (Con_Io.Row_Range(I)-1) * 2 + 1,
                         Color_First_Col - 1);
        Screen_Win.Put ('+');
        Screen_Win.Move (Color_First_Row + (Con_Io.Row_Range(I)-1) * 2 + 1,
                         Color_Last_Col + 1);
        Screen_Win.Put ('+');
      end if;
    end loop;

    -- Draw title
    Global_Win.Move (1, Try_Last_Col + 2);
    Global_Win.Put ("M A S T E R   M I N D");

    -- Draw level
    for I in Common.Last_Level_Range loop
      Put_Level (I, Selected => False);
    end loop;

    -- Draw Exit
    Exit_Win.Move;
    Exit_Win.Put (" E X I T ", Move => False);

    -- No try
    for I in Common.Propal_Range loop
      Put_Try (I, Cannot_Try);
    end loop;

  end Init;

  procedure Clear is
  begin
    Screen_Win.Clear;
    Screen_Win.Move;
  end Clear;


  ------------
  -- PROPAL --
  ------------
  procedure Put_Default_Pos (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Show   : in Boolean) is

    Color : Con_Io.Effective_Colors;
    -- Character for row, col corner
    Chars  : array (1 .. 3) of Character;
    Square : Con_Io.Square;
    use Common;
  begin
    if Level > Current_Level then
      raise Constraint_Error;
    end if;

    -- Set color and square in global
    if Show then
      Color := White;
      Chars (1) := '+';
      Chars (2) := '+';
      Chars (3) := '+';
    else
      Color := Foreground_Color;
      Chars (1) := '-';
      Chars (2) := '|';
      Chars (3) := '+';
    end if;

    Square := Propal_Square (Propal, Level);
    Square := Propal_Win.To_Absolute (Square);
    Square := Global_Win.To_Relative (Square);

    -- Draw frame of square
    Global_Win.Move (Square.Row - 1, Square.Col - 1);
    Global_Win.Put (Chars(3), Foreground => Color);
    Global_Win.Move (Square.Row - 1, Square.Col + Propal_Col_Width);
    Global_Win.Put (Chars(3), Foreground => Color);
    Global_Win.Move (Square.Row + 1, Square.Col - 1);
    Global_Win.Put (Chars(3), Foreground => Color);
    Global_Win.Move (Square.Row + 1, Square.Col + Propal_Col_Width);
    Global_Win.Put (Chars(3), Foreground => Color);

    Global_Win.Move (Square.Row, Square.Col - 1);
    Global_Win.Put (Chars(2), Foreground => Color);
    Global_Win.Move (Square.Row, Square.Col + Propal_Col_Width);
    Global_Win.Put (Chars(2), Foreground => Color);

    for K in 1 .. Propal_Col_Width loop
      Global_Win.Move (Square.Row - 1, Square.Col + K - 1);
      Global_Win.Put (Chars(1), Foreground => Color);
      Global_Win.Move (Square.Row + 1, Square.Col + K - 1);
      Global_Win.Put (Chars(1), Foreground => Color);
    end loop;

  end Put_Default_Pos;

  procedure Put_Try (
   Propal   : in Common.Propal_Range;
   Try_State : in Put_Try_List) is
    Square : Con_Io.Square;
  begin
    Square.Row := Propal_Square (Propal, Common.Level_Range'First).Row;
    Square.Col := 0;
    for I in Common.Level_Range'First .. Current_Level loop
      Try_Win.Move (Square.Row, Square.Col+Con_Io.Col_Range(I)-1);
      case Try_State is
        when Cannot_Try =>
          Try_Win.Put ('X', Move => False);
        when Can_Try =>
          Try_Win.Put ('?', Foreground => Try_Color, Move => False);
        when Selected =>
          Try_Win.Put ('?', Foreground => Background_Color,
                           Background => Background_Select,
                           Move => False);
      end case;
    end loop;

  end Put_Try;

  procedure Put_Color (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Color  : in Common.Color_Range) is
    Square : Con_Io.Square;
    use Common;
  begin
    if Level > Current_Level then
      raise Constraint_Error;
    end if;
    Square := Propal_Square (Propal, Level);
    for I in 1 .. Propal_Col_Width loop
      Propal_Win.Move (Square.Row, Square.Col+I-1);
      if Color /= Common.Color_Range'First then
        Propal_Win.Put (Pin, Foreground => Colors(Color));
      else
        Propal_Win.Put (' ', Foreground => Foreground_Color, Move => False);
      end if;
    end loop;

  end Put_Color;

  procedure Put_Answer (
   Propal : in Common.Propal_Range;
   Placed_Ok, Colors_Ok : in Natural) is
    Square : Con_Io.Square;
    use Common;
  begin
    if Colors_Ok + Placed_Ok > Natural(Current_Level) then
      raise Constraint_Error;
    end if;

    Square.Row := Propal_Square (Propal, Common.Level_Range'First).Row;
    Square.Col := 0;
    -- Clear
    for I in Common.Level_Range'First .. Current_Level loop
      Try_Win.Move (Square.Row, Square.Col+Con_Io.Col_Range(I)-1);
      Try_Win.Put (' ', Move => False);
    end loop;
    -- Put
    for I in 1 .. Placed_Ok loop
      Try_Win.Move (Square);
      Try_Win.Put ('*', Foreground => Ok_Color);
      Square.Col := Square.Col + 1;
    end loop;
    for I in 1 .. Colors_Ok loop
      Try_Win.Move (Square);
      Try_Win.Put ('*', Foreground => Nok_Color);
      Square.Col := Square.Col + 1;
    end loop;

  end Put_Answer;

  ------------
  -- SECRET --
  ------------
  procedure Put_Secret_Color (
   Level  : in Common.Level_Range;
   Color  : in Common.Color_Range) is
    Square : Con_Io.Square;
  begin
    Square.Row := 0;
    Square.Col := Propal_Square (1, Level).Col;
    for I in 1 .. Propal_Col_Width loop
      Secret_Win.Move (Square.Row, Square.Col+I-1);
      Secret_Win.Put (Pin, Foreground => Colors(Color));
    end loop;

  end Put_Secret_Color;

  ----------
  -- MENU --
  ----------
  procedure Put_Start_Giveup (Start : in Boolean; Selected : in Boolean) is
    Fore : Con_Io.Effective_Colors;
    Back : Con_Io.Effective_Colors;
  begin
    if Selected then
      Fore := Background_Color;
      Back := Background_Select;
    else
      Fore := Foreground_Color;
      Back := Background_Color;
    end if;
    Menu_Win.Move;
    if Start then
      Menu_Win.Put (" S T A R T ",
       Foreground => Fore,
       Background => Back,
       Move => False);
    else
      Menu_Win.Put ("  GIVE UP  ",
       Foreground => Fore,
       Background => Back,
       Move => False);
    end if;

  end Put_Start_Giveup;

  -----------
  -- LEVEL --
  -----------
  procedure Put_Level (Level_No : in Common.Last_Level_Range;
   Selected : in Boolean) is
    Col : constant Con_Io.Row_Range :=
     Con_Io.Row_Range (Level_No) -
     Con_Io.Row_Range (Common.Last_Level_Range'First);
    Fore : Con_Io.Effective_Colors;
    Back : Con_Io.Effective_Colors;
  begin
    if Selected then
      Fore := Background_Color;
      Back := Background_Select;
    else
      Fore := Foreground_Color;
      Back := Background_Color;
    end if;
    Level_Win.Move (0, Col * 2 + 2);
    Level_Win.Put (Normal (Integer(Level_No), 1),
     Foreground => Fore,
     Background => Back,
     Move => False);
  end Put_Level;

  procedure Put_Current_Level (Level_No : in Common.Last_Level_Range) is
    Col : constant Con_Io.Row_Range :=
     Con_Io.Row_Range (Level_No) -
     Con_Io.Row_Range (Common.Last_Level_Range'First);
  begin
    Frame (Name => Level_Win);
    Global_Win.Move (Menu_Row - 2, Level_First_Col + 2 * Col + 1);
    Global_Win.Put ('|');
    Global_Win.Move (Menu_Row,     Level_First_Col + 2 * Col + 1);
    Global_Win.Put ('|');
  end Put_Current_Level;


  procedure Put_Exit (Selected : in Boolean) is
  begin
    Exit_Win.Move;
    if Selected then
      Exit_Win.Put  (" E X I T ",
       Foreground => Background_Color,
       Background => Background_Select,
       Move => False);
    else
      Exit_Win.Put  (" E X I T ", Move => False);
    end if;
  end Put_Exit;

  ----------
  -- HELP --
  ----------
  procedure Put_Help (Help : Help_State) is
  begin
    Help_Win.Clear;
    case Help is
      when Released =>
        Help_Win.Move;
        Help_Win.Put_Line ("Select :");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A color to set");
        Help_Win.Put_Line ("     it in a proposition.");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A proposition to clear");
        Help_Win.Put_Line ("     or move it.");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A try to get answer");
        Help_Win.Put_Line ("     to a try.");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A menu option :");
        Help_Win.Put_Line ("     Give-up.");
        Help_Win.Put_Line ("     Exit.");
      when Click_Color =>
        Help_Win.Move;
        Help_Win.Put_Line ("Release on :");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A position to affect it");
        Help_Win.Put_Line ("     at this position.");
      when Click_Propal =>
        Help_Win.Move;
        Help_Win.Put_Line ("Release on :");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> an empty square to move");
        Help_Win.Put_Line ("     this color.");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> outside propositions");
        Help_Win.Put_Line ("     to clear this color.");
      when Click_Other =>
        Help_Win.Move;
        Help_Win.New_Line (Number => 2);
        Help_Win.Put_Line ("Release on the same item");
        Help_Win.Put_Line (" to validate.");
      when Start =>
        Help_Win.Move;
        Help_Win.Put_Line ("Select :");
        Help_Win.New_Line;
        Help_Win.Put_Line (" -> A menu option :");
        Help_Win.Put_Line ("     Start.");
        Help_Win.Put_Line ("     Level (3, 4 or 5).");
        Help_Win.Put_Line ("     Exit.");
      when Discarded =>
        Help_Win.Move;
        Help_Win.New_Line (Number => 2);
        Help_Win.Put_Line ("WRONG CLICK POSITION");
        Help_Win.New_Line;
        Help_Win.Put_Line ("Release anywhere");
        Help_Win.Put_Line (" and click again.");
    end case;

  end Put_Help;

  -----------
  -- COLOR --
  -----------
  procedure Put_Selected_Color (
   Color : in Common.Eff_Color_Range;
   Selected : in Boolean) is
    Fore : Con_Io.Effective_Colors;
    -- Character for row, col corner
    Chars  : array (1 .. 3) of Character;
    Square : Con_Io.Square;
    use Common;
  begin
    -- Set color and square in global
    if Selected then
      Fore := White;
      Chars (1) := '+';
      Chars (2) := '+';
      Chars (3) := '+';
    else
      Fore := Foreground_Color;
      Chars (1) := '-';
      Chars (2) := '|';
      Chars (3) := '+';
    end if;

    Color_Win.Move ((Con_Io.Row_Range(Color) - 1) * 2, 0);
    Square := Color_Win.Position;
    Square := Color_Win.To_Absolute (Square);
    Square := Global_Win.To_Relative (Square);

    -- Draw frame of square
    Global_Win.Move (Square.Row - 1, Square.Col - 1);
    Global_Win.Put (Chars(3) & Chars (1) & Chars (1) & Chars(3),
                Foreground => Fore);
    Global_Win.Move (Square.Row + 1, Square.Col - 1);
    Global_Win.Put (Chars(3) & Chars (1) & Chars (1) & Chars(3),
                Foreground => Fore);
    Global_Win.Move (Square.Row, Square.Col - 1);
    Global_Win.Put (Chars(2), Foreground => Fore);
    Global_Win.Move (Square.Row, Square.Col + 2);
    Global_Win.Put (Chars(2), Foreground => Fore);

  end Put_Selected_Color;

  ---------------
  -- SELECTION --
  ---------------
  function To_Propal (Row : Con_Io.Row_Range) return Common.Propal_Range is
  begin
    return
     Common.Propal_Range (
      Con_Io.Row_Range (Common.Max_Number_Propal) - (Row / 2) );
  end To_Propal;

  procedure Get_Selected (
   Where : in Con_Io.Square;
   What  : out Selection_Rec) is
    Square : Con_Io.Square;
    use Common;
  begin
    What := (Selection_Kind => Nothing, Selection => Nothing);

    if Propal_Win.In_Window (Where) then
      Square := Propal_Win.To_Relative (Where);
      if Square.Row mod 2 /= 0 then
        What.Selection := Propal;
        return;
      end if;
      if Square.Col mod (Propal_Col_Width+1) = Propal_Col_Width then
        What.Selection := Propal;
        return;
      end if;
      What := (Selection_Kind => Propal,
               Propal_No => To_Propal (Square.Row),
               Column_No => Common.Level_Range(
                             (Square.Col / (Propal_Col_Width+1))+1) );
    elsif Try_Win.In_Window (Where) then
      Square := Try_Win.To_Relative (Where);
      if Square.Row mod 2 /= 0 then
        What.Selection := Try;
        return;
      end if;
      What := (Selection_Kind => Try,
               Try_No => To_Propal (Square.Row) );
    elsif Color_Win.In_Window (Where) then
      Square := Color_Win.To_Relative (Where);
      if Square.Row mod 2 /= 0 then
        What.Selection := Color;
        return;
      end if;
      What := (
       Selection_Kind => Color,
       Color_No => Common.Eff_Color_Range (1 + (Square.Row / 2)) );
    elsif Menu_Win.In_Window (Where) then
      Square := Menu_Win.To_Relative (Where);
      What := (Selection_Kind => Menu);
    elsif Level_Win.In_Window (Where) then
      Square := Level_Win.To_Relative (Where);
      What.Selection := Level;
      if (Square.Col) mod 2 /= 0 then
        return;
      end if;
      What := (
       Selection_Kind => Level,
       Level_No => Common.Last_Level_Range (
        (Square.Col - 2) / 2 + Integer (Common.Last_Level_Range'First)) );
    elsif Exit_Win.In_Window (Where) then
      Square := Exit_Win.To_Relative (Where);
      What := (Selection_Kind => Exit_Game);
    end if;

  end Get_Selected;

end Screen;

