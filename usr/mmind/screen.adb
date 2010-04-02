with Normal;
package body Screen is

  -------------------------------
  -- GLOBAL SCREEN DEFINITIONS --
  -------------------------------
  Global_Win, Secret_Win, Propal_Win, Try_Win,
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
  Color_Definition : constant array (Common.Color_Range) of
   Con_Io.Effective_Colors := (
       0 => Con_Io.Color_Of ("Brown"),
       1 => Con_Io.Color_Of ("Blue"),
       2 => Con_Io.Color_Of ("Dark_Green"),
       3 => Con_Io.Color_Of ("Cyan"),
       4 => Con_Io.Color_Of ("Red"),
       5 => Con_Io.Color_Of ("Magenta"),
       6 => Con_Io.Color_Of ("Light_Gray"),
       7 => Con_Io.Color_Of ("Orange"),
       8 => Con_Io.Color_Of ("Yellow"));
  White : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("White");

  Foreground_Color  : constant Con_Io.Effective_Colors
                    := Con_Io.Color_Of ("Dark_Gray");
  Background_Color  : constant Con_Io.Effective_Colors :=
   Color_Definition(0);

  -- When possible to try
  Try_Color : constant Con_Io.Effective_Colors := White;
  -- When click in try or menu window
  Background_Select : constant Con_Io.Effective_Colors
                    := Con_Io.Color_Of ("Light_Gray");
  -- Used to answer
  Ok_Color  : constant Con_Io.Effective_Colors := Con_Io.Color_Of ("Black");
  Nok_Color : constant Con_Io.Effective_Colors := White;

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
     Con_Io.Get_Relative_Lower_Right (Propal_Win);
  begin
    return (Lower_Right.Row - (Con_Io.Row_Range(Propal)-1) * 2,
            (Con_Io.Col_Range(Level)-1) * (Propal_Col_Width+1) );
  end Propal_Square;


  -- Draw a frame (+---+) around a window
  procedure Frame (Name : in Con_Io.Window) is
    Upper_Left  : constant Con_Io.Square
                := Con_Io.Get_Absolute_Upper_Left  (Name);
    Lower_Right : constant Con_Io.Square
                := Con_Io.Get_Absolute_Lower_Right (Name);
    -- A line will be "+---   ---+". Here, define the "---  ---"
    Line : constant String (1 .. Con_Io.Get_Relative_Lower_Right (Name).Col + 1)
         :=  (others => '-');
  begin
    -- Set attributes
    Con_Io.Set_Foreground (Con_Io.Get_Foreground (Name),
                           Con_Io.Get_Blink_Stat (Name));
    Con_Io.Set_Background (Con_Io.Get_Background (Name));
    -- First and last rows
    Con_Io.Move (Upper_Left.Row - 1,  Upper_Left.Col - 1);
    Con_Io.Put ("+" & Line & "+", Move => False);
    Con_Io.Move (Lower_Right.Row + 1, Upper_Left.Col - 1);
    Con_Io.Put ("+" & Line & "+", Move => False);
    -- All cols
    for Row in Upper_Left.Row .. Lower_Right.Row loop
      Con_Io.Move (Row, Upper_Left.Col  - 1);
      Con_Io.Put ('|', Move => False);
      Con_Io.Move (Row, Lower_Right.Col + 1);
      Con_Io.Put ('|', Move => False);
    end loop;
  end Frame;

  -- Init the screen, the windows, draw borders
  procedure Init (Level : in Common.Last_Level_Range) is
    Square : Con_Io.Square;
    use Common;
  begin
    if not Con_Io.Is_Open (Global_Win) then
      Con_Io.Reset_Term;
      -- Open windows
      Con_Io.Open (Global_Win, (1, 1), (23, 78));
      Con_Io.Open (Color_Win,  (Color_First_Row,  Color_First_Col),
                               (Color_Last_Row,   Color_Last_Col) );
      Con_Io.Open (Help_Win,   (4, Menu_First_Col),(Menu_Row-3, 76) );
      Con_Io.Open (Menu_Win,   (Menu_Row,  Menu_First_Col),
                               (Menu_Row,  Menu_Last_Col) );
      Con_Io.Open (Level_Win,  (Menu_Row,  Level_First_Col),
                               (Menu_Row,  Level_Last_Col) );
      Con_Io.Open (Exit_Win,   (Menu_Row,  Exit_First_Col),
                               (Menu_Row,  Exit_Last_Col) );
      -- Set default colors
      Con_Io.Set_Background (Background_Color, Name => Con_Io.Screen);
      Con_Io.Set_Foreground (Foreground_Color, Name => Global_Win);
      Con_Io.Set_Background (Background_Color, Name => Global_Win);
      Con_Io.Set_Foreground (Foreground_Color, Name => Color_Win);
      Con_Io.Set_Background (Background_Color, Name => Color_Win);
      Con_Io.Set_Foreground (Foreground_Color, Name => Help_Win);
      Con_Io.Set_Background (Background_Color, Name => Help_Win);
      Con_Io.Set_Foreground (Foreground_Color, Name => Menu_Win);
      Con_Io.Set_Background (Background_Color, Name => Menu_Win);
      Con_Io.Set_Foreground (Foreground_Color, Name => Level_Win);
      Con_Io.Set_Background (Background_Color, Name => Level_Win);
      Con_Io.Set_Foreground (Foreground_Color, Name => Exit_Win);
      Con_Io.Set_Background (Background_Color, Name => Exit_Win);
    else
      Con_Io.Close (Secret_Win);
      Con_Io.Close (Propal_Win);
      Con_Io.Close (Try_Win);
    end if;

    -- Compute level dependant geometry
    Current_Level := Level;
    Propal_First_Col := Propal_Last_Col
     - (Con_Io.Col_Range(Current_Level)-1) * (Propal_Col_Width+1)
     - (Propal_Col_Width-1);
    Try_Last_Col := Try_First_Col + (Con_Io.Col_Range(Current_Level)-1);

    Con_Io.Open (Secret_Win, (1, Propal_First_Col), (1, Propal_Last_Col) );
    Con_Io.Open (Propal_Win, (Propal_First_Row, Propal_First_Col),
                             (Propal_Last_Row,  Propal_Last_Col) );
    Con_Io.Open (Try_Win,    (Propal_First_Row, Try_First_Col),
                             (Propal_Last_Row,  Try_Last_Col) );
    Con_Io.Set_Foreground (Foreground_Color, Name => Secret_Win);
    Con_Io.Set_Background (Background_Color, Name => Secret_Win);
    Con_Io.Set_Foreground (Foreground_Color, Name => Propal_Win);
    Con_Io.Set_Background (Background_Color, Name => Propal_Win);
    Con_Io.Set_Foreground (Foreground_Color, Name => Try_Win);
    Con_Io.Set_Background (Background_Color, Name => Try_Win);

    -- Redraw and frames
    Con_Io.Clear (Con_Io.Screen);
    Con_Io.Clear (Global_Win);
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
          Con_Io.Move (Square.Row, Square.Col + Propal_Col_Width, Propal_Win);
          Con_Io.Put ('|', Name => Propal_Win);
        end if;
        if I /= Common.Propal_Range'Last then
          -- Draw -- of propal
          for K in 1 .. Propal_Col_Width loop
            Con_Io.Move (Square.Row-1, Square.Col+K-1, Propal_Win);
            Con_Io.Put ('-', Name => Propal_Win);
          end loop;
          if J /= Current_Level then
            -- Draw + of propal
            Con_Io.Move (Square.Row-1, Square.Col+Propal_Col_Width, Propal_Win);
            Con_Io.Put ('+', Name => Propal_Win);
          end if;

          -- Draw - of try
          Con_Io.Move (Square.Row-1, Con_Io.Col_Range(J)-1, Try_Win);
          Con_Io.Put ('-', Name => Try_Win);
        end if;

      end loop;

    end loop;

    -- Adapt propal and try frames
    for I in Common.Propal_Range'First ..
             Common.Propal_Range'Pred(Common.Propal_Range'Last) loop
      -- |- in propal
      Con_Io.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                   Propal_First_Col - 1);
      Con_Io.Put ('+', Foreground => Foreground_Color,
                          Background => Background_Color);
      -- -| in propal
      Con_Io.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                   Propal_Last_Col + 1);
      Con_Io.Put ('+', Foreground => Foreground_Color,
                          Background => Background_Color);
      -- |- in try
      Con_Io.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                   Try_First_Col - 1);
      Con_Io.Put ('|', Foreground => Foreground_Color,
                          Background => Background_Color);
      -- -| in try
      Con_Io.Move (Propal_Last_Row - 1 - (Con_Io.Row_Range(I)-1) * 2,
                   Try_Last_Col + 1);
      Con_Io.Put ('|', Foreground => Foreground_Color,
                          Background => Background_Color);
    end loop;

   for J in Common.Level_Range'First .. Current_Level - 1 loop
      -- T in propal
      Con_Io.Move (Propal_First_Row - 1,
                   Propal_First_Col + Propal_Col_Width +
                   (Con_Io.Col_Range(J)- 1) * (Propal_Col_Width+1) );
      Con_Io.Put ('+', Foreground => Foreground_Color,
                          Background => Background_Color);
      -- L in propal
      Con_Io.Move (Propal_Last_Row + 1,
                   Propal_First_Col + Propal_Col_Width +
                   (Con_Io.Col_Range(J)- 1) * (Propal_Col_Width+1) );
      Con_Io.Put ('+', Foreground => Foreground_Color,
                          Background => Background_Color);
    end loop;


    -- Draw propal numbers
    for I in Common.Propal_Range loop
      Con_Io.Move (Propal_Last_Row - (Con_Io.Row_Range(I)-1) * 2,
                   Propal_Last_Col + 3);
      Con_Io.Put (Normal (Integer(I), 2), Foreground => Foreground_Color,
                                          Background => Background_Color);
    end loop;

    -- Draw colors
    for I in Common.Eff_Color_Range loop
      Con_Io.Move ((Con_Io.Row_Range(I)-1) * 2, 0, Color_Win);
      Con_Io.Put (Pin, Foreground => Color_Definition(I),
                           Name => Color_Win);
      Con_Io.Move ((Con_Io.Row_Range(I)-1) * 2, 1, Color_Win);
      Con_Io.Put (Pin, Foreground => Color_Definition(I),
                       Name => Color_Win);
      if I /= Common.Eff_Color_Range'Last then
        Con_Io.Move ((Con_Io.Row_Range(I)-1) * 2 + 1, 0, Color_Win);
        Con_Io.Put ('-', Name => Color_Win);
        Con_Io.Move ((Con_Io.Row_Range(I)-1) * 2 + 1, 1, Color_Win);
        Con_Io.Put ('-', Name => Color_Win);

        Con_Io.Move (Color_First_Row + (Con_Io.Row_Range(I)-1) * 2 + 1,
                     Color_First_Col - 1);
        Con_Io.Put ('+', Foreground => Foreground_Color,
                            Background => Background_Color);
        Con_Io.Move (Color_First_Row + (Con_Io.Row_Range(I)-1) * 2 + 1,
                     Color_Last_Col + 1);
        Con_Io.Put ('+', Foreground => Foreground_Color,
                            Background => Background_Color);
      end if;
    end loop;

    -- Draw title
    Con_Io.Move (1, Try_Last_Col + 2, Global_Win);
    Con_Io.Put ("M A S T E R   M I N D", Global_Win);

    -- Draw level
    for I in Common.Last_Level_Range loop
      Put_Level (I, Selected => False);
    end loop;

    -- Draw Exit
    Con_Io.Move (Name => Exit_Win);
    Con_Io.Put  (" E X I T ", Name => Exit_Win, Move => False);

    -- No try
    for I in Common.Propal_Range loop
      Put_Try (I, Cannot_Try);
    end loop;

  end Init;

  procedure Clear is
  begin
    Con_Io.Reset_Term;
    Con_Io.Move;
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
    Square := Con_Io.To_Absolute (Square, Propal_Win);
    Square := Con_Io.To_Relative (Square, Global_Win);

    -- Draw frame of square
    Con_Io.Move (Square.Row - 1, Square.Col - 1 , Name => Global_Win);
    Con_Io.Put (Chars(3), Foreground => Color, Name => Global_Win);
    Con_Io.Move (Square.Row - 1, Square.Col + Propal_Col_Width,
                 Name => Global_Win);
    Con_Io.Put (Chars(3), Foreground => Color, Name => Global_Win);
    Con_Io.Move (Square.Row + 1, Square.Col - 1 , Name => Global_Win);
    Con_Io.Put (Chars(3), Foreground => Color, Name => Global_Win);
    Con_Io.Move (Square.Row + 1, Square.Col + Propal_Col_Width,
                 Name => Global_Win);
    Con_Io.Put (Chars(3), Foreground => Color, Name => Global_Win);

    Con_Io.Move (Square.Row, Square.Col - 1 , Name => Global_Win);
    Con_Io.Put (Chars(2), Foreground => Color, Name => Global_Win);
    Con_Io.Move (Square.Row, Square.Col + Propal_Col_Width,
                 Name => Global_Win);
    Con_Io.Put (Chars(2), Foreground => Color, Name => Global_Win);

    for K in 1 .. Propal_Col_Width loop
      Con_Io.Move (Square.Row - 1, Square.Col + K - 1, Name => Global_Win);
      Con_Io.Put (Chars(1), Foreground => Color, Name => Global_Win);
      Con_Io.Move (Square.Row + 1, Square.Col + K - 1, Name => Global_Win);
      Con_Io.Put (Chars(1), Foreground => Color, Name => Global_Win);
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
      Con_Io.Move (Square.Row, Square.Col+Con_Io.Col_Range(I)-1, Try_Win);
      case Try_State is
        when Cannot_Try =>
          Con_Io.Put ('X', Name => Try_Win, Move => False);
        when Can_Try =>
          Con_Io.Put ('?', Foreground => Try_Color, Name => Try_Win,
           Move => False);
        when Selected =>
          Con_Io.Put ('?', Foreground => Background_Color,
                           Background => Background_Select, Name => Try_Win,
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
      Con_Io.Move (Square.Row, Square.Col+I-1, Propal_Win);
      if Color /= Common.Color_Range'First then
        Con_Io.Put (Pin, Foreground => Color_Definition(Color),
         Name => Propal_Win);
      else
        Con_Io.Put (' ', Foreground => Foreground_Color, Move => False,
         Name => Propal_Win);
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
      Con_Io.Move (Square.Row, Square.Col+Con_Io.Col_Range(I)-1, Try_Win);
      Con_Io.Put (' ', Move => False, Name => Try_Win);
    end loop;
    -- Put
    for I in 1 .. Placed_Ok loop
      Con_Io.Move (Square, Try_Win);
      Con_Io.Put ('*', Foreground => Ok_Color, Name => Try_Win);
      Square.Col := Square.Col + 1;
    end loop;
    for I in 1 .. Colors_Ok loop
      Con_Io.Move (Square, Try_Win);
      Con_Io.Put ('*', Foreground => Nok_Color, Name => Try_Win);
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
      Con_Io.Move (Square.Row, Square.Col+I-1, Secret_Win);
      Con_Io.Put (Pin, Foreground => Color_Definition(Color),
       Name => Secret_Win);
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
    Con_Io.Move (Name => Menu_Win);
    if Start then
      Con_Io.Put (" S T A R T ",
       Foreground => Fore,
       Background => Back,
       Name => Menu_Win,
       Move => False);
    else
      Con_Io.Put ("  GIVE UP  ",
       Foreground => Fore,
       Background => Back,
       Name => Menu_Win,
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
    Con_Io.Move (0, Col * 2 + 2, Name => Level_Win);
    Con_Io.Put (Normal (Integer(Level_No), 1),
     Foreground => Fore,
     Background => Back,
     Name => Level_Win,
     Move => False);
  end Put_Level;

  procedure Put_Current_Level (Level_No : in Common.Last_Level_Range) is
    Col : constant Con_Io.Row_Range :=
     Con_Io.Row_Range (Level_No) -
     Con_Io.Row_Range (Common.Last_Level_Range'First);
  begin
    Frame (Name => Level_Win);
    Con_Io.Move (Menu_Row - 2, Level_First_Col + 2 * Col + 1,
     Name => Global_Win);
    Con_Io.Put ('|', Name => Global_Win);
    Con_Io.Move (Menu_Row,     Level_First_Col + 2 * Col + 1,
     Name => Global_Win);
    Con_Io.Put ('|', Name => Global_Win);
  end Put_Current_Level;


  procedure Put_Exit (Selected : in Boolean) is
  begin
    Con_Io.Move (Name => Exit_Win);
    if Selected then
      Con_Io.Put  (" E X I T ",
       Name => Exit_Win,
       Foreground => Background_Color,
       Background => Background_Select,
       Move => False);
    else
      Con_Io.Put  (" E X I T ", Name => Exit_Win, Move => False);
    end if;
  end Put_Exit;

  ----------
  -- HELP --
  ----------
  procedure Put_Help (Help : Help_State) is
  begin
    Con_Io.Clear (Name => Help_Win);
    case Help is
      when Released =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.Put_Line ("Select :",                      Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A color to set",            Name => Help_Win);
        Con_Io.Put_Line ("     it in a proposition.",     Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A proposition to clear",    Name => Help_Win);
        Con_Io.Put_Line ("     or move it.",              Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A try to get answer",       Name => Help_Win);
        Con_Io.Put_Line ("     to a try.",                Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A menu option :",           Name => Help_Win);
        Con_Io.Put_Line ("     Give-up.",                 Name => Help_Win);
        Con_Io.Put_Line ("     Exit.",                    Name => Help_Win);
      when Click_Color =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.Put_Line ("Release on :",                  Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A position to affect it",   Name => Help_Win);
        Con_Io.Put_Line ("     at this position.",        Name => Help_Win);
      when Click_Propal =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.Put_Line ("Release on :",                  Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> an empty square to move",   Name => Help_Win);
        Con_Io.Put_Line ("     this color.",              Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> outside propositions",      Name => Help_Win);
        Con_Io.Put_Line ("     to clear this color.",     Name => Help_Win);
      when Click_Other =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win, Number => 2);
        Con_Io.Put_Line ("Release on the same item",      Name => Help_Win);
        Con_Io.Put_Line (" to validate.",                 Name => Help_Win);
      when Start =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.Put_Line ("Select :",                      Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line (" -> A menu option :",           Name => Help_Win);
        Con_Io.Put_Line ("     Start.",                   Name => Help_Win);
        Con_Io.Put_Line ("     Level (3, 4 or 5).",       Name => Help_Win);
        Con_Io.Put_Line ("     Exit.",                    Name => Help_Win);
      when Discarded =>
        Con_Io.Move (Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win, Number => 2);
        Con_Io.Put_Line ("WRONG CLICK POSITION",          Name => Help_Win);
        Con_Io.New_Line (Name => Help_Win);
        Con_Io.Put_Line ("Release anywhere",              Name => Help_Win);
        Con_Io.Put_Line (" and click again.",             Name => Help_Win);
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

    Con_Io.Move ((Con_Io.Row_Range(Color) - 1) * 2, 0, Name => Color_Win);
    Square := Con_Io.Position (Color_Win);
    Square := Con_Io.To_Absolute (Square, Color_Win);
    Square := Con_Io.To_Relative (Square, Global_Win);

    -- Draw frame of square
    Con_Io.Move (Square.Row - 1, Square.Col - 1, Name => Global_Win);
    Con_Io.Put (Chars(3) & Chars (1) & Chars (1) & Chars(3),
                Foreground => Fore, Name => Global_Win);
    Con_Io.Move (Square.Row + 1, Square.Col - 1, Name => Global_Win);
    Con_Io.Put (Chars(3) & Chars (1) & Chars (1) & Chars(3),
                Foreground => Fore, Name => Global_Win);
    Con_Io.Move (Square.Row, Square.Col - 1, Name => Global_Win);
    Con_Io.Put (Chars(2), Foreground => Fore, Name => Global_Win);
    Con_Io.Move (Square.Row, Square.Col + 2, Name => Global_Win);
    Con_Io.Put (Chars(2), Foreground => Fore, Name => Global_Win);

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

    begin
      Square := Con_Io.To_Relative (Where, Propal_Win);
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
    exception
      when Con_Io.Invalid_Square => null;
    end;

    begin
      Square := Con_Io.To_Relative (Where, Try_Win);
      if Square.Row mod 2 /= 0 then
        What.Selection := Try;
        return;
      end if;
      What := (Selection_Kind => Try,
               Try_No => To_Propal (Square.Row) );
    exception
      when Con_Io.Invalid_Square => null;
    end;

    begin
      Square := Con_Io.To_Relative (Where, Color_Win);
      if Square.Row mod 2 /= 0 then
        What.Selection := Color;
        return;
      end if;
      What := (
       Selection_Kind => Color,
       Color_No => Common.Eff_Color_Range (1 + (Square.Row / 2)) );
    exception
      when Con_Io.Invalid_Square => null;
    end;

    begin
      Square := Con_Io.To_Relative (Where, Menu_Win);
      What := (Selection_Kind => Menu);
    exception
      when Con_Io.Invalid_Square => null;
    end;

    begin
      Square := Con_Io.To_Relative (Where, Level_Win);
      What.Selection := Level;
      if (Square.Col) mod 2 /= 0 then
        return;
      end if;
      What := (
       Selection_Kind => Level,
       Level_No => Common.Last_Level_Range (
        (Square.Col - 2) / 2 + Integer (Common.Last_Level_Range'First)) );
    exception
      when Con_Io.Invalid_Square => null;
      when Constraint_Error => null;
    end;

    begin
      Square := Con_Io.To_Relative (Where, Exit_Win);
      What := (Selection_Kind => Exit_Game);
    exception
      when Con_Io.Invalid_Square => null;
    end;

  end Get_Selected;

end Screen;

