with Ada.Characters.Latin_1;
with Normal, Upper_Str, Upper_Char, Basic_Proc;
package body Text is

  Lf : constant Character := Ada.Characters.Latin_1.Lf;
  procedure Put (Str : in String) renames Basic_Proc.Put_Output;
  procedure Put_Line (Str : in String) renames Basic_Proc.Put_Line_Output;
  procedure New_Line renames Basic_Proc.New_Line_Output;

  -- Get a String of fixed lenght
  -- Returns a string filled with Lf on error
  function Get (Length : Positive) return String is
    Str : constant String := Basic_Proc.Get_Line;
    Err : constant String (1 .. Length) := (others => Lf);
  begin
    if Str'Length /= Length then
      return Err;
    end if;
    return Str;
  end Get;

  -- Intro and choice of game kind
  function Intro return Common.Game_Kind_List is
    Str : String (1 .. 1);
  begin
    Put_Line ("This game presents several sticks aligned on four rows:");
    Put_Line (" - seven sticks in the first row,");
    Put_Line (" - five in the second,");
    Put_Line (" - three in the third,");
    Put_Line (" - and one in the fourth.");
    New_Line;

    Put_Line ("Each of us at his turn will remove some sticks, as many as we");
    Put_Line (" want with only two constraints:");
    Put_Line (" - we must take at least one stick at each turn,");
    Put_Line (" - at each turn, all the sticks we take must be on the same row.");
    New_Line;

    Put_Line ("I always play first at the beginning of a game, which gives you");
    Put_Line (" the possibility to win each game.");
    New_Line;

    Put_Line ("Two variants are available:");
    Put_Line (" - at Nim, the winner is the one who takes the last stick,");
    Put_Line (" - at Marienbad, the player who takes the last stick loses.");
    New_Line;

    loop
      Put ("Do you want to play Nim (N) or Marienbad (M)? ");
      Str := Upper_Str (Get (1));
      if Str = "N" then
        return Common.Nim;
      elsif Str = "M" then
        return Common.Marienbad;
      end if;
    end loop;
  end Intro;

  function Row_Image (Row : Common.Row_Range) return Character is
  begin
    return (Character'Val (Character'Pos ('A') + Row - 1));
  end Row_Image;

  procedure Put_Title is
    Scores : constant Common.Score_Array := Common.Get_Scores;
    use type Common.Game_Kind_List;
  begin
    New_Line;
    if Common.Get_Game_Kind = Common.Nim then
      Put ("   Nim   ");
    else
      Put ("Marienbad");
    end if;
    Put ("       ");
    Put ("You: " & Normal (Scores(Common.Human), 3));
    Put ("  ");
    Put_Line ("Me: " & Normal (Scores(Common.Machine), 3));
  end Put_Title;

  procedure Put_Bars is
    Status : Common.Bar_Status_Array;
  begin
    for Row in Common.Row_Range loop
      Status := Common.Get_Bars (Row);
      Put (Row_Image (Row) & ": ");
      for Col in Common.Bar_Range loop
        if Status(Col) then
          Put ("I ");
        else
          Put ("  ");
        end if;
      end loop;
      New_Line;
    end loop;
  end Put_Bars;

  -- Let human play
  procedure Play (Row : out Common.Row_Range;
                  Remove : out Common.Bar_Status_Array) is
    Str : String (1 .. 2);
    Ok : Boolean;
    Status : Common.Bar_Status_Array;
    Nb_To_Remove : Common.Bar_Range;
    Nb_Removed : Common.Full_Bar_Range;
  begin
    Put_Title;
    Put_Bars;
    loop
      Put ("Enter the row and the number to remove (ex a2): ");
      Str := Upper_Str (Get (2));
      -- Check Row is OK
      Ok := False;
      for I in Common.Row_Range loop
        if Row_Image (I) = Upper_Char (Str(1)) then
          Row := I;
          Ok := True;
          exit;
        end if;
      end loop;
      if Ok then
        -- Check Nb is valid
        begin
          Nb_To_Remove := Common.Bar_Range'Value (Str(2) & "");
        exception
          when others =>
            Ok := False;
        end;
      end if;
      if Ok then
        -- Check Nb is not too big
        Status := Common.Get_Bars (Row);
        if Nb_To_Remove > Common.Nb_Bars (Status) then
          Ok := False;
        end if;
      end if;
      exit when Ok;
    end loop;
    -- Remove the first bars
    Nb_Removed := 0;
    Remove := (others => False);
    for Col in Remove'Range loop
      if Status(Col) then
        Remove(Col) := True;
        Nb_Removed := Nb_Removed + 1;
        exit when Nb_Removed = Nb_To_Remove;
      end if;
    end loop;
  end Play;

  -- Update according machine played
  procedure Update (Row : in Common.Row_Range;
                    Remove : in Common.Bar_Status_Array) is
    Nb_Removed : Common.Bar_Range;
  begin
    Put_Title;
    Put_Bars;
    Nb_Removed := Common.Nb_Bars (Remove);
    Put_Line ("I play " & Row_Image (Row) & Normal (Nb_Removed, 1));
  end Update;

  -- Show end of a game
  procedure End_Game (Result : in Common.Done_Result_List;
                      Change_Game : out Boolean) is
    Str : String (1 .. 1);
    use type Common.Game_Kind_List, Common.Done_Result_List;
  begin
    Put_Title;
    if Result = Common.Played_And_Won or else Result = Common.Won then
      Put_Line ("I win :-)");
    else
      Put_Line ("You win :-(");
    end if;
    loop
      if Common.Get_Game_Kind = Common.Nim then
        Put ("Enter M to play Marienbad, Return to go on playing Nim");
      else
        Put ("Enter N to play Nim, Return to go on playing Marienbad");
      end if;
      Put (" or Q or X to Quit: ");
      Str := Upper_Str (Get (1));
      if (Common.Get_Game_Kind = Common.Marienbad and then Str(1) = 'N')
      or else (Common.Get_Game_Kind = Common.Nim and then Str(1) = 'M') then
        Change_Game := True;
        exit;
      elsif Str(1) = 'Q' or else Str(1) = 'X' then
        raise Common.Exit_Requested;
      elsif Str(1) = Lf then
        Change_Game := False;
        exit;
      end if;
    end loop;
  end End_Game;

end Text;

