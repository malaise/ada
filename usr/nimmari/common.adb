-- Common types and current state
package body Common is

  -- Kind of game
  -- type Game_Kind_List is (Nim, Marienbad);
  Curr_Game_Kind : Game_Kind_List := Nim;
  procedure Set_Game_Kind (Game_Kind : in Game_Kind_List) is
  begin
    Curr_Game_Kind := Game_Kind;
  end Set_Game_Kind;

  procedure Switch_Game_Kind is
  begin
    if Curr_Game_Kind = Nim then
      Curr_Game_Kind := Marienbad;
    else
      Curr_Game_Kind := Nim;
    end if;
  end Switch_Game_Kind;

  function Get_Game_Kind return Common.Game_Kind_List is
  begin
    return Curr_Game_Kind;
  end Get_Game_Kind;

  -- Scores
  -- subtype Score_Range is Natural;
  Curr_Score : Score_Array := (others => 0);
  procedure Add_Win (Player : in Player_List) is
  begin
    Curr_Score(Player) := Curr_Score(Player) + 1;
  end Add_Win;

  function Get_Scores return Score_Array is
  begin
    return Curr_Score;
  end Get_Scores;

  -- Bars
  type Bar_Mattrix is array (Row_Range) of Bar_Status_Array;
  Bars : Bar_Mattrix := (others => (others => False));
  procedure Reset_Bars is
  begin
    Bars := (1 => (True,  True,  True,  True,  True,  True,  True),
             2 => (False, True,  True,  True,  True,  True,  False),
             3 => (False, False, True,  True,  True,  False, False),
             4 => (False, False, False, True,  False, False, False));
  end Reset_Bars;

  procedure Remove_Bars (Row : in Common.Row_Range;
                         Remove : in Bar_Status_Array) is
  begin
    for I in Remove'Range loop
      if Remove(I) then
        if not Bars(Row)(I) then
          raise No_Bar_Here;
        end if;
        Bars(Row)(I) := False;
      end if;
    end loop;
  end Remove_Bars;

  function Get_Bars (Row : Common.Row_Range) return Bar_Status_Array is
  begin
    return Bars(Row);
  end Get_Bars;

end Common;

