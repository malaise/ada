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

  function Get_Game_Kind return Common.Game_Kind_List is (Curr_Game_Kind);

  -- Scores
  -- subtype Score_Range is Natural;
  Curr_Score : Score_Array := (others => 0);
  procedure Add_Win (Player : in Player_List) is
  begin
    Curr_Score(Player) := Curr_Score(Player) + 1;
  end Add_Win;

  function Get_Scores return Score_Array is (Curr_Score);

  -- Bars
  type Bar_Mattrix is array (Row_Range) of Bar_Status_Array;
  Init_Bars : constant Bar_Mattrix
            := (1 => (True,  True,  True,  True,  True,  True,  True),
                2 => (False, True,  True,  True,  True,  True,  False),
                3 => (False, False, True,  True,  True,  False, False),
                4 => (False, False, False, True,  False, False, False));
  Bars : Bar_Mattrix := Init_Bars;
  procedure Reset_Bars is
  begin
    Bars := Init_Bars;
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
    (Bars(Row));

  function Nb_Init_Bars (Row : Row_Range) return Bar_Range is
    (Nb_Bars (Init_Bars(Row)));

  function Nb_Bars (Status : Bar_Status_Array) return Full_Bar_Range is
    Res : Common.Full_Bar_Range := 0;
  begin
    for Stat of reverse Status loop
      if Stat then
        Res := Res + 1;
      end if;
    end loop;
    return Res;
  end Nb_Bars;

  function Index2Row_Col (Index : Index_Range) return Row_Col_Rec is
    (case Index is
       when 01 .. 07 => (1, Bar_Range(Index)),
       when 08 .. 12 => (2, Bar_Range(Index - 6)),
       when 13 .. 15 => (3, Bar_Range(Index - 10)),
       when 16       => (4, 4));

  function Row_Col2Index (Row_Col : Row_Col_Rec) return Index_Range is
    (case Row_Col.Row is
       when 1 => Afpx.Absolute_Field_Range(Row_Col.Col),
       when 2 => Afpx.Absolute_Field_Range(Row_Col.Col + 6),
       when 3 => Afpx.Absolute_Field_Range(Row_Col.Col + 6 + 4),
       when 4 => Afpx.Absolute_Field_Range(Row_Col.Col + 6 + 4 + 2));

  function Get_Cols (Row : Row_Range) return Cols_Rec is
    (case Row is
       when 1 => (1, 7),
       when 2 => (2, 6),
       when 3 => (3, 5),
       when 4 => (4, 4));

  function Get_Indexes (Row : Row_Range) return Indexes_Rec is
   (case Row is
       when 1 => (01, 07),
       when 2 => (08, 12),
       when 3 => (13, 15),
       when 4 => (16, 16));

end Common;

