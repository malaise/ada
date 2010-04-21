-- Common types and current state
package Common is

  -- Kind of game
  type Game_Kind_List is (Nim, Marienbad);
  procedure Set_Game_Kind (Game_Kind : in Game_Kind_List);
  procedure Switch_Game_Kind;
  function Get_Game_Kind return Common.Game_Kind_List;

  -- Scores
  subtype Score_Range is Natural;
  type Player_List is (Human, Machine);
  type Score_Array is array (Player_List) of Score_Range;
  procedure Add_Win (Player : in Player_List);
  function Get_Scores return Score_Array;

  -- The bars
  subtype Row_Range is Positive range 1 .. 4;
  subtype Full_Bar_Range is Natural range 0 .. 7;
  subtype Bar_Range is Full_Bar_Range range 1 .. Full_Bar_Range'Last;
  type Bar_Status_Array is array (Bar_Range) of Boolean;
  procedure Reset_Bars;
  No_Bar_Here : exception;
  procedure Remove_Bars (Row : in Common.Row_Range;
                         Remove : in Bar_Status_Array);
  function Get_Bars (Row : Common.Row_Range) return Bar_Status_Array;

  -- Result of a machine play
  type Result_List is (Won, Lost, Played_And_Won, Played_And_Lost, Played);
  subtype Played_Result_List is Result_List range Played_And_Won .. Played;

end Common;

