with Common, Compute;

package Screen is

  Exit_Requested : exception;

  -- Intro and choice of game kind
  function Intro return Common.Game_Kind_List;

  -- Init of a game
  procedure Reset (Game : in Common.Game_Kind_List;
                   Scores : in Common.Score_Array);

  -- Let human play
  procedure Play (Row : out Common.Row_Range;
                  Bars : out Common.Bar_Status_Array);

  -- Update according machine played
  procedure Update (Row : in Common.Row_Range;
                    Bars : in Common.Bar_Status_Array);

  -- Show end of a game
  procedure End_Game (Result : in Common.Played_Result_List;
                      Change_Game : out Boolean);

end Screen;

