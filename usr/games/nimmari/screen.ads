with Common;
package Screen is

  -- Intro and choice of game kind
  function Intro return Common.Game_Kind_List;

  -- Init of a game
  procedure Reset;

  -- Let human play
  procedure Play (Row : out Common.Row_Range;
                  Remove : out Common.Bar_Status_Array);

  -- Update according machine played
  procedure Update (Row : in Common.Row_Range;
                    Remove : in Common.Bar_Status_Array);

  -- Show end of a game
  procedure End_Game (Result : in Common.Done_Result_List;
                      Change_Game : out Boolean);

end Screen;

