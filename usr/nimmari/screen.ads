with Common, Compute;

package Screen is

  Exit_Requested : exception;

  function Intro return Common.Game_List;


  procedure Reset (Game : in Common.Game_List);

  procedure Play;

  function Content (Row : Common.Row_Range) return Common.Full_Bar_Range;


  procedure Update (Row : in Common.Row_Range; Bars : in Common.Full_Bar_Range;
                    Result : in Compute.Result_List; Change_Game : out Boolean);

  procedure Score (Human, Machine : in Natural);

end Screen;
