with Space, Players, Game;

package Screen is

  -- (Re)display the board
  procedure Display_Board (Color : Space.Color_List);

  -- Update some squares of the board
  procedure Update_Board (Color : Space.Color_List; 
                          Squares : in Space.Square_Array);

  -- Reset time for a new get or a new wait
  procedure Reset_Time;

  -- Get a movement
  function Get (Color : Space.Color_List) return Players.Action_Rec;

  -- Put message and wait (a bit or until return)
  -- Time is erased if Ack
  procedure Put (Color : Space.Color_List;
                 Message : in String; Ack : in Boolean := False);

  -- Put a move
  procedure Put_Move (Color  : in Space.Color_List;
                      Action : in Game.Action_Rec;
                      Result : in Game.Move_Status_List);

  -- Wait a bit
  procedure Wait (Color : Space.Color_List; Delay_Ms : in Natural);

  -- Close the board
  procedure Close;

end Screen;

