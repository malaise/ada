with Space, Players;

package Screen is

  -- (Re)display the board
  procedure Display_Board;

  -- Update some squares of the board
  procedure Update_Board (Squares : in Space.Square_Array);

  -- Get a movement
  function Get (Color : Space.Color_List) return Players.Action_Rec;

  -- Put message and wait (a bit or until return)
  procedure Put (Color : Space.Color_List;
                 Message : in String; Ack : in Boolean := False);

  -- Close the board
  procedure Close;

end Screen;

