with Con_Io;
with Cards;
package Table is

  package Deck renames Cards.Deck;

  -- The main window
  Console : aliased Con_Io.Console;

  -- Needs to be called only once, create the table, move the stacks, cards, menu...
  procedure Init;

  -- Stacks
  subtype Stack_Range is Deck.Name_Range;
  -- 4 Cards, the last one being the Ace of a complete color
  subtype Depth_Range is Natural range 1 .. 3 + Deck.Name_Range'Last;

  -- Number of empty stacks, from 0 to 13-9
  subtype Empty_Stacks_Range is Natural range 0 .. 9;
  Empty_Stacks : Empty_Stacks_Range;

  -- Position (X, Y) of card within a stack
  function Pos_Of (Stack : Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec;


  -- Wait for next event, return Play or the reason for exiting the game
  type Action_List is (Play, Quit, New_Game, Restart);
  function Wait_Event return Action_List;

end Table;

