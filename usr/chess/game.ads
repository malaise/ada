with Space, Players;

package Game is

  procedure Init (Color : in Space.Color_List);

  -- Valid actions
  subtype Action_Rec is Players.Action_Rec(True);

  -- Status of move
  -- Nok       : Our king would be in check
  -- Ok        : Move ok and opponent king not in check
  -- Check     : Move ok and opponent king is in check
  -- Stalemate : Move ok and opponent king is stalemate
  -- CheckMate : Move ok and opponent king is checkmate
  type Move_Status_List is (Nok, Ok, Check, Stalemate, Checkmate);

  -- Check a move is valid (no King in chess) and commit
  function Do_Move (Action : Action_Rec) return Move_Status_List;

end Game;

