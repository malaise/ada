with Space, Pieces;
package Players is

  -- Build list of possible movements
  procedure Think (Color : in Space.Color_List);

  type Action_Rec (Valid : Boolean := False) is record
    case Valid is
      when True =>
        Piece : Pieces.Piece_Kind_List := Pieces.Pawn;
        From  : Space.Square_Coordinate := Space.Origin;
        To    : Pieces.Action_Rec;
      when False => null;
    end case;
  end record;
  subtype Valid_Action_Rec is Action_Rec(True);
  Valid_Action : constant Valid_Action_Rec := (Valid => True, others => <>);

  -- Sequential access : Returns a not Valid Action when end of list
  procedure Rewind_Actions (Color : in Space.Color_List);
  function  Next_Action    (Color : Space.Color_List) return Action_Rec;

  -- Direct access :  Returns a not Valid Action when not in list
  function Get_Action (Color : Space.Color_List;
                       Index : Positive) return Action_Rec;

  -- Find an action (Move, Castle, Take, Take_En_Passant, Promote, Take_And_Promote) matching
  --  from and to. Not valid if not found.
  -- Promotions are searched if (and only if) Promote is a Promotion_Piece_List
  -- No more than one should match
  More_Than_One : exception;
  function Find_Action (Color : Space.Color_List;
                        From, To : Space.Square_Coordinate;
                        Promote  : Pieces.Piece_Kind_List) return Action_Rec;

  -- Check that a valid action exists the the list of a player
 function Action_Exists (Color : Space.Color_List;
                         Action : Valid_Action_Rec) return Boolean;

end Players;

